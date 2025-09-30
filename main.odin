package main

import "core:c"
import "core:fmt"
import "core:log"
import "core:math"
import "core:math/linalg"
import "core:math/rand"
import "core:strings"
import "core:thread"
import "core:unicode/utf8"
import mu "vendor:microui"
import rl "vendor:raylib"

GROWABLE :: bit_set[Particle_Kind]{.NONE, .WATER}

// TODO: check if liquid particles of the same kind ignore each other (performance)
// TODO: update_x" should just set velocity and move proc should be called by update_particle, at the end
// TODO: i think particle velocities don't get reset after the particle stops moving? (especially sand and lava)
// TODO: particle kinds: DIRT->MUD, ICE, STEAM, FLAMMABLE GAS, MOSS
// TODO: freefall bool (https://www.youtube.com/watch?v=5Ka3tbbT-9E&t=1s) (makes particles not freak out as they're falling) (makes it easier to know which particles are at rest/inactive) -> active/inactive zones
// TODO: spark particles for fire/lava/corruption/acid etc.
// TODO: black smoke for oil burning
// TODO: salt should approach water's color, not invisibility, as it dissolves
// TODO: coal -> sand, but black and burns easily
// TODO: wood (stationary)

// TODO: (HARD) send grid as an array of RGBA (32-bit) values to a shader instead of rendering each rectangle separately.

// TODO: considee using a bitset with a list of updated (ex. moved-to) xy cells (instead of storing time information for each particle) (the point is to avoid processing the same particle in the same frame after it has moved to another position)

// TODO: stop using alpha and use brigtness only - much better for performance
// TODO: use `nil` instead of `.None` (though this doesn't allow us to blindly index arrays that are indexed by the enum type)
// TODO: particle.brightness should be a float, so the rendering functino can control what ex. 0.5 actually means in terms of *pixel* brightness (rename to particle.brightness_noise)
// TODO: different kinds of smoke (black smoke for burning oil, white steam instead of smoke for water evaporation, etc.)

/**
Noita like particle/sand simulation.
**/

LIQUIDS :: bit_set[Particle_Kind]{.WATER, .LAVA, .OIL}
USER_HIDDED_KINDS :: bit_set[Particle_Kind]{.ASH, .NONE}

MIN_THREAD_X_STEP :: 20

WINDOW_WIDTH :: 800
WINDOW_HEIGHT :: 600

GRID_WIDTH :: WINDOW_WIDTH / 3
GRID_HEIGHT :: WINDOW_HEIGHT / 3
CELL_WIDTH :: WINDOW_WIDTH / GRID_WIDTH
CELL_HEIGHT :: WINDOW_HEIGHT / GRID_HEIGHT

WATER_GRAVITY :: 4

SAND_GRAVITY_ACCELERATION :: 0.1
WATER_GRAVITY_ACCELERATION :: 0.5
ACID_GRAVITY_ACCELERATION :: 0.3

SAND_SLIDE_OFF_CHANCE :: 0.05
LIQUID_SPREAD_CHANCE :: 0.7

WATER_SPREAD_DISTANCE :: 3

SMOKE_LIFETIME :: 250
SALT_LIFETIME :: 255
ACID_LIFETIME :: 200

CORRUPTION_SPREAD_CHANCE :: 0.02

SPRINKLE_RADIUS_MIN :: 3
SPRINKLE_RADIUS_MAX :: 35

SPRINKLE_DENSITY_MIN :: 0.01
SPRINKLE_DENSITY_MAX :: 0.1

particle_kind_keyboard_keys := [Particle_Kind]rl.KeyboardKey {
	.SAND       = .ONE,
	.WATER      = .TWO,
	.STONE      = .THREE,
	.FIRE       = .FOUR,
	.SALT       = .FIVE,
	.ACID       = .SIX,
	.LAVA       = .SEVEN,
	.OIL        = .EIGHT,
	.CORRUPTION = .NINE,
	.SEED       = .ZERO,
	.VINE       = .KEY_NULL,
	.NONE       = .KEY_NULL,
	.SMOKE      = .KEY_NULL,
	.ASH        = .KEY_NULL,
}

colors := [Particle_Kind]rl.Color {
	.SAND       = rl.YELLOW,
	.WATER      = rl.BLUE,
	.SMOKE      = rl.GRAY,
	.FIRE       = rl.ORANGE,
	.SALT       = rl.WHITE,
	.ACID       = rl.GREEN,
	.LAVA       = rl.RED,
	.STONE      = rl.GRAY,
	.OIL        = rl.BROWN,
	.CORRUPTION = rl.PURPLE,
	.SEED       = rl.BROWN,
	.VINE       = rl.DARKGREEN,
	.NONE       = rl.BLACK,
	.ASH        = rl.GRAY,
}

Particle_Kind :: enum {
	NONE,
	SAND,
	WATER,
	SMOKE,
	FIRE,
	SALT,
	ACID,
	LAVA,
	STONE,
	OIL,
	CORRUPTION,
	SEED,
	VINE,
	ASH,
}

// TODO: use union/bitfields for storing different fields for different kinds of particles
Particle :: struct {
	kind:                  Particle_Kind,
	velocity:              Velocity,
	brightness:            u8,
	updated_frame:         u16, // TODO: updated_frame is bad (elapses every 255 frames) (maybe store the updated_frame in an external array of u16/u32)
	lifetime:              u8,
	fire_should_leave_ash: bool, // TODO: separate kinds for ash-leaving fire and non-ash-leaving fire
}

UNSELECTABLE_PARTICLE_KINDS :: bit_set[Particle_Kind]{.SMOKE, .VINE}

particle_kind_names := [Particle_Kind]string {
	.SAND       = "Sand",
	.WATER      = "Water",
	.SMOKE      = "Smoke",
	.FIRE       = "Fire",
	.SALT       = "Salt",
	.ACID       = "Acid",
	.LAVA       = "Lava",
	.STONE      = "Stone",
	.OIL        = "Oil",
	.CORRUPTION = "Corruption",
	.SEED       = "Seed",
	.VINE       = "Vine",
	.NONE       = "None",
	.ASH        = "Ash",
}

// game state
state := struct {
	// low-level
	pool:                      thread.Pool,
	// raylib
	scanlines_shader:          rl.Shader,
	screen_texture:            rl.RenderTexture2D,
	mu_atlas_texture:          rl.RenderTexture2D, // used by microui for font rendering. why? not sure.
	debug_texture:             rl.RenderTexture2D, // for debug shapes etc.
	// game
	grid:                      [GRID_WIDTH][GRID_HEIGHT]Particle,
	frame:                     u16,
	selected_particle_kind:    Particle_Kind,
	sprinkle_radius:           i32,
	sprinkle_density:          f32,
	mouse_pos, prev_mouse_pos: Mouse_Pos,
	// ui settings
	scanlines_enabled:         bool,
	show_thread_boundaries:    bool,
	show_fps:                  bool,
	// microui
	mu_ctx:                    mu.Context,
	log_buf:                   [1 << 16]byte, // TODO: remove all logging and bg
	log_buf_len:               int,
	log_buf_updated:           bool,
	bg:                        mu.Color,
} {
	selected_particle_kind = .SAND,
	sprinkle_radius        = (SPRINKLE_RADIUS_MIN + SPRINKLE_RADIUS_MAX) / 2,
	sprinkle_density       = (SPRINKLE_DENSITY_MIN + SPRINKLE_DENSITY_MAX) / 2,
	bg                     = {90, 95, 100, 255},
}

flammability := [Particle_Kind]f32 {
	.OIL        = 0.9,
	.CORRUPTION = 0.15,
	.SEED       = 0.1,
	.VINE       = 0.3,
	.SAND       = 0,
	.WATER      = 0,
	.SMOKE      = 0,
	.FIRE       = 0,
	.SALT       = 0,
	.ACID       = 0,
	.LAVA       = 0,
	.STONE      = 0,
	.NONE       = 0,
	.ASH        = 0,
}

corrodability := [Particle_Kind]f32 {
	.WATER      = 0.01,
	.SAND       = 0.2,
	.SMOKE      = 0.2,
	.FIRE       = 0.05,
	.SALT       = 0.1,
	.OIL        = 0.2,
	.CORRUPTION = 0.1,
	.SEED       = 0.2,
	.VINE       = 0.4,
	.STONE      = 0.2,
	.ACID       = 0,
	.LAVA       = 0.01,
	.NONE       = 0,
	.ASH        = 0,
}

valid_x :: proc(x: i32) -> bool {
	return x >= 0 && x < GRID_WIDTH
}
valid_y :: proc(y: i32) -> bool {
	return y >= 0 && y < GRID_HEIGHT
}

valid_pos :: proc(pos: Grid_Pos) -> bool {
	return pos.x >= 0 && pos.x < GRID_WIDTH && pos.y >= 0 && pos.y < GRID_HEIGHT
}

get :: proc(pos: Grid_Pos) -> ^Particle {
	if valid_pos(pos) {
		return &state.grid[pos.x][pos.y]
	}
	return nil
}

set :: proc(pos: Grid_Pos, particle: Particle) {
	if valid_pos(pos) {
		state.grid[pos.x][pos.y] = particle
	}
}

random_i32 :: proc(a, b: i32) -> i32 {
	return rand.int31_max(b - a) + a
}

has_neighbor_of_kind :: proc(pos: Grid_Pos, kinds: bit_set[Particle_Kind]) -> bool {
	for offset in neighbouring_offsets {
		if exists_cell_of_kind(pos + offset, kinds) {
			return true
		}
	}
	return false
}

leaves_ash :: proc(kind: Particle_Kind) -> bool {
	return kind in bit_set[Particle_Kind]{.VINE, .SEED, .SAND, .STONE}
}

// Vector2f :: [2]f32
Velocity :: distinct [2]f32

make_particle :: proc(kind: Particle_Kind, velocity_offset: Velocity = {0, 0}) -> Particle {
	vel_x := f32(rand.float32_range(-1, 1))
	vel_y := f32(rand.float32_range(-1, 0))
	vel := velocity_offset + {vel_x, vel_y}

	MIN_BRIGHTNESS :: 200
	brightness := u8(random_i32(MIN_BRIGHTNESS, 255))

	switch kind {
	case .SMOKE:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(150, 250)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .FIRE:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(100, 180)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .ACID:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(50, 200)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .WATER:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(50, 200)),
			brightness = brightness,
		}
	case .LAVA:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(255),
			brightness = u8(random_i32(40, 180)),
			updated_frame = state.frame,
		}
	case .OIL:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(255),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .CORRUPTION:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(120, 200)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .SEED:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(20, 50)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .VINE:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(20, 50)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .ASH:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(random_i32(10, 20)),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .NONE:
		return {kind = kind, updated_frame = state.frame}
	case .STONE:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(255),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .SAND:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(255),
			brightness = brightness,
			updated_frame = state.frame,
		}
	case .SALT:
		return {
			kind = kind,
			velocity = vel,
			lifetime = u8(255),
			brightness = brightness,
			updated_frame = state.frame,
		}
	}
	unreachable()
}

is_within_radius :: proc(pos: [2]i32, radius: i32) -> bool {
	distance_squared := pos.x * pos.x + pos.y * pos.y
	return distance_squared <= radius * radius
}

add_particles_in_radius :: proc(
	center: Grid_Pos,
	kind: Particle_Kind,
	radius: i32,
	density: f32,
	velocity_offset: Velocity,
) {
	density := density

	for dx in -radius ..= radius {
		for dy in -radius ..= radius {
			if !is_within_radius({dx, dy}, radius) do continue

			particle := get(center + {dx, dy})

			if particle == nil do continue
			if particle.kind != .NONE do continue

			if rand.float32() < density {
				particle^ = make_particle(kind, velocity_offset)
			}
		}
	}
}

erase_particles_in_radius :: proc(pos: Grid_Pos) {
	for dx in -state.sprinkle_radius ..= state.sprinkle_radius {
		for dy in -state.sprinkle_radius ..= state.sprinkle_radius {
			if !is_within_radius({dx, dy}, state.sprinkle_radius) do continue

			particle := get(pos + {dx, dy})

			if particle != nil && particle.kind != .NONE {
				particle^ = {}
			}
		}
	}
}

// returns starting position if there are no empty cells along the way
// TODO: if difference between src and dest is 1, maybe we should just check that neighbour wihout doing any steps?
find_last_empty_cell_along_vector :: proc(
	src, dest: Grid_Pos,
	passable_particle_kinds: bit_set[Particle_Kind],
) -> (
	result: Grid_Pos,
	ok: bool,
) {
	steps := i32(math.max(math.abs(dest.x - src.x), math.abs(dest.y - src.y)))
	if steps == 0 {
		return src, false
	}
	dx := (f32(dest.x) - f32(src.x)) / f32(steps)
	dy := (f32(dest.y) - f32(src.y)) / f32(steps)
	cur_x := f32(src.x)
	cur_y := f32(src.y)
	ix_prev := src.x
	iy_prev := src.y

	for i in 0 ..= steps {
		cur_x += dx
		cur_y += dy
		ix := i32(math.floor(cur_x))
		iy := i32(math.floor(cur_y))

		if !valid_x(ix) || !valid_y(iy) {
			return Grid_Pos{ix_prev, iy_prev}, true
		}

		if state.grid[ix][iy].kind not_in passable_particle_kinds {
			return Grid_Pos{ix_prev, iy_prev}, true
		}

		ix_prev = ix
		iy_prev = iy
	}

	return Grid_Pos{ix_prev, iy_prev}, true
}

move_particle :: proc(pos: Grid_Pos, passable_particle_kinds: bit_set[Particle_Kind]) {
	particle := get(pos)

	pos_offset := Grid_Pos{i32(particle.velocity.x), i32(particle.velocity.y)}
	if pos_offset.x == 0 && pos_offset.y == 0 {
		return
	}

	new_pos, _ := find_last_empty_cell_along_vector(pos, pos + pos_offset, passable_particle_kinds)
	swap(pos, new_pos)

	target := &state.grid[new_pos.x][new_pos.y]
	target.updated_frame = state.frame

	// TODO: consider storing to a new field called "moved_time" (to separate "update" from "movement" so we can haze zones with inactivity tracking to prevent updating chunks with no movement)
}

swap :: proc(a, b: Grid_Pos) {
	state.grid[b.x][b.y], state.grid[a.x][a.y] = state.grid[a.x][a.y], state.grid[b.x][b.y]
}

exists_cell_of_kind :: proc(pos: Grid_Pos, kinds: bit_set[Particle_Kind]) -> bool {
	p := get(pos)
	return p != nil && p.kind in kinds
}

neighbouring_offsets := [8]Grid_Pos {
	{1, 1},
	{-1, -1},
	{-1, 1},
	{1, -1},
	{0, -1},
	{0, 1},
	{1, 0},
	{-1, 0},
}

mouse_buttons_map := [mu.Mouse]rl.MouseButton {
	.LEFT   = .LEFT,
	.RIGHT  = .RIGHT,
	.MIDDLE = .MIDDLE,
}

key_map := [mu.Key][2]rl.KeyboardKey {
	.SHIFT     = {.LEFT_SHIFT, .RIGHT_SHIFT},
	.CTRL      = {.LEFT_CONTROL, .RIGHT_CONTROL},
	.ALT       = {.LEFT_ALT, .RIGHT_ALT},
	.BACKSPACE = {.BACKSPACE, .KEY_NULL},
	.DELETE    = {.DELETE, .KEY_NULL},
	.RETURN    = {.ENTER, .KP_ENTER},
	.LEFT      = {.LEFT, .KEY_NULL},
	.RIGHT     = {.RIGHT, .KEY_NULL},
	.HOME      = {.HOME, .KEY_NULL},
	.END       = {.END, .KEY_NULL},
	.A         = {.A, .KEY_NULL},
	.X         = {.X, .KEY_NULL},
	.C         = {.C, .KEY_NULL},
	.V         = {.V, .KEY_NULL},
}

mu_capturing_user_input :: proc() -> bool {
	return state.mu_ctx.hover_root != nil || state.mu_ctx.focus_id != 0
}

TYPICAL_FALL_TARGETS :: LIQUIDS + {.NONE, .SMOKE}

velocity_to_offset :: proc(velocity: Velocity) -> Grid_Pos {
	return Grid_Pos{i32(velocity.x), i32(velocity.y)}
}

update_sandlike_particle :: proc(pos: Grid_Pos, particle: ^Particle) {
	TARGETS :: TYPICAL_FALL_TARGETS

	can_fall_down := exists_cell_of_kind(pos + {0, -1}, TARGETS)

	if !can_fall_down {
		if rand.float32() < SAND_SLIDE_OFF_CHANCE {
			kick_strength := rand.float32_range(1, 2)
			kick_down := rand.float32_range(-2, -1)

			if exists_cell_of_kind(pos + {1, -1}, TARGETS) {
				particle.velocity = {f32(kick_strength), f32(kick_down)}
			} else if exists_cell_of_kind(pos + {-1, -1}, TARGETS) {
				particle.velocity = {f32(-kick_strength), f32(kick_down)}
			}
		}
	}

	in_liquid := exists_cell_of_kind(pos + {0, 1}, LIQUIDS)

	particle.velocity.y -= f32(SAND_GRAVITY_ACCELERATION)
	particle.velocity.x *= 0.85
	if in_liquid {
		particle.velocity *= 0.9
	}

	turbulence := rand.float32() < f32(0.1 if !in_liquid else 0.75)
	if turbulence {
		particle.velocity.x += f32(rand.float32_range(-1, 1))
	}

	move_particle(pos, TARGETS)
}

update_water :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle.velocity.y -= f32(WATER_GRAVITY_ACCELERATION)
	particle.velocity.x *= 0.9

	if rand.float32() < 0.1 {
		particle.velocity.x += f32(rand.float32_range(-1, 1))
	}

	WATER_FALL_TARGETS :: TYPICAL_FALL_TARGETS - {.WATER, .LAVA}

	if exists_cell_of_kind(pos + {0, -1}, WATER_FALL_TARGETS) {
		move_particle(pos, WATER_FALL_TARGETS)
	} else {
		if exists_cell_of_kind(pos + {1, -1}, WATER_FALL_TARGETS) {
			particle.velocity = {1, -1}
			move_particle(pos, WATER_FALL_TARGETS)

		} else if exists_cell_of_kind(pos + {-1, -1}, WATER_FALL_TARGETS) {
			particle.velocity = {-1, -1}
			move_particle(pos, WATER_FALL_TARGETS)

		} else if exists_cell_of_kind(pos + {1, 0}, WATER_FALL_TARGETS) &&
		   rand.float32() < LIQUID_SPREAD_CHANCE {
			particle.velocity = {f32(rand.float32_range(1, WATER_SPREAD_DISTANCE)), 0}
			move_particle(pos, WATER_FALL_TARGETS)

		} else if exists_cell_of_kind(pos + {-1, 0}, WATER_FALL_TARGETS) &&
		   rand.float32() < LIQUID_SPREAD_CHANCE {
			particle.velocity = {-f32(rand.float32_range(1, WATER_SPREAD_DISTANCE)), 0}
			move_particle(pos, WATER_FALL_TARGETS)
		} else {
			particle.velocity = {0, 0}
		}
	}
}

update_salt :: proc(pos: Grid_Pos, particle: ^Particle) {
	if !exists_cell_of_kind(pos + {0, -1}, TYPICAL_FALL_TARGETS) {
		if rand.float32() < SAND_SLIDE_OFF_CHANCE {
			kick_strength := rand.float32_range(1, 2)
			kick_down := rand.float32_range(-2, -1)

			if exists_cell_of_kind(pos + {1, -1}, TYPICAL_FALL_TARGETS) {
				particle.velocity.x = f32(kick_strength)
				particle.velocity.y = f32(kick_down)
			} else if exists_cell_of_kind(pos + {-1, -1}, TYPICAL_FALL_TARGETS) {
				particle.velocity.x = f32(-kick_strength)
				particle.velocity.y = f32(kick_down)
			}
		}
	}

	in_liquid := exists_cell_of_kind(pos + {0, 1}, LIQUIDS)
	in_lava := exists_cell_of_kind(pos + {0, 1}, {.LAVA})

	particle.velocity.y -= f32(SAND_GRAVITY_ACCELERATION)
	particle.velocity.x *= 0.85

	if in_lava {
		particle.velocity *= 0.85
	}

	if in_liquid {
		particle.velocity *= 0.85
		particle.lifetime -= 1
		if particle.lifetime == 0 {
			state.grid[pos.x][pos.y] = {}
			return
		}
	}

	if in_liquid && rand.float32() < 0.8 {
		particle.velocity.x += f32(rand.float32_range(-2, 2))
	}

	move_particle(pos, TYPICAL_FALL_TARGETS)
}

update_acid :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle := &state.grid[pos.x][pos.y]

	if particle.lifetime > 0 {
		particle.lifetime -= 1
	}

	if particle.lifetime == 0 {
		state.grid[pos.x][pos.y] = {}
		return
	}

	particle.velocity.y -= f32(ACID_GRAVITY_ACCELERATION)
	particle.velocity.x *= 0.9

	if rand.float32() < 0.1 {
		particle.velocity.x += f32(rand.float32_range(-1, 1))
	}

	in_liquid := exists_cell_of_kind(pos + {0, 1}, LIQUIDS)

	VALID_FALL_TARGETS :: LIQUIDS + {.NONE}

	if in_liquid {
		particle.velocity *= 0.7
		particle.lifetime -= 1

		if rand.float32() < 0.2 {
			particle.velocity.x += f32(rand.float32_range(-3, 3))
		}

		if particle.lifetime == 0 {
			state.grid[pos.x][pos.y] = {}
			return
		}
	} else {
		if exists_cell_of_kind(pos + {0, -1}, VALID_FALL_TARGETS) {
			move_particle(pos, VALID_FALL_TARGETS)
		} else {
			if exists_cell_of_kind(pos + {1, -1}, VALID_FALL_TARGETS) {
				particle.velocity = {1, -1}
				move_particle(pos, VALID_FALL_TARGETS)

			} else if exists_cell_of_kind(pos + {-1, -1}, VALID_FALL_TARGETS) {
				particle.velocity = {-1, -1}
				move_particle(pos, VALID_FALL_TARGETS)

			} else if exists_cell_of_kind(pos + {1, 0}, VALID_FALL_TARGETS) &&
			   rand.float32() < LIQUID_SPREAD_CHANCE {
				particle.velocity = {f32(rand.float32_range(1, WATER_SPREAD_DISTANCE)), 0}
				move_particle(pos, VALID_FALL_TARGETS)

			} else if exists_cell_of_kind(pos + {-1, 0}, VALID_FALL_TARGETS) &&
			   rand.float32() < LIQUID_SPREAD_CHANCE {
				particle.velocity = {-f32(rand.float32_range(1, WATER_SPREAD_DISTANCE)), 0}
				move_particle(pos, VALID_FALL_TARGETS)
			} else {
				particle.velocity = {0, 0}
			}
		}
	}

	for offset in neighbouring_offsets {
		nx := pos.x + offset.x
		ny := pos.y + offset.y

		if valid_x(nx) && valid_y(ny) {
			if rand.float32() < corrodability[state.grid[nx][ny].kind] {
				state.grid[nx][ny] = {}
				particle.lifetime = u8(max(0, i32(particle.lifetime) - 20))
				if particle.lifetime == 0 {
					state.grid[pos.x][pos.y] = {}
					add_particles_in_radius({nx, ny}, .SMOKE, 2, 0.1, {0, 0})
					return
				}
			}
		}
	}
}

update_smoke :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle := &state.grid[pos.x][pos.y]

	if particle.lifetime > 0 {
		particle.lifetime -= 1
	} else {
		state.grid[pos.x][pos.y] = {}
		return
	}

	particle.velocity.y += f32(0.15) // Buoyancy force
	particle.velocity.x *= 0.95

	if rand.float32() < 0.3 {
		particle.velocity.x += f32(rand.float32_range(-0.5, 0.5))
	}

	if exists_cell_of_kind(pos + {0, 1}, {.NONE}) {
		move_particle(pos, {.NONE})
	} else {
		if exists_cell_of_kind(pos + {1, 1}, {.NONE}) {
			particle.velocity = {1, 1}
			move_particle(pos, {.NONE})

		} else if exists_cell_of_kind(pos + {-1, 1}, {.NONE}) {
			particle.velocity = {-1, 1}
			move_particle(pos, {.NONE})

		} else if exists_cell_of_kind(pos + {1, 0}, {.NONE}) && rand.float32() < 0.5 {
			particle.velocity = {f32(rand.float32_range(1, 2)), 0}
			move_particle(pos, {.NONE})

		} else if exists_cell_of_kind(pos + {-1, 0}, {.NONE}) && rand.float32() < 0.5 {
			particle.velocity = {f32(rand.float32_range(-2, -1)), 0}
			move_particle(pos, {.NONE})
		} else {
			particle.velocity = {0, 0}
		}
	}
}

update_fire :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle := &state.grid[pos.x][pos.y]

	if particle.lifetime > 0 {
		particle.lifetime -= 1
	}

	should_die := particle.lifetime == 0 || has_neighbor_of_kind(pos, {.WATER})

	if should_die {
		if rand.float32() < 0.8 {
			add_particles_in_radius(pos, .SMOKE, 10, 0.02, {0, 0})
		}
		if particle.fire_should_leave_ash && rand.float32() < 0.2 {
			add_particles_in_radius(pos, .ASH, 2, 0.1, {0, 0})
		}
		state.grid[pos.x][pos.y] = {}
		return
	}

	for offset in neighbouring_offsets {
		nx := pos.x + offset.x
		ny := pos.y + offset.y
		if valid_pos({nx, ny}) {
			target_particle := &state.grid[nx][ny]
			base_flammability := flammability[target_particle.kind]

			if base_flammability > 0 {
				spread_chance: f32
				if ny < pos.y {
					spread_chance = base_flammability * 0.15 // Downward spread
				} else if ny == pos.y {
					spread_chance = base_flammability * 0.10 // Horizontal spread
				} else {
					spread_chance = base_flammability * 0.02 // Rare upward spread
				}

				if rand.float32() < spread_chance {
					// Burn the particle, only create fire if there's underlying support
					// if exists_kind(nx, ny - 1, {.SAND, .FIRE, .OIL}) || ny == 0 {
					leaves_ash := leaves_ash(target_particle.kind)
					target_particle.kind = .FIRE
					target_particle.fire_should_leave_ash = leaves_ash
					target_particle.lifetime = u8(random_i32(40, 100))
					target_particle.brightness = u8(random_i32(200, 255))
					target_particle.velocity = {0, 0} // Fire doesn't move much
					// }
				}
			}
		}
	}

	particle.velocity.x *= 0.95
	particle.velocity.y = 0 // Keep fire grounded

	if rand.float32() < 0.3 {
		particle.velocity.x += f32(rand.float32_range(-0.2, 0.2))
	}

	// Produce smoke while burning - amount scales with flammability of what burned
	smoke_production_rate := 0.2 * flammability[.FIRE]
	if exists_cell_of_kind(pos + {0, -1}, {.SAND, .OIL, .CORRUPTION}) {
		fuel_kind := state.grid[pos.x][pos.y + 1].kind
		smoke_production_rate = 0.1 + 0.4 * flammability[fuel_kind]
	}

	if rand.float32() < smoke_production_rate && exists_cell_of_kind(pos + {0, 1}, {.NONE}) {
		// TODO: use set()
		state.grid[pos.x][pos.y + 1] = {
			kind          = .SMOKE,
			velocity      = {
				f32(rand.float32_range(-0.5, 0.5)),
				f32(rand.float32_range(1.0, 2.0)),
			},
			brightness    = u8(random_i32(100, 150)),
			updated_frame = state.frame,
			lifetime      = u8(random_i32(80, 200)),
		}
	}
}

update_lava :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle := &state.grid[pos.x][pos.y]

	for offset in neighbouring_offsets {
		nx := pos.x + offset.x
		ny := pos.y + offset.y
		target_cell := Grid_Pos{nx, ny}
		target := get(target_cell)

		valid_target := target != nil && target.kind not_in (bit_set[Particle_Kind]{.NONE, .LAVA})

		if valid_target {
			assert(target.kind != .NONE)

			if target.kind == .WATER {
				// turn both lava andwater to stone (lava cools instantly when touching water)
				// TODO: use make_particle
				target^ = {
					kind          = .STONE,
					velocity      = {0, 0}, // Stone doesn't move
					brightness    = u8(random_i32(40, 60)), // Dark gray stone color
					updated_frame = state.frame,
				}
				// also turn this lava particle to stone
				particle.kind = .STONE
				particle.velocity = {0, 0}
				particle.brightness = u8(random_i32(40, 60))
				particle.lifetime = 255
				add_particles_in_radius(pos, .STONE, 10, 0.5, {0, 0})
				add_particles_in_radius(pos, .SMOKE, 15, 0.2, {0, 0})
				return
			} else {
				flammability := flammability[target.kind]
				if flammability > 0 {
					target.kind = .FIRE
					target.lifetime = u8(random_i32(10, 20))
					target.brightness = u8(random_i32(200, 255))
					target.velocity = {
						f32(rand.float32_range(-1, 1)),
						f32(rand.float32_range(0.5, 1.5)),
					}
				} else {
					if target.kind == .STONE {
						if rand.float32() < 0.001 {
							target^ = make_particle(.NONE)
							add_particles_in_radius({nx, ny}, .SMOKE, 5, 0.1, {0, 0})
						}
					} else {
						if rand.float32() < 0.1 {
							if target.kind != .SMOKE {
								add_particles_in_radius({nx, ny}, .SMOKE, 5, 0.1, {0, 0})
							}
							target^ = {}
						}
					}
				}
			}
		}
	}

	particle.velocity.y -= f32(1.2)
	particle.velocity.x *= 0.85

	if rand.float32() < 0.1 {
		particle.velocity.x += f32(rand.float32_range(-0.8, 0.8))
	}

	LAVA_FALL_TARGETS :: TYPICAL_FALL_TARGETS - {.LAVA}

	if exists_cell_of_kind(pos + {0, -1}, LAVA_FALL_TARGETS) {
		move_particle(pos, LAVA_FALL_TARGETS)
	} else {
		if exists_cell_of_kind(pos + {1, -1}, LAVA_FALL_TARGETS) {
			particle.velocity = {1.5, -1.5}
			move_particle(pos, LAVA_FALL_TARGETS)

		} else if exists_cell_of_kind(pos + {-1, -1}, LAVA_FALL_TARGETS) {
			particle.velocity = {-1.5, -1.5}
			move_particle(pos, LAVA_FALL_TARGETS)

		} else if exists_cell_of_kind(pos + {1, 0}, LAVA_FALL_TARGETS) && rand.float32() < 0.2 {
			particle.velocity = {f32(rand.float32_range(1, 2)), 0}
			move_particle(pos, LAVA_FALL_TARGETS)

		} else if exists_cell_of_kind(pos + {-1, 0}, LAVA_FALL_TARGETS) && rand.float32() < 0.2 {
			particle.velocity = {f32(rand.float32_range(-2, -1)), 0}
			move_particle(pos, LAVA_FALL_TARGETS)
		} else {
			particle.velocity.x *= 0.7
			particle.velocity.y *= 0.7
		}
	}
}

update_oil :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle := &state.grid[pos.x][pos.y]

	particle.velocity.y -= f32(WATER_GRAVITY_ACCELERATION * 0.7)
	particle.velocity.x *= 0.85

	if rand.float32() < 0.05 {
		particle.velocity.x += f32(rand.float32_range(-0.5, 0.5))
	}

	// Oil floats on water - check if there's water below
	if exists_cell_of_kind(pos + {0, 1}, {.WATER}) {
		particle.velocity.y = f32(0.5)
		move_particle(pos, {.WATER})
	} else {
		if exists_cell_of_kind(pos + {0, -1}, {.NONE}) {
			move_particle(pos, {.NONE})
		} else {
			if exists_cell_of_kind(pos + {1, -1}, {.NONE}) {
				particle.velocity = {1, -1}
				move_particle(pos, {.NONE})

			} else if exists_cell_of_kind(pos + {-1, -1}, {.NONE}) {
				particle.velocity = {-1, -1}
				move_particle(pos, {.NONE})
			} else if exists_cell_of_kind(pos + {1, 0}, {.NONE}) &&
			   rand.float32() < LIQUID_SPREAD_CHANCE * 0.5 {
				particle.velocity = {f32(rand.float32_range(1, WATER_SPREAD_DISTANCE)), 0}
				move_particle(pos, {.NONE})

			} else if exists_cell_of_kind(pos + {-1, 0}, {.NONE}) &&
			   rand.float32() < LIQUID_SPREAD_CHANCE * 0.5 {
				particle.velocity = {-f32(rand.float32_range(1, WATER_SPREAD_DISTANCE)), 0}
				move_particle(pos, {.NONE})
			} else {
				particle.velocity = {0, 0}
			}
		}
	}
}

update_corruption :: proc(pos: Grid_Pos, particle: ^Particle) {
	particle := &state.grid[pos.x][pos.y]

	if particle.lifetime > 0 {
		particle.lifetime -= 1
	} else {
		state.grid[pos.x][pos.y] = {}
		return
	}

	if rand.float32() < 0.3 {
		particle.velocity.y += f32(rand.float32_range(-0.8, 0.6))
	} else {
		particle.velocity.y -= f32(0.3)
	}

	if rand.float32() < 0.4 {
		particle.velocity.x += f32(rand.float32_range(-0.9, 0.9))
	} else {
		particle.velocity.x *= 0.7
	}

	if rand.float32() < 0.05 {
		particle.velocity.x += f32(rand.float32_range(-2.5, 2.5))
		particle.velocity.y += f32(rand.float32_range(-1.5, 1.5))
	}

	for offset in neighbouring_offsets {
		nx := pos.x + offset.x
		ny := pos.y + offset.y
		if valid_pos({nx, ny}) {
			target := &state.grid[nx][ny]
			if target.kind not_in (bit_set[Particle_Kind]{.CORRUPTION, .NONE}) &&
			   rand.float32() < CORRUPTION_SPREAD_CHANCE {
				// Convert to corruption
				target.kind = .CORRUPTION
				target.lifetime = u8(random_i32(80, 200))
				target.brightness = u8(random_i32(100, 250))
				target.velocity = {
					f32(rand.float32_range(-10, 10)),
					f32(rand.float32_range(-10, 10)),
				}
			}
		}
	}

	direction := rand.int31_max(3)
	moved := false

	switch direction {
	case 0:
		if exists_cell_of_kind(pos + {0, -1}, {.NONE, .WATER}) && !moved {
			move_particle(pos, {.NONE, .WATER})
			moved = true
		}
	case 1:
		if exists_cell_of_kind(pos + {0, 1}, {.NONE}) && !moved && rand.float32() < 0.2 {
			move_particle(pos, {.NONE})
			moved = true
		}
	case 2:
		if !moved {
			dir_x := rand.int31_max(3) - 1 // -1, 0, 1
			dir_y := rand.int31_max(3) - 1
			if exists_cell_of_kind(pos + {dir_x, dir_y}, {.NONE, .WATER}) {
				particle.velocity = {f32(dir_x), f32(dir_y)}
				move_particle(pos, {.NONE, .WATER})
				moved = true
			}
		}
	case 3:
		if rand.float32() < 0.1 && !moved {
			jump_x := rand.int31_max(3) + pos.x - 1
			jump_y := rand.int31_max(3) + pos.y - 1
			if valid_pos({jump_x, jump_y}) &&
			   exists_cell_of_kind({jump_x, jump_y}, {.NONE, .WATER}) {
				particle.velocity = {f32(jump_x - pos.x), f32(jump_y - pos.y)}
				move_particle(pos, {.NONE, .WATER})
				moved = true
			}
		}
	case:
		unreachable()
	}

	if rand.float32() < 0.02 && !moved {
		solid_break := random_i32(0, 3)
		switch solid_break {
		case 0:
			if exists_cell_of_kind(pos + {-1, 0}, {.SAND, .SALT}) {
				particle.velocity = {-1, 0}
				move_particle(pos, {.SAND, .SALT})
			}
		case 1:
			if exists_cell_of_kind(pos + {1, 0}, {.SAND, .SALT}) {
				particle.velocity = {1, 0}
				move_particle(pos, {.SAND, .SALT})
			}
		case 2:
			if exists_cell_of_kind(pos + {0, -1}, {.SAND, .SALT}) {
				particle.velocity = {0, -1}
				move_particle(pos, {.SAND, .SALT})
			}
		case 3:
			if exists_cell_of_kind(pos + {0, 1}, {.SAND, .SALT}) {
				particle.velocity = {0, 1}
				move_particle(pos, {.SAND, .SALT})
			}
		case:
			unreachable()
		}
	}
}

update_seed :: proc(pos: Grid_Pos, particle: ^Particle) {
	if exists_cell_of_kind(pos + {0, -1}, {.NONE, .WATER, .SMOKE}) {
		particle.velocity.y -= f32(SAND_GRAVITY_ACCELERATION)
		particle.velocity.x *= 0.85
		if rand.float32() < 0.1 {
			particle.velocity.x += f32(rand.float32_range(-1, 1))
		}
		move_particle(pos, {.NONE, .WATER, .SMOKE})
	} else {
		if rand.float32() < 0.02 { 	// 2% chance per frame to sprout
			if exists_cell_of_kind(pos + {0, 1}, GROWABLE) {
				state.grid[pos.x][pos.y + 1] = {
					kind          = .VINE,
					velocity      = {0, 0},
					brightness    = u8(random_i32(80, 120)),
					updated_frame = state.frame,
					lifetime      = u8(random_i32(20, 50)),
				}
				// Remove the seed
				state.grid[pos.x][pos.y] = {}
				return
			}
		}
	}
}

update_vine :: proc(pos: Grid_Pos, particle: ^Particle) {
	water_nearby := has_neighbor_of_kind(pos, {.WATER})

	growth_multiplier: f32 = water_nearby ? 10 : 1

	// Vine grows using lifetime as growth potential
	// lifetime = 0 means no more growth possible
	// Each vine can only grow once - after creating a new vine, lifetime becomes 0
	if particle.lifetime > 0 {
		if rand.float32() < 0.01 * growth_multiplier &&
		   exists_cell_of_kind(pos + {0, 1}, GROWABLE) {
			// Grow upward
			// TODO: use set()
			state.grid[pos.x][pos.y + 1] = {
				kind          = .VINE,
				velocity      = {0, 0},
				brightness    = u8(random_i32(80, 120)), // Darker green for new growth
				updated_frame = state.frame,
				lifetime      = u8(random_i32(20, 50)),
			}
			particle.lifetime = 0 // This vine is done growing
		}

		if rand.float32() < 0.005 * growth_multiplier {
			// Try to spread left or right
			spread_direction := i32(rand.int31_max(2) == 0 ? -1 : 1)
			if exists_cell_of_kind(pos + {spread_direction, 0}, GROWABLE) {
				state.grid[pos.x + spread_direction][pos.y] = {
					kind          = .VINE,
					velocity      = {0, 0},
					brightness    = u8(random_i32(100, 140)),
					updated_frame = state.frame,
					lifetime      = u8(random_i32(20, 50)),
				}
				particle.lifetime = 0
			}
		}

		if rand.float32() < 0.003 * growth_multiplier &&
		   exists_cell_of_kind(pos + {0, -1}, GROWABLE) &&
		   (exists_cell_of_kind(pos + {-1, 0}, {.VINE, .SAND, .STONE}) ||
				   exists_cell_of_kind(pos + {1, 0}, {.VINE, .SAND, .STONE})) {
			state.grid[pos.x][pos.y - 1] = {
				kind          = .VINE,
				velocity      = {0, 0},
				brightness    = u8(random_i32(90, 130)),
				updated_frame = state.frame,
				lifetime      = u8(random_i32(20, 50)),
			}
			particle.lifetime = 0
		}
	}
}

update_particle :: proc(pos: Grid_Pos) {
	particle := &state.grid[pos.x][pos.y]

	if particle.kind == .NONE do return
	if particle.updated_frame == state.frame do return // skip particles that have already been updated this frame

	// particle.updated_frame = state.frame

	pos_at_floor := pos.y == 0

	if pos_at_floor {
		particle.velocity.y = min(particle.velocity.y, 0)
	}

	switch particle.kind {

	case .SAND:
		update_sandlike_particle(pos, particle)

	case .WATER:
		update_water(pos, particle)

	case .ASH:
		update_sandlike_particle(pos, particle)

	case .SALT:
		update_salt(pos, particle)

	case .ACID:
		update_acid(pos, particle)

	case .SMOKE:
		update_smoke(pos, particle)

	case .FIRE:
		update_fire(pos, particle)

	case .LAVA:
		update_lava(pos, particle)

	case .STONE: // no physics

	case .OIL:
		update_oil(pos, particle)

	case .CORRUPTION:
		update_corruption(pos, particle)

	case .SEED:
		update_seed(pos, particle)

	case .VINE:
		update_vine(pos, particle)

	case .NONE:
		unreachable()
	}
}

THREAD_COUNT :: 4
#assert(THREAD_COUNT % 2 == 0) // makes it easier to spread threads evenly

// TODO: thrading doens't impact performance much, why?
update_particles_threaded :: proc() {
	// we're only processing HALF of the zones (in separate threads) at a time, this is why we need to multiply by 2
	VERTICAL_ZONES :: THREAD_COUNT * 2

	thread_x_step := i32(GRID_WIDTH / VERTICAL_ZONES)
	assert(thread_x_step >= MIN_THREAD_X_STEP)

	thread_xs: [VERTICAL_ZONES][2]i32
	for i in i32(0) ..< VERTICAL_ZONES {
		start := thread_x_step * i
		// make sure the last thread covers all remaining columns
		end := GRID_WIDTH if i == VERTICAL_ZONES - 1 else thread_x_step * (i + 1)
		thread_xs[i] = [2]i32{start, end}
		if state.show_thread_boundaries {
			rl.DrawLine(end * CELL_WIDTH, 0, end * CELL_WIDTH, WINDOW_HEIGHT, rl.RED)
		}
	}

	task :: proc(task: thread.Task) {
		x_range := (^[2]i32)(task.data)
		assert(x_range.x >= 0)
		assert(x_range.y <= GRID_WIDTH)
		for x in x_range.x ..< x_range.y {
			for y in i32(0) ..< GRID_HEIGHT {
				update_particle({x, y})
			}
		}
	}

	// each column should be as distances from others as possible, so particle moving between each thread borders don't get processed at the same time
	// this is why we process even first, then wait, then process odd, then wait again
	for i := i32(0); i < VERTICAL_ZONES; i += 2 {
		thread.pool_add_task(&state.pool, context.allocator, task, rawptr(&thread_xs[i]))
	}
	thread.pool_finish(&state.pool)

	for i := i32(1); i < VERTICAL_ZONES; i += 2 {
		thread.pool_add_task(&state.pool, context.allocator, task, rawptr(&thread_xs[i]))
	}
	thread.pool_finish(&state.pool)
}

broadcast_rl_inputs_to_mu :: proc() {
	// mouse buttons
	for button_rl, button_mu in mouse_buttons_map {
		switch {
		case rl.IsMouseButtonPressed(button_rl):
			mu.input_mouse_down(&state.mu_ctx, state.mouse_pos.x, state.mouse_pos.y, button_mu)
		case rl.IsMouseButtonReleased(button_rl):
			mu.input_mouse_up(&state.mu_ctx, state.mouse_pos.x, state.mouse_pos.y, button_mu)
		}
	}

	// keyboard keys
	for keys_rl, key_mu in key_map {
		for key_rl in keys_rl {
			switch {
			case key_rl == .KEY_NULL: // ignore
			case rl.IsKeyPressed(key_rl), rl.IsKeyPressedRepeat(key_rl):
				mu.input_key_down(&state.mu_ctx, key_mu)
			case rl.IsKeyReleased(key_rl):
				mu.input_key_up(&state.mu_ctx, key_mu)
			}
		}
	}
}

Grid_Pos :: distinct [2]i32
Mouse_Pos :: distinct [2]i32

mouse_pos_to_grid_pos :: #force_inline proc(pos: Mouse_Pos) -> Grid_Pos {
	return Grid_Pos{pos.x / CELL_WIDTH, (GRID_HEIGHT - 1) - (pos.y / CELL_HEIGHT)}
}

main :: proc() {
	logger := log.create_console_logger()
	context.logger = logger

	thread.pool_init(&state.pool, context.allocator, THREAD_COUNT)
	defer thread.pool_finish(&state.pool)

	// consider intergrating raylib's logger using `rl.SetTraceLogCallback()`
	rl.SetTraceLogLevel(.WARNING)

	// init raylib
	rl.InitWindow(800, 600, "Sand")
	defer rl.CloseWindow()

	rl.SetTargetFPS(60)

	state.screen_texture = rl.LoadRenderTexture(WINDOW_WIDTH, WINDOW_HEIGHT)
	defer rl.UnloadRenderTexture(state.screen_texture)

	state.debug_texture = rl.LoadRenderTexture(WINDOW_WIDTH, WINDOW_HEIGHT)
	defer rl.UnloadRenderTexture(state.debug_texture)

	state.scanlines_shader = rl.LoadShader(nil, "shaders/scanlines.fs")
	defer rl.UnloadShader(state.scanlines_shader)
	ensure(rl.IsShaderValid(state.scanlines_shader))
	ensure(rl.IsShaderReady(state.scanlines_shader))
	// end init raylib

	// init microui
	mu.init(&state.mu_ctx)
	state.mu_ctx.text_width = mu.default_atlas_text_width
	state.mu_ctx.text_height = mu.default_atlas_text_height

	state.mu_atlas_texture = rl.LoadRenderTexture(
		c.int(mu.DEFAULT_ATLAS_WIDTH),
		c.int(mu.DEFAULT_ATLAS_HEIGHT),
	)
	defer rl.UnloadRenderTexture(state.mu_atlas_texture)

	image := rl.GenImageColor(
		c.int(mu.DEFAULT_ATLAS_WIDTH),
		c.int(mu.DEFAULT_ATLAS_HEIGHT),
		rl.Color{0, 0, 0, 0},
	)
	defer rl.UnloadImage(image)

	for alpha, i in mu.default_atlas_alpha {
		x := i % mu.DEFAULT_ATLAS_WIDTH
		y := i / mu.DEFAULT_ATLAS_WIDTH
		color := rl.Color{255, 255, 255, alpha}
		rl.ImageDrawPixel(&image, c.int(x), c.int(y), color)
	}

	rl.BeginTextureMode(state.mu_atlas_texture)
	rl.UpdateTexture(state.mu_atlas_texture.texture, rl.LoadImageColors(image))
	rl.EndTextureMode()
	// end init microui

	// main loop
	for !rl.WindowShouldClose() {
		state.frame += 1
		free_all(context.temp_allocator)

		rl.BeginTextureMode(state.debug_texture)
		rl.ClearBackground(rl.Color{0, 0, 0, 0})

		state.prev_mouse_pos = state.mouse_pos
		state.mouse_pos = Mouse_Pos{rl.GetMouseX(), rl.GetMouseY()}

		// mouse position is {0,0} if the cursor didnt't move at all since raylib window was created. not a bug.
		mu.input_mouse_move(&state.mu_ctx, state.mouse_pos.x, state.mouse_pos.y)

		mu.input_scroll(&state.mu_ctx, 0, i32(rl.GetMouseWheelMove()) * -30) // TODO: float->int might cause problems if move < 1

		// particle kinds keyboard shortcuts
		for key, particle_kind in particle_kind_keyboard_keys {
			if rl.IsKeyPressed(key) {
				state.selected_particle_kind = particle_kind
				break
			}
		}

		broadcast_rl_inputs_to_mu()

		mu.begin(&state.mu_ctx)
		update_mu(&state.mu_ctx)
		mu.end(&state.mu_ctx)

		if !mu_capturing_user_input() {
			// sprinkle radius
			if rl.GetMouseWheelMove() > 0 {
				state.sprinkle_radius += 1
			} else if rl.GetMouseWheelMove() < 0 {
				state.sprinkle_radius -= 1
			}
			state.sprinkle_radius = clamp(
				state.sprinkle_radius,
				SPRINKLE_RADIUS_MIN,
				SPRINKLE_RADIUS_MAX,
			)

			if rl.IsMouseButtonDown(.LEFT) && valid_pos(mouse_pos_to_grid_pos(state.mouse_pos)) {
				// TODO: in order for this to look good particles would have to keep their float positions as a field (integer-only position makes upward/sideways physics look really bad)
				// mouse_delta := state.mouse_pos - state.prev_mouse_pos
				// mouse_delta.y = -mouse_delta.y
				// factor :: 0.05
				add_particles_in_radius(
					mouse_pos_to_grid_pos(state.mouse_pos),
					state.selected_particle_kind,
					state.sprinkle_radius,
					state.sprinkle_density if state.selected_particle_kind != .STONE else 1,
					// Velocity{f32(mouse_delta.x) * factor, f32(mouse_delta.y) * factor},
					{0, 0},
				)
			} else if rl.IsMouseButtonDown(.RIGHT) &&
			   valid_pos(mouse_pos_to_grid_pos(state.mouse_pos)) {
				erase_particles_in_radius(mouse_pos_to_grid_pos(state.mouse_pos))
			}
		}

		update_particles_threaded()

		rl.EndTextureMode() // end debug texture

		render_game_to_texture()

		// render state texture with custom shaders
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)
		window_width: f32
		window_height: f32
		if state.scanlines_enabled {
			window_width = WINDOW_WIDTH
			window_height = WINDOW_HEIGHT
			rl.SetShaderValue(
				state.scanlines_shader,
				rl.GetShaderLocation(state.scanlines_shader, "renderWidth"),
				rawptr(&window_width),
				.FLOAT,
			)
			rl.SetShaderValue(
				state.scanlines_shader,
				rl.GetShaderLocation(state.scanlines_shader, "renderHeight"),
				rawptr(&window_height),
				.FLOAT,
			)
			rl.BeginShaderMode(state.scanlines_shader)
		}
		rl.DrawTextureRec(
			texture = state.screen_texture.texture,
			source = {0, 0, f32(WINDOW_WIDTH), -f32(WINDOW_HEIGHT)},
			position = {0, 0},
			tint = rl.WHITE,
		)
		rl.EndShaderMode()

		rl.DrawTextureRec(
			texture = state.debug_texture.texture,
			source = {0, 0, f32(WINDOW_WIDTH), -f32(WINDOW_HEIGHT)},
			position = {0, 0},
			tint = rl.WHITE,
		)

		render_mu_to_screen()

		rl.EndDrawing()
	}
}

render_particles_activity :: proc() {
	for x in i32(0) ..< GRID_WIDTH {
		for y in i32(0) ..< GRID_HEIGHT {
			particle := &state.grid[x][y]
			render_x := x * CELL_WIDTH
			render_y := (GRID_HEIGHT - 1 - y) * CELL_HEIGHT

			updated := particle.updated_frame == state.frame

			color := rl.GREEN if updated else rl.RED
			color.a = 100

			rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)
		}
	}
}

render_particles :: proc() {
	for x in i32(0) ..< GRID_WIDTH {
		for y in i32(0) ..< GRID_HEIGHT {
			render_x := x * CELL_WIDTH
			render_y := (GRID_HEIGHT - 1 - y) * CELL_HEIGHT

			particle := &state.grid[x][y]

			switch particle.kind {

			case .ASH:
				color := rl.ColorAlphaBlend(rl.BLACK, rl.GRAY, particle.brightness)
				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .SAND:
				color := rl.ColorAlphaBlend(rl.BLACK, rl.YELLOW, particle.brightness)
				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .SALT:
				color := rl.ColorAlphaBlend(rl.BLACK, rl.WHITE, particle.brightness)

				t := f32(particle.lifetime) / SALT_LIFETIME
				alpha := u8(math.lerp(f32(0), f32(255), t))

				color.a = alpha

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .ACID:
				brightness := rand.choice([]u8{220, 230, 240, 250})

				color := rl.Color{0, 255, 255 / 3, 255}
				color = rl.ColorAlphaBlend(rl.BLACK, color, brightness)

				alpha := u8(math.lerp(f32(150), f32(255), f32(particle.lifetime) / ACID_LIFETIME))
				color.a = alpha

				if (rand.float32() < 0.001) {
					color = rl.ColorAlphaBlend(rl.WHITE, color, 230)
				}

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .WATER:
				brightness := rand.choice([]u8{220, 230, 240, 250})

				color := rl.Color{0, brightness / 3, brightness, 255}

				if (rand.float32() < 0.001) {
					color = rl.ColorAlphaBlend(rl.WHITE, color, 230)
				}

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .SMOKE:
				t := f32(particle.lifetime) / SMOKE_LIFETIME
				alpha := u8(math.lerp(f32(0), f32(255), t))

				brightness1 := rand.choice([]u8{220, 230, 240, 250})
				brightness2 := u8(math.lerp(f32(0), f32(255), t))
				brightness := u8((f32(brightness1) + f32(brightness2)) / 2)

				color := rl.ColorAlphaBlend(rl.BLACK, rl.WHITE, brightness)
				color.a = alpha

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .FIRE:
				color := rand.choice([]rl.Color{rl.ORANGE, rl.RED, rl.YELLOW})
				color.a -= u8(random_i32(0, 200))

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .LAVA:
				base_color: rl.Color

				color_determinant := i32(particle.brightness)
				color_determinant += random_i32(-25, 25)

				if color_determinant < i32(255 / 3) {
					base_color = rl.RED
				} else if color_determinant < i32(255 * 2 / 3) {
					base_color = rl.ORANGE
				} else {
					base_color = rl.YELLOW
				}

				noise := u8(random_i32(225, 250))

				color := rl.ColorAlphaBlend(rl.BLACK, base_color, noise)

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .STONE:
				t := f32(particle.brightness) / 255
				brightness := u8(math.lerp(f32(20), f32(50), t))

				color := rl.Color{brightness, brightness, brightness, 255}
				color.g = u8(min(int(color.g), int(particle.brightness - 5)))

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .OIL:
				// brightness := particle.brightness
				// if rand.float32() < 0.02 {
				// 	brightness -= u8(random_i32(0, 30))
				// }

				// color := rl.BROWN
				// color = rl.ColorAlphaBlend(rl.BLACK, color, brightness)

				// rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

				color := rl.BROWN
				noise := rand.int31_max(50)
				color.a = 255 - u8(noise)

				if (rand.float32() < 0.01) {
					color = rl.ColorAlphaBlend(rl.WHITE, color, 230)
				}

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .CORRUPTION:
				base_brightness := int(state.grid[x][y].brightness)

				noise := i32(rand.int31_max(100))
				color_i32 := [3]i32 {
					i32(base_brightness) + noise,
					i32(f32(base_brightness) * 0.6) + noise / 2,
					i32(base_brightness) + rand.int31_max(80),
				}

				color_i32.r = clamp(color_i32.r, 50, 255)
				color_i32.g = clamp(color_i32.g, 20, 200)
				color_i32.b = clamp(color_i32.b, 30, 255)

				// pulse
				if rand.float32() < 0.5 {
					OFFSET :: 150

					color_i32.r += random_i32(-OFFSET, OFFSET)
					color_i32.g += random_i32(-OFFSET, OFFSET)
					color_i32.b += random_i32(-OFFSET, OFFSET)

					color_i32.r = clamp(color_i32.r, 0, 255)
					color_i32.g = clamp(color_i32.g, 0, 255)
					color_i32.b = clamp(color_i32.b, 0, 255)
				}

				t := f32(particle.lifetime) / 255
				alpha := u8(math.lerp(f32(100), f32(255), t))

				color := rl.Color{u8(color_i32.r), u8(color_i32.g), u8(color_i32.b), alpha}

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .SEED:
				brightness := particle.brightness

				color := rl.BROWN
				color = rl.ColorAlphaBlend(rl.BLACK, color, brightness)

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .VINE:
				base_brightness := int(particle.brightness)

				r_brightness := i32(base_brightness) / 4
				g_brightness := i32(base_brightness)
				b_brightness := i32(base_brightness) / 3

				r_brightness = clamp(r_brightness, 10, 80)
				g_brightness = clamp(g_brightness, 50, 200)
				b_brightness = clamp(b_brightness, 10, 80)

				color := rl.Color{u8(r_brightness), u8(g_brightness), u8(b_brightness), 255}

				if particle.lifetime < 50 {
					t := f32(particle.lifetime) / 50
					alpha := u8(math.lerp(f32(180), f32(255), t))
					color.a = alpha
				}

				rl.DrawRectangle(render_x, render_y, CELL_WIDTH, CELL_HEIGHT, color)

			case .NONE:
			}
		}
	}
}

render_game_to_texture :: proc() {
	rl.BeginTextureMode(state.screen_texture)
	defer rl.EndTextureMode()

	rl.ClearBackground(rl.BLACK)

	if state.show_fps {
		rl.DrawFPS(WINDOW_WIDTH - 100, 10)
	}

	mouse_x := rl.GetMouseX()
	mouse_y := rl.GetMouseY()

	render_particles()
	// render_particles_activity()

	if !mu_capturing_user_input() {
		// render sprinkle radius
		color := colors[state.selected_particle_kind]
		rl.DrawCircleLines(mouse_x, mouse_y, f32(state.sprinkle_radius * CELL_HEIGHT), color)
	}


}

to_rl_color :: proc "contextless" (in_color: mu.Color) -> (out_color: rl.Color) {
	return {in_color.r, in_color.g, in_color.b, in_color.a}
}

to_mu_color :: proc "contextless" (in_color: rl.Color) -> (out_color: mu.Color) {
	return {in_color.r, in_color.g, in_color.b, in_color.a}
}

button :: proc(particle_kind: Particle_Kind) {
	prev_colors := state.mu_ctx.style.colors
	defer state.mu_ctx.style.colors = prev_colors

	state.mu_ctx.style.colors[.TEXT] = to_mu_color(rl.BLACK)
	state.mu_ctx.style.colors[.BUTTON] = to_mu_color(colors[particle_kind])
	state.mu_ctx.style.colors[.BUTTON_FOCUS] = to_mu_color(
		rl.ColorBrightness(colors[particle_kind], 2),
	)
	state.mu_ctx.style.colors[.BUTTON_HOVER] = to_mu_color(
		rl.ColorBrightness(colors[particle_kind], 0.5),
	)
	if .SUBMIT in mu.button(&state.mu_ctx, particle_kind_names[particle_kind]) {
		state.selected_particle_kind = particle_kind
	}
}

slider :: proc(ctx: ^mu.Context, val: ^$T, lo, hi: T, fmt_string: string) -> (res: mu.Result_Set) {
	mu.push_id(ctx, uintptr(val))

	@(static) tmp: mu.Real
	tmp = mu.Real(val^)
	res = mu.slider(ctx, &tmp, mu.Real(lo), mu.Real(hi), 0, fmt_string, {.ALIGN_CENTER})
	val^ = T(tmp)
	mu.pop_id(ctx)
	return
}

update_mu :: proc(ctx: ^mu.Context) {
	@(static) opts := mu.Options{.NO_CLOSE, .NO_RESIZE, .AUTO_SIZE} // .AUTO_SIZE

	MU_WINDOW_WIDTH :: 350
	MU_WINDOW_HEIGHT :: 280

	if mu.window(ctx, "Menu", {10, 10, MU_WINDOW_WIDTH, MU_WINDOW_HEIGHT}, opts) {
		mu.label(ctx, "Particle Types:")
		mu.layout_row(ctx, {0, 0, 0, 0}, 0)
		for kind in Particle_Kind {
			if kind not_in USER_HIDDED_KINDS {
				button(kind)
			}
		}

		mu.layout_row(ctx, {0, 0, 0, 0}, 0)
		mu.label(ctx, "Radius:")
		slider(ctx, &state.sprinkle_radius, SPRINKLE_RADIUS_MIN, SPRINKLE_RADIUS_MAX, "%.0f")
		mu.label(ctx, "Density:")
		slider(ctx, &state.sprinkle_density, SPRINKLE_DENSITY_MIN, SPRINKLE_DENSITY_MAX, "%.2f")
		mu.layout_row(ctx, {0, 150, 0}, 0)
		mu.checkbox(ctx, "Scanlines", &state.scanlines_enabled)
		mu.checkbox(ctx, "Thread boundaries", &state.show_thread_boundaries)
		mu.checkbox(ctx, "Show FPS", &state.show_fps)
	}
}

write_log :: proc(str: string) {
	state.log_buf_len += copy(state.log_buf[state.log_buf_len:], str)
	state.log_buf_len += copy(state.log_buf[state.log_buf_len:], "\n")
	state.log_buf_updated = true
}

read_log :: proc() -> string {
	return string(state.log_buf[:state.log_buf_len])
}
reset_log :: proc() {
	state.log_buf_updated = true
	state.log_buf_len = 0
}

render_texture :: proc "contextless" (
	renderer: rl.RenderTexture2D,
	dst: ^rl.Rectangle,
	src: mu.Rect,
	color: rl.Color,
) {
	dst.width = f32(src.w)
	dst.height = f32(src.h)

	rl.DrawTextureRec(
		texture = state.mu_atlas_texture.texture,
		source = {f32(src.x), f32(src.y), f32(src.w), f32(src.h)},
		position = {dst.x, dst.y},
		tint = color,
	)
}

render_mu_to_screen :: proc "contextless" () {
	height := rl.GetScreenHeight()

	// rl.BeginTextureMode(state.screen_texture)
	// defer rl.EndTextureMode()
	rl.EndScissorMode()

	command_backing: ^mu.Command
	for variant in mu.next_command_iterator(&state.mu_ctx, &command_backing) {
		switch cmd in variant {
		case ^mu.Command_Text:
			dst := rl.Rectangle{f32(cmd.pos.x), f32(cmd.pos.y), 0, 0}
			for ch in cmd.str {
				if ch & 0xc0 != 0x80 {
					r := min(int(ch), 127)
					src := mu.default_atlas[mu.DEFAULT_ATLAS_FONT + r]
					render_texture(state.screen_texture, &dst, src, to_rl_color(cmd.color))
					dst.x += dst.width
				}
			}
		case ^mu.Command_Rect:
			rl.DrawRectangle(
				cmd.rect.x,
				cmd.rect.y,
				cmd.rect.w,
				cmd.rect.h,
				to_rl_color(cmd.color),
			)
		case ^mu.Command_Icon:
			src := mu.default_atlas[cmd.id]
			x := cmd.rect.x + (cmd.rect.w - src.w) / 2
			y := cmd.rect.y + (cmd.rect.h - src.h) / 2
			render_texture(
				state.screen_texture,
				&rl.Rectangle{f32(x), f32(y), 0, 0},
				src,
				to_rl_color(cmd.color),
			)
		case ^mu.Command_Clip:
			rl.BeginScissorMode(
				cmd.rect.x,
				height - (cmd.rect.y + cmd.rect.h),
				cmd.rect.w,
				cmd.rect.h,
			)
		case ^mu.Command_Jump:
			unreachable()
		}
	}
	// rl.EndTextureMode()
}
