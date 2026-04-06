#+build !js
package main

import "core:fmt"

import SDL "vendor:sdl3"
import "vendor:wgpu"
import "vendor:wgpu/sdl3glue"

OS :: struct {
	window: ^SDL.Window,
}

os_init :: proc() {
	if !SDL.Init({.VIDEO}) {
		fmt.panicf("SDL.Init error: ", SDL.GetError())
	}

	state.os.window = SDL.CreateWindow(
		"WGPU Native Triangle",
		960,
		540,
		{.RESIZABLE, .HIGH_PIXEL_DENSITY},
	)
	if state.os.window == nil {
		fmt.panicf("SDL.CreateWindow error: ", SDL.GetError())
	}
}

os_run :: proc() {
	now := SDL.GetPerformanceCounter()
	last: u64
	dt: f32
	main_loop: for {
		last = now
		now = SDL.GetPerformanceCounter()
		dt = f32((now - last) * 1000) / f32(SDL.GetPerformanceFrequency())

		state.mouse_wheel = 0
		e: SDL.Event
		for SDL.PollEvent(&e) {
			#partial switch (e.type) {
			case .QUIT:
				break main_loop
			case .WINDOW_RESIZED, .WINDOW_PIXEL_SIZE_CHANGED:
				resize()
			case .MOUSE_MOTION:
				state.mouse_pos = Mouse_Pos{i32(e.motion.x), i32(e.motion.y)}
			case .MOUSE_BUTTON_DOWN, .MOUSE_BUTTON_UP:
				state.mouse_pos = Mouse_Pos{i32(e.button.x), i32(e.button.y)}
				if button, ok := sdl_button_to_mouse_button(e.button.button); ok {
					if e.type == .MOUSE_BUTTON_DOWN {
						state.mouse_buttons_pressed += {button}
					} else {
						state.mouse_buttons_pressed -= {button}
					}
				}
			case .MOUSE_WHEEL:
				scroll := e.wheel.y
				if e.wheel.direction == .FLIPPED {
					scroll *= -1
				}
				state.mouse_wheel += scroll
			case .KEY_DOWN, .KEY_UP:
				if key, ok := sdl_scancode_to_keyboard_key(e.key.scancode); ok {
					if e.type == .KEY_DOWN {
						state.keyboard_keys_pressed += {key}
					} else {
						state.keyboard_keys_pressed -= {key}
					}
				}
			}
		}

		frame(dt)
	}

	finish()

	SDL.DestroyWindow(state.os.window)
	SDL.Quit()
}


os_get_framebuffer_size :: proc() -> (width, height: u32) {
	w, h: i32
	SDL.GetWindowSizeInPixels(state.os.window, &w, &h)
	return u32(w), u32(h)
}

os_get_window_size :: proc() -> (width, height: u32) {
	w, h: i32
	SDL.GetWindowSize(state.os.window, &w, &h)
	return u32(w), u32(h)
}

os_get_surface :: proc(instance: wgpu.Instance) -> wgpu.Surface {
	return sdl3glue.GetSurface(instance, state.os.window)
}

sdl_button_to_mouse_button :: proc(button: u8) -> (MouseButton, bool) {
	switch button {
	case SDL.BUTTON_LEFT:
		return .LEFT, true
	case SDL.BUTTON_RIGHT:
		return .RIGHT, true
	case SDL.BUTTON_MIDDLE:
		return .MIDDLE, true
	}
	return .LEFT, false
}

sdl_scancode_to_keyboard_key :: proc(scancode: SDL.Scancode) -> (KeyboardKey, bool) {
	#partial switch scancode {
	case ._1:
		return .ONE, true
	case ._2:
		return .TWO, true
	case ._3:
		return .THREE, true
	case ._4:
		return .FOUR, true
	case ._5:
		return .FIVE, true
	case ._6:
		return .SIX, true
	case ._7:
		return .SEVEN, true
	case ._8:
		return .EIGHT, true
	case ._9:
		return .NINE, true
	case ._0:
		return .ZERO, true
	case .LSHIFT:
		return .LEFT_SHIFT, true
	case .RSHIFT:
		return .RIGHT_SHIFT, true
	case .LCTRL:
		return .LEFT_CONTROL, true
	case .RCTRL:
		return .RIGHT_CONTROL, true
	case .LALT:
		return .LEFT_ALT, true
	case .RALT:
		return .RIGHT_ALT, true
	case .BACKSPACE:
		return .BACKSPACE, true
	case .DELETE:
		return .DELETE, true
	case .RETURN:
		return .ENTER, true
	case .KP_ENTER:
		return .KP_ENTER, true
	case .LEFT:
		return .LEFT, true
	case .RIGHT:
		return .RIGHT, true
	case .HOME:
		return .HOME, true
	case .END:
		return .END, true
	case .A:
		return .A, true
	case .X:
		return .X, true
	case .C:
		return .C, true
	case .V:
		return .V, true
	}
	return .KEY_NULL, false
}
