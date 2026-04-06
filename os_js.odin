#+build js
package main

import "base:runtime"

import "core:sys/wasm/js"

import "vendor:wgpu"

// odin.js resolves elements with getElementById, not querySelector — no leading '#'.
CANVAS_DOM_ID :: "wgpu-canvas"
// WebGPU surface from canvas uses a CSS selector string.
CANVAS_WGPU_SELECTOR :: "#wgpu-canvas"

OS :: struct {
	initialized: bool,
}

os_init :: proc() {
	ok := js.add_window_event_listener(.Resize, nil, size_callback)
	assert(ok)
	ok = js.add_window_event_listener(.Mouse_Move, nil, mouse_move_callback)
	assert(ok)
	ok = js.add_window_event_listener(.Mouse_Down, nil, mouse_button_callback)
	assert(ok)
	ok = js.add_window_event_listener(.Mouse_Up, nil, mouse_button_callback)
	assert(ok)
	ok = js.add_window_event_listener(.Wheel, nil, wheel_callback)
	assert(ok)
	ok = js.add_window_event_listener(.Key_Down, nil, key_callback)
	assert(ok)
	ok = js.add_window_event_listener(.Key_Up, nil, key_callback)
	assert(ok)
	// Canvas only: keep browser context menu from stealing right-button erase.
	ok = js.add_event_listener(CANVAS_DOM_ID, .Context_Menu, nil, context_menu_callback)
	assert(ok)
}

// NOTE: frame loop is done by the runtime.js repeatedly calling `step`.
os_run :: proc() {
	state.os.initialized = true
}

@(private = "file", export)
step :: proc(dt: f32) -> bool {
	if !state.os.initialized {
		return true
	}

	frame(dt)
	return true
}

os_get_framebuffer_size :: proc() -> (width, height: u32) {
	rect := js.get_bounding_client_rect(CANVAS_DOM_ID)
	dpi := js.device_pixel_ratio()
	return u32(f64(rect.width) * dpi), u32(f64(rect.height) * dpi)
}

os_get_window_size :: proc() -> (width, height: u32) {
	rect := js.get_bounding_client_rect(CANVAS_DOM_ID)
	return u32(rect.width), u32(rect.height)
}

os_get_surface :: proc(instance: wgpu.Instance) -> wgpu.Surface {
	return wgpu.InstanceCreateSurface(
		instance,
		&wgpu.SurfaceDescriptor {
			nextInChain = &wgpu.SurfaceSourceCanvasHTMLSelector {
				sType = .SurfaceSourceCanvasHTMLSelector,
				selector = CANVAS_WGPU_SELECTOR,
			},
		},
	)
}

@(private = "file", fini)
os_fini :: proc "contextless" () {
	context = runtime.default_context()
	js.remove_window_event_listener(.Resize, nil, size_callback)
	js.remove_window_event_listener(.Mouse_Move, nil, mouse_move_callback)
	js.remove_window_event_listener(.Mouse_Down, nil, mouse_button_callback)
	js.remove_window_event_listener(.Mouse_Up, nil, mouse_button_callback)
	js.remove_window_event_listener(.Wheel, nil, wheel_callback)
	js.remove_window_event_listener(.Key_Down, nil, key_callback)
	js.remove_window_event_listener(.Key_Up, nil, key_callback)
	js.remove_event_listener(CANVAS_DOM_ID, .Context_Menu, nil, context_menu_callback)

	finish()
}

@(private = "file")
size_callback :: proc(e: js.Event) {
	resize()
}

// Match SDL: logical window coords for mouse_pos_to_grid_pos (not CSS of full page unless canvas fills it).
@(private = "file")
canvas_pointer_css :: proc(e: js.Event) -> (x, y: i32) {
	rect := js.get_bounding_client_rect(CANVAS_DOM_ID)
	return i32(f64(e.mouse.client[0]) - rect.x), i32(f64(e.mouse.client[1]) - rect.y)
}

@(private = "file")
mouse_move_callback :: proc(e: js.Event) {
	mx, my := canvas_pointer_css(e)
	state.mouse_pos = Mouse_Pos{mx, my}
}

@(private = "file")
mouse_button_callback :: proc(e: js.Event) {
	mx, my := canvas_pointer_css(e)
	state.mouse_pos = Mouse_Pos{mx, my}
	if button, ok := js_button_to_mouse_button(e.mouse.button); ok {
		#partial switch e.kind {
		case .Mouse_Down:
			state.mouse_buttons_pressed += {button}
		case .Mouse_Up:
			state.mouse_buttons_pressed -= {button}
		}
	}
}

@(private = "file")
wheel_callback :: proc(e: js.Event) {
	js.event_prevent_default()
	state.mouse_wheel += f32(e.wheel.delta[1]) / 100.0
}

@(private = "file")
key_callback :: proc(e: js.Event) {
	if e.kind == .Key_Down && e.key.repeat {
		return
	}
	if key, ok := js_code_to_keyboard_key(e.key.code); ok {
		#partial switch e.kind {
		case .Key_Down:
			state.keyboard_keys_pressed += {key}
		case .Key_Up:
			state.keyboard_keys_pressed -= {key}
		}
	}
}

@(private = "file")
context_menu_callback :: proc(e: js.Event) {
	js.event_prevent_default()
}

@(private = "file")
js_button_to_mouse_button :: proc(button: i16) -> (MouseButton, bool) {
	switch button {
	case 0:
		return .LEFT, true
	case 1:
		return .MIDDLE, true
	case 2:
		return .RIGHT, true
	}
	return .LEFT, false
}

// Physical `code` values (layout-stable), aligned with sdl_scancode_to_keyboard_key in os_sdl3.odin.
@(private = "file")
js_code_to_keyboard_key :: proc(code: string) -> (KeyboardKey, bool) {
	switch code {
	case "Digit1":
		return .ONE, true
	case "Digit2":
		return .TWO, true
	case "Digit3":
		return .THREE, true
	case "Digit4":
		return .FOUR, true
	case "Digit5":
		return .FIVE, true
	case "Digit6":
		return .SIX, true
	case "Digit7":
		return .SEVEN, true
	case "Digit8":
		return .EIGHT, true
	case "Digit9":
		return .NINE, true
	case "Digit0":
		return .ZERO, true
	case "ShiftLeft":
		return .LEFT_SHIFT, true
	case "ShiftRight":
		return .RIGHT_SHIFT, true
	case "ControlLeft":
		return .LEFT_CONTROL, true
	case "ControlRight":
		return .RIGHT_CONTROL, true
	case "AltLeft":
		return .LEFT_ALT, true
	case "AltRight":
		return .RIGHT_ALT, true
	case "Backspace":
		return .BACKSPACE, true
	case "Delete":
		return .DELETE, true
	case "Enter":
		return .ENTER, true
	case "NumpadEnter":
		return .KP_ENTER, true
	case "ArrowLeft":
		return .LEFT, true
	case "ArrowRight":
		return .RIGHT, true
	case "Home":
		return .HOME, true
	case "End":
		return .END, true
	case "KeyA":
		return .A, true
	case "KeyX":
		return .X, true
	case "KeyC":
		return .C, true
	case "KeyV":
		return .V, true
	}
	return .KEY_NULL, false
}
