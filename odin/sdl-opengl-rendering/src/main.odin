package visuals

import "core:log"
import "core:math"
import "vendor:sdl3"

// {{{ Render
render :: proc() {
	clear_screen()

	state := g_renderer_state()
	state.tick += 1

	// dims := screen_dimensions()
	// center := dims / 2
	// draw_rect(ℝ²{30, 20}, ℝ²{100, 200}, Shape_Options{fill = {1, 0, 0, 1}, z = 0.4})
	// draw_rect(
	// 	10,
	// 	center + center * math.sin(f32(state.tick) / 60),
	// 	Shape_Options{fill = {0, 1, 0, 1}, z = 0.6},
	// )
	// draw_rect(
	// 	ℝ²{1000, 800},
	// 	center / 3,
	// 	Shape_Options{fill = {0, 1, 1, 1}, z = 0.5, stroke = {0, 0.5, 0.5, 1}, stroke_width = 3},
	// )
	//
	// // count := math.pow(2, math.mod(ℝ(state.tick) / 30, 7))
	// count := ℝ(32)
	// for x in ℝ(0) ..< count {
	// 	for y in ℝ(0) ..< count {
	// 		i := x * count + y
	// 		pos :=
	// 			ℝ²{x + math.sin_f32(2 * math.π * (ℝ(state.tick) + i) / 60), y} / ℝ(count)
	// 		color := Color{(pos.x + 1) / 2, (pos.y + 1) / 2, 1, 1}
	//
	// 		r := dims.x / ℝ(count) / 4
	// 		pos = pos * dims
	// 		// pos.y = 2 * center.y - pos.y
	// 		opts := Shape_Options {
	// 			fill = color,
	// 			z    = 0.3,
	// 		}
	// 		opts.stroke.a = 1
	// 		opts.stroke.rgb = 1 - opts.fill.rgb
	// 		opts.stroke_width = 1
	//
	// 		if x > y {
	// 			draw_rect(pos, 2 * r, opts)
	// 		} else {
	// 			draw_circle(pos + r, r, opts)
	// 		}
	// 	}
	// }
	//
	// rect := □{center / 2, center / 2 + center * math.sin(f32(state.tick) / 120)}
	// draw_rect(rect, Shape_Options{fill = {1, 0.6, 0.85, 0.3}})
	//
	// // set_clip_rect(rect)
	// draw_circle(
	// 	center + center * {-0.25, -0.3},
	// 	450,
	// 	Shape_Options {
	// 		fill = {0, 0, 0.5, 0.75},
	// 		z = -0.1,
	// 		stroke = Color{0.7, 0.85, 1, 1},
	// 		stroke_width = 1,
	// 	},
	// )
	//
	// draw_line(
	// 	Line{ℝ²{750, 200}, ℝ²{1800, 1600}, 10},
	// 	Shape_Options{fill = Color{1, 1, 1, 1}, z = -0.5},
	// 	rounded = false,
	// )
	//
	// draw_line(
	// 	Line{ℝ²{750, 1000}, ℝ²{1200, 1000}, 5},
	// 	Shape_Options{fill = Color{1, 1, 1, 1}, z = -0.5},
	// )
	//
	// draw_line(
	// 	Line{ℝ²{1200, 1000}, ℝ²{300, 450}, 5},
	// 	Shape_Options{fill = Color{1, 1, 1, 1}, z = -0.5},
	// )
	//
	// draw_line(
	// 	Line{ℝ²{230, 1000}, ℝ²{1700, 350}, 20},
	// 	Shape_Options {
	// 		fill = Color{1, 0, 1, 0.3},
	// 		z = -0.7,
	// 		stroke_width = 3,
	// 		stroke = Color{0.9, 0.68, 0.8, 1},
	// 	},
	// )
	//
	// render_queue()

	draw_rect(ℝ²{100, 100}, ℝ²{100, 100}, Shape_Options{fill = {1, 0, 0, 1}})
	draw_rect(ℝ²{100, 400}, ℝ²{100, 100}, Shape_Options{fill = {1, 0, 0, 1}})
	draw_rect(ℝ²{400, 100}, ℝ²{100, 100}, Shape_Options{fill = {1, 0, 0, 1}})
	draw_circle(ℝ²{1200, 350}, 200, Shape_Options{fill = {1, 0, 0, 1}})
	jfa()
}
// }}}
// {{{ Main
main :: proc() {
	log.Level_Headers = {
		0 ..< 10 = "[DEBUG] ",
		10 ..< 20 = "[INFO ] ",
		20 ..< 30 = "[WARN ] ",
		30 ..< 40 = "[ERROR] ",
		40 ..< 50 = "[FATAL] ",
	}

	logger := log.create_console_logger()
	logger.options -= {.Date, .Time}
	context.logger = logger

	ok := sdl_init()
	log.assertf(ok, "Got SDL error: %v", sdl3.GetError())
	defer sdl_close()

	quit := false

	for !quit {
		event: sdl3.Event
		for sdl3.PollEvent(&event) {
			#partial switch event.type {
			case .WINDOW_RESIZED:
				sdl_on_resize({f32(event.window.data1), f32(event.window.data2)})
			case .QUIT:
				quit = true
			case .TEXT_INPUT:
				switch ([^]u8)(event.text.text)[0] {
				case 'w':
					g_renderer_state().wireframe = !g_renderer_state().wireframe
				case 'p':
					g_renderer_state().pass = Render_Pass((u8(g_renderer_state().pass) + 1) % 3)
				}
			}
		}

		render()
		sdl3.GL_SwapWindow(g_renderer_state().window)

		free_all(context.temp_allocator)
	}
}
// }}}
