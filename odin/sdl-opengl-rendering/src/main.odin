package visuals

import "core:log"
import "core:math"
import "vendor:sdl3"

// {{{ Render
render :: proc() {
	clear_screen()

	state := g_renderer_state()
	state.tick += 1

	dims := screen_dimensions()
	center := dims / 2
	draw_rect(ℝ²{30, 20}, ℝ²{100, 200}, Color{1, 0, 0, 1}, z = 0.4)
	draw_rect(10, center + center * math.sin(f32(state.tick) / 60), Color{0, 1, 0, 1}, z = 0.6)
	draw_rect(ℝ²{1000, 800}, center / 3, Color{0, 1, 1, 1}, z = 0.5)

	count := ℝ(32)
	for x in ℝ(0) ..< count {
		for y in ℝ(0) ..< count {
			i := x * count + y
			pos :=
				ℝ²{x + math.sin_f32(2 * math.π * (ℝ(state.tick) + i) / 60), y} *
					2 /
					ℝ(count) -
				1
			color := Color{(pos.x + 1) / 2, (pos.y + 1) / 2, 1, 1}

			r := dims.x / ℝ(count) / 4
			pos = (pos + 1) * center
			pos.y = 2 * center.y - pos.y
			if x > y {
				draw_rect(pos, 2 * r, color)
			} else {
				draw_circle(pos + r, r, color)
			}
		}
	}

	rect := □{center / 2, center / 2 + center * math.sin(f32(state.tick) / 120)}
	draw_rect(rect, fill = {1, 0.6, 0.85, 0.3})

	// set_clip_rect(rect)
	draw_circle(center + center * {-0.25, -0.3}, 450, Color{0, 0, 0.5, 0.75}, z = -0.1)

	draw_line(
		Line{ℝ²{750, 200}, ℝ²{1800, 1600}, 10},
		Shape_Options{fill = Color{1, 1, 1, 1}, z = -0.5},
		rounded = false,
	)

	draw_line(
		Line{ℝ²{750, 1000}, ℝ²{1200, 1000}, 5},
		Shape_Options{fill = Color{1, 1, 1, 1}, z = -0.5},
	)

	draw_line(
		Line{ℝ²{1200, 1000}, ℝ²{300, 450}, 5},
		Shape_Options{fill = Color{1, 1, 1, 1}, z = -0.5},
	)

	draw_line(
		Line{ℝ²{230, 1000}, ℝ²{1700, 350}, 20},
		Shape_Options {
			fill = Color{1, 0, 1, 0.3},
			z = -0.7,
			stroke_width = 3,
			stroke = Color{0.9, 0.68, 0.8, 1},
		},
	)

	// unset_clip_rect()

	render_queue()
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
				}
			}
		}

		render()
		sdl3.GL_SwapWindow(g_renderer_state().window)

		free_all(context.temp_allocator)
	}
}
// }}}
