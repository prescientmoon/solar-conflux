package visuals

import "base:runtime"
import "core:c"
import "core:fmt"
import "core:log"
import "core:math"
import "vendor:OpenGL"
import "vendor:sdl3"

State :: struct {
	window:               ^sdl3.Window,

	// GPU data
	rect_program:         Program,
	circle_program:       Program,
	line_program:         Program,
	rounded_line_program: Program,
	rect_vao:             VAO,
	globals_ubo:          UBO,

	// Instance buffers
	buf_matrices:         [INSTANCES]Mat3,
	buf_colors:           [INSTANCES]Color,

	// Flags
	tick:                 u32,
	wireframe:            bool,
	globals:              Global_Uniforms,
}

// {{{ Initialization
init :: proc() -> (state: State, ok: bool) {
	GL_MAJOR :: 4
	GL_MINOR :: 2

	// {{{ Configure logging
	@(static) g_ctx: runtime.Context
	g_ctx = context

	sdl3.SetLogPriorities(.VERBOSE)
	sdl3.SetLogOutputFunction(
		proc "c" (
			userdata: rawptr,
			category: sdl3.LogCategory,
			priority: sdl3.LogPriority,
			message: cstring,
		) {
			context = g_ctx

			level: log.Level
			switch priority {
			case .TRACE, .DEBUG, .VERBOSE:
				level = .Debug
			case .INFO:
				level = .Info
			case .WARN:
				level = .Warning
			case .ERROR:
				level = .Error
			case .CRITICAL:
				level = .Fatal
			case .INVALID:
				fallthrough
			case:
				log.panicf("Unexpected log level %v", priority)
			}

			options: runtime.Logger_Options =
				context.logger.options - {.Short_File_Path, .Long_File_Path, .Procedure, .Line}

			context.logger.procedure(
				context.logger.data,
				level,
				fmt.tprintf("[SDL/%v]: %v", category, message),
				options,
			)
		},
		nil,
	)
	// }}}

	sdl3.SetAppMetadata(
		"odin-rendering-experiments",
		"<hash-here>",
		"dev.moonythm.odin-rendering-experiments",
	) or_return
	sdl3.Init(sdl3.InitFlags{.VIDEO}) or_return
	sdl3.GL_SetAttribute(.CONTEXT_MAJOR_VERSION, GL_MAJOR) or_return
	sdl3.GL_SetAttribute(.CONTEXT_MINOR_VERSION, GL_MINOR) or_return
	sdl3.GL_SetAttribute(.CONTEXT_PROFILE_MASK, c.int(sdl3.GL_CONTEXT_PROFILE_CORE)) or_return
	sdl3.GL_SetSwapInterval(1) // vsync

	state.window = sdl3.CreateWindow(
		"SDL visual experiment",
		640,
		480,
		{.FULLSCREEN, .OPENGL, .RESIZABLE},
	)
	(state.window != nil) or_return

	sdl3.StartTextInput(state.window) or_return

	gl_ctx := sdl3.GL_CreateContext(state.window)
	(gl_ctx != nil) or_return

	OpenGL.load_up_to(GL_MAJOR, GL_MINOR, sdl3.gl_set_proc_address)
	OpenGL.ClearColor(0, 0, 0, 1)
	OpenGL.Enable(OpenGL.DEPTH_TEST)
	OpenGL.Enable(OpenGL.BLEND)
	OpenGL.BlendFunc(OpenGL.SRC_ALPHA, OpenGL.ONE_MINUS_SRC_ALPHA)

	state.rect_program = OpenGL.load_shaders_source(
		#load("./shaders/vert.glsl"),
		#load("./shaders/rect.frag.glsl"),
	) or_return

	state.circle_program = OpenGL.load_shaders_source(
		#load("./shaders/vert.glsl"),
		#load("./shaders/circle.frag.glsl"),
	) or_return

	state.line_program = OpenGL.load_shaders_source(
		#load("./shaders/vert.glsl"),
		#load("./shaders/line.frag.glsl"),
	) or_return

	state.rounded_line_program = OpenGL.load_shaders_source(
		#load("./shaders/vert.glsl"),
		#load("./shaders/rounded-line.frag.glsl"),
	) or_return

	state.rect_vao = create_vao({{-1, -1}, {1, -1}, {1, 1}, {-1, 1}}, {0, 1, 2, 3}) or_return
	state.globals_ubo = create_globals_ubo()

	// Perform initial resize
	w, h: i32
	sdl3.GetWindowSize(state.window, &w, &h)
	on_resize(&state, w, h)

	init_command_queue()

	return state, true
}
// }}}
// {{{ Close
close :: proc(state: State) {
	OpenGL.DeleteProgram(state.rect_program)
	OpenGL.DeleteProgram(state.circle_program)

	_ = sdl3.StopTextInput(state.window)
	sdl3.DestroyWindow(state.window)
	sdl3.Quit()
}
// }}}
// {{{ Resize
on_resize :: proc(state: ^State, width, height: i32) {
	OpenGL.Viewport(0, 0, width, height)

	m: Mat4
	m[0, 0] = 2.0 / f32(width)
	m[1, 1] = -2.0 / f32(height)
	m[2, 2] = 1
	m[3, 3] = 1
	m[0, 3] = -1
	m[1, 3] = 1

	state.globals.viewport_matrix = RMat3(m)
	log.debug(m)
}
// }}}


// {{{ Render
render :: proc(state: ^State) {
	state.tick += 1

	wi, hi: i32
	sdl3.GetWindowSize(state.window, &wi, &hi)
	center: ℝ² = {f32(wi), f32(hi)} / 2
	draw_rect({30, 20}, {100, 200}, {1, 0, 0, 1}, z = 0.4)
	draw_rect(10, center + center * math.sin(f32(state.tick) / 60), {0, 1, 0, 1}, z = 0.6)
	draw_rect({1000, 800}, center / 3, {0, 1, 1, 1}, z = 0.5)

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

			r := f32(wi) / ℝ(count) / 4
			pos = (pos + 1) * center
			pos.y = 2 * center.y - pos.y
			if x > y {
				draw_rect(pos, 2 * r, color)
			} else {
				draw_circle(pos + r, r, color)
			}
		}
	}

	draw_circle(center + center * {-0.25, -0.3}, 450, Color{0, 0, 0.5, 0.75}, z = -0.1)
	draw_line({750, 200}, {1800, 1600}, 10, Color{1, 1, 1, 1}, z = -0.5)
	draw_rounded_line({200, 750}, {1200, 100}, 50, Color{1, 1, 1, 1}, z = -0.5)

	clear_screen()
	render_queue(state)
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

	state, ok := init()
	log.assertf(ok, "Got SDL error: %v", sdl3.GetError())
	defer close(state)

	quit := false

	for !quit {
		event: sdl3.Event
		for sdl3.PollEvent(&event) {
			#partial switch event.type {
			case .WINDOW_RESIZED:
				on_resize(&state, event.window.data1, event.window.data2)
			case .QUIT:
				quit = true
			case .TEXT_INPUT:
				switch ([^]u8)(event.text.text)[0] {
				case 'w':
					state.wireframe = !state.wireframe
				}
			}
		}

		render(&state)
		sdl3.GL_SwapWindow(state.window)

		free_all(context.temp_allocator)
	}
}
// }}}
