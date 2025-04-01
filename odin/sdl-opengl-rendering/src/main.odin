package visuals

import "base:runtime"
import "core:c"
import "core:fmt"
import "core:log"
import "core:math"
import "vendor:OpenGL"
import "vendor:sdl3"

State :: struct {
	tick:      u32,
	window:    ^sdl3.Window,
	program:   u32,
	rect_vao:  VAO,
	wireframe: bool,
}

init :: proc() -> (state: State, ok: bool) {
	GL_MAJOR :: 3
	GL_MINOR :: 3

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

	state.program = OpenGL.load_shaders_source(
		#load("./vert.glsl"),
		#load("./frag.glsl"),
	) or_return

	state.rect_vao = create_vao({{-1, -1}, {1, -1}, {1, 1}, {-1, 1}}, {0, 1, 2, 3}) or_return

	init_command_queue()

	return state, true
}

render :: proc(state: ^State) {
	state.tick += 1
	OpenGL.Clear(OpenGL.COLOR_BUFFER_BIT | OpenGL.DEPTH_BUFFER_BIT)

	OpenGL.UseProgram(state.program)
	defer OpenGL.UseProgram(0)

	if state.wireframe {
		OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, OpenGL.LINE)
	} else {
		OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, OpenGL.FILL)
	}

	draw_rect({-0.5, 0}, {0.75, 0.5}, {1, 0, 0, 1}, z = 0.5)
	draw_rect({0.5, 0.25}, {0.3, 0.5}, {0, 1, 0, 1}, z = -0.5)

	count := ℝ(32)
	for x in ℝ(0) ..< count {
		for y in ℝ(0) ..< count {
			i := x * count + y
			pos :=
				ℝ²{x + math.sin_f32(2 * math.π * (ℝ(state.tick) + i) / 60), y} *
					2 /
					ℝ(count) -
				1
			draw_rect(Rect{pos, 1 / ℝ(count), 0, {(pos.x + 1) / 2, (pos.y + 1) / 2, 1, 1}})
		}
	}

	render_queue(state)
}

close :: proc(state: State) {
	OpenGL.DeleteProgram(state.program)
	_ = sdl3.StopTextInput(state.window)
	sdl3.DestroyWindow(state.window)
	sdl3.Quit()
}

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
				OpenGL.Viewport(0, 0, event.window.data1, event.window.data2)
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
