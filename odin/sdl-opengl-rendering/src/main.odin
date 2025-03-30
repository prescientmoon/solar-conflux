package visuals

import "base:runtime"
import "core:c"
import "core:fmt"
import "core:log"
import "vendor:OpenGL"
import "vendor:sdl3"

State :: struct {
	window:              ^sdl3.Window,
	program:             u32,
	vertex_pos_location: u32,
	vao:                 u32,
	vbo:                 u32,
	ibo:                 u32,
	wireframe:           bool,
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

	state.program = OpenGL.load_shaders_source(
		#load("./vert.glsl"),
		#load("./frag.glsl"),
	) or_return
	// log.debug("Doing Opengl stuff")

	vertex_pos_location := OpenGL.GetAttribLocation(state.program, "aPos")
	(vertex_pos_location != -1) or_return
	state.vertex_pos_location = u32(vertex_pos_location)

	OpenGL.ClearColor(0, 0, 0, 1)

	// VBO data
	vertex_data := 2 * [?]f32{-0.5, -0.5, 0.5, -0.5, 0.5, 0.5, -0.5, 0.5}

	// IBO data
	index_data := [?]u32{0, 1, 2, 3}

	OpenGL.GenVertexArrays(1, &state.vao)
	OpenGL.BindVertexArray(state.vao)

	//Create VBO
	OpenGL.GenBuffers(1, &state.vbo)
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, state.vbo)
	OpenGL.BufferData(OpenGL.ARRAY_BUFFER, 2 * 4 * size_of(f32), &vertex_data, OpenGL.STATIC_DRAW)

	OpenGL.EnableVertexAttribArray(state.vertex_pos_location)
	OpenGL.VertexAttribPointer(
		state.vertex_pos_location,
		2,
		OpenGL.FLOAT,
		false,
		2 * size_of(f32),
		0,
	)

	OpenGL.GenBuffers(1, auto_cast &state.ibo)
	OpenGL.BindBuffer(OpenGL.ELEMENT_ARRAY_BUFFER, state.ibo)
	OpenGL.BufferData(
		OpenGL.ELEMENT_ARRAY_BUFFER,
		4 * size_of(u32),
		&index_data,
		OpenGL.STATIC_DRAW,
	)

	OpenGL.BindVertexArray(0)
	OpenGL.DisableVertexAttribArray(state.vertex_pos_location)

	return state, true
}

render :: proc(state: State) {
	OpenGL.Clear(OpenGL.COLOR_BUFFER_BIT)

	OpenGL.UseProgram(state.program)
	defer OpenGL.UseProgram(0)

	if state.wireframe {
		OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, OpenGL.LINE)
	} else {
		OpenGL.PolygonMode(OpenGL.FRONT_AND_BACK, OpenGL.FILL)
	}

	OpenGL.BindVertexArray(state.vao)
	OpenGL.DrawElements(OpenGL.TRIANGLE_FAN, 4, OpenGL.UNSIGNED_INT, nil)
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
				log.debug(event.text)
			}
		}

		render(state)
		sdl3.GL_SwapWindow(state.window)
	}
}
