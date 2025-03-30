package visuals

import "core:c"
import "core:log"
import "vendor:OpenGL"
import "vendor:sdl3"

State :: struct {
	window:              ^sdl3.Window,
	program:             u32,
	vertex_pos_location: u32,
	vbo:                 u32,
	ibo:                 u32,
}

init :: proc() -> (state: State, ok: bool) {
	GL_MAJOR :: 3
	GL_MINOR :: 1

	sdl3.Init(sdl3.InitFlags{.VIDEO}) or_return
	sdl3.GL_SetAttribute(.CONTEXT_MAJOR_VERSION, GL_MAJOR) or_return
	sdl3.GL_SetAttribute(.CONTEXT_MINOR_VERSION, GL_MINOR) or_return
	sdl3.GL_SetAttribute(.CONTEXT_PROFILE_MASK, c.int(sdl3.GL_CONTEXT_PROFILE_CORE)) or_return

	state.window = sdl3.CreateWindow("SDL visual experiment", 640, 480, {.FULLSCREEN, .OPENGL})
	(state.window != nil) or_return

	gl_ctx := sdl3.GL_CreateContext(state.window)
	(gl_ctx != nil) or_return

	OpenGL.load_up_to(GL_MAJOR, GL_MINOR, sdl3.gl_set_proc_address)

	state.program = OpenGL.load_shaders_source(
		#load("./vert.glsl"),
		#load("./frag.glsl"),
	) or_return
	// log.debug("Doing Opengl stuff")

	vertex_pos_location := OpenGL.GetAttribLocation(state.program, "LVertexPos2D")
	(vertex_pos_location != -1) or_return
	state.vertex_pos_location = u32(vertex_pos_location)

	OpenGL.ClearColor(0, 0, 0, 1)

	// VBO data
	vertex_data := [?]f32{-0.5, -0.5, 0.5, -0.5, 0.5, 0.5, -0.5, 0.5}
	vbos: [1]u32

	// IBO data
	index_data := [?]u32{0, 1, 2, 3}
	ibos: [1]u32

	//Create VBO
	OpenGL.GenBuffers(1, auto_cast &vbos)
	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, vbos[0])
	OpenGL.BufferData(OpenGL.ARRAY_BUFFER, 2 * 4 * size_of(f32), &vertex_data, OpenGL.STATIC_DRAW)
	state.vbo = vbos[0]

	OpenGL.GenBuffers(1, auto_cast &ibos)
	OpenGL.BindBuffer(OpenGL.ELEMENT_ARRAY_BUFFER, ibos[0])
	OpenGL.BufferData(
		OpenGL.ELEMENT_ARRAY_BUFFER,
		4 * size_of(u32),
		&index_data,
		OpenGL.STATIC_DRAW,
	)
	state.ibo = ibos[0]

	return state, true
}

render :: proc(state: State) {
	OpenGL.Clear(OpenGL.COLOR_BUFFER_BIT)

	OpenGL.UseProgram(state.program)
	defer OpenGL.UseProgram(0)

	OpenGL.EnableVertexAttribArray(state.vertex_pos_location)
	defer OpenGL.DisableVertexAttribArray(state.vertex_pos_location)

	OpenGL.BindBuffer(OpenGL.ARRAY_BUFFER, state.vbo)
	OpenGL.VertexAttribPointer(
		state.vertex_pos_location,
		2,
		OpenGL.FLOAT,
		false,
		2 * size_of(f32),
		0,
	)

	OpenGL.BindBuffer(OpenGL.ELEMENT_ARRAY_BUFFER, state.ibo)
	OpenGL.DrawElements(OpenGL.TRIANGLE_FAN, 4, OpenGL.UNSIGNED_INT, nil)
}

close :: proc(state: State) {
	OpenGL.DeleteProgram(state.program)
	sdl3.DestroyWindow(state.window)
	sdl3.Quit()
}

main :: proc() {
	context.logger = log.create_console_logger()
	state, ok := init()
	log.assertf(ok, "Got SDL error: %v", sdl3.GetError())
	defer close(state)

	quit := false
	log.debug("Created context")

	for !quit {
		sdl3.StartTextInput(state.window) or_break
		event: sdl3.Event
		for sdl3.PollEvent(&event) {
			#partial switch event.type {
			case .QUIT:
				quit = true
			}
		}

		render(state)
		sdl3.GL_SwapWindow(state.window)
		sdl3.StopTextInput(state.window) or_break
	}
}
// 				//Handle keypress
// 				else if( e.type == SDL_EVENT_TEXT_INPUT )
// 					handleKeys( e.text.text[ 0 ] );
