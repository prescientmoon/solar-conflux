package visuals

import "base:runtime"
import "core:c"
import "core:fmt"
import "core:log"
import "vendor:OpenGL"
import "vendor:sdl3"

State :: struct {
	window:               ^sdl3.Window,

	// TODO: do not use dynamic arrays here
	// Render queues
	q_rects:              [dynamic]Shape(□),
	q_circles:            [dynamic]Shape(Circle2),
	q_lines:              [dynamic]Shape(Line),
	q_rounded_lines:      [dynamic]Shape(Rounded_Line),

	// GPU data
	rect_mesh:            Mesh,
	ubo_globals:          UBO,

	// Programs
	rect_program:         Program,
	circle_program:       Program,
	line_program:         Program,
	rounded_line_program: Program,

	// Instance buffers (CPU)
	buf_matrices:         [INSTANCES]Mat3,
	buf_colors:           [INSTANCES]Color,
	buf_lines:            [INSTANCES][2]ℝ²,
	buf_floats:           [INSTANCES]ℝ,
	buf_vecs:             [INSTANCES]ℝ²,

	// Instance buffers (GPU)
	instance_buffers:     [Instance_Param_Buf]u32,

	// Flags
	tick:                 u32,
	wireframe:            bool,
	globals:              Global_Uniforms,
}


// {{{ Screen dimensions
screen_dimensions :: proc() -> ℝ² {
	w, h: i32
	sdl3.GetWindowSize(g_renderer_state().window, &w, &h)

	return {f32(w), f32(h)}
}
// }}}
// {{{ Clip rects
// Only draw inside a given region of the screen
set_clip_rect :: proc(rect: □) {
	render_queue()

	OpenGL.Enable(OpenGL.SCISSOR_TEST)
	OpenGL.Scissor(
		i32(rect.top_left.x),
		i32(screen_dimensions().y - rect.dimensions.y - rect.top_left.y),
		i32(rect.dimensions.x),
		i32(rect.dimensions.y),
	)
}

unset_clip_rect :: proc() {
	render_queue()
	OpenGL.Disable(OpenGL.SCISSOR_TEST)
}
// }}}

// {{{ Initialization
sdl_init :: proc() -> (ok: bool) {
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
	// {{{ Initialize window & GL context
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

	state := g_renderer_state()
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

	// Perform initial resize
	sdl_on_resize(screen_dimensions())
	// }}}
	// {{{ Initialize GPU buffers & programs
	// Initialize GPU buffers
	OpenGL.GenBuffers(len(state.instance_buffers), ([^]u32)(&state.instance_buffers))

	// Initialize meshes
	state.rect_mesh = create_mesh({{0, 0}, {1, 0}, {1, 1}, {0, 1}}, {0, 1, 2, 3})

	// Initialize programs
	state.rect_program = gen_program(
		{
			sdf_name = "sdf_rect",
			sdf_args = {"v_center", "v_dimensions"},
			params = {{buf = .Center, name = "center"}, {buf = .Dimensions, name = "dimensions"}},
			id = 0,
		},
	) or_return

	state.circle_program = gen_program(
		{
			sdf_name = "sdf_circle",
			sdf_args = {"v_center", "v_radius"},
			params = {{buf = .Center, name = "center"}, {buf = .Radius, name = "radius"}},
			id = 1,
		},
	) or_return

	state.rounded_line_program = gen_program(
		{
			sdf_name = "sdf_line",
			sdf_args = {"v_line", "v_thickness"},
			params = {{buf = .Line, name = "line"}, {buf = .Thickness, name = "thickness"}},
			id = 2,
		},
	) or_return

	state.rounded_line_program = gen_program(
		{
			sdf_name = "sdf_rounded_line",
			sdf_args = {"v_line", "v_thickness"},
			params = {{buf = .Line, name = "line"}, {buf = .Thickness, name = "thickness"}},
			id = 3,
		},
	) or_return

	state.ubo_globals = create_ubo_globals()
	// }}}

	state.q_rects = make([dynamic]Shape(□))
	state.q_circles = make([dynamic]Shape(Circle2))
	state.q_lines = make([dynamic]Shape(Line))
	state.q_rounded_lines = make([dynamic]Shape(Rounded_Line))

	return true
}
// }}}
// {{{ Close
sdl_close :: proc() {
	state := g_renderer_state()

	_ = sdl3.StopTextInput(state.window)
	sdl3.DestroyWindow(state.window)
	sdl3.Quit()
}
// }}}
// {{{ Resize
sdl_on_resize :: proc(dims: ℝ²) {
	OpenGL.Viewport(0, 0, i32(dims.x), i32(dims.y))
	
  // odinfmt: disable
	m: Mat4 = {  
    2.0 / dims.x,             0, 0, -1,
               0, -2.0 / dims.y, 0,  1,
               0,             0, 1,  0,
               0,             0, 0,  1,
  }
  // odinfmt: enable

	g_renderer_state().globals.viewport_matrix = m
}
// }}}
