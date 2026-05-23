package experiment

import "base:runtime"
import "core:log"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:slice"
import "core:time"
import "core:math"
import "core:reflect"
import sdl "vendor:sdl3"

DEFAULT_SCREEN_RES_X :: 1280
DEFAULT_SCREEN_RES_Y :: 720

SDL_DEBUG_MODE :: ODIN_DEBUG

Sdl_App :: struct {
	window:        ^sdl.Window,
	device:        ^sdl.GPUDevice,
	pipelines:     [Gpu_Pipeline]^sdl.GPUGraphicsPipeline,
  transfer_bufs: [SBuffer]^sdl.GPUTransferBuffer,
  sbuffers:      [SBuffer]^sdl.GPUBuffer,
  samplers:      [Texture]^sdl.GPUSampler,
  textures:      [Texture]^sdl.GPUTexture,
}

GPU_Sprite :: struct {
  transform: Affine2,
  _: [2]f32, // padding!
  uv_rect:   [4]f32, // x, y, w, h
}

MAX_SPRITES :: 1024
GPU_Globals :: struct {
  sprites: [MAX_SPRITES]GPU_Sprite,
  sprite_count: u32,
  tick:         u32,
  screen_size: [2]f32,
}

State :: struct {
  app: Sdl_App,
  globals: GPU_Globals,
}

// Resource tables
// {{{ Textures
Texture :: enum {
  Atlas,
}

@rodata
TEXTURE_DATA := [Texture][]byte {
  .Atlas = #load("./atlas.png"),
}

@rodata
TEXTURE_LABEL := [Texture]cstring {
  .Atlas = "atlas"
}
// }}}
// {{{ Buffers
SBuffer :: enum {
  Globals,
}

SBUFFER_TYPES :: [SBuffer]typeid {
  .Globals = GPU_Globals,
}
// }}}
// {{{ Pipelines
Gpu_Pipeline :: enum {
  Example,
}

GPU_PIPELINE_SHADERS :: [Gpu_Pipeline]string {
  .Example = "example"
}
// }}}

// App lifecycle
// {{{ Main
main :: proc() {
  if !main_with_return() do os.exit(1)
}

main_with_return :: proc() -> (ok: bool) {
	context.logger = log.create_console_logger()

  state: State
	state.app = sdl_init() or_return
	defer sdl_fini(state.app)

	for frame(&state) {}

  return true
}
// }}}
// {{{ Loop
frame :: proc(state: ^State) -> bool {
  state.globals.tick += 1
  state.globals.screen_size = sdl_window_size(state.app) or_return
  state.globals.sprite_count = 2
  state.globals.sprites[0] = {
    // screen_rect = { 100, 100, 455, 455 },
    transform = compose_affine2(
      { 355, 0, 100, 0, 355 * math.sin(f32(state.globals.tick) / 60), 400, },
      affine2_rotate_around(0.5, f32(state.globals.tick) / 30),
    ),
    uv_rect = { 0.25, 0, 0.25, 0.25 },
  }
  state.globals.sprites[1] = {
    transform = { 300, 0, 500, 0, 355, 100 },
    uv_rect = { 0.75, 0, 0.25, 0.25 },
  }

	event: sdl.Event
	for sdl.PollEvent(&event) {
		#partial switch event.type {
		case .QUIT: return false
		case .KEY_DOWN: if event.key.scancode == .ESCAPE do return false
		}
	}

	cmdBuf := sdl.AcquireGPUCommandBuffer(state.app.device)
  sdl_guard(cmdBuf != nil, "acquire command buffer") or_return

	backbuffer: ^sdl.GPUTexture
  acquiredSwapchain := sdl.WaitAndAcquireGPUSwapchainTexture(
    cmdBuf, state.app.window, &backbuffer, nil, nil
  )

  sdl_guard(acquiredSwapchain, "aquire swapchain") or_return
  sdl_guard(backbuffer != nil, "aquire backbuffer") or_return

  copyPass := sdl.BeginGPUCopyPass(cmdBuf)
  sdl_upload_data(state.app, .Globals, copyPass, &state.globals) or_return
  sdl.EndGPUCopyPass(copyPass)

	colorTarget := sdl.GPUColorTargetInfo {
		texture     = backbuffer,
		clear_color = sdl.FColor { 0.1, 0.1, 0.1, 1.0 },
		load_op     = .CLEAR,
		store_op    = .STORE,
	}

	renderPass := sdl.BeginGPURenderPass(cmdBuf, &colorTarget, 1, nil)
  sdl.BindGPUGraphicsPipeline(renderPass, state.app.pipelines[.Example])

  sdl_bind_sbuffers(state.app, renderPass, .VERTEX, .Globals)
  sdl_bind_sbuffers(state.app, renderPass, .FRAGMENT, .Globals)
  sdl_bind_textures(state.app, renderPass, .FRAGMENT, .Atlas)

  sdl.DrawGPUPrimitives(renderPass, 6, state.globals.sprite_count, 0, 0)
	sdl.EndGPURenderPass(renderPass)

  submittedCommands := sdl.SubmitGPUCommandBuffer(cmdBuf)
  sdl_guard(submittedCommands, "submit command queue") or_return

	return true
}
// }}}

// SDL
// {{{ Init
sdl_init :: proc() -> (app: Sdl_App, ok: bool) {
	// {{{ Configure logging
	@(static) g_ctx: runtime.Context
	g_ctx = context

  sdl.SetLogPriorities(.VERBOSE)
	sdl.SetLogOutputFunction(
		proc "c" (
			userdata: rawptr,
			category: sdl.LogCategory,
			priority: sdl.LogPriority,
			message: cstring,
		) {
			context = g_ctx

			level: log.Level
			switch priority {
			case .TRACE, .DEBUG, .VERBOSE: level = .Debug
			case .INFO: level = .Info
			case .WARN: level = .Warning
			case .ERROR: level = .Error
			case .CRITICAL: level = .Fatal
			case .INVALID: fallthrough
			case: log.panicf("Unexpected log level %v", priority)
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

	metadataSet := sdl.SetAppMetadata(
		"odin-sdl-gpu-experiment",
		"<hash-here>",
		"dev.moonythm.odin-sdl-gpu-experiment",
	)

  sdl_guard(metadataSet, "set metadata") or_return

  initSDL := sdl.Init({ .VIDEO })
  sdl_guard(initSDL, "initialize SDL3") or_return

	app.device = sdl.CreateGPUDevice({ .SPIRV, .DXIL }, SDL_DEBUG_MODE, nil)
  sdl_guard(app.device != nil, "initialize GPU") or_return

	screenRes := [2]i32 { DEFAULT_SCREEN_RES_X, DEFAULT_SCREEN_RES_Y }
	app.window = sdl.CreateWindow(
    "SDL3 GPU experiment",
    screenRes.x,
    screenRes.y,
    { .RESIZABLE, .HIDDEN, .FULLSCREEN }
  )

  deviceClaimed := sdl.ClaimWindowForGPUDevice(app.device, app.window)
  sdl_guard(deviceClaimed, "tie window to GPU") or_return

	log.infof("GPU driver: %s", sdl.GetGPUDeviceDriver(app.device))
	log.infof("GPU shader formats: %v", sdl.GetGPUShaderFormats(app.device))

  SBUFFER_TYPES := SBUFFER_TYPES
  for id in SBuffer {
    app.sbuffers[id] = sdl.CreateGPUBuffer(
      app.device,
      {
        usage = { .GRAPHICS_STORAGE_READ },
        size = u32(reflect.size_of_typeid(SBUFFER_TYPES[id])),
      }
    )

    sdl_guard(app.sbuffers[id] != nil, "create storage buffer") or_return

    app.transfer_bufs[id] = sdl.CreateGPUTransferBuffer(
      app.device,
      {
        usage = .UPLOAD,
        size = u32(reflect.size_of_typeid(SBUFFER_TYPES[id])),
      }
    )

    sdl_guard(app.transfer_bufs[id] != nil, "create transfer buffer") or_return
  }

	sdl_make_pipeline(&app, .Example) or_return
	sdl.ShowWindow(app.window)
  
  // Upload initial data to the GPU
	cmdBuf := sdl.AcquireGPUCommandBuffer(app.device)
  sdl_guard(cmdBuf != nil, "acquire command buffer") or_return

  copyPass := sdl.BeginGPUCopyPass(cmdBuf)
  sdl_upload_textures(&app, copyPass) or_return
  sdl.EndGPUCopyPass(copyPass)

  submittedCommands := sdl.SubmitGPUCommandBuffer(cmdBuf)
  sdl_guard(submittedCommands, "submit command queue") or_return

	return app, true
}
// }}}
// {{{ Fini
sdl_fini :: proc(app: Sdl_App) {
  for id in Gpu_Pipeline {
    if app.pipelines[id] != nil {
      sdl.ReleaseGPUGraphicsPipeline(app.device, app.pipelines[id])
    }
  }

  for id in Texture {
    if app.samplers[id] != nil {
      sdl.ReleaseGPUSampler(app.device, app.samplers[id])
    }

    if app.textures[id] != nil {
      sdl.ReleaseGPUTexture(app.device, app.textures[id])
    }
  }

  for id in SBuffer {
    if app.sbuffers[id] != nil {
      sdl.ReleaseGPUBuffer(app.device, app.sbuffers[id])
    }

    if app.transfer_bufs[id] != nil {
      sdl.ReleaseGPUTransferBuffer(app.device, app.transfer_bufs[id])
    }
  }

	if app.window != nil {
    sdl.ReleaseWindowFromGPUDevice(app.device, app.window)
		sdl.DestroyWindow(app.window)
	}

	if app.device != nil {
		sdl.DestroyGPUDevice(app.device)
	}
}
// }}}
// {{{ Error handling
sdl_guard :: proc(cond: bool, action: string) -> bool {
  if !cond {
    log.errorf("Failed to %v. Error: %s", action, sdl.GetError())
  }

  return cond
}
// }}}
// {{{ Shader loading
Shader_Stats :: struct { samplers, sbuffers: u8, }

sdl_load_shader :: proc(
  app: Sdl_App,
  $name: string,
  $stage: sdl.GPUShaderStage,
) -> (^sdl.GPUShader, bool) {
  STAGE_PREFIX :: #sparse [sdl.GPUShaderStage]string {
    .VERTEX = ".vert",
    .FRAGMENT = ".frag",
  }

  PATH :: "../build/shaders/" + name
  STATS :: #load(PATH + STAGE_PREFIX[stage] + ".stats.bin")
  stats := cast(^Shader_Stats)raw_data(STATS)

  return sdl_load_shader_from_mem(
    app,
    stage,
    #load(PATH + STAGE_PREFIX[stage] + ".dxil"),
    #load(PATH + STAGE_PREFIX[stage] + ".spv"),
    stats^,
  )
}

// Separated out to minimize duplicate para-poly code.
sdl_load_shader_from_mem :: proc(
  app: Sdl_App,
  stage: sdl.GPUShaderStage,
  source_dxil, source_spirv: []byte,
  stats: Shader_Stats,
) -> (shader: ^sdl.GPUShader, ok: bool) {
  log.assert(stage == .VERTEX || stage == .FRAGMENT)

  @(static, rodata)
  ENTRYPOINTS := #sparse [sdl.GPUShaderStage]cstring {
    .VERTEX = "vertMain",
    .FRAGMENT = "fragMain",
  }

	sdlFormats := sdl.GetGPUShaderFormats(app.device)
  sdlFormat: sdl.GPUShaderFormatFlag
  switch {
  case .SPIRV in sdlFormats: sdlFormat = .SPIRV
  case .DXIL in sdlFormats: sdlFormat = .DXIL
  case .DXBC in sdlFormats: sdlFormat = .DXBC
  case .METALLIB in sdlFormats: sdlFormat = .METALLIB
  case .MSL in sdlFormats: sdlFormat = .MSL
  case: log.panicf("No conversion for SDL shader format: %v", sdlFormats)
  }

  source: []byte
  #partial switch sdlFormat {
  case .SPIRV: source = source_spirv
  case .DXIL:  source = source_dxil
  case: log.panicf("No shader source for format %v", sdlFormat)
  }

  shaderCreateInfo := sdl.GPUShaderCreateInfo {
    code                 = raw_data(source),
    code_size            = len(source),
    entrypoint           = ENTRYPOINTS[stage],
    format               = { sdlFormat },
    stage                = stage,
    num_samplers         = u32(stats.samplers),
    num_uniform_buffers  = 0,
    num_storage_buffers  = u32(stats.sbuffers),
    num_storage_textures = 0,
  }

  shader = sdl.CreateGPUShader(app.device, shaderCreateInfo)
  sdl_guard(shader != nil, "create shader") or_return

  return shader, true
}
// }}}
// {{{ Pipeline
sdl_make_pipeline :: proc(app: ^Sdl_App, $id: Gpu_Pipeline) -> bool {
  name :: GPU_PIPELINE_SHADERS[id]

	shaders: [2]^sdl.GPUShader = {
    sdl_load_shader(app^, name, .VERTEX) or_return,
    sdl_load_shader(app^, name, .FRAGMENT) or_return,
  }

  return sdl_make_pipeline_from_shaders(app, shaders, id)
}

// Split off from sdl_make_pipeline to not generate the same logic over and over
// again due to para-poly.
sdl_make_pipeline_from_shaders :: proc(
  app: ^Sdl_App,
  shaders: [2]^sdl.GPUShader,
  id: Gpu_Pipeline,
) -> bool {
	colorTargetDesc: sdl.GPUColorTargetDescription = {
    format = sdl.GetGPUSwapchainTextureFormat(app.device, app.window),
    blend_state = {
      enable_blend = true,
      color_blend_op = .ADD,
      alpha_blend_op = .ADD,
      src_color_blendfactor = .SRC_ALPHA,
      dst_color_blendfactor = .ONE_MINUS_SRC_ALPHA,
      src_alpha_blendfactor = .SRC_ALPHA,
      dst_alpha_blendfactor = .ONE_MINUS_SRC_ALPHA,
    }
  }

	pipelineCreateInfo := sdl.GPUGraphicsPipelineCreateInfo {
		target_info = sdl.GPUGraphicsPipelineTargetInfo {
			num_color_targets         = 1,
			color_target_descriptions = &colorTargetDesc,
		},

		primitive_type  = .TRIANGLELIST,
		vertex_shader   = shaders[0],
		fragment_shader = shaders[1],

		rasterizer_state = sdl.GPURasterizerState {
			fill_mode = .FILL,
		},
	}

	newPipeline := sdl.CreateGPUGraphicsPipeline(app.device, pipelineCreateInfo)
  sdl_guard(newPipeline != nil, "create graphics pipeline") or_return
  log.debug("here")

  pipeline := &app.pipelines[id]
	if pipeline^ != nil {
		sdl.ReleaseGPUGraphicsPipeline(app.device, pipeline^)
	}

	pipeline^ = newPipeline

  for shader in shaders do sdl.ReleaseGPUShader(app.device, shader)

	return true
}
// }}}
// {{{ Upload data to the GPU
sdl_upload_data :: proc(
  app: Sdl_App,
  $id: SBuffer,
  copyPass: ^sdl.GPUCopyPass,
  data: ^SBUFFER_TYPES[id],
) -> (ok: bool) {
  return sdl_upload_data_raw(
    app, id, copyPass, data, size_of(#type SBUFFER_TYPES[id]),
  )
}

// Separated out to avoid duplication because of para-poly.
sdl_upload_data_raw :: proc(
  app: Sdl_App,
  id: SBuffer,
  copyPass: ^sdl.GPUCopyPass,
  data: rawptr, data_size: int,
) -> (ok: bool) {
  transfer_data := sdl.MapGPUTransferBuffer(
    app.device, app.transfer_bufs[id], true,
  )

  sdl_guard(transfer_data != nil, "map transfer buffer") or_return

  mem.copy(transfer_data, data, data_size)
  sdl.UnmapGPUTransferBuffer(app.device, app.transfer_bufs[id])

  location: sdl.GPUTransferBufferLocation = {
    transfer_buffer = app.transfer_bufs[id],
    offset = 0,
  }

  region: sdl.GPUBufferRegion = {
    buffer = app.sbuffers[id],
    offset = 0,
    size = u32(data_size),
  }

  sdl.UploadToGPUBuffer(copyPass, location, region, true)

  return true
}
// }}}
// {{{ Image loading
sdl_format_to_gpu :: proc(format: sdl.PixelFormat) -> sdl.GPUTextureFormat {
  #partial switch format {
  case .ABGR8888: return .R8G8B8A8_UNORM
  case: log.panicf("Unsupported surface format: %v", format)
  }
}

sdl_upload_textures :: proc(
  app: ^Sdl_App,
  copyPass: ^sdl.GPUCopyPass,
) -> (ok: bool) {
  // Decode PNGs
  surfaces: [Texture]^sdl.Surface
  defer for surface in surfaces {
    if surface != nil do sdl.DestroySurface(surface)
  }

  for id in Texture {
    data := TEXTURE_DATA[id]
    stream := sdl.IOFromConstMem(raw_data(data), len(data))
    sdl_guard(stream != nil, "create IO stream") or_return

    surface := sdl.LoadPNG_IO(stream, true)
    sdl_guard(surface != nil, "create SDL surface") or_return
    if surface.format != .ABGR8888 {
      next := sdl.ConvertSurface(surface, .ABGR8888)
      sdl_guard(next != nil, "convert SDL surface") or_return

      sdl.DestroySurface(surface)
      surface = next
    }

    surfaces[id] = surface
  }

  // Compute GPU transfer buffer memory layout
  total_size: uint
  texture_sizes:   [Texture]uint
  texture_offsets: [Texture]uint
  for id in Texture {
    surface := surfaces[id]
    amount := uint(4 * surface.w * surface.h)
    texture_sizes[id] = amount
    texture_offsets[id] = total_size
    total_size += amount
  }

  // Create transfer buffer & copy data to it
  transfer_buffer := sdl.CreateGPUTransferBuffer(
    app.device,
    { usage = .UPLOAD, size = u32(total_size), }
  )

  sdl_guard(transfer_buffer != nil, "create texture transfer buffer") or_return
  defer sdl.ReleaseGPUTransferBuffer(app.device, transfer_buffer)

  transfer_data := cast([^]byte)sdl.MapGPUTransferBuffer(
    app.device, transfer_buffer, false,
  )

  sdl_guard(transfer_data != nil, "map transfer buffer") or_return
  
  for id in Texture {
    dest := rawptr(uintptr(transfer_data) + uintptr(texture_offsets[id]))
    mem.copy(dest, surfaces[id].pixels, int(texture_sizes[id]))
  }

  sdl.UnmapGPUTransferBuffer(app.device, transfer_buffer)

  // Create GPU textures & samplers
  for id in Texture {
    surface := surfaces[id]
    sampler := sdl.CreateGPUSampler(app.device, {
      min_filter     = .NEAREST,
      mag_filter     = .NEAREST,
      mipmap_mode    = .NEAREST,
      address_mode_u = .CLAMP_TO_EDGE,
      address_mode_v = .CLAMP_TO_EDGE,
      address_mode_w = .CLAMP_TO_EDGE,
    })

    sdl_guard(sampler != nil, "create sampler") or_return
    app.samplers[id] = sampler

    props := sdl.CreateProperties()
    defer sdl.DestroyProperties(props)

    sdl.SetStringProperty(
      props,
      sdl.PROP_GPU_BUFFER_CREATE_NAME_STRING,
      TEXTURE_LABEL[id],
    )

    texture := sdl.CreateGPUTexture(app.device, {
      type                 = .D2,
      format               = sdl_format_to_gpu(surface.format),
      width                = u32(surface.w),
      height               = u32(surface.h),
      layer_count_or_depth = 1,
      num_levels           = 1,
      usage                = { .SAMPLER },
      props                = props,
    })

    sdl_guard(texture != nil, "create texture") or_return
    app.textures[id] = texture

    sdl.UploadToGPUTexture(
      copyPass,
      { transfer_buffer = transfer_buffer, offset = u32(texture_offsets[id]) },
      { texture = texture, w = u32(surface.w), h = u32(surface.h), d = 1 },
      false,
    )
  }

  return true
}
// }}}
// {{{ Resource binding helpers
sdl_bind_textures :: proc(
  app: Sdl_App,
  renderPass: ^sdl.GPURenderPass,
  stage: sdl.GPUShaderStage,
  ids: ..Texture,
) {
  MAX_BINDINGS :: len(Texture)
  log.assert(len(ids) <= MAX_BINDINGS)
  bindings: [dynamic; MAX_BINDINGS]sdl.GPUTextureSamplerBinding

  for id in ids {
    binding: sdl.GPUTextureSamplerBinding = {
      texture = app.textures[id],
      sampler = app.samplers[id],
    }

    append(&bindings, binding)
  }

  bindings_p := raw_data(bindings[:])
  switch stage {
  case .FRAGMENT:
    sdl.BindGPUFragmentSamplers(renderPass, 0, bindings_p, u32(len(ids)))
  case .VERTEX:
    sdl.BindGPUVertexSamplers(renderPass, 0, bindings_p, u32(len(ids)))
  case:
    log.panicf("Unsupported shader stage %v", stage)
  }
}

sdl_bind_sbuffers :: proc(
  app: Sdl_App,
  renderPass: ^sdl.GPURenderPass,
  stage: sdl.GPUShaderStage,
  ids: ..SBuffer,
) {
  MAX_BINDINGS :: len(SBuffer)
  log.assert(len(ids) <= MAX_BINDINGS)
  bindings: [dynamic; MAX_BINDINGS]^sdl.GPUBuffer
  for id in ids do append(&bindings, app.sbuffers[id])

  bindings_p := raw_data(bindings[:])
  switch stage {
  case .FRAGMENT:
    sdl.BindGPUFragmentStorageBuffers(renderPass, 0, bindings_p, u32(len(ids)))
  case .VERTEX:
    sdl.BindGPUVertexStorageBuffers(renderPass, 0, bindings_p, u32(len(ids)))
  case:
    log.panicf("Unsupported shader stage %v", stage)
  }
}
// }}}
// {{{ Get window size
sdl_window_size :: proc(app: Sdl_App) -> (size: [2]f32, ok: bool) {
  w, h: i32
  ok = sdl.GetWindowSizeInPixels(app.window, &w, &h)
  sdl_guard(ok, "get window size") or_return
  return { f32(w), f32(h) }, true
}
// }}}
