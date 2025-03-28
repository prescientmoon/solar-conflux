package visuals

import "core:c"
import "core:log"
import "vendor:OpenGL"
import "vendor:sdl3"

State :: struct {
	window: ^sdl3.Window,
}

init :: proc() -> (state: State, ok: bool) {
	sdl3.Init(sdl3.InitFlags{.VIDEO}) or_return
	sdl3.GL_SetAttribute(.CONTEXT_MAJOR_VERSION, 3) or_return
	sdl3.GL_SetAttribute(.CONTEXT_MINOR_VERSION, 1) or_return
	sdl3.GL_SetAttribute(.CONTEXT_PROFILE_MASK, c.int(sdl3.GL_CONTEXT_PROFILE_CORE)) or_return

	window := sdl3.CreateWindow("SDL visual experiment", 640, 480, {.FULLSCREEN, .OPENGL})
	(window != nil) or_return

	gl_ctx := sdl3.GL_CreateContext(window)
	(gl_ctx != nil) or_return

	// Init GL here

	return {window = window}, true
}

init_gl :: proc() -> (ok: bool) {
	program_id := OpenGL.load_shaders_source(#load("./vert.glsl"), #load("./frag.glsl")) or_return
	return true
}

// 				//Get vertex attribute location
// 				gVertexPos2DLocation = glGetAttribLocation( gProgramID, "LVertexPos2D" );
// 				if( gVertexPos2DLocation == -1 )
// 				{
// 					SDL_Log( "LVertexPos2D is not a valid glsl program variable!\n" );
// 					success = false;
// 				}
// 				else
// 				{
// 					//Initialize clear color
// 					glClearColor( 0.f, 0.f, 0.f, 1.f );
//
// 					//VBO data
// 					GLfloat vertexData[] =
// 					{
// 						-0.5f, -0.5f,
// 						 0.5f, -0.5f,
// 						 0.5f,  0.5f,
// 						-0.5f,  0.5f
// 					};
//
// 					//IBO data
// 					GLuint indexData[] = { 0, 1, 2, 3 };
//
// 					//Create VBO
// 					glGenBuffers( 1, &gVBO );
// 					glBindBuffer( GL_ARRAY_BUFFER, gVBO );
// 					glBufferData( GL_ARRAY_BUFFER, 2 * 4 * sizeof(GLfloat), vertexData, GL_STATIC_DRAW );
//
// 					//Create IBO
// 					glGenBuffers( 1, &gIBO );
// 					glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, gIBO );
// 					glBufferData( GL_ELEMENT_ARRAY_BUFFER, 4 * sizeof(GLuint), indexData, GL_STATIC_DRAW );

// void render()
// {
// 	//Clear color buffer
// 	glClear( GL_COLOR_BUFFER_BIT );
//
// 	//Render quad
// 	if( gRenderQuad )
// 	{
// 		//Bind program
// 		glUseProgram( gProgramID );
//
// 		//Enable vertex position
// 		glEnableVertexAttribArray( gVertexPos2DLocation );
//
// 		//Set vertex data
// 		glBindBuffer( GL_ARRAY_BUFFER, gVBO );
// 		glVertexAttribPointer( gVertexPos2DLocation, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), NULL );
//
// 		//Set index data and render
// 		glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, gIBO );
// 		glDrawElements( GL_TRIANGLE_FAN, 4, GL_UNSIGNED_INT, NULL );
//
// 		//Disable vertex position
// 		glDisableVertexAttribArray( gVertexPos2DLocation );
//
// 		//Unbind program
// 		glUseProgram( 0 );
// 	}
// }

close :: proc(state: State) {
	sdl3.DestroyWindow(state.window)
	sdl3.Quit()
}

// void close()
// {
// 	//Deallocate program
// 	glDeleteProgram( gProgramID );
//
// 	//Destroy window
// 	SDL_DestroyWindow( gWindow );
// 	gWindow = NULL;
//
// 	//Quit SDL subsystems
// 	SDL_Quit();
// }
//
// void printProgramLog( GLuint program )
// {
// 	//Make sure name is shader
// 	if( glIsProgram( program ) )
// 	{
// 		//Program log length
// 		int infoLogLength = 0;
// 		int maxLength = infoLogLength;
//
// 		//Get info string length
// 		glGetProgramiv( program, GL_INFO_LOG_LENGTH, &maxLength );
//
// 		//Allocate string
// 		char* infoLog = new char[ maxLength ];
//
// 		//Get info log
// 		glGetProgramInfoLog( program, maxLength, &infoLogLength, infoLog );
// 		if( infoLogLength > 0 )
// 		{
// 			//Print Log
// 			SDL_Log( "%s\n", infoLog );
// 		}
//
// 		//Deallocate string
// 		delete[] infoLog;
// 	}
// 	else
// 	{
// 		SDL_Log( "Name %d is not a program\n", program );
// 	}
// }
//
// void printShaderLog( GLuint shader )
// {
// 	//Make sure name is shader
// 	if( glIsShader( shader ) )
// 	{
// 		//Shader log length
// 		int infoLogLength = 0;
// 		int maxLength = infoLogLength;
//
// 		//Get info string length
// 		glGetShaderiv( shader, GL_INFO_LOG_LENGTH, &maxLength );
//
// 		//Allocate string
// 		char* infoLog = new char[ maxLength ];
//
// 		//Get info log
// 		glGetShaderInfoLog( shader, maxLength, &infoLogLength, infoLog );
// 		if( infoLogLength > 0 )
// 		{
// 			//Print Log
// 			SDL_Log( "%s\n", infoLog );
// 		}
//
// 		//Deallocate string
// 		delete[] infoLog;
// 	}
// 	else
// 	{
// 		SDL_Log( "Name %d is not a shader\n", shader );
// 	}
// }


main :: proc() {
	context.logger = log.create_console_logger()
	log.debug("Starting")
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

		sdl3.GL_SwapWindow(state.window)
		sdl3.StopTextInput(state.window) or_break
	}
}
// 		//Event handler
// 		SDL_Event e;
//
// 		//Enable text input
// 		SDL_StartTextInput( gWindow );
//
// 		//While application is running
// 		while( !quit )
// 		{
// 			//Handle events on queue
// 			while( SDL_PollEvent( &e ) != 0 )
// 			{
// 				//User requests quit
// 				if( e.type == SDL_EVENT_QUIT )
// 				{
// 					quit = true;
// 				}
// 				//Handle keypress
// 				else if( e.type == SDL_EVENT_TEXT_INPUT )
// 				{
// 					handleKeys( e.text.text[ 0 ] );
// 				}
// 			}
//
// 			//Render quad
// 			render();
//
// 			//Update screen
// 			SDL_GL_SwapWindow( gWindow );
// 		}
//
// 		//Disable text input
// 		SDL_StopTextInput( gWindow );
// 	}
//
// 	//Free resources and close SDL
// 	close();
//
// 	return 0;
// }
