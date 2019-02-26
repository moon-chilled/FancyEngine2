module windowing.windows;
import derelict.sdl2.sdl;
import derelict.opengl;
import std.string: cstr = toStringz;
import std.string: dstr = fromStringz;

import logging;

struct GraphicsState {
	SDL_GLContext gl_context;

	SDL_Window *window; // maybe I should allow for multiple windows.  But meh
	GLuint program_id, VBO, IBO; // each window gets a different one

	WindowSpec window_spec;
}

enum Fullscreenstate {
	None,
	Desktop,
	Fullscreen
}
struct WindowSpec {
	string title;
	uint win_width, win_height;
	uint render_width, render_height;
	Fullscreenstate fullscreen;
	bool borders, vsync;
}
private void sdlerror() {
	fatal("SDL Error: %s", SDL_GetError());
}


GraphicsState new_winstate(WindowSpec window) {
	try {
		DerelictGL3.load();
	} catch(Throwable) {
		fatal("Error loading OpenGL (mark I)");
	}
	try {
		version (Win64) {
			DerelictSDL2.load("lib/win/SDL2.dll");
		} else version (Win32) {
			DerelictSDL2.load("lib/win32/SDL2.dll");
		} else {
			DerelictSDL2.load();
		}
	} catch(Throwable) {
		fatal("Error loading SDL2");
	}

	with (window) trace("Opening %s window titled '%s' (%sx%s, render resolution %sx%s), fullscreen state %s, with%s vsync", borders ? "bordered" : "borderless", title, win_width, win_height, render_width, render_height, window.fullscreen, vsync ? "" : "out");

	GraphicsState ret;
	ret.window_spec = window;

	if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_EVENTS) < 0) sdlerror;

	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

	SDL_WindowFlags win_flags = SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN;
	if (!window.borders) win_flags |= SDL_WINDOW_BORDERLESS;
	if (window.fullscreen == Fullscreenstate.Fullscreen) win_flags |= SDL_WINDOW_FULLSCREEN;
	if (window.fullscreen == Fullscreenstate.Desktop) win_flags |= SDL_WINDOW_FULLSCREEN_DESKTOP;

	// 0, 0: window position
	ret.window = SDL_CreateWindow(window.title.cstr, 0, 0, window.win_width, window.win_height, win_flags);
	if (!ret.window) sdlerror;

	ret.gl_context = SDL_GL_CreateContext(ret.window);
	if (ret.gl_context is null) {
		fatal("Error creating OpenGL context");
	}

	try {
		DerelictGL3.reload();
	} catch(Throwable) {
		fatal("Error loading OpenGL (mark II)");
	}

	if (window.vsync) {
		if (SDL_GL_SetSwapInterval(1) == -1) {
			warning("Unable to enable vsync.  SDL says: %s", SDL_GetError());
		}
	} else {
		if (SDL_GL_SetSwapInterval(0) == -1) {
			warning("Unable to turn off vsync.  SDL says: %s", SDL_GetError());
		}
	}


	info("Initialized OpenGL version %s", glGetString(GL_VERSION).dstr);

	return ret;
}
