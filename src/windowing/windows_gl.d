module windowing.windows_gl;
import stdlib;
import cstdlib;

import derelict.sdl2.sdl;
import derelict.opengl;

struct GfxContext {
	SDL_GLContext gl_context;
}


private GLint max_aa_samples() {
	SDL_Window *win = SDL_CreateWindow(":)".cstr, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1, 1, SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
	SDL_GL_CreateContext(win);
	GLint ret;
	glGetIntegerv(GL_MAX_SAMPLES, &ret);
	SDL_DestroyWindow(win);
	return ret;
}


void setup_aa(uint samples) {
	GLint max_samples = max_aa_samples();
	if (samples > max_samples) {
		error("was asked for %s samples, but only supported %s", samples, max_samples);
		samples = max_samples;
	}
	if (SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1) < 0) {
		error("unable to turn on aa; SDL says '%s'", SDL_GetError().dstr);
		SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
	}

	if (SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, samples) < 0) {
		error("unable to set aa level to %s; SDL says '%s'", samples, SDL_GetError().dstr);
		SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
	}

	glEnable(GL_MULTISAMPLE);
}

void pre_window_setup() {
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
}

enum auxiliary_sdl_window_flags = SDL_WINDOW_OPENGL;

GfxContext setup_context(SDL_Window *window) {
	GfxContext ret = GfxContext(SDL_GL_CreateContext(window));
	if (ret.gl_context is null) {
		fatal("Error creating OpenGL context.  SDL says '%s'", SDL_GetError().dstr);
	}
	return ret;
}

void post_window_setup(SDL_Window *window) {
	try {
		DerelictGL3.reload();
	} catch(Throwable t) {
		fatal("Error loading OpenGL (mark II).  '%s'", t.msg);
	}

	glEnable(GL_DEPTH_TEST);

	int w, h;
	SDL_GL_GetDrawableSize(window, &w, &h);
	glViewport(0, 0, w, h);
	//TODO: handle render width/height

	info("Initialized OpenGL version %s", glGetString(GL_VERSION).dstr);
}

void set_vsync(bool enabled) {
	if (SDL_GL_SetSwapInterval(enabled) == -1) {
		warning("unable to set vsync.  SDL says: '%s'", SDL_GetError().dstr);
	}

	//TODO: add adaptive sync support (SDL_GL_SetSwapInterval(-1)
}

// stub because direct3d needs it
void set_fullscreen(bool enabled) {
}

void set_wireframe(bool enabled) {
	if (enabled) {
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	} else {
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	}
}

pragma(inline, true) void gfx_blit(GfxContext ctx, SDL_Window *win) {
	SDL_GL_SwapWindow(win);
}

pragma(inline, true) void gfx_clear(GfxContext ctx, float r, float g, float b) {
	glClearColor(r, g, b, 1.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}
