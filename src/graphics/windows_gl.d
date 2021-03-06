module graphics.windows_gl;
import stdlib;
import cstdlib;

import bindbc.sdl;
import bindbc.opengl;

import graphics.framebuffer_gl;
import graphics.shading_gl;
import graphics.mesh_gl;
import windowing.windows;

struct GfxContext {
	SDL_GLContext gl_context;
}

struct GfxExtra {
	Framebuffer framebuffer;
	Shader tex_copy;
	Mesh mesh;
}

// in order to do aa properly, we need to make sure we don't ask for more
// samples than the card is willing to do, otherwise bad stuff happens.
// However, in order to determine the maximum allowed number of samples, we
// need to already have an open window.  The solution: open a window.
private GLint max_aa_samples() {
	SDL_Window *win = SDL_CreateWindow("sneaky sneaky ;o".cstr, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1, 1, SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
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
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 5);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
}

enum auxiliary_sdl_window_flags = SDL_WINDOW_OPENGL;

GfxContext win_setup_context(GraphicsState gs) {
	GfxContext ret;
       	ret = GfxContext(SDL_GL_CreateContext(gs.window));
	if (ret.gl_context is null) {
		fatal("Error creating OpenGL context.  SDL says '%s'", SDL_GetError().dstr);
	}

	GLSupport status;
       	status = loadOpenGL();
	if (status == GLSupport.noContext) {
		fatal("Unable to configure OpenGL context.");
	} else if (status == GLSupport.noLibrary) {
		fatal("The OpenGL library file could not be found.  Have you moved (or removed) the DLL?");
	} else if (status == GLSupport.badLibrary) {
		fatal("The OpenGL library file appears to be corrupt");
	} else {
		info("Successfully booted OpenGL (mark II)");
	}

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);
	glFrontFace(GL_CW);

	glClipControl(GL_UPPER_LEFT, GL_NEGATIVE_ONE_TO_ONE);

	static if (build_type == BuildType.Dev) {
		glEnable(GL_DEBUG_OUTPUT);
		extern (C) void gl_debugmsg(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* message, const void* userParam) nothrow {
			try { log("OpenGL: '%s'", message.dstr); } catch (Throwable) {}
		}

		glDebugMessageCallback(&gl_debugmsg, null);
	}

	info("Initialized OpenGL version %s", glGetString(GL_VERSION).dstr);

	return ret;
}

GfxExtra win_setup_extra(GfxContext ctx, GraphicsState gs) {
	GfxExtra ret = GfxExtra(
			Framebuffer(gs.window_spec.render_width, gs.window_spec.render_height, ctx),
			Shader(fslurp("dist/shaders/tex_copy.vert"), fslurp("dist/shaders/tex_copy.frag"), ctx),
			Mesh([-1,+1, 0,1,
			      +1,-1, 1,0,
			      -1,-1, 0,0,

			      +1,-1, 1,0,
			      -1,+1, 0,1,
			      +1,+1, 1,1], [2, 2]));

	ret.tex_copy.set_int("screen_tex", 0);

	return ret;
}

void set_vsync(bool enabled) {
	warning("unable to set vsync yet . . ."); //TODO integrate with graphics manager
	/+
	if (SDL_GL_SetSwapInterval(enabled) == -1) {
		warning("unable to set vsync.  SDL says: '%s'", SDL_GetError().dstr);
	}
	+/

	//TODO: add adaptive sync support (SDL_GL_SetSwapInterval(-1)
}

// stub because direct3d needs it
void set_fullscreen(bool enabled) {
}

void gfx_end(GfxContext ctx) {
}
