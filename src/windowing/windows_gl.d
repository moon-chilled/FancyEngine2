module windowing.windows_gl;
import stdlib;
import cstdlib;

import bindbc.sdl;
import bindbc.opengl;

import graphics.framebuffer_gl;
import graphics.shading_gl;
import graphics.model_gl;
import windowing.windows;
import graphics.gl_thread;

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

GfxContext setup_context(SDL_Window *window) {
	log("About to start gl thread");
	glthread_start();
	GfxContext ret;
       	glwait({ret = GfxContext(SDL_GL_CreateContext(window));});
	if (ret.gl_context is null) {
		fatal("Error creating OpenGL context.  SDL says '%s'", SDL_GetError().dstr);
	}

	return ret;
}

GfxExtra setup_extra(GfxContext ctx, WindowSpec ws) {
	GfxExtra ret = GfxExtra(
			Framebuffer(ws.render_width, ws.render_height, ctx),
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


void post_window_setup(SDL_Window *window) {
	GLSupport status;
       	glwait({status = loadOpenGL();});
	if (status == GLSupport.noContext) {
		fatal("Unable to configure OpenGL context.");
	} else if (status == GLSupport.noLibrary) {
		fatal("The OpenGL library file could not be found.  Have you moved (or removed) the DLL?");
	} else if (status == GLSupport.badLibrary) {
		fatal("The OpenGL library file appears to be corrupt");
	} else {
		info("Successfully booted OpenGL (mark II)");
	}

	glwait({
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
	});

}

void set_vsync(bool enabled) {
	glwait({
		if (SDL_GL_SetSwapInterval(enabled) == -1) {
			warning("unable to set vsync.  SDL says: '%s'", SDL_GetError().dstr);
		}
	});

	//TODO: add adaptive sync support (SDL_GL_SetSwapInterval(-1)
}

// stub because direct3d needs it
void set_fullscreen(bool enabled) {
}

void set_wireframe(bool enabled) {
	if (enabled) {
		glwait({glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);});
	} else {
		glwait({glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);});
	}
}

pragma(inline, true) void gfx_blit(GfxContext ctx, ref GfxExtra extra, SDL_Window *win) {
	glwait({
	SDL_GL_MakeCurrent(win, ctx.gl_context);

	glDisable(GL_DEPTH_TEST);
	int w, h;
	SDL_GL_GetDrawableSize(win, &w, &h);
	glViewport(0, 0, w, h);

	// XXX: this causes flickering with mesa+dri3.
	//glBlitNamedFramebuffer(extra.framebuffer.fbo, 0, 0, 0, extra.framebuffer.w, extra.framebuffer.h, 0, 0, w, h, GL_COLOR_BUFFER_BIT, GL_LINEAR);

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glBindTexture(GL_TEXTURE_2D, extra.framebuffer.tex);
	extra.tex_copy.blit(extra.mesh);

	SDL_GL_SwapWindow(win);	

	glBindFramebuffer(GL_FRAMEBUFFER, extra.framebuffer.fbo);
	glViewport(0, 0, extra.framebuffer.w, extra.framebuffer.h);
	glEnable(GL_DEPTH_TEST);

	});
}

pragma(inline, true) void gfx_clear(GfxContext ctx, float r, float g, float b) {
	glwait({
	glClearColor(r, g, b, 1.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	});
}

void gfx_end(GfxContext ctx) {
	glthread_stop();
}
