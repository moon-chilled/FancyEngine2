module graphics.font_gl;
import stdlib;
import cstdlib;

import asset;
import windowing.windows_gl;

import bindbc.opengl;

// pathfinder bindings
private extern (C) {
	alias PFGLFunctionLoader = const void *function(const char *name, void *userdata);
	void PFGLLoadWith(PFGLFunctionLoader loader, void *userdata);
	struct PFDestFramebufferGLDevicePrivate;
	alias PFGLDestFramebufferRef = PFDestFramebufferGLDevicePrivate*;
	PFGLDestFramebufferRef PFGLDestFramebufferCreateFullWindow(const PFVector2I *window_size);
	struct PFVector2I { int x; int y; }
	struct PFRendererGLDevicePrivate;
	alias PFGLRendererRef = PFRendererGLDevicePrivate*;
	PFGLRendererRef PFGLRendererCreate(PFGLDeviceRef device, PFResourceLoaderRef resources, PFGLDestFramebufferRef dest_framebuffer, const PFRendererOptions *options);
	struct PFGLDevicePrivate;
	alias PFGLDeviceRef = PFGLDevicePrivate*;
	struct PFResourceLoaderWrapperPrivate ;
	alias PFResourceLoaderRef = PFResourceLoaderWrapperPrivate*;

	alias PFRendererOptionsFlags = ubyte;
	struct PFRendererOptions {
		PFColorF background_color;
		PFRendererOptionsFlags flags;
	}
	struct PFColorF { float r, g, b, a; }

	enum PF_GL_VERSION_GL3 = 1;
	alias PFGLVersion = ubyte;
	PFGLDeviceRef PFGLDeviceCreate(PFGLVersion version, uint default_framebuffer);
	PFResourceLoaderRef PFFilesystemResourceLoaderLocate();
	enum PF_RENDERER_OPTIONS_FLAGS_HAS_BACKGROUND_COLOR = 1;
	struct PFCanvasRenderingContext2DPrivate;
	alias PFCanvasRef = PFCanvasRenderingContext2DPrivate*;
	PFCanvasFontContextRef PFCanvasFontContextCreateWithSystemSource();
	PFCanvasRef PFCanvasCreate(PFCanvasFontContextRef font_context, const PFVector2F *size);

	struct PFCanvasFontContextPrivate;
	alias PFCanvasFontContextRef = PFCanvasFontContextPrivate*;

	PFCanvasFontContextRef PFCanvasFontContextCreateWithFonts(const FKHandleRef *fonts, size_t font_count);
	struct FKHandlePrivate;
	alias FKHandleRef = FKHandlePrivate*;

}

class Fontstuff {
	uint width;
	uint height;

	this(string fpath) {
		if (!fpath.fexists) fatal("tried to read nonexistent texture '%s'", fpath);
		string data = fslurp(fpath);
	}
}
