module graphics.fancy_font_gl;
import stdlib;
import cstdlib;

import asset;
import windowing.windows_gl;

import bindbc.opengl;



// pathfinder bindings
private extern (C) {
	// opaque types
	struct PFRendererGLDevicePrivate;
	alias PFGLRendererRef = PFRendererGLDevicePrivate*;
	struct PFDestFramebufferGLDevicePrivate;
	alias PFGLDestFramebufferRef = PFDestFramebufferGLDevicePrivate*;
	struct PFGLDevicePrivate;
	alias PFGLDeviceRef = PFGLDevicePrivate*;
	struct PFResourceLoaderWrapperPrivate ;
	alias PFResourceLoaderRef = PFResourceLoaderWrapperPrivate*;
	struct PFCanvasRenderingContext2DPrivate;
	alias PFCanvasRef = PFCanvasRenderingContext2DPrivate*;
	struct PFCanvasFontContextPrivate;
	alias PFCanvasFontContextRef = PFCanvasFontContextPrivate*;
	struct FKHandlePrivate;
	alias FKHandleRef = FKHandlePrivate*;
	struct FK_Vec_u8;
	alias FkDataRef = const FK_Vec_u8*;
	struct PFSceneProxyPrivate;
       	alias PFSceneProxyRef = PFSceneProxyPrivate*;
	struct PFScenePrivate;
	alias PFSceneRef = PFScenePrivate*;
	struct PFBuildOptionsPrivate;
	alias PFBuildOptionsRef = PFBuildOptionsPrivate*;

	// other types
	struct PFVector2I { int x; int y; }
	struct PFVector2F { float x; float y; }

	alias PFRendererOptionsFlags = ubyte;
	struct PFRendererOptions {
		PFColorF background_color;
		PFRendererOptionsFlags flags;
	}

	struct PFColorF { float r, g, b, a; }

	alias PFGLVersion = ubyte;

	alias PFGLFunctionLoader = const void *function(const char *name, void *userdata);

	// constants
	enum PF_RENDERER_OPTIONS_FLAGS_HAS_BACKGROUND_COLOR = 1;
	enum PF_GL_VERSION_GL3 = 1;

	void PFGLLoadWith(PFGLFunctionLoader loader, void *userdata);
	PFGLDestFramebufferRef PFGLDestFramebufferCreateFullWindow(const PFVector2I *window_size);
	PFGLRendererRef PFGLRendererCreate(PFGLDeviceRef device, PFResourceLoaderRef resources, PFGLDestFramebufferRef dest_framebuffer, const PFRendererOptions *options);

	PFGLDeviceRef PFGLDeviceCreate(PFGLVersion version_, uint default_framebuffer);
	PFResourceLoaderRef PFFilesystemResourceLoaderLocate();

	PFCanvasFontContextRef PFCanvasFontContextCreateWithSystemSource();
	PFCanvasRef PFCanvasCreate(PFCanvasFontContextRef font_context, const PFVector2F *size);

	PFCanvasFontContextRef PFCanvasFontContextCreateWithFonts(const FKHandleRef *fonts, size_t font_count);

	FkDataRef FKDataCreate(const ubyte *bytes, size_t len);
	void FKDataDestroy(FkDataRef data);
	FKHandleRef FKHandleCreateWithMemory(FkDataRef bytes, uint font_index);
	void FKHandleDestroy(FKHandleRef handle);


	void PFCanvasFillText(PFCanvasRef canvas, const char *string, size_t string_len, const PFVector2F *position);

	void PFSceneProxyBuildAndRenderGL(PFSceneProxyRef scene_proxy, PFGLRendererRef renderer, PFBuildOptionsRef build_options);
	PFSceneProxyRef PFSceneProxyCreateFromSceneAndRayonExecutor(PFSceneRef scene);

	PFSceneRef PFCanvasCreateScene(PFCanvasRef canvas);

	void PFCanvasDestroy(PFCanvasRef canvas);
}

extern (C) void *gl_loader_funcp(const char *name, void *userdata) {
	import bindbc.opengl.context;
	void *ret;
	bindGLSymbol(libGL, &ret, name);
	return ret;
}

class Fontstuff {
	uint width, height;
	PFGLDeviceRef device;
	PFResourceLoaderRef resource_loader;
	PFGLDestFramebufferRef fb;
	PFGLRendererRef renderer;
	FkDataRef font_data;
	FKHandleRef font_handle;
	PFCanvasFontContextRef font_ctx;
	PFCanvasRef canvas;

	this(string fpath) {
		if (!fpath.fexists) fatal("tried to read nonexistent texture '%s'", fpath);
		string data = fslurp(fpath);

		PFVector2F win_sizef = PFVector2F(1280, 720); // TODO: determine dynamically
		PFVector2I win_sizei = PFVector2I(1280, 720); // TODO: determine dynamically

		PFGLLoadWith(&gl_loader_funcp, null);

		// 2nd arg is framebuffer; should probably associate one?
		device = PFGLDeviceCreate(PF_GL_VERSION_GL3, 0);
		if (!device) fatal("can't create pf device");
		resource_loader = PFFilesystemResourceLoaderLocate();
		if (!resource_loader) fatal("can't create pf resource loader");

		fb = PFGLDestFramebufferCreateFullWindow(&win_sizei);
		if (!fb) fatal("can't create pf framebuffer");

		PFRendererOptions opts;
		opts.background_color = PFColorF(1, 1, 1, 1);
		renderer = PFGLRendererCreate(device, resource_loader, fb, &opts);
		if (!renderer) fatal("can't create pf renderer");

		// can these be local?
		font_data = FKDataCreate(cast(const ubyte*)data.ptr, data.length);
		font_handle = FKHandleCreateWithMemory(font_data, 0);

		font_ctx = PFCanvasFontContextCreateWithFonts(&font_handle, 1);

		canvas = PFCanvasCreate(font_ctx, &win_sizef);
	}

	void draw_text(string text) {
		PFVector2F pos = PFVector2F(0, 0);
		PFCanvasFillText(canvas, text.ptr, text.length, &pos);

		PFSceneProxyCreateFromSceneAndRayonExecutor(PFCanvasCreateScene(canvas));
	}

	~this() {
		PFCanvasDestroy(canvas);
		FKDataDestroy(font_data);
		FKHandleDestroy(font_handle);
	}
}
