module windowing.windows_d3d11;
import stdlib;
import cstdlib;

import directx.d3d11;
import derelict.sdl2.sdl;

enum auxiliary_sdl_window_flags = 0;
private __gshared uint aa_samples = 0;
private __gshared bool vsync, windowed;

struct GfxContext {
	IDXGISwapChain swapchain;
	ID3D11Device device;
	ID3D11DeviceContext device_context;
	ID3D11RenderTargetView back_buffer;
}

// these stubs are needed for compatability with opengl, because opengl needs aa setup _before_ the context is created, opengl version hinting needs to be done before the window is created
void setup_aa(uint samples) {
	if (samples > D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT) {
		error("was asked for %s samples, but only supported %s", samples, D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT);
		samples = D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT;
	}

	synchronized aa_samples = samples;
}

void pre_window_setup() {
}

private HWND get_window_handle(SDL_Window *window) {
	SDL_SysWMinfo info;
	SDL_VERSION(&info.version_);
	SDL_GetWindowWMInfo(window, &info);
	return info.info.win.window;
}

GfxContext setup_context(SDL_Window *window) {
	GfxContext ret;

	DXGI_SWAP_CHAIN_DESC desc;
	desc.BufferCount = 1;
	desc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT; // yeesh.  I may not be allowed to comment on this, seeing as how I like perl and all, but come on.  A _bit_ of terseness would be nice
	desc.SampleDesc.Count = aa_samples;
	desc.Windowed = windowed;
	desc.OutputWindow = get_window_handle(window);

	uint flags;
	debug flags = D3D11_CREATE_DEVICE_DEBUG;
	auto res = D3D11CreateDeviceAndSwapChain(null, D3D_DRIVER_TYPE_HARDWARE, null, flags, [D3D_FEATURE_LEVEL_11_0].ptr, 1, D3D11_SDK_VERSION, &desc, &ret.swapchain, &ret.device, null, &ret.device_context);
	if (FAILED(res) || !(ret.swapchain && ret.device && ret.device_context)) {
		fatal("failed to initialize Direct3D 11.  Result of '%s'", res);
	}

	ID3D11Texture2D pBackBuffer; // the one time I agree to stoop to the level of the hungarians
	ret.swapchain.GetBuffer(0, &IID_ID3D11Texture2D, cast(void**)&pBackBuffer);
	ret.device.CreateRenderTargetView(pBackBuffer, null, &ret.back_buffer);
	pBackBuffer.Release();
	ret.device_context.OMSetRenderTargets(1, &ret.back_buffer, null);

	int w, h;
	SDL_GL_GetDrawableSize(window, &w, &h);
	D3D11_VIEWPORT viewport;
	viewport.TopLeftX = viewport.TopLeftY = 0;
	viewport.Width = w;
	viewport.Height = h;
	ret.device_context.RSSetViewports(1, &viewport);

	return ret;
}

void post_window_setup(SDL_Window *window) {
}

void set_vsync(bool enabled) {
	vsync = enabled;
}
void set_fullscreen(bool enabled) {
	synchronized windowed = !enabled;
}

void set_wireframe(bool enabled) {
	//TODO
}

pragma(inline, true) void gfx_blit(GfxContext ctx, SDL_Window *win) {
	ctx.swapchain.Present(vsync, 0);
}

pragma(inline, true) void gfx_clear(GfxContext ctx, float r, float g, float b) {
	ctx.device_context.ClearRenderTargetView(ctx.back_buffer, [r, g, b, 1.0f].ptr);
}

void gfx_end(GfxContext ctx) {
	ctx.swapchain.Release();
	ctx.device.Release();
	ctx.device_context.Release();
	ctx.back_buffer.Release();
}
