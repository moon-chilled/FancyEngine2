module windowing.windows_vk;
import stdlib;
import cstdlib;

import derelict.sdl2.sdl;
import erupted;

struct GfxContext {
}

void setup_aa(uint samples) {
}

void pre_window_setup() {
	uint num_extensions;
	vkEnumerateInstanceExtensionProperties(null, &num_extensions, null);
	log("Booted vulkan with %s extensions", num_extensions);
}

enum auxiliary_sdl_window_flags = SDL_WINDOW_VULKAN;

GfxContext setup_context(SDL_Window *window) {
	return GfxContext();
}

void post_window_setup(SDL_Window *window) {
}

void set_vsync(bool enabled) {
}

void set_fullscreen(bool) {
}

void set_wireframe(bool) {
}

pragma(inline, true) void gfx_blit(GfxContext ctx, SDL_Window *win) {
}

pragma(inline, true) void gfx_clear(GfxContext ctx, float r, float g, float b) {
}

void gfx_end(GfxContext ctx) {
}
