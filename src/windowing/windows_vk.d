module windowing.windows_vk;
import stdlib;
import cstdlib;

import derelict.sdl2.sdl;
import erupted;

struct GfxContext {
	VkInstance instance;
}

void setup_aa(uint samples) {
}

void pre_window_setup() {
}

enum auxiliary_sdl_window_flags = SDL_WINDOW_VULKAN;

GfxContext setup_context(SDL_Window *window) {
	GfxContext ret;

	VkApplicationInfo app_info = {sType:VK_STRUCTURE_TYPE_APPLICATION_INFO,
			pApplicationName:"I am not a portable sexapod!",
			pEngineName:"FancyEngine2",
			apiVersion:VK_MAKE_VERSION(1, 0, 3)};

	VkInstanceCreateInfo info;
	info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
	info.pApplicationInfo = &app_info;

	uint sdl_vk_ext_count;
	SDL_Vulkan_GetInstanceExtensions(null, &sdl_vk_ext_count, null);
	const(char)*[] sdl_vk_exts = new char*[sdl_vk_ext_count];
	SDL_Vulkan_GetInstanceExtensions(null, &sdl_vk_ext_count, sdl_vk_exts.ptr);

	info.enabledExtensionCount = sdl_vk_ext_count;
	info.ppEnabledExtensionNames = sdl_vk_exts.ptr;

	VkResult res = vkCreateInstance(&info, null, &ret.instance);
	if (res != VK_SUCCESS) {
		fatal("Error creating vulkan instance!  '%s'", res);
	}

	log("Successfully initialized vulkan device");

	return ret;
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
	//TODO: this segfaults
	//vkDestroyInstance(ctx.instance, null);
}
