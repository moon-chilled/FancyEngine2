module windowing.windows_vk;
import stdlib;
import cstdlib;

import bindbc.sdl;
import erupted;

struct GfxContext {
	VkInstance instance;
	VkPhysicalDevice phys_device;
	VkDevice device;
	VkSurfaceKHR win_surface;
	VkQueue gfx_queue;
}

void setup_aa(uint samples) {
}

void pre_window_setup() {
}

enum auxiliary_sdl_window_flags = SDL_WINDOW_VULKAN;

GfxContext setup_context(SDL_Window *window) {
	GfxContext ret;

	setup_instance(ret);

	if (!SDL_Vulkan_CreateSurface(window, ret.instance, &ret.win_surface)) {
		fatal("Vulkan: failed to create surface for window");
	}

	select_physical_device(ret);
	select_logical_device(ret);

	log("Successfully booted vulkan");

	return ret;
}

void setup_instance(ref GfxContext ctx) {
	static if (build_type == BuildType.Dev) {
		string[] desired_validation_layers = ["VK_LAYER_KHRONOS_validation"];
	} else {
		string[] desired_validation_layers = [];
	}

	string[] request_validation_layers(string[] desired) {
		uint num_available_validators;
		vkEnumerateInstanceLayerProperties(&num_available_validators, null);

		auto available = new VkLayerProperties[num_available_validators];
		vkEnumerateInstanceLayerProperties(&num_available_validators, available.ptr);

		return desired.filter!((x) => available.map!((a) => a.layerName.dstr).contains(x));
	}

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

	string[] received_layers = request_validation_layers(desired_validation_layers);
	if (received_layers != desired_validation_layers) {
		string[] missed_layers = desired_validation_layers.filter!((x) => !received_layers.contains(x));
		warning("Vulkan: requested %d layers, but only received %d -- %-(%s, %) were missing.", desired_validation_layers.length,
				received_layers.length,
				missed_layers);
	} else {
		log("Vulkan: continuing with %d validation layers: %-(%s, %)", received_layers.length, received_layers);
	}

	info.enabledLayerCount = cast(uint)received_layers.length; // I promise never to enable more than (2^32)-1 validation layers
	info.ppEnabledLayerNames = received_layers.map!cstr.ptr;

	VkResult res = vkCreateInstance(&info, null, &ctx.instance);
	if (res != VK_SUCCESS) {
		fatal("Vulkan: error creating instance!  '%s'", res);
	}
}

private void select_physical_device(ref GfxContext ctx) {
	uint num_devices;
	vkEnumeratePhysicalDevices(ctx.instance, &num_devices, null);
	if (num_devices == 0) fatal("Vulkan: no GPUs available.");

	auto devices = new VkPhysicalDevice[num_devices];
	vkEnumeratePhysicalDevices(ctx.instance, &num_devices, devices.ptr);

	// just pick the first one for now
	ctx.phys_device = devices[0];
}

void select_logical_device(ref GfxContext ctx) {
	long graphics_family_index = -1;

	uint num_queue_families;
	vkGetPhysicalDeviceQueueFamilyProperties(ctx.phys_device, &num_queue_families, null);
	auto queue_families = new VkQueueFamilyProperties[num_queue_families];
	vkGetPhysicalDeviceQueueFamilyProperties(ctx.phys_device, &num_queue_families, queue_families.ptr);

	foreach (i; 0 .. queue_families.length) {
		if (queue_families[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) {
			graphics_family_index = i;
		}
	}

	if (graphics_family_index < 0) fatal("Vulkan: no suitable graphics queue found.");

	VkDeviceQueueCreateInfo queue_info;
	queue_info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
	queue_info.queueFamilyIndex = cast(uint)graphics_family_index;
	queue_info.queueCount = 1;
	float prio = 1; queue_info.pQueuePriorities = &prio;

	VkPhysicalDeviceFeatures device_features;

	VkDeviceCreateInfo device_info;
	device_info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
	device_info.pQueueCreateInfos = &queue_info;
	device_info.queueCreateInfoCount = 1;
	device_info.pEnabledFeatures = &device_features;

	VkResult res = vkCreateDevice(ctx.phys_device, &device_info, null, &ctx.device);
	if (res != VK_SUCCESS) {
		fatal("Vulkan: failed to create logical devices.  '%s'", res);
	}

	vkGetDeviceQueue(ctx.device, cast(uint)graphics_family_index, 0, &ctx.gfx_queue);
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
	vkDestroyDevice(ctx.device, null);
	//vkDestroyInstance(ctx.instance, null);
}
