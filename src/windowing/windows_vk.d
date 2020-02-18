module windowing.windows_vk;
import stdlib;
import cstdlib;

import bindbc.sdl;
import erupted;

import windowing.windows;


struct GfxContext {
	VkInstance instance;
	VkPhysicalDevice phys_device;
	VkDevice device;
	VkSurfaceKHR win_surface;
	VkQueue gfx_queue;
	VkSwapchainKHR swapchain;
}
struct GfxExtra {
	/+
	Framebuffer framebuffer;
	Shader tex_copy;
	Mesh mesh;
	+/
}

// TODO: make it a no-op in release builds?
void vkcheck(int line = __LINE__, string file = __FILE__, string func_name = __FUNCTION__, string pretty_func_name = __PRETTY_FUNCTION__, string module_name = __MODULE__, T...)(VkResult res, T args) {
	if (res != VK_SUCCESS) {
		_real_log(LogLevel.fatal, line, file, func_name, pretty_func_name, module_name, strfmt("Vulkan: %s (%s)", args.length ? strfmt(args) : "error", res));
	}
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
	create_swapchain(window, ret);

	log("Successfully booted vulkan (mark II)");

	return ret;
}

GfxExtra setup_extra(GfxContext ctx, WindowSpec ws) {
	return GfxExtra();
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

	vkcheck(vkCreateInstance(&info, null, &ctx.instance), "error creating instance");

	import erupted.functions: loadInstanceLevelFunctions, loadDeviceLevelFunctions;
	loadInstanceLevelFunctions(ctx.instance);
	loadDeviceLevelFunctions(ctx.instance);
}

private void select_physical_device(ref GfxContext ctx) {
	uint num_devices;
	vkEnumeratePhysicalDevices(ctx.instance, &num_devices, null);
	if (num_devices == 0) fatal("Vulkan: no GPUs available.");

	auto devices = new VkPhysicalDevice[num_devices];
	vkEnumeratePhysicalDevices(ctx.instance, &num_devices, devices.ptr);

	// just pick the first one for now
	// TODO: make this user-configurable, and add a smart way to detect what's probably the best one
	ctx.phys_device = devices[0];
}

private void select_logical_device(ref GfxContext ctx) {
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

	const(char)*[] device_extensions = ["VK_KHR_swapchain"];

	VkDeviceCreateInfo device_info;
	device_info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
	device_info.pQueueCreateInfos = &queue_info;
	device_info.queueCreateInfoCount = 1;
	device_info.pEnabledFeatures = &device_features;
	device_info.enabledExtensionCount = cast(uint)device_extensions.length;
	device_info.ppEnabledExtensionNames = device_extensions.ptr;

	vkcheck(vkCreateDevice(ctx.phys_device, &device_info, null, &ctx.device), "failed to create logical device");

	vkGetDeviceQueue(ctx.device, cast(uint)graphics_family_index, 0, &ctx.gfx_queue);
}

private void create_swapchain(SDL_Window *win, ref GfxContext ctx) {
	VkSurfaceCapabilitiesKHR surface_cap;
	vkcheck(vkGetPhysicalDeviceSurfaceCapabilitiesKHR(ctx.phys_device, ctx.win_surface, &surface_cap), "error getting device surface capabilities");
	VkExtent2D extent;
	if (surface_cap.currentExtent.width == uint.max) {
		int w, h;
		SDL_GetWindowSize(win, &w, &h);
		extent.width = clamp(w, surface_cap.minImageExtent.width, surface_cap.maxImageExtent.width);
		extent.height = clamp(h, surface_cap.minImageExtent.height, surface_cap.maxImageExtent.height);
	} else {
		extent = surface_cap.currentExtent;
	}

	uint img_count = surface_cap.minImageCount + 1;
	if (surface_cap.maxImageCount != 0) img_count = min(img_count, surface_cap.maxImageCount);

	uint num_clr_formats;
	vkcheck(vkGetPhysicalDeviceSurfaceFormatsKHR(ctx.phys_device, ctx.win_surface, &num_clr_formats, null), "error getting device surface colour formats");
	if (!num_clr_formats) fatal("no device surface colour formats");
	auto surface_clr_formats = new VkSurfaceFormatKHR[num_clr_formats];
	vkcheck(vkGetPhysicalDeviceSurfaceFormatsKHR(ctx.phys_device, ctx.win_surface, &num_clr_formats, surface_clr_formats.ptr), "error getting device surface colour formats");

	VkSurfaceFormatKHR surface_fmt;
	bool got_format = false;
	foreach (f; surface_clr_formats) {
		if (f.format == VK_FORMAT_B8G8R8A8_SRGB && f.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
			surface_fmt = f;
			got_format = true;
			break;
		}
	}
	if (!got_format) surface_fmt = surface_clr_formats[0];


	/+
	uint num_present_modes;
	vkcheck(vkGetPhysicalDeviceSurfacePresentModesKHR(ctx.phys_device, ctx.win_surface, &num_present_modes, null), "error device surface presentation modes");
	if (!num_present_modes) fatal("no device surface presentation modes");
	auto surface_present_modes = new VkPresentModeKHR[num_present_modes];
	vkcheck(vkGetPhysicalDeviceSurfacePresentModesKHR(ctx.phys_device, ctx.win_surface, &num_present_modes, surface_present_modes.ptr), "error getting device surface presentation modes");
	+/
	// TODO: fancier present modes w/ above
	VkPresentModeKHR present_mode = VK_PRESENT_MODE_FIFO_KHR;

	VkSwapchainCreateInfoKHR swapchain_info;
	swapchain_info.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
	swapchain_info.surface = ctx.win_surface;
	swapchain_info.minImageCount = img_count;
	swapchain_info.imageFormat = surface_fmt.format;
	swapchain_info.imageColorSpace = surface_fmt.colorSpace;
	swapchain_info.imageExtent = extent;
	swapchain_info.imageArrayLayers = 1; //TODO: vr?
	swapchain_info.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT; // TODO: framebuffers

	uint num_queue_families;
	vkGetPhysicalDeviceQueueFamilyProperties(ctx.phys_device, &num_queue_families, null);
	VkQueueFamilyProperties[] queue_families = new VkQueueFamilyProperties[num_queue_families];
	vkGetPhysicalDeviceQueueFamilyProperties(ctx.phys_device, &num_queue_families, queue_families.ptr);

	uint gfx_family_i, present_family_i;
	bool got_gfx, got_present;
	foreach (i; 0 .. num_queue_families) {
		if (queue_families[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) {
			gfx_family_i = i;
			got_gfx = true;
		}

		VkBool32 support_present;
		vkcheck(vkGetPhysicalDeviceSurfaceSupportKHR(ctx.phys_device, i, ctx.win_surface, &support_present), "couldn't find whether phys device supports presentation to surface");

		if (support_present) {
			got_present = true;
			present_family_i = i;
		}
	}

	if (!got_gfx) fatal("Vulkan: couldn't find graphics queue family with support for graphics");
	if (!got_present) fatal("Vulkan: couldn't find graphics queue family with support for surface presentation");

	uint[2] gfx_families = [gfx_family_i, present_family_i];

	if (gfx_family_i == present_family_i) {
		swapchain_info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
	} else {
		swapchain_info.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
		swapchain_info.queueFamilyIndexCount = 2;
		swapchain_info.pQueueFamilyIndices = gfx_families.ptr;
	}

	swapchain_info.preTransform = surface_cap.currentTransform;
	swapchain_info.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;

	swapchain_info.presentMode = present_mode;
	swapchain_info.clipped = true;

	swapchain_info.oldSwapchain = VK_NULL_HANDLE;

	vkcheck(vkCreateSwapchainKHR(ctx.device, &swapchain_info, null, &ctx.swapchain), "could not create swapchain");
}


void post_window_setup(SDL_Window *window) {
}

void set_vsync(bool enabled) {
}

void set_fullscreen(bool) {
}

void set_wireframe(bool) {
}

pragma(inline, true) void gfx_blit(GfxContext ctx, GfxExtra extra, SDL_Window *win) {
}

pragma(inline, true) void gfx_clear(GfxContext ctx, float r, float g, float b) {
}

void gfx_end(GfxContext ctx) {
	vkDestroySwapchainKHR(ctx.device, ctx.swapchain, null);
	vkDestroySurfaceKHR(ctx.instance, ctx.win_surface, null);
	vkDestroyDevice(ctx.device, null);
	vkDestroyInstance(ctx.instance, null);
	log("Died vulkan");
}
