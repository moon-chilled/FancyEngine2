module graphics.fancy_model;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.fancy_model_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.fancy_model_gl;
}
