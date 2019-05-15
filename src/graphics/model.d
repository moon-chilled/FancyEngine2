module graphics.model;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.model_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.model_gl;
}
