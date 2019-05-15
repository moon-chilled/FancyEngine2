module graphics.shading;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.shading_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.shading_gl;
}
