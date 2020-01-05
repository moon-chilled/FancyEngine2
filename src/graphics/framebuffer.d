module graphics.framebuffer;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.framebuffer_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.framebuffer_gl;
}
