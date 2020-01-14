module graphics.font;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.font_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.font_gl;
}
