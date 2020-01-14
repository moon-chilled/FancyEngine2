module graphics.fancy_font;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.fancy_font_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.fancy_font_gl;
}
