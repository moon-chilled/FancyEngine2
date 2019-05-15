module graphics.tex;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.tex_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.tex_gl;
}
