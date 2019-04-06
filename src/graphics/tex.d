module graphics.tex;
import stdlib;

static if (gfx_backend == GfxBackend.Direct3D11) {
	public import graphics.tex_d3d11;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.tex_gl;
}
