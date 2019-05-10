module graphics.model;
import stdlib;

static if (gfx_backend == GfxBackend.D3D11) {
	public import graphics.model_d3d11;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.model_gl;
}
