module graphics.shading;
import stdlib;

static if (gfx_backend == GfxBackend.D3D11) {
	public import graphics.shading_d3d11;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.shading_gl;
}
