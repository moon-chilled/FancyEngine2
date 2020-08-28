module graphics.mesh;
import stdlib;

static if (gfx_backend == GfxBackend.Vulkan) {
	public import graphics.mesh_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	public import graphics.mesh_gl;
}
