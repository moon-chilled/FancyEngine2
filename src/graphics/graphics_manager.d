module graphics.graphics_manager;
import stdlib;
import stdmath;

import windowing.windows;

import graphics.fancy_model;
import graphics.font;
import graphics.framebuffer;
import graphics.mesh;
import graphics.shading;
import graphics.tex;


import std.concurrency;

static if (gfx_backend == GfxBackend.Vulkan) {
	import graphics.graphics_manager_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	import graphics.graphics_manager_gl;
}

//TODO this should all be asynchronous.
//Backend should immediately return a partially constructed object with a flag that says when it's done

// 'req' events are synchronous and get responded to with a corresponding 'ack' event
// 'push' events are completely asynchronous (though you can be assured they're all flushed by explicitly syncing)
package struct GMReqSync { Tid from; }
package struct GMAckSync {}

package struct GMReqDie { Tid from; }
package struct GMAckDie {}

package struct GMPushBlit {}

package struct GMPushClear { float r, g, b; }

package struct GMReqNewFancyModel { Tid from; string fpath; }
package struct GMAckNewFancyModel { FancyModel fm; }

package struct GMReqNewFont { Tid from; string fpath; uint height; uint scr_w; uint scr_h; }
package struct GMAckNewFont { Font f; }
package struct GMPushDrawText { Font f; float x, y; string s; }
package struct GMPushDestroyFont { Font f; }

package struct GMReqNewFramebuffer { Tid from; uint w; uint h; }
package struct GMAckNewFramebuffer { Framebuffer f; }
package struct GMPushDestroyFramebuffer { Framebuffer f; }

package struct GMReqNewMesh { Tid from; float[] vertices; uint[] sizes; }
package struct GMAckNewMesh { Mesh m; }
package struct GMPushMeshLoadVerts { Mesh m; float[] vertices; }

package struct GMReqNewShader { Tid from; string vertex, fragment; }
package struct GMAckNewShader { Shader s; }
package struct GMPushShaderSetInt { Shader s; string id; int value; }
package struct GMPushShaderSetMat4 { Shader s; string id; mat4f value; }
package struct GMPushShaderBlitMesh { Shader s; Mesh m; }
package struct GMPushShaderBlitMeshWithTex { Shader s; Mesh m; Texture t; }
package struct GMPushShaderBlitFancyModel { Shader s; FancyModel fm; }
package struct GMPushDestroyShader { Shader s; }

package struct GMReqNewTextureFromData { Tid from; ubyte[] data; uint width, height; ubyte clr_depth; }
package struct GMReqNewTextureFromFile { Tid from; string fpath; }
package struct GMAckNewTexture { Texture t; }
package struct GMPushTextureBlit { Texture t; vec2f[2] loc; }

//alias GMMsg = Sum!(GMReqSync, GMAckSync, GMReqDie, GMAckDie, GMPushBlit, GMPushClear, GMReqNewFancyModel, GMAckNewFancyModel, GMReqNewFont, GMAckNewFont, GMPushDrawText, GMPushDestroyFont, GMReqNewFramebuffer, GMAckNewFramebuffer, GMPushDestroyFramebuffer, GMReqNewMesh, GMAckNewMesh, GMPushMeshLoadVerts, GMReqNewShader, GMAckNewShader, GMPushShaderSetInt, GMPushShaderSetMat4, GMPushShaderBlitMesh, GMPushShaderBlitFancyModel, GMPushDestroyShader, GMReqNewTextureFromData, GMReqNewTextureFromFile, GMAckNewTexture);

class GraphicsManager {
	Tid backend;

	this(GraphicsState gs) {
		backend = spawn(&real_graphics_manager, cast(shared)gs);
		sync();
	}

	void sync() {
		backend.send(GMReqSync(thisTid));
		receive((GMAckSync _){});
	}

	void die() {
		backend.send(GMReqDie(thisTid));
		receive((GMAckDie _){});
	}

	void blit() {
		backend.send(GMPushBlit());
		sync(); //TODO figure out if we need 'sync'
	}

	void clear(float r, float g, float b) {
		backend.send(GMPushClear(r, g, b));
	}

	FancyModel new_fancy_model(string fpath) {
		backend.send(GMReqNewFancyModel(thisTid, fpath)); FancyModel ret; receive((GMAckNewFancyModel fm) => ret = cast()fm.fm);
		return ret;
	}

	Font new_font(string fpath, uint height, uint scr_w, uint scr_h) {
		backend.send(GMReqNewFont(thisTid, fpath, height, scr_w, scr_h));
		Font ret;
		receive((GMAckNewFont f) => ret = cast()f.f);
		return ret;
	}
	void draw_text(Font f, float x, float y, string s) {
		backend.send(GMPushDrawText(f, x, y, s));
	}
	void destroy_font(Font f) {
		backend.send(GMPushDestroyFont(f));
	}

	Framebuffer new_framebuffer(uint w, uint h) {
		backend.send(GMReqNewFramebuffer(thisTid, w, h));
		Framebuffer ret;
		receive((GMAckNewFramebuffer f) => ret = f.f);
		return ret;
	}
	void destroy_framebuffer(Framebuffer f) {
		backend.send(GMPushDestroyFramebuffer(f));
	}

	Mesh new_mesh(float[] vertices, uint[] sizes) {
		backend.send(GMReqNewMesh(thisTid, vertices, sizes));
		Mesh ret;
		receive((GMAckNewMesh m) => ret = m.m);
		return ret;
	}
	void mesh_load_verts(Mesh m, float[] vertices) {
		backend.send(GMPushMeshLoadVerts(m, vertices));
	}

	Shader new_shader(string vertex, string fragment) {
		backend.send(GMReqNewShader(thisTid, vertex, fragment));
		Shader ret;
		receive((GMAckNewShader s) => ret = s.s);
		return ret;
	}
	void shader_set_int(Shader s, string id, int value) {
		backend.send(GMPushShaderSetInt(s, id, value));
	}
	void shader_set_mat4(Shader s, string id, mat4f value) {
		backend.send(GMPushShaderSetMat4(s, id, value));
	}
	void shader_blit(Shader s, Mesh m) {
		backend.send(GMPushShaderBlitMesh(s, m));
	}
	//todo allow for more textures
	void shader_blit(Shader s, Mesh m, Texture tex) {
		backend.send(GMPushShaderBlitMeshWithTex(s, m, tex));
	}
	void shader_blit(Shader s, FancyModel fm) {
		backend.send(GMPushShaderBlitFancyModel(s, fm));
	}
	void destroy_shader(Shader s) {
		backend.send(GMPushDestroyShader(s));
	}

	Texture new_texture(ubyte[] data, uint width, uint height, ubyte clr_depth) {
		backend.send(GMReqNewTextureFromData(thisTid, data, width, height, clr_depth));
		Texture ret;
		receive((GMAckNewTexture t) => ret = t.t);
		return ret;
	}
	Texture new_texture(string fpath) {
		backend.send(GMReqNewTextureFromFile(thisTid, fpath));
		Texture ret;
		receive((GMAckNewTexture t) => ret = t.t);
		return ret;
	}
	void texture_blit(Texture t, vec2f[2] loc) {
		backend.send(GMPushTextureBlit(t, loc));
	}
}
