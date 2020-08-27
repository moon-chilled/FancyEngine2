module graphics.graphics_manager_gl;
import stdlib;

import graphics.graphics_manager;

import std.concurrency;

import windowing.windows;

import bindbc.opengl;
import bindbc.sdl;

import graphics.windows_gl;
import graphics.fancy_model;
import graphics.font;
import graphics.framebuffer;
import graphics.mesh;
import graphics.shading;
import graphics.tex;

private void blit(GraphicsState gs, GfxContext ctx, GfxExtra extra) {
	//SDL_GL_MakeCurrent(gs.window, ctx.gl_context);

	glDisable(GL_DEPTH_TEST);
	int w, h;
	SDL_GL_GetDrawableSize(gs.window, &w, &h);
	glViewport(0, 0, w, h);

	// XXX: this causes flickering with mesa+dri3.
	//glBlitNamedFramebuffer(gs.gfx_extra.framebuffer.fbo, 0, 0, 0, gs.gfx_extra.framebuffer.w, gs.gfx_extra.framebuffer.h, 0, 0, w, h, GL_COLOR_BUFFER_BIT, GL_LINEAR);

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glBindTexture(GL_TEXTURE_2D, extra.framebuffer.tex);
	extra.tex_copy.blit(extra.mesh);

	SDL_GL_SwapWindow(gs.window);

	glBindFramebuffer(GL_FRAMEBUFFER, extra.framebuffer.fbo);
	glViewport(0, 0, extra.framebuffer.w, extra.framebuffer.h);
	glEnable(GL_DEPTH_TEST);
}

private void clear(float r, float g, float b) {
	glClearColor(r, g, b, 1);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

package void real_graphics_manager(shared GraphicsState GS) {
	GraphicsState gs = cast()GS;
	auto ctx = win_setup_context(gs);
	auto extra = win_setup_extra(ctx, gs);
	bool done = false;
	Tid send_obituary_to;

	Mesh tex_copy_mesh = Mesh(
			[-1,+1, 0,1,
			+1,-1, 1,0,
			-1,-1, 0,0,

			+1,-1, 1,0,
			-1,+1, 0,1,
			+1,+1, 1,1], [2, 2]);



	while (!done) {
		receive(
			(GMReqSync r) => r.from.send(GMAckSync()),

			(GMReqDie d) {
				send_obituary_to = d.from;
				done = true;
			},

			(GMPushBlit b) => blit(gs, ctx, extra),

			(GMPushClear c) => clear(c.r, c.g, c.b),

			(GMReqNewFancyModel m) => m.from.send(GMAckNewFancyModel(FancyModel(m.fpath))),

			(GMReqNewFont f) => f.from.send(GMAckNewFont(Font(f.fpath, f.height, f.scr_w, f.scr_h, ctx))),
			(GMPushDrawText f) => f.f.draw(f.x, f.y, f.s),
			(GMPushDestroyFont f) => f.f.destroy(),

			(GMReqNewFramebuffer f) => f.from.send(GMAckNewFramebuffer(Framebuffer(f.w, f.h, ctx))),
			(GMPushDestroyFramebuffer f) => f.f.destroy(),

			(GMReqNewMesh m) => m.from.send(GMAckNewMesh(Mesh(m.vertices, m.sizes))),
			(GMPushMeshLoadVerts m) => m.m.load_verts(m.vertices),

			(GMReqNewShader s) => s.from.send(GMAckNewShader(Shader(s.vertex, s.fragment, ctx))),
			(GMPushShaderSetInt s) => s.s.set_int(s.id, s.value),
			(GMPushShaderSetMat4 s) => s.s.set_mat4(s.id, s.value),
			(GMPushShaderBlitMesh s) => s.s.blit(s.m),
			(GMPushShaderBlitMeshWithTex s) {
				upload_texture(0, s.t);
			       	s.s.blit(s.m);
			},
			(GMPushShaderBlitFancyModel s) => s.s.blit(s.fm),
			(GMPushDestroyShader s) => s.s.destruct(),

			(GMReqNewTextureFromData t) => t.from.send(GMAckNewTexture(new Texture(t.data, t.width, t.height, t.clr_depth))),
			(GMReqNewTextureFromFile t) => t.from.send(GMAckNewTexture(new Texture(t.fpath))),
			(GMPushTextureBlit t) {
				tex_copy_mesh.load_verts(
						[t.loc[0].x,t.loc[1].y, 0,1,
						 t.loc[1].x,t.loc[0].y, 1,0,
						 t.loc[0].x,t.loc[0].y, 0,0,

						 t.loc[1].x,t.loc[0].y, 1,0,
						 t.loc[0].x,t.loc[1].y, 0,1,
						 t.loc[1].x,t.loc[1].y, 1,1]);

				upload_texture(0, t.t);
				extra.tex_copy.blit(tex_copy_mesh);
			},
			(Variant v) => fatal("gm backend got unknown message '%s'", v));
	}

	send_obituary_to.send(GMAckDie());
}
