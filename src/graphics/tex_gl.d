module graphics.tex_gl;
import stdlib;
import cstdlib;

import graphics.gl_thread;

import asset;
import windowing.windows_gl;

import bindbc.opengl;


private extern (C) ubyte *stbi_load(const(char) *filename, int *x, int *y, int *channels, int desired_channels);
private extern (C) void stbi_image_free(void *retval_from_stbi_load);
Texture[string] texture_cache;

class Texture: Asset {
	AssetType asset_type = AssetType.Texture;

	uint w, h;
	GLuint tex_id;

	this(ubyte[] data, uint width, uint height, ubyte clr_depth) {
		if (data.length != width * height * clr_depth) fatal("%s != %s*%s*%s", data.length, width, height, clr_depth);

		w = width;
		h = height;

		GLuint colour_fmt, internalfmt;
		switch (clr_depth) {
			case 1: colour_fmt = GL_RED;  internalfmt = GL_R8;    break;
			case 3: colour_fmt = GL_RGB;  internalfmt = GL_RGB8;  break;
			case 4: colour_fmt = GL_RGBA; internalfmt = GL_RGBA8; break;
			default: fatal("Need 1-, 3-, or 4-byte colour depth, got %s", clr_depth); assert(0);
		}

		glwait({
		glCreateTextures(GL_TEXTURE_2D, 1, &tex_id);
		glTextureParameteri(tex_id, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
		glTextureParameteri(tex_id, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);
		glTextureParameteri(tex_id, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR); //TODO: make this configurable
		glTextureParameteri(tex_id, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

		glTextureStorage2D(tex_id, 1, internalfmt, w, h);
		glTextureSubImage2D(tex_id, 0, 0, 0, w, h, colour_fmt, GL_UNSIGNED_BYTE, data.ptr);
		});
	}


	this(string fpath) {
		if (fpath in texture_cache) {
			w = texture_cache[fpath].w;
			h = texture_cache[fpath].h;
			tex_id = texture_cache[fpath].tex_id;
			return;
		}

		import std.file: getcwd;
		if (!fpath.fexists) fatal("tried to read nonexistent texture '%s' (in %s)", fpath, getcwd);
		int clr_depth;

		int width;
		int height;
		ubyte *tex_data = stbi_load(fpath.cstr, &width, &height, &clr_depth, 0);
		if (!tex_data) fatal("unable to read texture from file '%s'", fpath);
		assert ((width > 0) && (height > 0));
		w = width;
		h = height;

		GLuint colour_fmt, internalfmt;
		switch (clr_depth) {
			case 1: colour_fmt = GL_RED;  internalfmt = GL_R8;    break;
			case 3: colour_fmt = GL_RGB;  internalfmt = GL_RGB8;  break;
			case 4: colour_fmt = GL_RGBA; internalfmt = GL_RGBA8; break;
			default: fatal("Need 1-, 3-, or 4-byte colour depth, got %s", clr_depth); assert(0);
		}

		glwait({
		glCreateTextures(GL_TEXTURE_2D, 1, &tex_id);
		glTextureParameteri(tex_id, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
		glTextureParameteri(tex_id, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);
		glTextureParameteri(tex_id, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR); //TODO: make this configurable
		glTextureParameteri(tex_id, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

		glTextureStorage2D(tex_id, 1, internalfmt, w, h);
		glTextureSubImage2D(tex_id, 0, 0, 0, w, h, colour_fmt, GL_UNSIGNED_BYTE, tex_data);
		});

		stbi_image_free(tex_data);

		texture_cache[fpath] = this;
	}
}

private extern (C) void stbi_set_flip_vertically_on_load(int);
shared static this() {
	stbi_set_flip_vertically_on_load(true);
}
