module graphics.tex_gl;
import stdlib;
import cstdlib;

import asset;
import windowing.windows_gl;

import derelict.opengl;

private extern (C) ubyte *stbi_load(const(char) *filename, int *x, int *y, int *channels, int desired_channels);
private extern (C) void stbi_image_free(void *retval_from_stbi_load);
Texture[string] texture_cache;

class Texture: Asset!(AssetType.Texture) {
	uint w, h;
	GLuint tex_id;

	this(string fpath, GfxContext) {
		if (fpath in texture_cache) {
			w = texture_cache[fpath].w;
			h = texture_cache[fpath].h;
			tex_id = texture_cache[fpath].tex_id;
			return;
		}

		if (!fpath.fexists) fatal("tried to read nonexistent texture '%s'", fpath);
		int clr_depth;

		int width;
		int height;
		ubyte *tex_data = stbi_load(fpath.cstr, &width, &height, &clr_depth, 0);
		if (!tex_data) fatal("unable to read texture from file '%s'", fpath);
		assert ((width > 0) && (height > 0));
		w = width;
		h = height;

		GLuint colour_fmt;
		switch (clr_depth) {
			case 3: colour_fmt = GL_RGB; break;
			case 4: colour_fmt = GL_RGBA; break;
			default: fatal("Need 3-byte or 4-byte colour depth, got %s", clr_depth); assert(0);
		}

		glGenTextures(1, &tex_id);
		glBindTexture(GL_TEXTURE_2D, tex_id);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR);

		glTexImage2D(GL_TEXTURE_2D, 0, colour_fmt, w, h, 0, colour_fmt, GL_UNSIGNED_BYTE, tex_data);

		glGenerateMipmap(GL_TEXTURE_2D);

		stbi_image_free(tex_data);

		texture_cache[fpath] = this;
	}
}

private extern (C) void stbi_set_flip_vertically_on_load(int);
shared static this() {
	stbi_set_flip_vertically_on_load(true);
}
