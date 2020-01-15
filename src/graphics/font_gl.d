module graphics.font_gl;
import stdlib;
import stdmath;
import cstdlib;

import asset;
import windowing.windows_gl;

import graphics.model_gl;
import graphics.shading_gl;

import bindbc.opengl;

nothrow:

private extern (C) {
	struct stbtt__buf {
		ubyte *data;
		int cursor;
		int size;
	}
	struct stbtt_fontinfo {
		void *userdata;
		ubyte *data;	// pointer to .ttf file
		int fontstart;	// offset of start of font

		int numGlyphs;	// number of glyphs, needed for range checking

		int loca,head,glyf,hhea,hmtx,kern,gpos;// table locations as offset from start of .ttf
		int index_map;		// a cmap mapping for our chosen character encoding
		int indexToLocFormat;	// format needed to map from glyph index to glyph

		stbtt__buf cff;		// cff font data
		stbtt__buf charstrings;	// the charstring index
		stbtt__buf gsubrs;	// global charstring subroutines index
		stbtt__buf subrs;	// private charstring subroutines index
		stbtt__buf fontdicts;	// array of font dicts
		stbtt__buf fdselect;	// map from glyph to fontdict
	};

	int stbtt_InitFont(stbtt_fontinfo *info, const(ubyte) *data, int offset);
	int stbtt_GetFontOffsetForIndex(const(ubyte) *data, int index);
	ubyte *stbtt_GetCodepointBitmap(const stbtt_fontinfo *info, float scale_x, float scale_y, int codepoint, int *width, int *height, int *xoff, int *yoff);
	void stbtt_FreeBitmap(ubyte *bitmap, void *userdata);
	float stbtt_ScaleForPixelHeight(const stbtt_fontinfo *info, float pixels);
}

struct Font {
	@disable this();
	@disable this(this);

	uint height;
	uint atlas_width;
	Shader draw_shader;
	Mesh character_model;

	private struct char_spec {
		uint atlas_offset; // memory offset (only in x direction)
		ubyte width;
		ubyte xoffset, yoffset; // display offset
	}
	char_spec[128] atlas_map;

	GLuint tex_id;

	this(string fpath, uint height, GfxContext ctx) {
		this.height = height;

		if (!fpath.fexists) fatal("tried to read nonexistent texture '%s'", fpath);
		string data = fslurp(fpath);

		stbtt_fontinfo f;
		stbtt_InitFont(&f, cast(const ubyte*)data.ptr, stbtt_GetFontOffsetForIndex(cast(const ubyte*)data.ptr, 0));

		ubyte*[128] font_bitmaps;
		ubyte[128] bitmap_heights;

		foreach (c; 33 .. 127) {
			int w, h, xo, yo;
			font_bitmaps[c] = stbtt_GetCodepointBitmap(&f, 0, stbtt_ScaleForPixelHeight(&f, height), c, &w, &h, &xo, &yo);
			atlas_map[c] = char_spec(atlas_width, cast(ubyte)w, cast(ubyte)xo, cast(ubyte)yo);
			bitmap_heights[c] = cast(ubyte)h;
			atlas_width += w;
			atlas_width++; // one pixel of separation between characters
			//atlas_width += height/2;
		}

		ubyte *font_bitmap = Alloc!ubyte(atlas_width * height);

		/+
		foreach (i; 0 .. atlas_width * height) {
			import std.random: uniform;
			font_bitmap[i] = uniform(0, 2) ? 0 : 255;
		}
		+/

		/+
		foreach (i; 33 .. 127) {
			foreach (j; 0 .. bitmap_heights[i]) {
				memcpy(font_bitmap + j*atlas_width + atlas_map[i].atlas_offset, font_bitmaps[i] + j * atlas_map[i].width, atlas_map[i].width);
			}
		}
		+/
		//log("Having %s (%sx%s)", font_bitmap[0 .. atlas_width * height], atlas_width, height);


		glGenTextures(1, &tex_id);
		glBindTexture(GL_TEXTURE_2D, tex_id);
		//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
                //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
		glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, [1f, 0, 0, 1].ptr);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

		glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, atlas_width, height, 0, GL_RED, GL_UNSIGNED_BYTE, null/*font_bitmap*/);

		foreach (i; 33 .. 127) {
			//log("doing %s", font_bitmaps[i][0 .. atlas_map[i].width * bitmap_heights[i]]);
			glTexSubImage2D(GL_TEXTURE_2D, 0, atlas_map[i].atlas_offset, 0, atlas_map[i].width, bitmap_heights[i], GL_RED, GL_UNSIGNED_BYTE, font_bitmaps[i]);
			stbtt_FreeBitmap(font_bitmaps[i], null);
		}


		draw_shader = Shader(fslurp("dist/shaders/font_render.vert"), fslurp("dist/shaders/font_render.frag"), ctx);

		glBindTexture(GL_TEXTURE_2D, tex_id);
		glActiveTexture(GL_TEXTURE0);
		draw_shader.set_int("font_atlas", 0);
		draw_shader.set_mat4("projection", mat4f.orthographic(1280, 0, 720, 0, 0.1, 100));

		auto t = mat4f.orthographic(1280, 0, 720, 0, 0.1, 100);

		// dummy value for vertices because it'll be reset each time
		// two vec2s: position and texture coordinates
		character_model = Mesh([], [2, 2]);

	}

	void draw_char(char c) {
		character_model.load_verts([
				0,0,   (cast(float)atlas_map[c].atlas_offset)/atlas_width,0,
				.2,1, (cast(float)atlas_map[c].atlas_offset+atlas_map[c].width)/atlas_width,1,
				.2,0,  (cast(float)atlas_map[c].atlas_offset+atlas_map[c].width)/atlas_width,0,

				0,0,   (cast(float)atlas_map[c].atlas_offset)/atlas_width,0,
				0,1,  (cast(float)atlas_map[c].atlas_offset)/atlas_width,1,
				.2,1, (cast(float)atlas_map[c].atlas_offset+atlas_map[c].width)/atlas_width,1]);

//		log("%s, %s, %s, %s", (cast(float)atlas_map[c].atlas_offset)/atlas_width, (cast(float)atlas_map[c].atlas_offset+atlas_map[c].width)/atlas_width, (cast(float)atlas_map[c].atlas_offset)/atlas_width, (cast(float)atlas_map[c].atlas_offset+atlas_map[c].width)/atlas_width);
//		log("(%s/%s)", atlas_map[c].width, atlas_width);

		/+
		character_model.load_verts([
				-.9,-.9,   0,0,
				.3,-.7, 1,1,
				.3,-.9,  1,0,

				-.9,-.9,   0,0,
				-.9,-.7,  0,1,
				.3,-.7, 1,1]);
		+/

			/+
		character_model.load_verts([
				0,0,   cast(float)atlas_map[c].atlas_offset,0,
				400,400, cast(float)atlas_map[c].atlas_offset+atlas_map[c].width,height,
				400,0,  cast(float)atlas_map[c].atlas_offset+atlas_map[c].width,0,

				0,0,   cast(float)atlas_map[c].atlas_offset,0,
				0,400,  cast(float)atlas_map[c].atlas_offset,height,
				400,400, cast(float)atlas_map[c].atlas_offset+atlas_map[c].width,height]);
		+/

		glActiveTexture(GL_TEXTURE0);
		glBindTexture(GL_TEXTURE_2D, tex_id);
		glActiveTexture(GL_TEXTURE0);
		draw_shader.set_int("font_atlas", 0);
		draw_shader.set_mat4("projection", mat4f.orthographic(1280, 0, 720, 0, 0.1, 100));
		draw_shader.blit(character_model);
	}

	~this() {
	}
}
