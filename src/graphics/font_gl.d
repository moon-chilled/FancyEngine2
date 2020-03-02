module graphics.font_gl;
import stdlib;
import stdmath;
import cstdlib;

import asset;
import windowing.windows_gl;

import graphics.model_gl;
import graphics.shading_gl;

import bindbc.opengl;
import bindbc.freetype;

struct Font {
	uint screen_width, screen_height; // when drawing to screen, need to normalize properly w.r.t to font height

	uint height;
	uint atlas_width;
	Shader draw_shader;
	Mesh character_model;

	private struct char_spec {
		uint atlas_offset; // memory offset (only in x direction)
		int width, height;
		int bearx, beary;
		int advance;
	}
	char_spec[128] atlas_map;

	GLuint tex_id;

	FT_Library f;
	FT_Face face;
	bool have_kerning;

	this(string fpath, uint height, uint scr_w, uint scr_h, GfxContext ctx) {
		height *= 2; // need this for some reason??
		this.height = height;
		this.screen_width = scr_w;
		this.screen_height = scr_h;

		if (!fpath.fexists) fatal("tried to read nonexistent texture '%s'", fpath);

		if (FT_Init_FreeType(&f)) fatal("Could not initialize FreeType");

		if (FT_New_Face(f, fpath.cstr, 0, &face)) fatal("FreeType: could not open face from file '%s", fpath);

		FT_Size_RequestRec req;
		req.type = FT_SIZE_REQUEST_TYPE_BBOX;
		req.height = height << 6; // it's a 26.6 fractional point
		FT_Request_Size(face, &req);

		ubyte*[128] font_bitmaps;

		foreach (c; 32 .. 128) {
			FT_Load_Char(face, c, FT_LOAD_RENDER);
			font_bitmaps[c] = SysAllocator.allocate!ubyte(face.glyph.bitmap.rows * face.glyph.bitmap.width);
			memcpy(font_bitmaps[c], face.glyph.bitmap.buffer, face.glyph.bitmap.rows * face.glyph.bitmap.width);

			atlas_map[c] = char_spec(atlas_width, face.glyph.bitmap.width, face.glyph.bitmap.rows, face.glyph.bitmap_left, face.glyph.bitmap_top, cast(int)face.glyph.advance.x);
			atlas_width += face.glyph.bitmap.width;
		}

		ubyte *font_bitmap = SysAllocator.allocate!ubyte(atlas_width * height);

		glGenTextures(1, &tex_id);
		glBindTexture(GL_TEXTURE_2D, tex_id);
		glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
		//glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
                //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
		glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, [1f, 0, 0, 1].ptr);


		glTexImage2D(GL_TEXTURE_2D, 0, GL_R8, atlas_width, height, 0, GL_RED, GL_UNSIGNED_BYTE, null/*font_bitmap*/);

		foreach (i; 32 .. 128) {
			glTexSubImage2D(GL_TEXTURE_2D, 0, atlas_map[i].atlas_offset, 0, atlas_map[i].width, atlas_map[i].height, GL_RED, GL_UNSIGNED_BYTE, font_bitmaps[i]);
			SysAllocator.free(font_bitmaps[i]);
		}


		draw_shader = Shader(fslurp("dist/shaders/font_render.vert"), fslurp("dist/shaders/font_render.frag"), ctx);

		glBindTexture(GL_TEXTURE_2D, tex_id);
		glActiveTexture(GL_TEXTURE0);
		draw_shader.enter();
		draw_shader.set_int("font_atlas", 0);
		draw_shader.exit();

		// dummy value for vertices because it'll be reset each time
		// two vec2s: position and texture coordinates
		character_model = Mesh([], [2, 2]);
		have_kerning = FT_HAS_KERNING(face);
	}

	// max height.  Note that the actual outputted size will /not/ necessarily be measure_height() by measure()
	uint measure_height(string s) {
		uint ret = 0;
		foreach (c; s) {
			ret = max(ret, atlas_map[c].height);
		}

		return ret;
	}

	// pixel width
	uint measure(string s, bool want_kerning = false) {
		uint ret = 0;
		char prev_char;
		foreach (c; s) {
			ret += atlas_map[c].advance/64;

			// might want to calculate without kerning because you'll still get an approximately right result and performance will be better
			if (have_kerning && want_kerning) {
				FT_Vector kern;
				FT_Get_Kerning(face, FT_Get_Char_Index(face, prev_char), FT_Get_Char_Index(face, c), 0, &kern);
				ret += kern.x/64;
				prev_char = c;
			}
		}

		return ret;
	}

	void draw(float x, float y, string s) {
		float[] verts;

		char prev_char = 0;
		foreach (c; s) {
			float
				tex_x0 = (cast(float)atlas_map[c].atlas_offset)/atlas_width,
				tex_x1 = (cast(float)atlas_map[c].atlas_offset+atlas_map[c].width)/atlas_width,
				tex_y1 = 1 - (cast(float)height - atlas_map[c].height) / height,
				tex_y0 = 0;
				//tex_y0 = (cast(float)height - atlas_map[c].height) / height,
				//tex_y1 = 1;
			// TODO: tex y0 and y1 should probably be reversed (and y0 should be 0, y1 should be 1 - (all that)),
			// but doing that properly involves making freetype flip its glyphs so they're in the format opengl expects.
			// (Currently, we flip the y coords in the shader, which is ~~ok
			// but something else would be cleaner.)

			// Need to divide by (.5*screen_dims) because NDC are [-1,1] so the total output range is 2
			float
				x0 = x + cast(float)atlas_map[c].bearx / (.5*screen_width),
				x1 = x0 + cast(float)atlas_map[c].width / (.5*screen_width),
				y1 = y + (cast(float)atlas_map[c].height - atlas_map[c].beary) / (.5*screen_height),
				y0 = y - cast(float)atlas_map[c].beary / (.5*screen_height);


			if (have_kerning) {
				FT_Vector kern;
				FT_Get_Kerning(face, FT_Get_Char_Index(face, prev_char), FT_Get_Char_Index(face, c), 0, &kern);
				x += kern.x / (64.*.5*screen_width);
				x0 += kern.x / (64.*.5*screen_width);
				x1 += kern.x / (64.*.5*screen_width);
				prev_char = c;
			}

			verts ~= [
					x0,y0, tex_x0,tex_y0,
					x1,y1, tex_x1,tex_y1,
					x1,y0, tex_x1,tex_y0,

					x0,y0, tex_x0,tex_y0,
					x0,y1, tex_x0,tex_y1,
					x1,y1, tex_x1,tex_y1];

			x += (atlas_map[c].advance/64.) / (.5*screen_width);
		}

		character_model.load_verts(verts);

		glActiveTexture(GL_TEXTURE0);
		glBindTexture(GL_TEXTURE_2D, tex_id);
		//draw_shader.set_int("font_atlas", 0);
		draw_shader.blit(character_model);
	}

	void destroy() {
		FT_Done_Face(face);
		FT_Done_FreeType(f);
		glDeleteTextures(1, &tex_id);
	}
}
