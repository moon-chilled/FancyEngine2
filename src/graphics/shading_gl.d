module graphics.shading_gl;
import stdlib;
import stdmath;
import cstdlib;

import windowing.windows_gl;
import graphics.tex_gl;
import graphics.model_gl;
import graphics.fancy_model_gl;

import bindbc.opengl;


enum ShaderType: GLuint {
	Vertex = GL_VERTEX_SHADER,
	Fragment = GL_FRAGMENT_SHADER,
}
void upload_texture(uint pos, Texture tex) {
	assert (pos < 16);
	glActiveTexture(GL_TEXTURE0 + pos);
	glBindTexture(GL_TEXTURE_2D, tex.tex_id);
} //TODO: move this to another file

struct Shader {
	@disable this();

	private GLuint program;

	this(string vertex_src, string fragment_src, GfxContext ctx) {
		GLuint vertex_shader = compile_shader(ShaderType.Vertex, vertex_src);
		GLuint fragment_shader = compile_shader(ShaderType.Fragment, fragment_src);

		program = glCreateProgram();
		glAttachShader(program, vertex_shader);
		glAttachShader(program, fragment_shader);
		glLinkProgram(program);
		glDeleteShader(vertex_shader);
		glDeleteShader(fragment_shader);

		GLint success = GL_TRUE;
		glGetProgramiv(program, GL_COMPILE_STATUS, &success);
		{
			GLint error_len;
			glGetProgramiv(program, GL_INFO_LOG_LENGTH, &error_len);
			scope char[] error = new char[error_len + 1];
			glGetProgramInfoLog(program, error_len, null, error.ptr);
			if (!success) {
				fatal("error linking program!  OpenGL says '%s'", error);
			} else {
				if (error_len) info("Program runlog: %s", error);
			}
		}
	}

	void set_int(string id, GLint value) {
		glUseProgram(program);
		glUniform1i(glGetUniformLocation(program, id.cstr), value);
	}
	void set_mat4(string id, mat4f value) {
		glUseProgram(program);
		glUniformMatrix4fv(glGetUniformLocation(program, id.cstr), 1, GL_TRUE, value.v.ptr);
	}

	void blit(const ref Mesh model) {
		glUseProgram(program);

		glBindVertexArray(model.VAO);
		glDrawArrays(GL_TRIANGLES, 0, model.num_verts);
		glBindVertexArray(0);

		glUseProgram(0);
	}

	// yes, I did.  What are you gonna do about it?
	void blit(const ref FancyModel model) {
		foreach (i; 0 .. model.stupid_meshes.length) {
			const Mesh mesh = model.stupid_meshes[i];
			const Texture[] diffuse_textures = model.meta_diffuse_textures[i];
			uint num_indices = cast(uint)model.meta_indices[i].length;

			foreach (int j; 0 .. cast(int)diffuse_textures.length) {
				glActiveTexture(GL_TEXTURE0 + j);
				glBindTexture(GL_TEXTURE_2D, diffuse_textures[j].tex_id);
				set_int("diffuse" ~ j.tostr, j);
			}

			glUseProgram(program);
			glBindVertexArray(mesh.VAO);
			glDrawElements(GL_TRIANGLES, num_indices, GL_UNSIGNED_INT, null);
			glBindVertexArray(0);
		}
	}

	void destruct() {
		glDeleteProgram(program);
	}
}


private GLuint compile_shader(ShaderType type, string src) {
	GLuint ret = glCreateShader(type);

	glShaderSource(ret, 1, [src.cstr].ptr, [cast(int)src.length].ptr);
	glCompileShader(ret);

	GLint success = GL_FALSE;
	glGetShaderiv(ret, GL_COMPILE_STATUS, &success);
	if (!success) {
		GLint error_len;
		glGetShaderiv(ret, GL_INFO_LOG_LENGTH, &error_len);
		scope char[] error = new char[error_len];
		glGetShaderInfoLog(ret, error_len, null, error.ptr);
		fatal("error compiling %s shader!  OpenGL says '%s'", type, error);
	}

	return ret;
}
