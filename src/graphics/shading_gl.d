module graphics.shading_gl;
import stdlib;
import stdmath;
import cstdlib;

import graphics.windows_gl;
import graphics.tex_gl;
import graphics.mesh_gl;
import graphics.fancy_model_gl;

import bindbc.opengl;


enum ShaderType: GLuint {
	Vertex = GL_VERTEX_SHADER,
	Fragment = GL_FRAGMENT_SHADER,
}
package void upload_texture(uint pos, Texture tex) {
	assert (pos < 16);
	glActiveTexture(GL_TEXTURE0 + pos);
	glBindTexture(GL_TEXTURE_2D, tex.tex_id);
} //TODO: move this to another file

struct Shader {
	private GLuint program;

	package this(string vertex_src, string fragment_src, GfxContext ctx) {
		GLuint vertex_shader = compile_shader(ShaderType.Vertex, vertex_src);
		GLuint fragment_shader = compile_shader(ShaderType.Fragment, fragment_src);

		program = glCreateProgram();
		glAttachShader(program, vertex_shader);
		glAttachShader(program, fragment_shader);
		glLinkProgram(program);
		glDeleteShader(vertex_shader);
		glDeleteShader(fragment_shader);

		GLint success = GL_TRUE;
		glGetProgramiv(program, GL_LINK_STATUS, &success);
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

	private void penter() { glUseProgram(program); }
	private void pexit() { glUseProgram(0); }

	package void set_int(string id, GLint value) {
		glProgramUniform1i(program, glGetUniformLocation(program, id.cstr), value);
	}
	package void set_mat4(string id, mat4f value) {
		glProgramUniformMatrix4fv(program, glGetUniformLocation(program, id.cstr), 1, GL_TRUE, value.v.ptr);
	}

	package void blit(const ref Mesh mesh) {
		penter();

		glBindVertexArray(mesh.VAO);
		glDrawArrays(GL_TRIANGLES, 0, mesh.num_verts);
		glBindVertexArray(0);

		pexit();
	}

	private void pblit(const ref FancyMesh mesh) {
		foreach (i; 0 .. cast(int)mesh.diffuse_textures.length) {
			glBindTextureUnit(i, mesh.diffuse_textures[i].tex_id);
			set_int("diffuse" ~ i.tostr, i);
		}

		glBindVertexArray(mesh.VAO);

		glDrawElements(GL_TRIANGLES, cast(uint)mesh.indices.length, GL_UNSIGNED_INT, null);
	}

	package void blit(const ref FancyModel model) {
		penter();

		foreach (ref m; model.meshes) pblit(m);

		pexit();
	}

	package void destruct() {
		glDeleteProgram(program);
	}
}


private GLuint compile_shader(ShaderType type, string src) {
	synchronized {
	import std.concurrency;
	log("compiling shader from thread %s", thisTid);

	GLuint ret;

	ret = glCreateShader(type);

	glShaderSource(ret, 1, [src.cstr].ptr, [cast(int)src.length].ptr);
	glCompileShader(ret);

	GLint error_len;
	glGetShaderiv(ret, GL_INFO_LOG_LENGTH, &error_len);
	scope char[] error = new char[error_len];
	glGetShaderInfoLog(ret, error_len, null, error.ptr);

	GLint success = GL_FALSE;
	glGetShaderiv(ret, GL_COMPILE_STATUS, &success);
	if (!success) {
		fatal("error compiling %s shader!  OpenGL says '%s'", type, error);
	} else if (error_len) {
		info("OpenGL log while compiling shader: %s", src, error);
	}

	return ret;
	}
}
