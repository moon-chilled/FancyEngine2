module graphics.shading_vk;
import stdlib;
import stdmath;
import cstdlib;

import windowing.windows_vk;
import graphics.tex_vk;
import graphics.mesh_vk;
import graphics.fancy_model_vk;

import erupted;


enum ShaderType {
	Vertex,
	Fragment,
}
void upload_texture(uint pos, Texture tex) {
	/+
	assert (pos < 16);
	glActiveTexture(GL_TEXTURE0 + pos);
	glBindTexture(GL_TEXTURE_2D, tex.tex_id);
	+/
} //TODO: move this to another file

struct Shader {
	this(string vertex_src, string fragment_src, GfxContext ctx) {
		/+
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
		if (!success) {
			GLint error_len;
			glGetProgramiv(program, GL_INFO_LOG_LENGTH, &error_len);
			scope char[] error = new char[error_len];
			glGetProgramInfoLog(program, error_len, null, error.ptr);
			fatal("error linking program!  OpenGL says '%s'", error);
		}
		+/
	}

	void set_int(string id, int value) {
		/+
		glUniform1i(glGetUniformLocation(program, id.cstr), value);
		+/
	}
	void set_mat4(string id, mat4f value) {
		//glUniformMatrix4fv(glGetUniformLocation(program, id.cstr), 1, GL_TRUE, value.v.ptr);
	}

	void blit(const ref Mesh mesh) {
		/+
		glUseProgram(program);

		glBindVertexArray(mesh.VAO);
		glDrawArrays(GL_TRIANGLES, 0, mesh.num_verts);
		glBindVertexArray(0);
		+/
	}

	// yes, I did.  What are you gonna do about it?
	void blit(const ref FancyModel modél) {
		/+
		foreach (i; 0 .. modél.retarted_meshes.length) {
			const Mesh mesh = modél.retarted_meshes[i];
			const Texture[] diffuse_textures = modél.meta_diffuse_textures[i];
			uint num_indices = cast(uint)modél.meta_indices[i].length;

			foreach (int j; 0 .. cast(int)diffuse_textures.length) {
				glActiveTexture(GL_TEXTURE0 + j);
				glBindTexture(GL_TEXTURE_2D, diffuse_textures[j].tex_id);
				set_int("diffuse" ~ j.tostr, j);
			}


			// TODO: textures
			glUseProgram(program);
			glBindVertexArray(mesh.VAO);
			glDrawElements(GL_TRIANGLES, num_indices, GL_UNSIGNED_INT, null);
			glBindVertexArray(0);
		}
		+/
	}

	void destruct() {
		//glDeleteProgram(program);
	}
}
