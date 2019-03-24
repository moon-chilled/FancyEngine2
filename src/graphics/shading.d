module graphics.shading;
import stdlib;

import derelict.opengl;

enum ShaderType: GLuint {
	Vertex = GL_VERTEX_SHADER,
	Fragment = GL_FRAGMENT_SHADER,
}

struct Program {
	@disable this();
	@disable this(this);

	private GLuint program, VAO, VBO, EBO;

	void upload_vertices(float[24] vertices) {
		glBufferData(GL_ARRAY_BUFFER, vertices.sizeof, vertices.ptr, GL_DYNAMIC_DRAW); // options: GL_STATIC_DRAW, GL_DYNAMIC_DRAW, GL_STREAM_DRAW
		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * float.sizeof, null);
		glEnableVertexAttribArray(0);
		glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * float.sizeof, cast(void*)(3 * float.sizeof));
		glEnableVertexAttribArray(1);
	}

	this(string vertex_src, string fragment_src) {
		glGenVertexArrays(1, &VAO);
		glBindVertexArray(VAO);

		glGenBuffers(1, &VBO);
		glBindBuffer(GL_ARRAY_BUFFER, VBO);

		GLuint[] indices = [0, 1, 2, 0, 2, 3];
		glGenBuffers(1, &EBO);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.length * indices[0].sizeof, indices.ptr, GL_DYNAMIC_DRAW);

		/*
		upload_vertices([-0.5f, -0.5f, 0.0f,
				0.5f, -0.5f, 0.0f,
				0.5f,  0.5f, 0.0f,
				-0.5f, 0.5f, 0.0]);
				*/
		/*

                   3                            2






		   0                            1
		   */


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

		glUseProgram(program);
	}

	void blit() {
		glUseProgram(program);
		glBindVertexArray(VAO);
		//glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
		glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, null);
		glBindVertexArray(0);
		/*
		glDrawArrays(GL_TRIANGLES, 0, 3);
		glDrawArrays(GL_TRIANGLES, 1, 4);
		*/
	}

	~this() {
//		glDeleteProgram(program);
	}
}




private GLuint compile_shader(ShaderType type, string src) {
	import cstdlib;

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
		fatal("error compiling shader!  OpenGL says '%s'", error);
	}

	return ret;
}
