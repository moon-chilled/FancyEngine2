module graphics.model_gl;
import stdlib;
import cstdlib;
import bindbc.opengl;



struct Mesh {

	GLuint VAO, VBO;
	uint num_verts;

	private uint[] sizes;
	private uint one_vertex_size;

	this(float[] vertices, uint[] sizes) {
		glGenVertexArrays(1, &VAO);
		glBindVertexArray(VAO);

		glGenBuffers(1, &VBO);
		glBindBuffer(GL_ARRAY_BUFFER, VBO);

		this.sizes = sizes;

		one_vertex_size = sizes.sum;//reduce!((a, b) => a + b);
		assert(vertices.length % one_vertex_size == 0);

		uint offset;
		glBufferData(GL_ARRAY_BUFFER, float.sizeof * vertices.length, vertices.ptr, GL_DYNAMIC_DRAW);
		// GL_STATIC_DRAW, GL_DYNAMIC_DRAW, GL_STREAM_DRAW

		foreach (uint i; 0 .. cast(uint)sizes.length) {
			glVertexAttribPointer(i, sizes[i], GL_FLOAT, GL_FALSE, one_vertex_size * cast(uint)float.sizeof, cast(void*)(offset * float.sizeof));
			glEnableVertexAttribArray(i);
			offset += sizes[i];
		}

		num_verts = cast(uint)vertices.length / one_vertex_size;

		glBindVertexArray(0);
		glBindBuffer(GL_ARRAY_BUFFER, 0);
	}

	// TODO: don't hardcode for floats (GL_FLOAT, float.sizeof, etc.)
	void load_verts(float[] vertices) {
		glBindVertexArray(VAO);
		glBindBuffer(GL_ARRAY_BUFFER, VBO);

		glBufferData(GL_ARRAY_BUFFER, float.sizeof * vertices.length, vertices.ptr, GL_DYNAMIC_DRAW);

		num_verts = cast(uint)vertices.length / one_vertex_size;

		glBindVertexArray(0);
		glBindBuffer(GL_ARRAY_BUFFER, 0);
	}
}
