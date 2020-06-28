module graphics.fancy_model_gl;
import stdlib;
import stdmath;
import cstdlib;
import bindbc.opengl;
import bindbc.assimp;

import graphics.model;
import graphics.tex;

struct Vertex {
	vec3f position;
	vec3f normal;
	vec2f tex_coords;
	vec3f tangent;
	vec3f bitangent;
}

struct FancyMesh {
	GLuint VAO, VBO, EBO;

	Vertex[] vertices;
	uint[] indices;
	Texture[] diffuse_textures, specular_textures;

	this(Vertex[] vertices, uint[] indices, Texture[] diffuse_textures, Texture[] specular_textures) {
		this.vertices = vertices;
		this.indices = indices;
		this.diffuse_textures = diffuse_textures;
		this.specular_textures = specular_textures;
	}

	void load_verts() {
		glGenVertexArrays(1, &VAO);
		glGenBuffers(1, &VBO);
		glGenBuffers(1, &EBO);

		glBindVertexArray(VAO);
		glBindBuffer(GL_ARRAY_BUFFER, VBO);

		glBufferData(GL_ARRAY_BUFFER, vertices.length * Vertex.sizeof, vertices.ptr, GL_STATIC_DRAW);

		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.length * uint.sizeof, indices.ptr, GL_STATIC_DRAW);

		// vertex positions
		glEnableVertexAttribArray(0);
		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, Vertex.sizeof, null);

		// vertex normals
		glEnableVertexAttribArray(1);
		glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, Vertex.sizeof, cast(void*)Vertex.normal.offsetof);

		// vertex texture coords
		glEnableVertexAttribArray(2);
		glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, Vertex.sizeof, cast(void*)Vertex.tex_coords.offsetof);

		glBindVertexArray(0);

		glBindBuffer(GL_ARRAY_BUFFER, 0);
	}
}


struct FancyModel {
	@disable this();

	FancyMesh[] meshes;

	this(string fpath) {
		import std.file: getcwd, chdir;

		if (!fpath.fexists) fatal("File '%s' does not exist", fpath);

		const aiScene *scene = aiImportFile(fpath.cstr, aiPostProcessSteps.Triangulate | aiPostProcessSteps.OptimizeMeshes | aiPostProcessSteps.OptimizeGraph | aiPostProcessSteps.GenNormals | aiPostProcessSteps.JoinIdenticalVertices | aiPostProcessSteps.FixInfacingNormals); // default winding order is counter-clockwise, we want clockwise, but the flip step in the shader turns the counter-clockwise triangles clockwise again

		if (!scene || scene.mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene.mRootNode) {
			fatal("Failed to properly load model '%s'.  AssImp says '%s'", fpath, aiGetErrorString());
		}

		string cwd = getcwd();
		chdir(fpath.split("/")[0 .. $ - 1].join("/"));
		load_meshes(scene.mRootNode, scene);
		chdir(cwd);

		// synchronized (opengl_lock)
		foreach (ref m; meshes) {
			m.load_verts();
		}
		trace("Loaded model %s with %s meshes", fpath, meshes.length);
	}

	void load_meshes(const aiNode *node, const aiScene *scene) {
		foreach (sss; 0 .. node.mNumMeshes) {
			const aiMesh *mesh = scene.mMeshes[node.mMeshes[sss]];
			Vertex[] vertices;
			Texture[] diffuse_textures;
			Texture[] specular_textures;
			uint[] indices;

			foreach (i; 0 .. mesh.mNumVertices) {
				Vertex v;

				v.position = vec3f(mesh.mVertices[i].x, mesh.mVertices[i].y, mesh.mVertices[i].z);

				v.normal = vec3f(mesh.mNormals[i].x, mesh.mNormals[i].y, mesh.mNormals[i].z);

				if (mesh.mTextureCoords[0]) {
					v.tex_coords = vec2f(mesh.mTextureCoords[0][i].x, mesh.mTextureCoords[0][i].y);
					//TODO: why do I need this
					v.tex_coords.x = fmod(v.tex_coords.x, 1);
					v.tex_coords.y = fmod(v.tex_coords.y, 1);
				} else {
					warning("no tex coords ;-;");
				}

				vertices ~= v;
			}

			foreach (i; 0 .. mesh.mNumFaces) {
				foreach (j; 0 .. mesh.mFaces[i].mNumIndices) {
					indices ~= mesh.mFaces[i].mIndices[j];
				}
			}

			diffuse_textures = load_materials(scene.mMaterials[mesh.mMaterialIndex], aiTextureType.DIFFUSE);
			specular_textures = load_materials(scene.mMaterials[mesh.mMaterialIndex], aiTextureType.SPECULAR);

			meshes ~= FancyMesh(vertices, indices, diffuse_textures, specular_textures);
		}

		foreach (i; 0 .. node.mNumChildren) {
			load_meshes(node.mChildren[i], scene);
		}
	}
}

Texture[] load_materials(const aiMaterial *material, aiTextureType type) {
	Texture[] ret;
	foreach (i; 0 .. aiGetMaterialTextureCount(material, type)) {
		aiString fpath;
		/+
		aiTextureMapping tex_mapping;
		uint uv_index;
		ai_real blend;
		aiTextureOp tex_op;
		aiTextureMapMode mode;
		uint flags;
		+/

		aiGetMaterialTexture(material, type, i, &fpath, null, null, null, null, null, null);
		ret ~= new Texture(fpath.data[0 .. fpath.length].idup);
	}

	return ret;
}
