module graphics.fancy_model_gl;
import stdlib;
import cstdlib;
import derelict.opengl;
import derelict.assimp3.assimp;

import graphics.model;
import graphics.tex;

struct FancyModel {
	@disable this();
	@disable this(this);

	/// SOA ftw!  Yeah I hate it ;<
	// Imagine this is:
	/*
	 * struct FancyMesh {
	 * 	Mesh retarted_mesh;
	 * 	Texture[] textures;
	 *	uint[] indices;
	 * }
	 *
	 * FancyMesh[] meshes;
	 *   _,,
	 *  //||\
	 * ///||\\
	 *///alas\\
	Mesh[] retarted_meshes;
	Texture[][] meta_textures;
	uint[][] meta_indices;


	this(string fname) {
		if (!fname.fexists) fatal("File '%s' does not exist", fname);

		const aiScene *scene = aiImportFile(fname.cstr, aiProcess_Triangulate | aiProcess_FlipUVs | aiProcess_OptimizeMeshes | aiProcess_GenNormals);
		if (!scene || scene.mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene.mRootNode) {
			fatal("Failed to properly load model '%s'.  AssImp says '%s'", fname, aiGetErrorString());
		}

		load_mesh(scene.mRootNode, scene);
		assert (retarted_meshes.length == meta_textures.length && meta_textures.length == meta_indices.length);
	}

	void load_mesh(const aiNode *node, const aiScene *scene) {
		foreach (sss; 0 .. node.mNumMeshes) {
			const aiMesh *mesh = scene.mMeshes[node.mMeshes[sss]];
			float[] vertices;
			Texture[] textures;
			uint[] indices;

			foreach (i; 0 .. mesh.mNumVertices) {
				// position
				vertices ~= mesh.mVertices[i].x;
				vertices ~= mesh.mVertices[i].y;
				vertices ~= mesh.mVertices[i].z;

				// normal
				vertices ~= mesh.mNormals[i].x;
				vertices ~= mesh.mNormals[i].y;
				vertices ~= mesh.mNormals[i].z;

				// tex coords
				if (mesh.mTextureCoords[0]) {
					vertices ~= mesh.mTextureCoords[0][i].x;
					vertices ~= mesh.mTextureCoords[0][i].x;
				} else {
					// for alignment:
					vertices ~= 0;
					vertices ~= 0;
				}

				/////      | | |
				//// TODO: v v v those things

				// tangent
				vertices ~= 0;
				vertices ~= 0;
				vertices ~= 0;

				// bitangent
				vertices ~= 0;
				vertices ~= 0;
				vertices ~= 0;

			}

			foreach (i; 0 .. mesh.mNumFaces) {
				foreach (j; 0 .. mesh.mFaces[i].mNumIndices) {
					indices ~= mesh.mFaces[i].mIndices[j];
				}
			}

			retarted_meshes ~= Mesh(vertices, [3, 3, 2, 3, 3]);

			GLuint EBO;
			glGenBuffers(1, &EBO);
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
			glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.length * indices[0].sizeof, indices.ptr, GL_STATIC_DRAW);
			meta_indices ~= indices;

			meta_textures ~= textures;
		}

		foreach (i; 0 .. node.mNumChildren) {
			load_mesh(node.mChildren[i], scene);
		}
	}
}
