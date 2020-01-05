module graphics.fancy_model_gl;
import stdlib;
import cstdlib;
import bindbc.opengl;
import bindbc.assimp;

import graphics.model;
import graphics.tex;

struct FancyModel {
	@disable this();

	/// SOA ftw!  Yeah I hate it ;<
	// Imagine this is:
	/*
	 * struct FancyMesh {
	 * 	Mesh stupid_mesh;
	 * 	Texture[] textures;
	 *	uint[] indices;
	 * }
	 *
	 * FancyMesh[] meshes;
	 *   _,,
	 *  //||\
	 * ///||\\
	 *///alas\\
	Mesh[] stupid_meshes;
	Texture[][] meta_diffuse_textures;
	Texture[][] meta_specular_textures;
	uint[][] meta_indices;


	this(string fpath) {
		import std.file: getcwd, chdir;

		if (!fpath.fexists) fatal("File '%s' does not exist", fpath);

		const aiScene *scene = aiImportFile(fpath.cstr, aiPostProcessSteps.Triangulate | aiPostProcessSteps.OptimizeMeshes | aiPostProcessSteps.GenNormals | aiPostProcessSteps.FlipWindingOrder); // default winding order is counter-clockwise, we want clockwise
		if (!scene || scene.mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene.mRootNode) {
			fatal("Failed to properly load model '%s'.  AssImp says '%s'", fpath, aiGetErrorString());
		}

		string cwd = getcwd();
		chdir(fpath.split("/")[0 .. $ - 1].join("/"));
		load_meshes(scene.mRootNode, scene);
		chdir(cwd);

		assert (stupid_meshes.length == meta_diffuse_textures.length && meta_diffuse_textures.length == meta_specular_textures.length && meta_specular_textures.length == meta_indices.length);

		trace("Loaded model %s with %s meshes", fpath, stupid_meshes.length);
	}

	void load_meshes(const aiNode *node, const aiScene *scene) {
		foreach (sss; 0 .. node.mNumMeshes) {
			const aiMesh *mesh = scene.mMeshes[node.mMeshes[sss]];
			float[] vertices;
			Texture[] diffuse_textures;
			Texture[] specular_textures;
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
					vertices ~= mesh.mTextureCoords[0][i].y;
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

			diffuse_textures = load_materials(scene.mMaterials[mesh.mMaterialIndex], aiTextureType.DIFFUSE);
			specular_textures = load_materials(scene.mMaterials[mesh.mMaterialIndex], aiTextureType.SPECULAR);


			stupid_meshes ~= Mesh(vertices, [3, 3, 2, 3, 3]);

			GLuint EBO;
			glGenBuffers(1, &EBO);
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
			glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.length * indices[0].sizeof, indices.ptr, GL_STATIC_DRAW);
			meta_indices ~= indices;

			meta_diffuse_textures ~= diffuse_textures;
			meta_specular_textures ~= specular_textures;
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
