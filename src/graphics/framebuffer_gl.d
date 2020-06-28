module graphics.framebuffer_gl;
import stdlib;
import cstdlib;

import asset;
import windowing.windows_gl;

import bindbc.opengl;



struct Framebuffer {
	GLuint fbo, /*rbo,*/ tex, tex2;
	uint w, h;

	@disable this(this);
	@disable this();

	this(uint w, uint h, GfxContext ctx) {
		this.w = w;
		this.h = h;

		glCreateFramebuffers(1, &fbo);

		//glGenRenderbuffers(1, &rbo);
		//glBindRenderbuffer(GL_RENDERBUFFER, rbo);
		//glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, w, h);
		//glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, rbo);
		//glBindRenderbuffer(GL_RENDERBUFFER, 0);

		glCreateTextures(GL_TEXTURE_2D, 1, &tex);
		glTextureParameteri(tex, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTextureParameteri(tex, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTextureStorage2D(tex, 1, GL_RGBA8, w, h);
		glNamedFramebufferTexture(fbo, GL_COLOR_ATTACHMENT0, tex, 0);

		glCreateTextures(GL_TEXTURE_2D, 1, &tex2);
		glTextureParameteri(tex2, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTextureParameteri(tex2, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTextureStorage2D(tex2, 1, GL_DEPTH24_STENCIL8, w, h);
		glNamedFramebufferTexture(fbo, GL_DEPTH_STENCIL_ATTACHMENT, tex2, 0);

		//glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, rbo);

		if (glCheckNamedFramebufferStatus(fbo, GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
			fatal("OpenGL: unable to create framebuffer");
	}

	~this() {
		glDeleteTextures(1, &tex);
		glDeleteFramebuffers(1, &fbo);
	}
}
