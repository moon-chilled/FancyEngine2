module graphics.framebuffer_vk;
import stdlib;
import cstdlib;

import asset;
import windowing.windows_vk;

import erupted;



struct Framebuffer {
	uint w, h;

	@disable this(this);
	@disable this();

	this(uint w, uint h, GfxContext ctx) {
	}

	~this() {
	}
}
