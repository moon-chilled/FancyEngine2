module graphics.tex_d3d11;
import stdlib;
import cstdlib;

import asset;
import windowing.windows_d3d11;

import directx.d3d11;

private extern (C) ubyte *stbi_load(const(char) *filename, int *x, int *y, int *channels, int desired_channels);
private extern (C) void stbi_image_free(void *retval_from_stbi_load);

class Texture: Asset!(AssetType.Texture) {
	uint w, h;
	ID3D11Texture2D tex_id;

	this(string fpath, GfxContext ctx) {
		import std.file: exists;
		if (!fpath.exists) fatal("tried to read nonexistent texture '%s'", fpath);
		int clr_depth, width, height;
		ubyte *tex_data = stbi_load(fpath.cstr, &width, &height, &clr_depth, 0);
		if (!tex_data) fatal("unable to read texture from file '%s'", fpath);
		assert ((width > 0) && (height > 0));
		w = width;
		h = height;

		D3D11_TEXTURE2D_DESC desc;
		desc.MipLevels = 0; // autodetect # of mipmap levels
		desc.Width = w;
		desc.Height = h;
		desc.ArraySize = 1;
		desc.MipLevels = 1;
		switch (clr_depth) {
			case 4: desc.Format = DXGI_FORMAT_R8G8B8A8_UNORM; break;
			default: fatal("Need 4-byte colour depth, got %s", clr_depth); assert(0);
		}
		desc.Usage = D3D11_USAGE_DEFAULT;
		desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;

		D3D11_SUBRESOURCE_DATA data;
		data.pSysMem = tex_data;
		data.SysMemPitch = width * clr_depth;

		HRESULT res = ctx.device.CreateTexture2D(&desc, &data, &tex_id);
		if (FAILED(res)) fatal("CreateTexture2D failed with file '%s'; return %x", fpath, res);
		stbi_image_free(tex_data);
	}
}
