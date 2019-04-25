module graphics.shading_d3d11;
import stdlib;
import stdmath;
import cstdlib;

import windowing.windows_d3d11;
import graphics.tex_d3d11;

import directx.d3d11;
import directx.d3dcompiler;

// shader model 4.0 is, apparently, the first 'modern' version in hlsl
enum ShaderType {
	Vertex = "vs_4_0",
	Fragment = "ps_4_0", // "Pixel Shader"
}

struct Program {
	@disable this();
	@disable this(this);

	ID3D11VertexShader vs;
	ID3D11PixelShader fs;

	ID3D11Device device;
	ID3D11DeviceContext device_context;

	ID3D11Buffer VBO;

	ID3D11InputLayout layout;

	void upload_vertices(float[15] vertices) {
		D3D11_MAPPED_SUBRESOURCE hrsrc;
		device_context.Map(VBO, 0, D3D11_MAP_WRITE_DISCARD, 0, &hrsrc);
		memcpy(hrsrc.pData, vertices.ptr, vertices.sizeof);
		device_context.Unmap(VBO, 0);
	}


	this(string vertex_src, string fragment_src, GfxContext ctx) {
		device = ctx.device;
		device_context = ctx.device_context;

		// (v)ertex (s)hader (s)ource
		ID3DBlob vss = compile_shader(ShaderType.Vertex, vertex_src);
		// ditto
		ID3DBlob fss = compile_shader(ShaderType.Fragment, fragment_src);

		// soo much easier than opengl!
		device.CreateVertexShader(vss.GetBufferPointer, vss.GetBufferSize, null, &vs);
		device.CreatePixelShader(fss.GetBufferPointer, fss.GetBufferSize, null, &fs);


		device_context.VSSetShader(vs, null, 0);
		device_context.PSSetShader(fs, null, 0);
		D3D11_INPUT_ELEMENT_DESC[] layout_src = [
			D3D11_INPUT_ELEMENT_DESC("POSITION".cstr, 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D11_INPUT_PER_VERTEX_DATA, 0),
			D3D11_INPUT_ELEMENT_DESC("TEXCOORD0".cstr, 0, DXGI_FORMAT_R32G32_FLOAT, 0, 3 * float.sizeof, D3D11_INPUT_PER_VERTEX_DATA, 0),
			];
		device.CreateInputLayout(layout_src.ptr, 2, vss.GetBufferPointer, vss.GetBufferSize, &layout);
		device_context.IASetInputLayout(layout);

		// ah, nevermind
		D3D11_BUFFER_DESC buf_des;
		buf_des.Usage = D3D11_USAGE_DYNAMIC;
		buf_des.ByteWidth = float.sizeof * 5 * 3;
		buf_des.BindFlags = D3D11_BIND_VERTEX_BUFFER;
		buf_des.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
		device.CreateBuffer(&buf_des, null, &VBO);
	}

	void blit() {
		uint stride = 5 * float.sizeof;
		uint offset = 0;
		device_context.IASetVertexBuffers(0, 1, &VBO, &stride, &offset);
		device_context.IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		device_context.Draw(3, 0);
	}

	void upload_texture(size_t pos, Texture tex) {

	}

	~this() {
		vs.Release();
		fs.Release();
	}
}

private ID3DBlob compile_shader(ShaderType type, string src) {
	ID3DBlob ret;
	uint compile_flags = D3DCOMPILE_ENABLE_STRICTNESS;
	debug compile_flags |= D3DCOMPILE_DEBUG;
	ID3DBlob error_msg;

	HRESULT status = D3DCompile(src.cstr, src.length, null, null, null, "main".cstr, type.cstr, compile_flags, 0, &ret, &error_msg);
	if (FAILED(status)) {
		fatal("failed compiling %s shader.  Error of '%s'", type, (cast(char*)error_msg.GetBufferPointer).dstr);
	}
	if (error_msg) {
		error_msg.Release();
	}
	return ret;
}
