module asset;
import stdlib;

enum AssetType {
	Data,
	BufferedSound,
	CachedSound,
	Texture,
}

interface Asset(AssetType asset_type)  {
	enum type = asset_type;
}

class Data: Asset!(AssetType.Data) {
	private ubyte[] data;
	this(string fpath) {
		import std.file: read;
		if (!fpath.fexists) fatal("tried to load nonexistent file '%s'", fpath);
		data = cast(ubyte[])read(fpath);
	}
}
