module asset;
import stdlib;

enum AssetType {
	Data,
	BufferedSound,
	CachedSound,
	Texture,
}

abstract class Asset {
	AssetType asset_type;
}

class Data: Asset {
	AssetType asset_type = AssetType.Data;
	private ubyte[] data;
	this(string fpath) {
		import std.file: read;
		if (!fpath.fexists) fatal("tried to load nonexistent file '%s'", fpath);
		data = cast(ubyte[])read(fpath);
	}
}
