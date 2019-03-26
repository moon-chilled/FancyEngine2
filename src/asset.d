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
		import std.file: read, exists;
		if (!fpath.exists) fatal("tried to load nonexistent file '%s'", fpath);
		data = cast(ubyte[])read(fpath);
	}
}

/*
class Effect: Asset {
	static bool is_valid(string fpath) {
		import std.file: exists;

		if (!fpath.exists) return false;

		//TODO
	}

	this(string fpath) {
	}
	~this() {
	}
}
class Music: Asset {
	static bool is_valid(string fpath) {
		import std.file: exists;
		if (!fpath.exists) return false;

		//TODO
	}
	this(string fpath) {
	}

	~this() {
	}
}
*/
