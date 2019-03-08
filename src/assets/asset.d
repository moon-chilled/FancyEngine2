module assets.asset;
import stdlib;

enum Assettype {
	Data,
	Sound,
}

interface Asset {
}

class Data: Asset {
	ubyte[] data;
	static bool f_is_valid(string fpath) {
		import std.file;
		return fpath.exists;
	}

	this(string fpath) {
		import std.file: read;
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
