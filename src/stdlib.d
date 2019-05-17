module stdlib;

public:
import logging;
import std.math: abs, trunc; //more to follow as needed
import std.container.array: ManArray = Array;
import std.variant: Sum = Algebraic;
import std.conv: to, tostr = text, towstr = wtext, todstr = dtext;
import std.array: split;
import std.algorithm.comparison: min, max, clamp;
import std.algorithm.iteration: reduce = fold, sum;
import std.file: fexists = exists;

enum BuildType {
	Dev,
	Release,
}
version (_Build_type_is_dev) {
	enum build_type = BuildType.Dev;
} else version (_Build_type_is_release) {
	enum build_type = BuildType.Release;
} else {
	static assert(0, "must select a build type -- either dev or release");
}

enum GfxBackend {
	Vulkan,
	OpenGL,
}
version (_Gfx_is_vulkan) {
	enum gfx_backend = GfxBackend.Vulkan;
} else version (_Gfx_is_opengl) {
	enum gfx_backend = GfxBackend.OpenGL;
} else {
	static assert(0, "Must select graphics backend");
}


// !IMPORTANT!
// size is NOT dependent on T.sizeof
// this is to avoid confusion
// the only purpose of this function is to make it so you don't have to cast your pointers to void*
pragma(inline, true) void memcpy(T, U)(T *dest, const U *src, size_t size) {
	// print a message if T and U are different sizes, unless one is void
	static if ((T.sizeof != U.sizeof) && !(is(T == void)) && !(is(U == void))) {
		import std.string: format;
		pragma(msg, format("Unequal types %s (%s bytes) and %s (%s bytes) being catenated", T.stringof, T.sizeof, U.stringof, U.sizeof));
	}

	static import core.stdc.string;
	core.stdc.string.memcpy(cast(void*)dest, cast(const void*)src, size);
}

__gshared bool are_libraries_loaded;

void segfault() {
	int inner() {
		import core.stdc.stdlib: malloc;

		int *p = cast(int*)malloc(-1);
		return *p;
	}

	inner();
}


version (Windows) {
	version (CRuntime_Microsoft) {
	} else {
		static assert (false, "Only supported MSVCRT on windows");
		// maybe someday this turns into mingw/gnu crt, when I have a better dev environment
		// until then, I will be sad
	}
} else version (OSX) {
} else version (linux) {
} else {
	static assert (false, "Only supported windows, macos, or linux");
}


version (X86_64) {
	//yayy
} else {
	static assert (false, "Only supported x86_64");
}
