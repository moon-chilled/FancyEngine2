module stdlib;

import std.typecons: Typedef;
import std.traits: ReturnType, Unqual;

public:
import logging;

import std.algorithm: contains = canFind;
import std.algorithm.comparison: min, max, clamp;
import std.algorithm.iteration: reduce = fold, sum;
import std.array: split, join;
import std.container.array: ManArray = Array;
import std.conv: to, tostr = text, towstr = wtext, todstr = dtext;
import std.file: fexists = exists;
import std.math: abs, trunc; //more to follow as needed
import std.variant: Sum = Algebraic;

// TODO: do we want negative values (for invalidation)?  Size_t so it can hold
// a pointer?
alias Handle = Typedef!uint;

// Boring stuff: version flags are passed in as simple booleans, but we really
// need them to be enums, so they're 1) always defined and 2) always have a
// valid value
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

enum OS {
	Windows,
	Mac,
	Linux,
}
version (Windows) {
	enum build_target = OS.Windows;
} else version (OSX) {
	enum build_target = OS.Mac;
} else version (linux) {
	enum build_target = OS.Linux;
}

T *Alloc(T)(size_t amount = 1) {
	import core.stdc.stdlib: calloc;
	return cast(T*)calloc(amount, T.sizeof);
}
void Free(T)(T *val) {
	import core.stdc.stdlib: free;
	free(val);
}


// !IMPORTANT!
// size is NOT dependent on T.sizeof
// this is to avoid confusion with the real memcpy
// the only purpose of this function is to make it so you don't have to cast your pointers to void*
pragma(inline, true) void memcpy(T, U)(T *dest, const U *src, size_t size) {
	// complain if T and U are different sizes, unless one is void
	static if ((T.sizeof != U.sizeof) && !(is(T == void)) && !(is(U == void))) {
		import std.string: format;
		pragma(msg, format("Unequal types %s (%s bytes) and %s (%s bytes) being catenated", T.stringof, T.sizeof, U.stringof, U.sizeof));
	}

	// has to be static because otherwise bare memcpy call would try to recurse
	static import core.stdc.string;
	core.stdc.string.memcpy(cast(void*)dest, cast(const void*)src, size);
}

// alphaglosined was here
//TODO: why doesn't T = ReturnType!fun work?
T[] map(alias fun, F, T = typeof(fun(F.init)))(F[] arr) {
	Unqual!T[] ret = new Unqual!T[arr.length];

	foreach (i; 0 .. arr.length) {
		ret[i] = fun(arr[i]);
	}

	return ret;
}
T[] filter(alias fun, T)(T[] original) {
	Unqual!T[] ret;

	foreach (item; original) {
		if (fun(item)) {
			ret ~= item;
		}
	}

	return ret;
}



__gshared bool are_libraries_loaded;

void segfault() {
	// I had a cleverer way of segfaulting once, but can't find it now
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
		static assert (false, "On windows, only msvcrt is supported");
		// maybe someday this turns into mingw/gnu crt, when I have a better dev environment
		// until then, I will be sad
	}
} else version (OSX) {
} else version (linux) {
} else {
	static assert (false, "Only windows, macos, or linux are supported");
}


version (X86_64) {
	//yayy
} else {
	static assert (false, "Only x86_64 is supported"); // sorry risc-v
}
