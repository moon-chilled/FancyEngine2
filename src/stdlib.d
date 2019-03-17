module stdlib;

public:
import logging;
import std.math: abs, trunc; //more to follow as needed
import std.container.array: ManArray = Array;

struct Vector3 {
	double x, y, z;
}

// !IMPORTANT!
// size is NOT dependent on T.sizeof
// this is to avoid confusion
// the only purpose of this function is to make it so you don't have to cast your pointers to void*
void memcpy(T, U)(T *dest, const U *src, size_t size) {
	static import core.stdc.string;
	core.stdc.string.memcpy(cast(void*)dest, cast(const void*)src, size);
}

__gshared bool are_libraries_loaded;
