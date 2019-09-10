module stdmem;
import stdlib;

T *Alloc(T)(size_t amount = 1) {
	import core.stdc.stdlib: calloc, abort;
	T *ret = cast(T*)calloc(amount, T.sizeof);
	if (!ret) abort();
	return ret;
}
void Free(T)(T *val) {
	import core.stdc.stdlib: free;
	free(val);
}

T New(T, R...)(R args) if (is(T == class)) {
	import core.memory: GC;
	import std.conv: emplace;

	size_t size = __traits(classInstanceSize, T);
	void[] ret = Alloc!void(size)[0 .. size];

	GC.addRange(ret.ptr, size);

	return emplace!(T, R)(ret, args);
}
void Delete(T)(T obj) if (is(T == class)) {
	if (!obj) return;

	destroy(obj);
	GC.removeRange(cast(void*)obj);
	Free(cast(void*)obj);
}

T *New(T, R...)(R args) {
	import core.memory: GC;
	import std.conv: emplace;

	T *ret = Alloc!T(1);

	GC.addRange(ret, T.sizeof);
	return emplace!(T, R)(ret, args);
}
void Delete(T)(T *obj) {
	if (!obj) return;

	static if (is(T == struct)) destroy(obj);

	GC.removeRange(obj);
	Free(obj);
}
