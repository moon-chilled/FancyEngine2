module stdmem;
import stdlib;

interface Allocator {
	void *alloc(size_t amnt);
	void *realloc(void *oldptr, size_t newsize);
	void free(void *ptr);

	//TODO: why doesn't this work?
	final T *allocate(T)(size_t num) {
		return cast(T*)this.alloc(T.sizeof * num);
	}
}
/+
abstract class Allocator: IAllocator {
	T *allocate(T)(size_t num) {
		return cast(T*)this.alloc(T.sizeof * num);
	}
}
+/

class SysAllocatorT: Allocator {
	void *alloc(size_t amnt) {
		import core.stdc.stdlib: calloc;
		void *ret = calloc(1, amnt);
		if (!ret) fatal("Failed to callocate");
		return ret;
	}
	void *realloc(void *oldptr, size_t newsize) {
		static import core.stdc.stdlib;
		void *ret = core.stdc.stdlib.realloc(oldptr, newsize);
		if (!ret) fatal("Failed to reallocate");
		return ret;
	}
	void free(void *ptr) {
		static import core.stdc.stdlib;
		core.stdc.stdlib.free(ptr);
	}
}
__gshared SysAllocatorT SysAllocator = new SysAllocatorT();
//static this() { SysAllocator = new SysAllocatorT(); }

class BumpAllocator: Allocator {
	private void *mem;
	private size_t len;
	private size_t pointer = 0;

	// 16MB
	this(size_t size = 0x1_000_000) {
		this.len = size;

		//TODO: VirtualAlloc version
		// ditto for dfly and fbsd if that ever becomes a thing
		//import core.sys.posix.sys.mman;
		static if (build_target == OS.Linux) import core.sys.linux.sys.mman;
		else static if (build_target == OS.FreeBSD) import core.sys.freebsd.sys.mman;
		void *memaddr = mmap(null, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		if (memaddr == MAP_FAILED) fatal("Failed to map memory");
		mem = memaddr;
	}
	~this() { import core.sys.posix.sys.mman; munmap(mem, len); }

	void *alloc(size_t amnt) {
		if (pointer + amnt >= len) {
			fatal("OOM!");
		}

		void *ret = mem + pointer;
		pointer += amnt;
		return ret;
	}
	void *realloc(void *oldptr, size_t newsize) { fatal("Realloc'd bump allocator"); assert(0); }
	void free(void *ptr) { fatal("Free'd bump allocator"); }

	void clear() { pointer = 0; }
}

T New(T, R...)(R args) if (is(T == class)) { return New!T(SysAllocator, args); }
T New(T, R...)(Allocator a, R args) if (is(T == class)) {
	import core.memory: GC;
	import std.conv: emplace;

	size_t size = __traits(classInstanceSize, T);
	void[] ret = a.alloc(size)[0 .. size];

	GC.addRange(ret.ptr, size);

	return emplace!(T, R)(ret, args);
}

void Delete(T)(T obj) if (is(T == class)) { Delete(SysAllocator, obj); }
void Delete(T)(Allocator a, T obj) if (is(T == class)) {
	if (!obj) return;

	destroy(obj);
	GC.removeRange(cast(void*)obj);
	a.free(cast(void*)obj);
}

T *New(T, R...)(R args) if (!is(R[0]: Allocator)) { return New!T(SysAllocator, args); }
T *New(T, R...)(Allocator a, R args) {
	import core.memory: GC;
	import std.conv: emplace;

	T *ret = cast(T*)a.alloc(T.sizeof);

	GC.addRange(ret, T.sizeof);
	return emplace!(T, R)(ret, args);
}

void Delete(T)(T *obj) { Delete(SysAllocator, obj); }
void Delete(T)(Allocator a, T *obj) {
	if (!obj) return;

	static if (is(T == struct)) destroy(obj);

	GC.removeRange(obj);
	a.free(obj);
}
