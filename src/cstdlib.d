module cstdlib;
import stdlib;

import core.memory: GC;

// includes useful stuff for c library wrappers
public:

char* cstr(string s) nothrow {
	char *ret = cast(char*)GC.malloc(s.length + 1);
	ret[s.length] = 0;
	memcpy(ret, s.ptr, s.length);
	return ret;
}
string dstr(char *s) nothrow { return dstr(cast(const(char)*)s); }
string dstr(const(char) *s) nothrow {
	string ret;

	while (*s) {
		ret ~= *s;
		s++;
	}

	return ret;
}
string dstr(size_t len)(char[len] s) nothrow {
	string ret;

	foreach (c; s) {
		if (!c) {
			break;
		}

		ret ~= c;
	}

	return ret;
}
