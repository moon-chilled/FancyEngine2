public:
import std.string: cstr = toStringz, dstr = fromStringz;
import logging;
import std.math: abs, trunc; //more to follow as needed
import std.container.array: ManArray = Array;

public __gshared bool are_libraries_loaded;

