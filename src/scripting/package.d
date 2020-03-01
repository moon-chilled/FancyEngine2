module scripting;
import stdlib;
import stdmath;

import graphics.fancy_model;
import graphics.shading;
import windowing.key;

struct NoneType{}

enum ScriptVarType {
	// normal objects
	//integral types:
	Int,
	Real,
	Str,
	Bool,

	// math:
	Vec3,
	Matx4,

	// graphics:
	FancyModel,
	Shader,
	Key,
//	Texture,

	// special
	OpaquePtr,
	Any,
	None,
}
alias ScriptVar = Sum!(long, float, string, bool, vec3f, mat4f, FancyModel, Shader, Key, void*, NoneType);
alias ScriptFun = ScriptVar delegate(ScriptVar[] args);
ScriptVar None = ScriptVar(NoneType());

ScriptVarType script_typeof(ScriptVar v) {
	return v.match!(
			(bool b) => ScriptVarType.Bool,
			(Key k) => ScriptVarType.Key,
			(long l) => ScriptVarType.Int,
			(float d) => ScriptVarType.Real,
			(string s) => ScriptVarType.Str,
			(vec3f v) => ScriptVarType.Vec3,
			(mat4f m) => ScriptVarType.Matx4,
			(FancyModel f) => ScriptVarType.FancyModel,
			(Shader s) => ScriptVarType.Shader,

			(void *v) => ScriptVarType.OpaquePtr,
			//(ScriptVar s) => ScriptVarType.Any,
			(NoneType n) => ScriptVarType.None)();
}

// TODO: should this function use std.traits: Unqual(ified)?
// (that template removes qualifications from a type like const, shared, etc.
ScriptVarType script_typeof(T)() {
	static if (isIntegral!T) {
		return ScriptVarType.Int;
	} else static if (isFloatingPoint!T) {
		return ScriptVarType.Real;
	} else static if (isSomeString!T) {
		return ScriptVarType.Str;
	} else static if (is(T == bool)) {
		return ScriptVarType.Bool;
	} else static if (is(T == vec3f)) {
		return ScriptVarType.Vec3;
	} else static if (is(T == mat4f)) {
		return ScriptVarType.Matx4;
	} else static if (is(T == FancyModel)) {
		return ScriptVarType.FancyModel;
	} else static if (is(T == Shader)) {
		return ScriptVarType.Shader;

	} else static if (is(T == void*)) {
		return ScriptVarType.OpaquePtr;
	} else static if (is(T == void)) {
		return ScriptVarType.NoneType;
	} else static if (is(T == ScriptVar)) {
		return ScriptVarType.Any;
	} else {
		static assert(0);
	}
}

interface ScriptlangImpl {
	void close();
	string eval_to_str(string text);
	ScriptVar eval(string text);
	void exec(string text); //TODO: remove this.  It's a temporary kludge; even eval shouldn't really be allowed
	ScriptVar call(string name, ScriptVar[] args = []);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes, bool variadic = false);

	final void expose_fun(R, A...)(string name, R function(A) fun) {
		expose_fun(name, toDelegate(fun));
	}
	final void expose_fun(R, A...)(string name, R delegate(A) fun) {
		ScriptVarType[] signature;

		static foreach (T; A) {
			signature ~= script_typeof!T;
		}

		enum mixin_str = {
			import std.conv: to;
			import std.traits: OriginalType;

			string ret = "fun(";
			static foreach (i; 0 .. A.length) {
				static if (is(A[i] == ScriptVar)) {
					ret ~= "args[" ~ i.to!string ~ "], ";
				} else {
					ret ~= "cast(" ~ A[i].stringof ~ ")args[" ~ i.to!string ~ "].peek!(" ~ OriginalType!(A[i]).stringof ~ "), ";
				}
			}
			ret ~= ")";
			return ret;
		}();

		static if (is(R == void)) {
			expose_fun(name,
					(ScriptVar[] args) {
						mixin(mixin_str ~ ";");
						return None;
					}, signature);
		} else {
			expose_fun(name,
					(ScriptVar[] args) {
						return ScriptVar(mixin(mixin_str));
					}, signature);
		}
	}

	bool can_load(string path);
	void load(string path);
	ScriptedFunction[string] load_getsyms(string path, string[] wanted_syms);
	bool has_symbol(string name);
}

abstract class Scriptlang: ScriptlangImpl { this() {} }

alias ScriptedFunction = ScriptVar delegate(ScriptVar[] args = []);
