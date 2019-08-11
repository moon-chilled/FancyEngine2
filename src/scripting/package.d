module scripting;
import stdlib;
import stdmath;

struct NoneType{}

enum ScriptVarType {
	Int,
	Real,
	Str,
	Bool,
	Vec3,
	Matx4,
	None,
	Any,
}
alias ScriptVar = Sum!(long, float, string, bool, vec3f, mat4f, NoneType);
alias ScriptFun = ScriptVar delegate(ScriptVar[] args);
ScriptVar None;
shared static this() {
	None = ScriptVar(NoneType());
}

ScriptVarType script_typeof(ScriptVar v) {
	import std.variant: visit;
	return v.visit!(
			(long l) => ScriptVarType.Int,
			(float d) => ScriptVarType.Real,
			(string s) => ScriptVarType.Str,
			(bool b) => ScriptVarType.Bool,
			(vec3f v) => ScriptVarType.Vec3,
			(mat4f m) => ScriptVarType.Matx4,
			(NoneType) => ScriptVarType.None)();
}
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
	} else static if (is(T == matx4f)) {
		return ScriptVarType.Matx4;
	} else static if (is(T == void)) {
		return None;
	} else {
		static assert(0);
	}
}

interface Scriptlang {
	void close();
	ScriptVar eval(string text);
	void exec(string text); //TODO: remove this.  It's a temporary kludge; even eval shouldn't really be allowed
	ScriptVar call(string name, ScriptVar[] args = []);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes);
	void expose_fun(R, A...)(string name, R delegate(A...) fun);
	bool can_load(string path);
	void load(string path);
}
