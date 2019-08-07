module scripting;
import stdlib;

struct NoneType{}

enum ScriptVarType {
	Int,
	Real,
	Str,
	Bool,
	None,
	Any,
}
alias ScriptVar = Sum!(long, double, string, bool, NoneType);
alias ScriptFun = ScriptVar delegate(ScriptVar[] args);
ScriptVar None;
shared static this() {
	None = ScriptVar(NoneType());
}

ScriptVarType script_typeof(ScriptVar v) {
	import std.variant: visit;
	return v.visit!(
			(long l) => ScriptVarType.Int,
			(double d) => ScriptVarType.Real,
			(string s) => ScriptVarType.Str,
			(bool b) => ScriptVarType.Bool,
			(NoneType) => ScriptVarType.None)();
}
ScriptVarType script_typeof(T)() {
	static if (is(T == long)) {
		return ScriptVarType.Int;
	} else static if (is(T == double)) {
		return ScriptVarType.Real;
	} else static if (is(T == string)) {
		return ScriptVarType.Str;
	} else static if (is(T == bool)) {
		return ScriptVarType.Bool;
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
