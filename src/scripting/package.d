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

interface Scriptlang {
	void close();
	ScriptVar eval(string text);
	void exec(string text); //TODO: remove this.  It's a temporary kludge; even eval shouldn't really be allowed
	ScriptVar call(string name, ScriptVar[] args);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes);
	bool can_load(string path);
	void load(string path);
}
