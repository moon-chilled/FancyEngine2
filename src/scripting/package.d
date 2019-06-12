module scripting;
import stdlib;

struct NoneType{}

enum ScriptVarType {
	Int,
	Real,
	Str,
	None,
	Any,
}
alias ScriptVar = Sum!(long, double, string, NoneType);
alias ScriptFun = ScriptVar delegate(ScriptVar[] args);
ScriptVar None;
shared static this() {
	None = ScriptVar(NoneType());
}

interface Scriptlang {
	ScriptVar eval(string text);
	ScriptVar call(string name, ScriptVar[] args);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes);
	bool can_be_loaded(string path);
	void load(string path);
}
