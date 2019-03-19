module scripting;
import stdlib;

struct NoneType{}

enum ScriptVarType {
	Int,
	Real,
	Str,
	Any,
	None,
}
alias ScriptVar = Sum!(long, double, string, NoneType);
alias ScriptFun = ScriptVar delegate(ScriptVar[] args);
ScriptVar None;
shared static this() {
	None = ScriptVar(NoneType());
}

interface Scriptlang {
	ScriptVar eval(string text);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] args);
}
