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

interface Scriptlang {
	void close();
	ScriptVar eval(string text);
	void exec(string text); //TODO: remove this.  It's a temporary kludge; even eval shouldn't really be allowed
	ScriptVar call(string name, ScriptVar[] args);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes);
	bool can_be_loaded(string path);
	void load(string path);
}
