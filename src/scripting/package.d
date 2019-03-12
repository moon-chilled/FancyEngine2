module scripting;
import stdlib;

import std.variant: Sum = Algebraic;

struct None{}

enum ScriptVarType {
	Int,
	Real,
	Str,
	Any,
	None,
}
alias ScriptVar = Sum!(long, double, string, None);
alias ScriptFun = ScriptVar delegate(ScriptVar[] args);

interface Scriptlang {
	ScriptVar eval(string text);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] args);
}
