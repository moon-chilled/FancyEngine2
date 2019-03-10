module scripting;
import stdlib;

import std.variant: Sum = Algebraic;

struct None{}

enum ScriptVarType {
	num,
	dec,
	str,
	any,
	none,
}
alias ScriptVar = Sum!(long, double, string, None);
alias ScriptFun = ScriptVar function(ScriptVar[] args);

interface Scriptlang {
	ScriptVar eval(string text);
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] args);
}
