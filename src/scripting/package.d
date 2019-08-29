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
	} else static if (is(T == mat4f)) {
		return ScriptVarType.Matx4;
	} else static if (is(T == void)) {
		return ScriptVarType.NoneType;
	} else static if (is(T == ScriptVar)) {
		return ScriptVarType.Any;
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
	final void expose_fun(R, A...)(string name, R function(A...) fun) {
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
					ret ~= "cast(" ~ A[i].stringof ~ ")*args[" ~ i.to!string ~ "].peek!" ~ OriginalType!(A[i]).stringof ~ ", ";
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
	bool has_symbol(string name);

	void expose_var(string name, ref ScriptVar var);
}

abstract class ScriptlangImpl: Scriptlang {
	private ScriptVar*[string] var_tab;

	void expose_var(string name, ref ScriptVar var) {
		var_tab[name] = &var;
	}

	this() {
		expose_fun("sset", (string name, ScriptVar var) { *var_tab[name] = var; });
		expose_fun("sget", (string name) => *var_tab.get(name, &None));
	}
}
