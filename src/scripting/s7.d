module scripting.s7;
import stdlib;
import cstdlib;

import scripting;
import scripting.s7_lib_interface;

private ScriptVar s7_to_script(s7_scheme *s7, s7_pointer ptr) {
	if (s7_is_null(s7, ptr)) {
		return None;
	} else if (s7_is_boolean(ptr)) {
		return ScriptVar(s7_boolean(s7, ptr));
	} else if (s7_is_string(ptr)) {
		return ScriptVar(s7_string(ptr).dstr);
	} else if (s7_is_integer(ptr)) {
		return ScriptVar(s7_integer(ptr));
	} else if (s7_is_ratio(ptr)) {
		return ScriptVar(cast(double)s7_numerator(ptr) / cast(double)s7_denominator(ptr));
	} else if (s7_is_real(ptr)) {
		return ScriptVar(s7_real(ptr));
	} else {
		fatal("Got unknown scheme value with value %s", s7_object_to_c_string(s7, ptr).dstr);
		assert(0);
	}
}
private s7_pointer script_to_s7(s7_scheme *s7, ScriptVar var) {
	import std.variant: visit;
	return var.visit!(
			(long l) => s7_make_integer(s7, l),
			(double d) => s7_make_real(s7, d),
			(string s) => s7_make_string_with_length(s7, s.ptr, s.length),
			(bool b) => s7_make_boolean(s7, b),
			(None) => s7_nil(s7))();
}

class S7Script: Scriptlang {
	s7_scheme *s7;

	this() {
		s7 = s7_init();
		log("Successfully booted %s", eval("(s7-version)"));
	}

	void close() {
		s7_quit(s7);
	}

	ScriptVar eval(string text) {
		return s7_to_script(s7, s7_eval_c_string(s7, text.cstr));
	}
	void exec(string text) {
		s7_eval_c_string(s7, text.cstr);
	}
	ScriptVar call(string name, ScriptVar[] args) {
		s7_pointer funcptr = s7_eval_c_string(s7, name.cstr); // lisp-1 ftw!
		s7_pointer argsptr = s7_nil(s7);
		foreach_reverse(arg; args) {
			argsptr = s7_cons(s7, script_to_s7(s7, arg), argsptr);
		}

		return s7_to_script(s7, s7_call(s7, funcptr, argsptr));
	}

	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtyeps) {
	}

	bool can_be_loaded(string path) {
		return false;
	}
	void load(string path) {
	}
}
