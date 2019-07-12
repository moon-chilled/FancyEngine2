module scripting.s7;
import stdlib;
import cstdlib;

import scripting;
import scripting.s7_lib_interface;

ScriptVar s7_to_script(s7_scheme *s7, s7_pointer ptr) {
	if (s7_is_null(s7, ptr)) {
		return None;
	} else if (s7_is_boolean(ptr)) {
		return ScriptVar(s7_boolean(s7, ptr));
	} else if (s7_is_string(ptr)) {
		return ScriptVar(s7_string(ptr).dstr);
	} else if (s7_is_integer(ptr)) {
		return ScriptVar(s7_integer(ptr));
	} else if (s7_is_real(ptr)) {
		return ScriptVar(s7_real(ptr));
	} else if (s7_is_ratio(ptr)) {
		return ScriptVar(cast(double)s7_numerator(ptr) / cast(double)s7_denominator(ptr));
	} else {
		fatal("Got unknown scheme value with value %s", s7_object_to_c_string(s7, ptr).dstr);
		assert(0);
	}
}

class S7Script: Scriptlang {
	s7_scheme *s7;

	this() {
		s7 = s7_init();
		s7_eval_c_string(s7, `(format #t "Successfully booted ~a~%" (s7-version))`.cstr);
	}

	void close() {
		s7_quit(s7);
	}

	ScriptVar eval(string text) {
		return s7_to_script(s7, s7_eval_c_string(s7, text.cstr));
	}
	ScriptVar call(string name, ScriptVar[] args) {
		//TODO
		return None;
	}

	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtyeps) {
	}

	bool can_be_loaded(string path) {
		return false;
	}
	void load(string path) {
	}
}
