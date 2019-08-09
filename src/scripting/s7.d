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

struct S7Fun {
	string name;
	ScriptFun fun;
	ScriptVarType[] argtypes;
}
__gshared S7Fun[] s7funs;
private __gshared Object s7funslock = new Object;

class S7Script: Scriptlang {
	s7_scheme *s7;

	this() {
		s7 = s7_init();
		log("Successfully booted %s", eval("(s7-version)"));

		void real_log(long ll, long line, string file, string func_name, string pretty_func_name, string module_name, string msg) { _real_log(cast(LogLevel)ll, cast(int)line, file, func_name, pretty_func_name, module_name, msg); }

		expose_fun("_real_push_log_msg", &_real_push_log_msg);
		s7_add_to_load_path(s7, "dist/scheme".cstr);
		load("stdlib.scm");

		extern (C) s7_pointer s7_funcwrapper(s7_scheme *sc, s7_pointer args) {
			S7Fun fun = s7funs[s7_integer(s7_car(args))];

			args = s7_cdr(args);

			ScriptVar[] fargs;
			while (args != s7_nil(sc)) {
				fargs ~= s7_to_script(sc, s7_car(args));
				args = s7_cdr(args);
			}

			bool bad_call;

			if (fargs.length != fun.argtypes.length) {
				bad_call = true;
			}

			foreach (i; 0 .. fargs.length) {
				if (script_typeof(fargs[i]) != fun.argtypes[i]) {
					bad_call = true;
					break;
				}
			}

			if (bad_call) {
				error("D function %s was passed arguments %s but required types %s", fun.name, fargs, fun.argtypes);
				return s7_nil(sc);
			}


			return script_to_s7(sc, fun.fun(fargs));
		}

		s7_define_function(s7, "__s7_funcwrapper", &s7_funcwrapper, 0, 0, true, "—".cstr);
		// s7, name, function, required args, optional args, rest (variadic?) args, docstring
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
	ScriptVar call(string name, ScriptVar[] args = []) {
		s7_pointer funcptr = s7_name_to_value(s7, name.cstr); // lisp-1 ftw!
		s7_pointer argsptr = s7_nil(s7);
		foreach_reverse(arg; args) {
			argsptr = s7_cons(s7, script_to_s7(s7, arg), argsptr);
		}

		return s7_to_script(s7, s7_call(s7, funcptr, argsptr));
	}

	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes) {
		long new_index;
		synchronized (s7funslock) {
			new_index = s7funs.length++;
		}
		s7funs[new_index] = S7Fun(name, fun, argtypes);

		exec(strfmt("(define (%s . args) (apply __s7_funcwrapper (cons %s args)))", name, new_index));
	}
	void expose_fun(R, A...)(string name, R delegate(A) fun) {
		ScriptVarType[] signature;

		static foreach (T; A) {
			signature ~= script_typeof!T;
		}

		enum mixin_str = {
			import std.conv: to;
			import std.traits: OriginalType;

			string ret = "fun(";
			static foreach (i; 0 .. A.length) {
				ret ~= "cast(" ~ A[i].stringof ~ ")*args[" ~ i.to!string ~ "].peek!" ~ OriginalType!(A[i]).stringof ~ ", ";
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

	void expose_fun(R, A...)(string name, R function(A) fun) {
		expose_fun(name, toDelegate(fun));
	}

	bool can_load(string path) {
		auto s7_obj = s7_eval_c_string_with_environment(s7, (`
(catch 'read-error
 (lambda () (load "` ~ path ~ `") #t)
 (lambda args
  #f))`).cstr, s7_inlet(s7, s7_nil(s7)));
		return *s7_to_script(s7, s7_obj).peek!bool;
	}
	void load(string path) {
		s7_load(s7, path.cstr);
	}
}
