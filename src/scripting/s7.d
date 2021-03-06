module scripting.s7;
import stdlib;
import stdmath;
import cstdlib;

import scripting;
import scripting.s7_lib_interface;

import sound.gorilla;

import graphics.fancy_model;
import graphics.shading;
import graphics.tex;
import graphics.font;

import windowing.key;


private ScriptVar s7_to_script(s7_scheme *s7, s7_pointer ptr) {
	if (s7_is_null(s7, ptr) || s7_is_unspecified(s7, ptr)) {
		return None;
	} else if (s7_is_boolean(ptr)) {
		return ScriptVar(s7_boolean(s7, ptr));
	} else if (s7_is_string(ptr)) {
		return ScriptVar(s7_string(ptr).dstr);
	} else if (s7_is_integer(ptr)) {
		return ScriptVar(s7_integer(ptr));
	} else if (s7_is_ratio(ptr)) {
		return ScriptVar(cast(float)s7_numerator(ptr) / cast(float)s7_denominator(ptr));
	} else if (s7_is_real(ptr)) {
		return ScriptVar(s7_real(ptr));
	} else if (s7_is_float_vector(ptr)) {
		if (s7_vector_length(ptr) == 2) {
			vec2f ret;
			ret.v[] = s7_float_vector_elements(ptr)[0 .. 2];
			return ScriptVar(ret);
		} else if (s7_vector_length(ptr) == 3) {
			vec3f ret;
			ret.v[] = s7_float_vector_elements(ptr)[0 .. 3];
			return ScriptVar(ret);
		} else if (s7_vector_length(ptr) == 16) {
			mat4f ret;
			ret.v = s7_float_vector_elements(ptr)[0 .. 16];
			return ScriptVar(ret);
		} else {
			fatal("Got scheme float vector %s; not of length 3 or 16, so not a vec3 or mat4", s7_float_vector_elements(ptr)[0 .. s7_vector_length(ptr)]);
			assert(0);
		}
	} else if (s7_is_list(s7, ptr)) {
		ScriptVar[] ret;
		do {
			ret ~= s7_to_script(s7, s7_car(ptr));
		} while (!s7_is_null(s7, ptr = s7_cdr(ptr)));
		return ScriptVar(ret);
	} else if (s7_is_c_pointer(ptr)) {
		//!WARNING!XXX
		// this is a bit dangerous
		// because anything could be in a c pointer
		// we provide the following assurance:

		// Please do not construct c-pointers in s7 unless you are making them from ScriptVars.  Thank you!
		return *cast(ScriptVar*)s7_c_pointer(ptr);
	} else {
		trace("Got unknown scheme value with value %s", s7_object_to_c_string(s7, ptr).dstr);
		return None;
	}
}

private s7_pointer script_to_s7(s7_scheme *s7, ScriptVar var, s7_pointer[Key] key_to_symtab = null) {
	void copy_to_s7_vec(float *dest, float[] src) {
		foreach (i; 0 .. src.length) {
			dest[i] = src[i];
		}
	}

	return var.match!(
		(bool b) => s7_make_boolean(s7, b),
		(Key k) => key_to_symtab ? key_to_symtab[k] : s7_make_symbol(s7, k.key_to_str.cstr),
		(long l) => s7_make_integer(s7, l),
		(float d) => s7_make_real(s7, d),
		(string s) => s7_make_string_with_length(s7, s.ptr, s.length),
		(vec2f v) { s7_pointer ret = s7_make_float_vector(s7, 2, 0, null); copy_to_s7_vec(s7_float_vector_elements(ret), v.v); return ret; },
		(vec3f v) { s7_pointer ret = s7_make_float_vector(s7, 3, 0, null); copy_to_s7_vec(s7_float_vector_elements(ret), v.v); return ret; },
		(mat4f m) { s7_pointer ret = s7_make_float_vector(s7, 16, 0, null); copy_to_s7_vec(s7_float_vector_elements(ret), m.v); return ret; },
		(ScriptVar[] arr) => arr_to_list(s7, arr),
		(FancyModel f) => s7_make_c_pointer(s7, New!ScriptVar(f)),
		(Shader s) => s7_make_c_pointer(s7, New!ScriptVar(s)),
		(Texture t) => s7_make_c_pointer(s7, New!ScriptVar(t)),
		(Font f) => s7_make_c_pointer(s7, New!ScriptVar(f)),
		(BufferedSound s) => s7_make_c_pointer(s7, New!ScriptVar(s)),
		(CachedSound s) => s7_make_c_pointer(s7, New!ScriptVar(s)),
		(void *v) => s7_nil(s7),
		(NoneType n) => s7_nil(s7))();
}

private s7_pointer arr_to_list(s7_scheme *s7, ScriptVar[] arr) {
	s7_pointer ret = s7_nil(s7);

	foreach_reverse(a; arr) {
		ret = s7_cons(s7, script_to_s7(s7, a), ret);
	}

	return ret;
}


struct S7Fun {
	string name;
	ScriptFun fun;
	ScriptVarType[] argtypes;
	bool variadic;
}
__gshared S7Fun[] s7funs;
private __gshared Object s7funslock = new Object;

class S7Script: Scriptlang {
	s7_scheme *s7;
	private s7_pointer[Key] key_to_symtab;

	this() {
		import std.concurrency;
		//log("ENTER (not yet) (%s)", thisTid);
		s7 = s7_init();
		log("Successfully booted %s (%s) (%s)", eval("(*s7* 'version)"), s7, thisTid);
		//scope(exit)log("and now EXIT (%s) (%s)", s7, thisTid);

		void real_push_log_msg(long ll, string str, string basic_str) { _real_push_log_msg(cast(LogLevel)ll, str, basic_str); }

		super.expose_fun("_real_push_log_msg", &real_push_log_msg);

		s7_add_to_load_path(s7, "dist/scheme".cstr);
		load("prelude.scm");
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

			if (!fun.variadic) {
				if (fargs.length != fun.argtypes.length) {
					bad_call = true;
				} else foreach (i; 0 .. fargs.length) {
					// allow casting from int to real
					if (script_typeof(fargs[i]) == ScriptVarType.Int && fun.argtypes[i] == ScriptVarType.Real) {
						fargs[i] = ScriptVar(cast(float)fargs[i].peek!long);
					}

					if ((script_typeof(fargs[i]) != fun.argtypes[i]) && (fun.argtypes[i] != ScriptVarType.Any)) {
						bad_call = true;
						break;
					}
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

		super();

		static foreach (keysym; __traits(allMembers, Key)) {
			key_to_symtab[mixin("Key." ~ keysym)] = s7_make_symbol(s7, (keysym == "key_delete" ? "delete" : keysym).cstr);
		}
	}

	void close() {
		s7_quit(s7);
	}

	ScriptVar eval(string text) {
		return s7_to_script(s7, s7_eval_c_string(s7, text.cstr));
	}
	string eval_to_str(string text) {
		return s7_object_to_c_string(s7, s7_eval_c_string(s7, text.cstr)).dstr;
	}
	void exec(string text) {
		import std.concurrency;
		//log("ENTER (%s) (%s)", s7, thisTid);
		//scope(exit)log("EXIT (%s) (%s)", s7, thisTid);
		s7_eval_c_string(s7, text.cstr);
	}
	ScriptedFunctionBase[string] load_getsyms(string path, string[] wanted_syms) {
		import std.concurrency;
		//log("ENTER (%s) (%s)", s7, thisTid);
		//scope(exit)log("EXIT (%s) (%s)", s7, thisTid);
		s7_pointer env;
		env = s7_inlet(s7, s7_nil(s7));

		ScriptedFunctionBase[string] ret;

		string pre_src = fslurp(path);
		string post_src;
		for (size_t i = 0; i < pre_src.length; i++) {
			if (pre_src[i] != '$') {
				post_src ~= pre_src[i];
				continue;
			}

			string vname = "";
			while ((i+1) < pre_src.length && !isspace(pre_src[i+1]) && pre_src[i+1] != '(' && pre_src[i+1] != ')' && pre_src[i+1] != '\'' && pre_src[i+1] != '"' && pre_src[i+1] != '$') {
				i++;
				vname ~= pre_src[i];
			}

			// TODO @perf: pre-hash vname, generate a slot for it
			// and replace with pointer (rather than hash)
			post_src ~= strfmt(` (vref "%s") `, vname);
		}

		string opath = path ~ "f";
		fspurt(opath, post_src);
		scope (exit) { fdelete(opath); }

		if (!s7_load_with_environment(s7, opath.cstr, env)) {
			error("Failed to load %s", path);
			return ret;
		}

		foreach (sym; wanted_syms) {
			s7_pointer p = s7_let_ref(s7, env, s7_make_symbol(s7, sym.replace('_', '-').cstr));
			s7_gc_protect(s7, p);
			//TODO: lifetime issue: currently, this closes around 's7'
			//but could outlive 'this', which destroys the 's7' ctx.
			//or is that not an issue since both live basically for the lifetime of the program?

			if (s7_is_function(p) || s7_is_procedure(p)) ret[sym] = ((p) => (ScriptVar[] args) => s7_to_script(s7, s7_call(s7, p, arr_to_list(s7, args))))(p);
			// ^^ need that indirection to properly close over p
			// because otherwise it's closed over by reference
			// meaning that all returned function pointers close over the last p we create (and that will be overwritten on future invocations)
			// this way, it first has to be copied into a newly created environment
			// so it's a copy, which persists
		}


		return ret;
	}

	ScriptVar call(string name, ScriptVar[] args = []) {
		import std.concurrency;
		//log("ENTER(%s) (%s) (%s)", name, s7, thisTid);
		//scope(exit)log("EXIT(%s) (%s) (%s)", name, s7, thisTid);
		s7_pointer funcptr = s7_name_to_value(s7, name.cstr); // lisp-1 ftw!

		return s7_to_script(s7, s7_call(s7, funcptr, arr_to_list(s7, args)));
	}

	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes, bool variadic = false) {
		import std.concurrency;
		//log("ENTER (%s) (%s)", s7, thisTid);
		//scope(exit)log("EXIT (%s) (%s)", s7, thisTid);

		name = name.replace('_', '-');
		long new_index;
		synchronized (s7funslock) {
			new_index = s7funs.length++;
		}
		s7funs[new_index] = S7Fun(name, fun, argtypes, variadic);

		exec(strfmt("(define (%s . args) (apply __s7_funcwrapper (cons %s args)))", name, new_index));
	}

	bool can_load(string path) {
		auto s7_obj = s7_eval_c_string_with_environment(s7, (`
(catch 'read-error
 (lambda () (load "` ~ path ~ `") #t)
 (lambda args
  #f))`).cstr, s7_inlet(s7, s7_nil(s7)));
		return s7_to_script(s7, s7_obj).peek!bool;
	}
	void load(string path) {
		import std.concurrency;
		//log("ENTER (%s) (%s)", s7, thisTid);
		//scope(exit)log("EXIT (%s) (%s)", s7, thisTid);
		s7_load(s7, path.cstr);
	}

	bool has_symbol(string name) {
		return s7_symbol_table_find_name(s7, name.cstr) ? true : false;
	}
}
