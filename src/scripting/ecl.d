module scripting.ecl;
import stdlib;

import scripting;
import scripting.ecl_lib_interface;

import std.string: format;

struct DFun {
	string name;
	ScriptFun fun;
	ScriptVarType[] sig;
}

private __gshared DFun[] fun_tab;
private __gshared Object fun_tab_lock = new Object;

ScriptVarType cl_obj_type(cl_object lisp_obj) {
	switch (lisp_obj.ecl_t_of) {
		case cl_type.t_fixnum: return ScriptVarType.Int;
		case cl_type.t_singlefloat, cl_type.t_doublefloat: return ScriptVarType.Real;
		case cl_type.t_string, cl_type.t_base_string: return ScriptVarType.Str;
		default: return ScriptVarType.None;
	}
}
private ScriptVar cl_to_script(cl_object lisp_obj) {
	switch (lisp_obj.ecl_t_of) {
		case cl_type.t_fixnum:
			return ScriptVar(cast(long)lisp_obj >> 2);
		case cl_type.t_singlefloat:
			return ScriptVar(cast(double)lisp_obj.SF.SFVAL);
		case cl_type.t_doublefloat:
			return ScriptVar(lisp_obj.DF.DFVAL);
		case cl_type.t_base_string:
			string x = new char[lisp_obj.base_string.dim];
			memcpy(x.ptr, lisp_obj.base_string.self, lisp_obj.base_string.dim);
			return ScriptVar(x);
		case cl_type.t_string:
			/+
			if (!ecl_fits_in_base_string(lisp_obj)) {
				log("It fits");
				return cl_to_script(si_copy_to_simple_base_string(lisp_obj));
				/*
				lisp_obj = si_copy_to_simple_base_string(lisp_obj);
				assert(lisp_obj.ecl_t_of == cl_type.t_base_string);
				goto case cl_type.t_base_string;
				*/
			} else {
				log("I sits :<");
				lisp_obj = cl_copy_seq(lisp_obj);
				goto case cl_type.t_base_string;
				//return cl_to_script(cl_copy_seq(lisp_obj));
				//return None;
			}
			+/

			wchar_t[] x = new wchar_t[lisp_obj.string.dim];
			memcpy(x.ptr, lisp_obj.string.self, wchar_t.sizeof * lisp_obj.string.dim);
			return ScriptVar(x.tostr);
		default:
			return None;
	}
}
private cl_object script_to_cl(ScriptVar script_obj) {
	import std.variant: visit;
	return script_obj.visit!(
			(long l) => cast(cl_object)((l << 2) | cl_type.t_fixnum),
			(double d) => ecl_make_double_float(d),
			(string s) => s.lstr,
			(None) => Nil)();
}

class ECLScript: Scriptlang {
	private void introduce_loggers() {
		import std.traits: EnumMembers;
		ScriptVar dlog(ScriptVar[] s) {
			_real_log(cast(LogLevel)*s[0].peek!long, 0, "lisp" /*file*/, "lisp" /*function*/, "lisp" /*pretty function*/, "lisp" /*module*/, *s[1].peek!string);
			return None;
		}
		expose_fun("_dlog", &dlog, [ScriptVarType.Int, ScriptVarType.Str]);
		static foreach (ll; EnumMembers!LogLevel[1 .. $]) {
			eval("(defun log" ~ ll.to!string ~ " (&rest args) (_dlog " ~ ll.to!int.to!string ~ " (apply #'format (cons nil args))))");
		}
		/*
		eval("(defun loginfo (&rest args) (_dlog 2 (apply #'format (cons nil args))))");
		*/
	}

	this() {
		introduce_loggers();
		eval(`(loginfo "hii from ECL")`);
	}
	~this(){}

	ScriptVar eval(string text) {
		return text.lsym.cl_eval.cl_to_script;
	}

	ScriptVar call(string fname, ScriptVar[] args) {
		cl_object list = Nil;

		foreach_reverse (arg; args) {
			list = cl_cons(arg.script_to_cl, list);
		}


		list = cl_cons(fname.lsym, list);

		return cl_eval(list).cl_to_script;
	}

	// A bit of explanation is due here.
	// there needs to be a unified script interface, so I can pass the same functions into lisp, c, perl6, whatever
	// so script functions can't use cl_object, instead they use ScriptVar and ScriptVarType etc.
	// as such, every script lang needs to wrap around ScriptFun with a language-specific function
	// but that function needs to be a closure (aka delegate) so it knows the address of the d function to call
	// but we can't pass delegates into c functions
	// so instead we name the real function _ecl_d_fancy_fn_<funname>
	// and declare a *lisp* function that stores a pointer to the 'closed over' context as an index into fun_tab, and passes that into the real wrapper function
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes) {
		size_t our_fun_index;

		// synchronized because otherwise this can happen:
		/*
		 * thread 1 sets our_fun_index to (say) fun_tab.length, say it's 5
		 * fun_tab append begins
		 * hasn't finished yet
		 * thread 2 starts to expose a function, sets our_fun_index, but it's still 5
		 * now both threads think their function is #5, even though they're different functions
		 */
		// now, thread 2 can't start until thread 1 finishes, so it always gets the right index value
		synchronized (fun_tab_lock) {
			our_fun_index = fun_tab.length;
			fun_tab ~= DFun(name, fun, argtypes);
		}

		string real_fn_name = "_ecl_d_fancy_fn_" ~ name;
		extern (C) cl_object real_fun(fixnum nargs, ...) {
			import core.vararg;
			assert (nargs >= 1); // have to have 1 arg which is a context pointer
			va_list vargs; //(v)arargs
			va_start(vargs, nargs);

			long fun_index = (*va_arg!cl_object(vargs).cl_to_script.peek!long);
			assert (fun_index >= 0);

			string name = fun_tab[fun_index].name;
			ScriptFun fun = fun_tab[fun_index].fun;
			ScriptVarType[] argtypes = fun_tab[fun_index].sig;
			nargs -= 1;

			if (nargs != argtypes.length) {
				error("D function %s was passed %s arguments by ECL, wanted %s.", name, nargs, argtypes.length);
				return Nil;
			}

			ScriptVar[] sargs; // (s)cript args
			foreach (i; 0 .. nargs) {
				cl_object c = va_arg!cl_object(vargs);
				if (c.cl_obj_type != argtypes[i]) {
					error("D function %s was passed argument %s from ECL, with type %s at position %s, but that position wanted type %s", name, c.cl_to_script, c.cl_obj_type, i, argtypes[i]);
					return Nil;
				}

				sargs ~= c.cl_to_script;
			}

			return fun(sargs).script_to_cl;
		}

		ecl_def_c_function_va(real_fn_name.lsym, &real_fun);

		this.eval(format("(defun %s (&rest args) (apply #'%s (cons %s args)))))", name, real_fn_name, our_fun_index));
	}

	bool can_be_loaded(string path) {
		// TODO: add booleans and make this use bool
		return *eval(format(`(if (compile-file "%s") 1 0)`, path)).peek!long ? true : false;
	}

	void load(string path) {
		eval(format(`(load "%s")`, path));
	}
}

void init_ecl() {
	import core.stdc.signal;
	ecl_set_option(ecl_option.trap_sigsegv, 0);
	ecl_set_option(ecl_option.trap_sigfpe, 0);
	ecl_set_option(ecl_option.trap_sigint, 0);
	ecl_set_option(ecl_option.trap_sigill, 0);

	ecl_set_option(ecl_option.trap_sigbus, 0);
	ecl_set_option(ecl_option.trap_sigpipe, 0);
	ecl_set_option(ecl_option.trap_sigchld, 0);
	ecl_set_option(ecl_option.trap_interrupt_signal, 0);

	// cl_boot wants an argc and argv.  I have no interest in giving them
	// to it, so I give it (1, [""]), which looks weird in d
	cl_boot(1, cast(char**)[cast(char*)[0].ptr].ptr);
	ecl_process_env().disable_interrupts = 1;
	signal(SIGSEGV, SIG_DFL);
	signal(SIGFPE, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	signal(SIGILL, SIG_DFL);
	info("initiated ECL");
}

void shutdown_ecl() {
	cl_shutdown();
	info("shutdown ECL");
}
