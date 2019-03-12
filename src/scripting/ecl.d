module scripting.ecl;
import stdlib;

import scripting;
import scripting.ecl_lib_interface;

import std.string: format;


string[] _dont_gc_names;
ScriptFun[] _dont_gc_funs;
ScriptVarType[][] _dont_gc_argtypes;

ScriptVarType cl_obj_type(cl_object lisp_obj) {
	switch (lisp_obj.ecl_t_of) {
		case cl_type.t_fixnum: return ScriptVarType.Int;
		case cl_type.t_singlefloat, cl_type.t_doublefloat: return ScriptVarType.Real;
		//case cl_type.t_string: return ScriptVarType.Str;
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
		default:
			return ScriptVar(None());
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
	this(){}
	~this(){}

	ScriptVar eval(string text) {
		return text.lstr.cl_eval.cl_to_script;
	}

	// A bit of explanation is due here.
	// there needs to be a unified script interface, so I can pass the same functions into lisp, c, perl6, whatever
	// so script functions can't use cl_object, instead they use ScriptVar and ScriptVarType etc.
	// as such, every script lang needs to wrap around ScriptFun with a language-specific function
	// but that function needs to be a closure (aka delegate) so it knows the address of the d function to call
	// but we can't pass delegates into c functions
	// so instead we name the real function _ecl_d_fancy_fn_<funname>
	// and declare a *lisp* function that stores pointers to the 'closed over' variables as integers and passes those into the real wrapper function
	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes) {
		_dont_gc_names ~= name;
		_dont_gc_funs ~= fun;
		_dont_gc_argtypes ~= argtypes;

		string real_fn_name = "_ecl_d_fancy_fn_" ~ name;
		extern (C) cl_object real_fun(fixnum nargs, ...) {
			import core.vararg;
			assert (nargs >= 3);
			va_list vargs; //(v)arargs
			va_start(vargs, nargs);

			string name = *(cast(string*)(*va_arg!cl_object(vargs).cl_to_script.peek!long));
			ScriptFun fun = *(cast(ScriptFun*)(*va_arg!cl_object(vargs).cl_to_script.peek!long));
			ScriptVarType[] argtypes = *(cast(ScriptVarType[]*)(*va_arg!cl_object(vargs).cl_to_script.peek!long));
			nargs -= 3;

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

		ecl_def_c_function_va(real_fn_name.lstr, &real_fun);

		this.eval(format("(defun %s (&rest args) (apply #'%s (cons %s (cons %s (cons %s args)))))", name, real_fn_name, cast(fixnum)(&name), cast(fixnum)(&fun), cast(fixnum)(&argtypes)));
	}
}

void init_ecl() {
	cl_boot(1, cast(char**)[cast(char*)[0].ptr].ptr);
	info("initiated ECL");
}
void shutdown_ecl() {
	cl_shutdown();
	info("shutdown ECL");
}
