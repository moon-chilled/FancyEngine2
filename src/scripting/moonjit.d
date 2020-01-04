module scripting.moonjit;
import stdlib;
import stdmath;
import cstdlib;

import scripting;
import bindbc.lua;

import graphics.fancy_model;
import graphics.shading;

import windowing.key;


private ScriptVar lua_popvar(lua_State *l) {
	ScriptVar ret;
	if (lua_isnoneornil(l, -1)) {
		ret = None;
	} else if (lua_isboolean(l, -1)) {
		ret = ScriptVar(cast(bool)lua_toboolean(l, -1));
	} else if (lua_isstring(l, -1)) {
		size_t len;
		const(char) *pstr = lua_tolstring(l, -1, &len);
		char[] str = new char[len];
		str[] = pstr[0 .. len];	// copy because the memory we get from lua
					// isn't guaranteed to last.
		ret = ScriptVar(cast(string)str);
	} else if (lua_isnumber(l, -1)) {
		double d = lua_tonumber(l, -1);
		long asint = cast(long)d;
		if (d == asint) {
			ret = ScriptVar(asint);
		} else {
			ret = ScriptVar(cast(float)d);
		}
	// TODO: vectors/lists
	} else if (lua_islightuserdata(l, -1)) {
		//!WARNING!XXX
		// this is actually a little bit dangerous
		// because anything could be in a c pointer
		// we provide the following assurance:

		// Please do not construct c-pointers in lua unless you are making them from ScriptVars.  Thank you!
		return *cast(ScriptVar*)lua_touserdata(l, -1);
	} else {
		fatal("Got unknown lua value");
		assert(0);
	}
	lua_pop(l, 1);
	return ret;
}

private void lua_push_var(lua_State *l, ScriptVar var) {
	import std.variant: visit;
	var.visit!(
		(long i) => lua_pushinteger(l, i),
		(float d) => lua_pushnumber(l, d),
		(string s) => lua_pushlstring(l, s.ptr, s.length),
		(bool b) => lua_pushboolean(l, b),
		(vec3f v) { /*s7_pointer ret = s7_make_float_vector(s7, 3, 0, null); copy_to_s7_vec(s7_float_vector_elements(ret), v.v);*/ },
		(mat4f m) { /*s7_pointer ret = s7_make_float_vector(s7, 16, 0, null); copy_to_s7_vec(s7_float_vector_elements(ret), m.v);*/ },
		(FancyModel f) { /* s7_make_c_pointer(s7, New!ScriptVar(f))*/ },
		(Shader s) { /*s7_make_c_pointer(s7, New!ScriptVar(s))*/ },
		(Key k) => lua_pushlstring(l, k.key_to_str.ptr, k.key_to_str.length),
		(None) => lua_pushnil(l))();
}

class MoonJitScript: ScriptlangImpl {
	lua_State *l;

	this() {
		l = luaL_newstate();
		luaL_openlibs(l);
		log("Booted LuaJIT");

		/+
		void real_push_log_msg(long ll, string str, string basic_str) { _real_push_log_msg(cast(LogLevel)ll, str, basic_str); }
		super.expose_fun("_real_push_log_msg", &real_push_log_msg);
		+/

		/+
		s7_add_to_load_path(s7, "dist/scheme".cstr);
		load("prelude.scm");
		+/

		super();
	}

	void close() {
		lua_close(l);
	}

	ScriptVar eval(string text) {
		//return s7_to_script(s7, s7_eval_c_string(s7, text.cstr));
		return ScriptVar(0L);
	}
	void exec(string text) {
		//s7_eval_c_string(s7, text.cstr);
	}
	ScriptVar call(string name, ScriptVar[] args = []) {
		lua_getglobal(l, name.cstr);
		if (lua_isnoneornil(l, -1)) {
			fatal("cant get function \"%s\"", name);
		}
		foreach (a; args) { lua_push_var(l, a); }
		lua_call(l, cast(int)args.length, 1);
		//lua_pushinteger(l, 1);
		//lua_pushinteger(l, 7);
		//lua_call(l, 2, 1);

		return lua_popvar(l);
	}

	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes) {
		lua_CFunction closure_caller =
		cast(lua_CFunction)(lua_State *l) {
			size_t arity = lua_tointeger(l, -1);
			ScriptVarType[] args = (cast(ScriptVarType*)lua_touserdata(l, -2))[0 .. arity];
			lua_pop(l, 2);
			ScriptFun fun;
			fun.ptr = lua_touserdata(l, -1);
			fun.funcptr = cast(ScriptVar function(ScriptVar[]) nothrow)lua_touserdata(l, -2);
			lua_pop(l, 2);

			log("got function with arity %s", args);
			return 0;
		};
		lua_pushlightuserdata(l, argtypes.ptr);
		lua_pushinteger(l, argtypes.length);
		lua_pushlightuserdata(l, fun.funcptr); // delagate = size_t[2]
		lua_pushlightuserdata(l, fun.ptr);

		lua_pushcclosure(l, closure_caller, 4);
		lua_setfield(l, LUA_GLOBALSINDEX, name.cstr);
		/+
		+/
	}

	bool can_load(string path) {
		/+
		auto s7_obj = s7_eval_c_string_with_environment(s7, (`
(catch 'read-error
 (lambda () (load "` ~ path ~ `") #t)
 (lambda args
  #f))`).cstr, s7_inlet(s7, s7_nil(s7)));
		return *s7_to_script(s7, s7_obj).peek!bool;
		+/
		return false;
	}
	void load(string path) {
		luaL_loadfile(l, path.cstr);
		int res = lua_pcall(l, 0, 0, 0);
		if (res == 0) return; // cool

		string err_cat = [LUA_ERRRUN: "runtime error",
		       		  LUA_ERRMEM: "memory allocation error"].get(res, "unknown error");

		string err_msg;

		if (lua_isstring(l, -1)) err_msg = *lua_popvar(l).peek!string;
		else err_msg = "unkown error";

		fatal("Lua: %s: %s", err_cat, err_msg);
	}

	bool has_symbol(string name) {
		//return s7_symbol_table_find_name(s7, name.cstr) ? true : false;
		return false;
	}
}
