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
	switch (lua_type(l, -1)) {
		case LUA_TNONE: case LUA_TNIL:
			ret = None;
			break;

		case LUA_TNUMBER:
			double d = lua_tonumber(l, -1);
			long asint = cast(long)d;
			if (d == asint) {
				ret = ScriptVar(asint);
			} else {
				ret = ScriptVar(cast(float)d);
			}
			break;

		case LUA_TBOOLEAN:
			ret = ScriptVar(cast(bool)lua_toboolean(l, -1));
			break;

		case LUA_TSTRING:
			size_t len;
			const(char) *pstr = lua_tolstring(l, -1, &len);
			char[] str = new char[len];
			str[] = pstr[0 .. len];	// copy because the memory we get from lua
						// isn't guaranteed to last.
			ret = ScriptVar(cast(string)str);
			break;

		// cdata
		// TODO: work for non-mat4f cdata
		case 10:
			const(void) *addr = lua_topointer(l, -1);
			float[] f = new float[16];
			f[] = (cast(const(float)*)addr)[0 .. 16];
			ret = mat4f(f);
			break;

		case LUA_TUSERDATA:
			void *addr = lua_touserdata(l, -1);
			size_t len = lua_objlen(l, -1);
			size_t flen = len/float.sizeof;
			float[] f = new float[flen];
			f[] = (cast(float*)addr)[0 .. len/float.sizeof];
			if (f.length == 3) ret = vec3f(f);
			else if (f.length == 16) ret = mat4f(f);
			else fatal("Got unknown userdata with length %s", len);
			break;

		case LUA_TLIGHTUSERDATA:
			//!WARNING!XXX
			// this is actually a little bit dangerous
			// because anything could be in a c pointer
			// we provide the following assurance:

			// Please do not construct light-user-data in lua unless you are making them from ScriptVars.  Thank you!
			ret = *cast(ScriptVar*)lua_touserdata(l, -1);
			break;
		default:
			fatal("Got unknown lua value of type %s (%s)", lua_typename(l, -1).dstr, lua_type(l, -1));
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
		(vec3f v) => memcpy(lua_newuserdata(l, float.sizeof * v.v.length), v.v.ptr, float.sizeof * v.v.length),
		(mat4f m) => memcpy(lua_newuserdata(l, float.sizeof * m.v.length), m.v.ptr, float.sizeof * m.v.length),
		(FancyModel f) => lua_pushlightuserdata(l, New!ScriptVar(f)),
		(Shader s) => lua_pushlightuserdata(l, New!ScriptVar(s)),
		(Key k) => lua_pushlstring(l, k.key_to_str.ptr, k.key_to_str.length),
		(None) => lua_pushnil(l))();
}

class MoonJitScript: ScriptlangImpl {
	lua_State *l;

	this() {
		l = luaL_newstate();
		luaL_openlibs(l);
		log("Booted LuaJIT");

		void real_push_log_msg(long ll, string str, string basic_str) { _real_push_log_msg(cast(LogLevel)ll, str, basic_str); }
		super.expose_fun("_real_push_log_msg", &real_push_log_msg);

		load("dist/lua/prelude.lua");
		load("dist/lua/math.lua");

		super();
	}

	void close() {
		lua_close(l);
	}

	ScriptVar eval(string text) {
		assert(0);
	}
	void exec(string text) {
		assert(0);
	}
	ScriptVar call(string name, ScriptVar[] args = []) {
		lua_getfield(l, LUA_GLOBALSINDEX, name.cstr);
		if (lua_isnoneornil(l, -1)) {
			fatal("cant get function \"%s\"", name);
		}
		foreach (a; args) { lua_push_var(l, a); }
		checkerror(lua_pcall(l, cast(int)args.length, 1, 0));

		return lua_popvar(l);
	}

	void expose_fun(string name, ScriptFun fun, ScriptVarType[] argtypes) {
		lua_CFunction closure_caller =
		cast(lua_CFunction)(lua_State *l) {
			size_t arity = lua_tointeger(l, lua_upvalueindex(1));
			ScriptVarType[] argtypes = (cast(ScriptVarType*)lua_touserdata(l, lua_upvalueindex(2)))[0 .. arity];
			ScriptFun fun;
			fun.ptr = lua_touserdata(l, lua_upvalueindex(3));
			fun.funcptr = cast(ScriptVar function(ScriptVar[]))lua_touserdata(l, lua_upvalueindex(4));

			ScriptVar[] args;
			foreach_reverse (t; argtypes) {
				ScriptVar x = lua_popvar(l);
				// allow automatic casting of ints to floats, since lua has no ints
				if ((script_typeof(x) == ScriptVarType.Int) && (t == ScriptVarType.Real)) {
					x = ScriptVar(cast(float)*x.peek!long);
				}

				if ((script_typeof(x) != t) && (t != ScriptVarType.Any)) {
					error("D function was passed arguments '%s' of type %s, but needed type %s", x, script_typeof(x), t);
					return 0;
				}
				args = [x] ~ args;
			}

			ScriptVar ret = fun(args);

			if (ret == None) return 0;
			else {
				lua_push_var(l, ret);
				return 1;
			}
		};
		lua_pushinteger(l, argtypes.length);
		lua_pushlightuserdata(l, argtypes.ptr);
		lua_pushlightuserdata(l, fun.ptr);
		lua_pushlightuserdata(l, fun.funcptr);

		lua_pushcclosure(l, closure_caller, 4);
		lua_setfield(l, LUA_GLOBALSINDEX, name.cstr);
	}

	bool can_load(string path) {
		bool ret = luaL_loadfile(l, path.cstr) == 0;
		lua_pop(l, 1);
		return ret;
	}
	void load(string path) {
		checkerror(luaL_loadfile(l, path.cstr));
		checkerror(lua_pcall(l, 0, 0, 0));
	}

	bool has_symbol(string name) {
		lua_getfield(l, LUA_GLOBALSINDEX, name.cstr);
		bool ret = !lua_isnoneornil(l, -1);
		lua_pop(l, 1);
		return ret;
	}

	private void checkerror(int res) {
		if (res == 0) return; // cool
		string err_cat = [LUA_ERRRUN: "runtime error",
		       		  LUA_ERRMEM: "memory allocation error",
				  LUA_ERRFILE: "could not open file"].get(res, "unknown error");

		string err_msg;

		if (lua_isstring(l, -1)) err_msg = *lua_popvar(l).peek!string;
		else err_msg = "unkown error";

		fatal("Lua: %s: %s", err_cat, err_msg);
	}
}
