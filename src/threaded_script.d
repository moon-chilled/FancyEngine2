module threaded_script;
import stdlib;
import scripting;
import scripting.s7;
import scripting.moonjit;
import std.concurrency;

private Scriptlang[string] get_new_languagepack() {
	return ["scm": new S7Script(),
	        "lua": new MoonJitScript()];
}

private struct ScriptFunWrap {
	string name;
	ScriptFun fun;
	ScriptVarType[] sig;
	bool variadic;
}
private struct ScriptedFunctionWrapper {
	string ext;
	string path;
	string[] wanted_syms;
	uint[] uuids; // length of last two is identical
}

private string current_scene_name = null;
                                                                                                                                       
string get_current_scene_name() { return current_scene_name; }

struct ScriptedFunction {
	package uint serial;
	string owned_scene_name;

	package this(uint serial) { this.serial = serial; }

	ScriptVar opCall(ScriptVar[] args = []) {
		if (!owned_scene_name) fatal("ScriptedFunctionBase does not belong to any scene");

		ScriptVar ret = None;

		string old_scene_name = current_scene_name;
		current_scene_name = owned_scene_name;


		synchronized(ThreadedScripter.this_thread_lock)
			ret = ThreadedScripter.get_scripted_functions[serial](args);
		

		current_scene_name = old_scene_name;

                return ret;
	}
}

private struct ThreadLocalBaggage {
	Scriptlang[string] languages;
	Mutex mutex;
	ScriptedFunctionBase[uint] scripted_functions;
}

private uint get_uuid() {
	static uint r = 0;
	// start at 1; 0 can be a sentinel
	return ++r;
}

static final __gshared class ThreadedScripter {
	static __gshared ThreadLocalBaggage[Tid] baggage;
	static __gshared Mutex baggage_mutex;

	static __gshared private ScriptedFunctionWrapper[] loaded_scripted_funs;
	static __gshared private ScriptFunWrap[] exposed_funs;

	static Scriptlang[string] get_languages() {
		if (thisTid !in baggage)
			fatal("%s not in threads?", thisTid);
		return baggage[thisTid].languages; }

	static package ScriptedFunctionBase[uint] get_scripted_functions() {
		return baggage[thisTid].scripted_functions;
	}

	static package Mutex this_thread_lock() { return baggage[thisTid].mutex; }

	static ScriptedFunction[string] load_getsyms(string ext, string path, string[] wanted_syms) {
		ScriptedFunction[string] ret;

		auto sfw = ScriptedFunctionWrapper(ext, path);
		uint[] uuids;
		string[] received_syms;
		uint[] received_uuids;

		foreach (key; wanted_syms) uuids ~= get_uuid();
		synchronized (baggage_mutex) foreach (ref k; baggage.keys) {
			synchronized (baggage[k].mutex) {
				ScriptedFunctionBase[string] base = baggage[k].languages[ext].load_getsyms(path, wanted_syms);
				foreach (i; 0 .. wanted_syms.length) {
					if (wanted_syms[i] !in base) continue;
					baggage[k].scripted_functions[uuids[i]] = base[wanted_syms[i]];

					// TODO better scheme than a hashtable lookup for _every call_
					ret[wanted_syms[i]] = ScriptedFunction(uuids[i]);
					if (i==0) {
						received_syms ~= wanted_syms[i];
						received_uuids ~= uuids[i];
					}
				}
			}
		}

		assert(received_syms.length == received_uuids.length);
		sfw.wanted_syms = received_syms;
		sfw.uuids = received_uuids;
		synchronized loaded_scripted_funs ~= sfw;

		return ret;
	}

	static void expose_vfun(string name, ScriptFun fun) {
		exposed_funs ~= ScriptFunWrap(name, fun, [], true);

		foreach (ref b; baggage.values) {
			foreach (ref l; b.languages.values) {
				synchronized(b.mutex) l.expose_fun(name, fun, cast(ScriptVarType[])[], true);
			}
		}
	}

	static void expose_fun(R, A...)(string name, R function(A) fun) {
		expose_fun(name, toDelegate(fun));
	}

	static void expose_fun(R, A...)(string name, R delegate(A) fun) {
		ScriptVarType[] signature;
		ScriptFun f = normalise_scriptfun(fun, signature);

		exposed_funs ~= ScriptFunWrap(name, f, signature, false);

		foreach (ref b; baggage.values) {
			foreach (l; b.languages.values) {
				synchronized(b.mutex) l.expose_fun(name, f, signature);
			}
		}
	}

	static void add_this_thread() {
		if (thisTid in baggage) fatal("Attempted to add current thread to threaded script, but was already present?");
		Scriptlang[string] langs = get_new_languagepack();

		foreach (v; langs.values) {
			foreach (f; exposed_funs) {
				v.expose_fun(f.name, f.fun, f.sig, f.variadic);
			}
		}

		ScriptedFunctionBase[uint] scripted_functions;
		foreach (lsf; loaded_scripted_funs) {
			auto o = langs[lsf.ext].load_getsyms(lsf.path, lsf.wanted_syms);
			foreach (i; 0 .. lsf.wanted_syms.length) {
				scripted_functions[lsf.uuids[i]] = o[lsf.wanted_syms[i]];
			}
		}

		//foreach (t; baggage.values) logs(t.scripted_functions);

		synchronized (baggage_mutex) {
			baggage[thisTid] = ThreadLocalBaggage(langs, new Mutex(), scripted_functions);
		}
		assert(thisTid in baggage);
	}

	shared static this() {
		baggage_mutex = new Mutex();
	}
}
