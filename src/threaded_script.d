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
}

static this() {
	synchronized {
		log("Adding thread %s", thisTid);
		ThreadedScripter.add_this_thread();
		log("Added thread %s", thisTid);
	}
}

private string current_scene_name = null;
                                                                                                                                       
string get_current_scene_name() { return current_scene_name; }

struct ScriptedFunction {
	package size_t serial;
	string owned_scene_name;

	package this(size_t serial) { this.serial = serial; }

	ScriptVar opCall(ScriptVar[] args = []) {
		if (!owned_scene_name) fatal("ScriptedFunctionBase does not belong to any scene");

		ScriptVar ret = None;

		synchronized {
			string old_scene_name = current_scene_name;
			current_scene_name = owned_scene_name;

			/+
				if (thisTid !in ThreadedScripter.thread_scripted_functions) {
					ThreadedScripter.add_this_thread();
					return this.opCall(args);
				}
			+/

				synchronized(ThreadedScripter.this_thread_lock)
				ret = ThreadedScripter.get_scripted_functions[serial](args);

			current_scene_name = old_scene_name;
		}

                return ret;
	}
}

private struct ThreadLocalBaggage {
	Scriptlang[string] languages;
	Mutex mutex;
	ScriptedFunctionBase[] scripted_functions;
}

static final __gshared class ThreadedScripter {
	static __gshared ThreadLocalBaggage[Tid] baggage;

	static __gshared private ScriptedFunctionWrapper[] loaded_scripted_funs;
	static __gshared private ScriptFunWrap[] exposed_funs;

	static Scriptlang[string] get_languages() {
		if (thisTid !in baggage)
			fatal("%s not in threads?", thisTid);
		return baggage[thisTid].languages; }

	static package ScriptedFunctionBase[] get_scripted_functions() {
		return baggage[thisTid].scripted_functions;
	}

	static package Mutex this_thread_lock() { return baggage[thisTid].mutex; }

	static ScriptedFunction[string] load_getsyms(string ext, string path, string[] wanted_syms) {
		ScriptedFunction[string] ret;

		synchronized loaded_scripted_funs ~= ScriptedFunctionWrapper(ext, path, wanted_syms);

		foreach (ref b; baggage.values) {
			synchronized (b.mutex) {
				ScriptedFunctionBase[string] base = b.languages[ext].load_getsyms(path, wanted_syms);
				foreach (key; base.keys.sorted) {
					b.scripted_functions ~= base[key];

					// TODO: enforce well-ordering?
					ret[key] = ScriptedFunction(b.scripted_functions.length-1);
				}
			}
		}

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

	static package void add_this_thread() {
		if (thisTid in baggage) fatal("Attempted to add current thread to threaded script, but was already present?");
		Scriptlang[string] langs = get_new_languagepack();

		foreach (v; langs.values) {
			foreach (f; exposed_funs) {
				v.expose_fun(f.name, f.fun, f.sig, f.variadic);
			}
		}

		ScriptedFunctionBase[] scripted_functions;
		foreach (lsf; loaded_scripted_funs) {
			auto o = langs[lsf.ext].load_getsyms(lsf.path, lsf.wanted_syms);
			foreach (v; o.keys.sorted) {
				scripted_functions ~= o[v];
			}
		}

		synchronized {
			baggage[thisTid] = ThreadLocalBaggage(langs, new Mutex(), scripted_functions);
		}
	}
}
