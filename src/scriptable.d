module scriptable;
import stdlib;

import scripting;

class ScriptManager {
	Scriptlang[string] languages;
	ScriptedFunction[] inits;
	ScriptedFunction[] updates;
	ScriptedFunction[] gfx_updates;
	ScriptedFunction[] keyhandlers;
	ScriptedFunction[] mousehandlers;

	this(Scriptlang[string] languages) {
		this.languages = languages;
	}
	~this() {
		foreach (l; languages.values) l.close();
	}
	void expose_fun(T...)(T args) {
		foreach (l; languages.values) l.expose_fun(args);
	}

	void load_script(string toml_fname) {
		import config;

		string[] fnames;
		Configure(toml_fname, Table("FancyEngine2"), "files", &fnames);

		foreach (fname; fnames) {
			string ext = fname.split('.')[$-1];
			if (ext !in languages) {
				error("Bad script '%s'", fname);
				continue;
			}

			auto funs = languages[ext].load_getsyms(fname, ["update", "graphics_update", "init", "keyhandler", "mousehandler"]);

			if (auto fn = ("update" in funs)) updates ~= *fn;
			if (auto fn = ("graphics_update" in funs)) gfx_updates ~= *fn;
			if (auto fn = ("init" in funs)) inits ~= *fn;
			if (auto fn = ("keyhandler" in funs)) keyhandlers ~= *fn;
			if (auto fn = ("mousehandler" in funs)) mousehandlers ~= *fn;
		}
	}

	//TODO: why do I need to pass [] to these functions?
	void init() {
		foreach (g; inits) g([]);
	}

	void update() {
		foreach (g; updates) g([]);
	}

	void graphics_update() {
		foreach (g; gfx_updates) g([]);
	}

	void keyhandler(ScriptVar[] args...) {
		foreach (f; keyhandlers) f(args);
	}

	void mousehandler(ScriptVar[] args...) {
		foreach (f; mousehandlers) f(args);
	}

	/+
	void call(T...)(string lang, string s, T aux) {
		languages[lang].call(s, aux);
	}
	+/
}
