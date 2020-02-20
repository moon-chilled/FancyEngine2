module scriptable;
import stdlib;

import scripting;

class ScriptManager {
	Scriptlang[string] languages;
	ScriptedFunction[] updates;
	ScriptedFunction[] gfx_updates;

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

		string fname;
		Configure(toml_fname, Table("FancyEngine2"), "file", &fname);
		string ext = fname.split('.')[$-1];
		if (ext !in languages) { error("Bad script '%s'", fname); return; }

		auto funs = languages[ext].load_getsyms(fname, ["update", "graphics_update"]);
		if (funs.length == 2) {
			updates ~= funs[0];
			gfx_updates ~= funs[1];
		} else {
			error("Unable to load update functions from '%s'", fname);
		}
	}

	void update() {
		foreach (g; updates) g();
	}

	void graphics_update() {
		foreach (g; gfx_updates) g();
	}

	void call(T...)(string lang, string s, T aux) {
		languages[lang].call(s, aux);
	}
}
