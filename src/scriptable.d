module scriptable;
import stdlib;

import scripting;

class ScriptManager {
	Scriptlang[string] languages;
	string current_language; //TODO

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

		current_language = ext;
		languages[ext].load(fname);
	}

	void update() {
		languages[current_language].call("update");
	}

	void graphics_update() {
		languages[current_language].call("graphics_update");
	}

	void call(T...)(string s, T aux) {
		languages[current_language].call(s, aux);
	}
}
