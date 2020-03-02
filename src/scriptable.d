module scriptable;
import stdlib;

import scripting;
import scripting.s7;
import scripting.moonjit;

class SceneManager {
	Scriptlang[string] languages;

	// hibernating_scenes
	Scene[string] playing_scenes, paused_scenes, saved_scenes;

	this() {
		languages = ["scm": new S7Script(),
			     "lua": new MoonJitScript()];
	}

	void load() {
		import config;
		string[] scene_s, play_s, pause_s;
                Configure("game.toml",
				Table("FancyEngine2"),
				"scenes", &scene_s,
				"playing-scenes", &play_s,
				"paused-scenes", &pause_s);

		foreach (s; scene_s) {
                        auto scn = new Scene(languages, s);
                        saved_scenes[s] = scn;
                }

		foreach (s; pause_s) {
			pause(s);
		}
		foreach (s; play_s) {
			play(s);
		}
	}

	void pause(string scene_name) {
		Scene s;
		if (auto scn = scene_name in playing_scenes) {
			playing_scenes.remove(scene_name);
			s = *scn;
		} else if (auto scn = scene_name in saved_scenes) {
			saved_scenes.remove(scene_name);
			s = *scn;
		//TODO: ditto for hibernating
		} else {
			error("No such non-paused scene '%s'", scene_name);
			return;
		}

		paused_scenes[scene_name] = s;
	}

	void play(string scene_name) {
		Scene s;
		if (auto scn = scene_name in playing_scenes) {
			playing_scenes.remove(scene_name);
			s = *scn;
		} else if (auto scn = scene_name in saved_scenes) {
			saved_scenes.remove(scene_name);
			s = *scn;
		//TODO: ditto for hibernating
		} else {
			error("No such non-paused scene '%s'", scene_name);
			return;
		}

		playing_scenes[scene_name] = s;
		if (s.fresh) {
			s.fresh = false;
			s.init();
		}
	}

	void expose_fun(T...)(T args) {
		foreach (l; languages.values) l.expose_fun(args);
	}

	void update() {
		foreach (g; playing_scenes.values) g.update();
	}

	void graphics_update() {
		foreach (g; playing_scenes.values) g.graphics_update();
	}

	void keyhandler(ScriptVar[] args...) {
		foreach (g; playing_scenes.values) g.keyhandler(args);
	}

	void mousehandler(ScriptVar[] args...) {
		foreach (f; playing_scenes.values) f.mousehandler(args);
	}
}


class Scene {
	// fresh => newly loaded from saving, need to init()
	package bool fresh = true;

	ScriptedFunction[] inits;
	ScriptedFunction[] updates;
	ScriptedFunction[] gfx_updates;
	ScriptedFunction[] keyhandlers;
	ScriptedFunction[] mousehandlers;

	this(Scriptlang[string] languages, string scene_name) {
		import config;

		string[] fnames;
		Configure(scene_name ~ ".scene/scene.toml", Table("Scene"), "files", &fnames);

		foreach (fname; fnames) {
			string ext = fname.split('.')[$-1];
			if (ext !in languages) {
				error("Bad script '%s'", fname);
				continue;
			}

			auto funs = languages[ext].load_getsyms(scene_name ~ ".scene/" ~ fname, ["update", "graphics_update", "init", "keyhandler", "mousehandler"]);

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
}
