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

		foreach (string s; scene_s) {
                        auto scn = new Scene(languages, s);
                        saved_scenes[s] = scn;
                }

		foreach (string s; play_s) {
			play(s);
		}
		foreach (string s; pause_s) {
			pause(s);
		}
	}

	Scene *get_scene(string name) {
		if (auto scn = name in playing_scenes) return scn;
		if (auto scn = name in paused_scenes) return scn;
		if (auto scn = name in saved_scenes) return scn;

		log("scenes are %s, %s, %s, couldn't find %s", playing_scenes, paused_scenes, saved_scenes, name);

		return null;
	}

	void pause(string scene_name) {
		Scene s;
		Scene[string] *src;
		if (auto scn = scene_name in playing_scenes) {
			s = *scn;
			src = &playing_scenes;
		} else if (auto scn = scene_name in saved_scenes) {
			s = *scn;
			src = &saved_scenes;
		} else if (auto scn = scene_name in paused_scenes) {
			return;
		} else {
			error("Tried to pause nonexistent scene '%s'", scene_name);
			return;
		}

		if (!s.loaded && !s.loading) {
			s.loading = true;
			s.preload();
			s.loading = false;
			s.loaded = true;
		}

		(*src).remove(scene_name);
		paused_scenes[scene_name] = s;
	}

	void play(string scene_name) {
		Scene s;
		Scene[string] *src;
		if (auto scn = scene_name in paused_scenes) {
			s = *scn;
			src = &paused_scenes;
		} else if (auto scn = scene_name in saved_scenes) {
			src = &saved_scenes;
			s = *scn;
		} else if (auto scn = scene_name in playing_scenes) {
			return;
		} else {
			error("Tried to play nonexistent scene '%s'", scene_name);
			return;
		}

		if (!s.loaded && !s.loading) {
			s.loading = true;
			s.preload();
			s.loading = false;
			s.loaded = true;
		}

		(*src).remove(scene_name);
		playing_scenes[scene_name] = s;

		if (s.fresh) {
			s.enter();
			s.fresh = false;
		}
	}

	void expose_fun(T...)(T args) {
		foreach (l; languages.values) l.expose_fun(args);
	}

	void expose_vfun(T...)(T args) {
		foreach (l; languages.values) l.expose_vfun(args);
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
	// fresh => newly loaded from saving, need to enter()
	bool fresh = true;

	// loaded => preload function has been run,
	// loading => preload or unload function is currently running
	bool loaded = false, loading = false;


	string name;
	ScriptVar[string] env;
	ScriptedFunction[] entrances;
	ScriptedFunction[] preloads;
	ScriptedFunction[] unloads;
	ScriptedFunction[] updates;
	ScriptedFunction[] gfx_updates;
	ScriptedFunction[] keyhandlers;
	ScriptedFunction[] mousehandlers;

	this(Scriptlang[string] languages, string scene_name) {
		name = scene_name;
		import config;

		string[] fnames;
		Configure(name ~ ".scene/scene.toml", Table("Scene"), "files", &fnames);

		foreach (fname; fnames) {
			string ext = fname.split('.')[$-1];
			if (ext !in languages) {
				error("Bad script '%s'", fname);
				continue;
			}

			auto fd=["update":		&updates,
				 "preload":		&preloads,
				 "unload":		&unloads,
				 "graphics_update":	&gfx_updates,
				 "enter":		&entrances,
				 "keyhandler":		&keyhandlers,
				 "mousehandler":	&mousehandlers];

			auto funs = languages[ext].load_getsyms(name ~ ".scene/" ~ fname, fd.keys);

			foreach (k, v; fd) {
				if (auto fn = (k in funs)) {
					*v ~= *fn;
					(*v)[$-1].owned_scene_name = name;
				}
			}
		}
	}

	//TODO: why do I need to pass [] to these functions?
	void enter() {
		foreach (g; entrances) g([]);
	}

	void preload() {
		foreach (g; preloads) g([]);
	}

	void unload() {
		foreach (g; unloads) g([]);
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
