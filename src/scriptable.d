module scriptable;
import stdlib;

import preload;
import scripting;

import threaded_script;

class SceneManager {
	Preloader[] preloaders;

	// hibernating_scenes
	Scene[string] playing_scenes, paused_scenes, saved_scenes;

	this(uint num_workers = 1) {
		foreach (_; 0 .. num_workers) {
			import std.concurrency;
			preloaders ~= new Preloader();
			preloaders[$-1].tid = spawn(&preloader, thisTid, cast(shared)preloaders[$-1]);
		}

	}

	void close() {
		import std.concurrency;

		foreach (p; preloaders) {
			send(p.tid, ReqPreloadExit());
		}
		foreach (p; preloaders) {
			receiveOnly!AckPreloadExit();
		}

		foreach (s; playing_scenes) if (s.loaded) { s.unload(); s.loaded = false; }
		foreach (s; paused_scenes) if (s.loaded) { s.unload(); s.loaded = false; }
		//foreach (s; saved_scenes) if (s.loaded) s.unload();
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
                        auto scn = new Scene(s);
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

	private Preloader get_free_preloader() {
		uint minw = uint.max;
		size_t idx;

		foreach (i; 0 .. preloaders.length) {
			if (!preloaders[i].working) return preloaders[i];

			if (preloaders[i].working < minw) {
				minw = preloaders[i].working;
				idx = i;
			}
		}

		return preloaders[idx];
	}

	private void preload(Scene s) {
		log("asking to preload");
		import std.concurrency;
		import core.atomic;
		Preloader p = get_free_preloader();
		//atomicOp!"+="(p.working, 1);
		p.working++;
		send(p.tid, cast(shared)PreloadScene(s));
		receiveOnly!AckPreloadScene();
	}
	private void unload(Scene s) {
		import std.concurrency;
		import core.atomic;
		Preloader p = get_free_preloader();
		//atomicOp!"+="(p.working, 1);
		p.working++;
		send(p.tid, cast(shared)UnloadScene(s));
		receiveOnly!AckUnloadScene();
	}
	private void enter(Scene s) {
		if (s.loading) {
			s.defer_enter = true;
		} else {
			s.enter();
		}
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
			preload(s);
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
			preload(s);
		}

		(*src).remove(scene_name);
		playing_scenes[scene_name] = s;

		if (s.fresh) {
			enter(s);
			s.fresh = false;
		}
	}

	void expose_fun(T...)(T args) {
		ThreadedScripter.expose_fun(args);
	}

	void expose_vfun(T...)(T args) {
		ThreadedScripter.expose_vfun(args);
	}

	void update() {
		foreach (g; playing_scenes.values) {
			if (!g.loading) {
				if (g.defer_enter) {
					g.defer_enter = false;
					g.enter();
				}
				g.update();
			}
		}
	}

	void graphics_update() {
		foreach (g; playing_scenes.values) {
			if (!g.loading) {
				if (g.defer_enter) {
					g.defer_enter = false;
					g.enter();
				}
				g.graphics_update();
			}
		}
	}

	void keyhandler(ScriptVar[] args...) {
		foreach (g; playing_scenes.values) {
			if (!g.loading) {
				if (g.defer_enter) {
					g.defer_enter = false;
					g.enter();
				}
				g.keyhandler(args);
			}
		}
	}

	void mousehandler(ScriptVar[] args...) {
		foreach (g; playing_scenes.values) {
			if (!g.loading) {
				if (g.defer_enter) {
					g.defer_enter = false;
					g.enter();
				}
				g.mousehandler(args);
			}
		}
	}
}

class Scene {
	// fresh => newly loaded from saving, need to enter()
	bool fresh = true;

	// loaded => preload function has been run,
	// loading => preload or unload function is currently running
	bool loaded = false, loading = false;

	// was asked to enter, but still loading; call enter once loading
	// is done
	bool defer_enter;


	string name;
	ScriptVar[string] env;
	ScriptedFunction[] entrances;
	ScriptedFunction[] preloads;
	ScriptedFunction[] unloads;
	ScriptedFunction[] updates;
	ScriptedFunction[] gfx_updates;
	ScriptedFunction[] keyhandlers;
	ScriptedFunction[] mousehandlers;

	this(string scene_name) {
		name = scene_name;
		import config;

		string[] fnames;
		Configure(name ~ ".scene/scene.toml", Table("Scene"), "files", &fnames);

		foreach (fname; fnames) {
			string ext = fname.split('.')[$-1];
			if (ext !in ThreadedScripter.get_languages) {
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

			auto funs = ThreadedScripter.load_getsyms(ext, name ~ ".scene/" ~ fname, fd.keys);

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
		log("in preload woo");
		foreach (g; preloads) {
			log("actually about to preload x");
			g([]);
			log("actually did preload x");
		}
		log("out preload woo");
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
