import stdlib;
import cstdlib;
import stdmath;

import windowing.windows;
import windowing.key;

import graphics.fancy_model;
import graphics.model;
import graphics.shading;
import graphics.tex;

import asset;

import scripting;
import scripting.s7;

import sound.gorilla;

import config;

enum SPEED = 0.1;

bool done;

void dispatch(Event[] evs, GraphicsState gfx, Scriptlang script) {
	bool have_keyhandler = script.has_symbol("keyhandler");

	foreach (ev; evs) {
		final switch (ev.type) {
			case Evtype.Keydown:
				script.call("keyhandler", [ScriptVar(ev.key.key_to_str), ScriptVar(true)]);
				break;
			case Evtype.Keyup:
				script.call("keyhandler", [ScriptVar(ev.key.key_to_str), ScriptVar(false)]);
				break;
			case Evtype.Mousemove:
			/+
				float sense = 0.3;
				state.pitch = clamp(state.pitch - ev.mouse.deltay * sense, -89, 89);
				state.yaw += sense * ev.mouse.deltax;
				state.cam_front = vec3f(cos(state.pitch.to_rad) * cos(state.yaw.to_rad),
						sin(state.pitch.to_rad),
						cos(state.pitch.to_rad) * sin(state.yaw.to_rad));
				state.cam_front.normalize();


				break;
			+/
			case Evtype.Keypress: break;
			case Evtype.Quit:
				done = true;
		}
	}
}

int real_main(string[] args) {
	load_all_libraries();
	Scriptlang faux = new S7Script();
	scope (exit) faux.close();

	static if (gfx_backend == GfxBackend.OpenGL) {
		string title = "FE2—OpenGL";
	} else static if (gfx_backend == GfxBackend.Vulkan) {
		string title = "FE2—Vulkan";
	}
	WindowSpec ws;
	string fs;
	uint fov;
	float physics_fps;
	float master_vol, music_vol, effect_vol;

	with (ws) Configure("prefs.toml",
		Table("Graphics"),
			"width", &win_width,
			"height", &win_height,
			"fullscreen", &fs,
			"borders", &borders,
			"vsync", &vsync,
			"wireframe", &wireframe,
			"msaa", &aa_samples,
			"fov", &fov,
			"physics_fps", &physics_fps,
			"monitor_index", &monitor_index,
		Table("Sound"),
			"master_volume", &master_vol,
			"music_volume", &music_vol,
			"effect_volume", &effect_vol);

	// volume in the config file has a range from 0..100, but internally we
	// have a range of 0..1
	master_vol /= 100;
	music_vol /= 100;
	effect_vol /= 100;


	switch (fs) {
		case "fullscreen": ws.fullscreen = Fullscreenstate.Fullscreen; break;
		case "desktop": ws.fullscreen = Fullscreenstate.Desktop; break;
		case "none": ws.fullscreen = Fullscreenstate.None; break;
		default: error("config error: unable to load option 'fullscreen': invalid value '%s'.  Defaulting to windowed mode.", fs); ws.fullscreen = Fullscreenstate.None; break;
	}
	ws.render_width = ws.win_width;
	ws.render_height = ws.win_height;
	ws.title = title;


	scope GraphicsState gfx = new GraphicsState(ws);
	scope GorillaAudio audio = new GorillaAudio();
	auto sound = audio.load_cache_sound("out.ogg");
	audio.set_volume(sound, master_vol * music_vol);
	audio.play(sound);

	gfx.grab_mouse();


	// TODO: move these somewhere better.
	/* TODO: perl6 and lisp use kebab-case, but most others use snek_case.
	 *       Make a system where some symbol (maybe '$') is replaced by a separator
	 *       (or should I not do that, make snek definitions here, and have each language's stdlib redefine the names?
	 */
	faux.expose_fun("blit", (Shader s, FancyModel m) => s.blit(m));
	faux.expose_fun("make_fancy_model", (string s) => FancyModel(s));
	faux.expose_fun("make_shader", (string path) => Shader(fslurp(path ~ ".vert"), fslurp(path ~ ".frag"), gfx.gfx_context));
	faux.expose_fun("shader_set_mat4f", (Shader s, string name, mat4f mat) => s.set_mat4(name, mat));
	faux.expose_fun("grab_mouse", &gfx.grab_mouse);
	faux.expose_fun("ungrab_mouse", &gfx.ungrab_mouse);

	faux.load("stdlib.scm");


	// START SECTION TO BE REPLACE BY SCRIPTS {{{

	ulong frames;

// END }}}


	faux.load("game.scm");
	faux.call("init");

	import std.datetime.stopwatch: StopWatch, AutoStart;

	float physics_frame = 1 / physics_fps;
	auto sw = StopWatch(AutoStart.yes);
	float time_so_far = 0;
	float avg_frame_time = physics_frame;
mainloop:
	while (!done) {
		global_pause_mutex.lock();

		bool something_worth_framing;

		float frame_time = sw.peek.total!"nsecs" / 1_000_000_000.0;
		sw.reset;
		time_so_far += frame_time;
		avg_frame_time = avg_frame_time*0.9 + 0.1*frame_time;

		if (time_so_far >= physics_frame) {
			time_so_far -= physics_frame;
			something_worth_framing = true;
		}
		if (something_worth_framing) gfx.set_title(strfmt("%s %.f FPS", title, 1/avg_frame_time));

		///////////////////////////////////
		////EVENT HANDLING ////////////////
		///               /////////////////
		//               /
		poll_events().dispatch(gfx, faux);


		//ALSO ALL THIS SHIT THROUGH SOUND

		///////////////////////////////////
		////  PHYSICS      ////////////////
		///               /////////////////
		//               /
		if (something_worth_framing) {
			faux.call("update");
		}


		///////////////////////////////////
		////  RENDERING    ////////////////
		///               /////////////////
		//               /
		clear(gfx, 0, 0, 0);

		faux.call("graphics-update");
		gfx.blit();


		//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//
		// TODO: don't need to re-set volume every loop.  Put this into
		// sound manager, make it set a flag so that next time it's
		// updated, the volume is changed.
		audio.set_volume(sound, master_vol * music_vol);
		audio.update(frame_time);

		global_pause_mutex.unlock();
	}

	return 0;
}


void load_all_libraries() {
	import derelict.sdl2.sdl: DerelictSDL2;
	import derelict.assimp3.assimp: DerelictASSIMP3;

	set_lib_path();

	static if (gfx_backend == GfxBackend.Vulkan) {
		import erupted.vulkan_lib_loader;

		// TODO: figure out a way to get at the error messages.
		// Currently, they're written to a file pointer you pass to
		// that function, which is really inconvenient.  I tried making
		// a pipe, but that didn't seem to work, and I don't want to
		// use an actual file, because finding a place to put it would
		// be a complete pain in the arse
		if (!loadGlobalLevelFunctions()) {
			fatal("Unknown error loading Vulkan (do you have it installed?)");
		}
		//TODO: figure out a place to do this
		//freeVulkanLib();
	} else static if (gfx_backend == GfxBackend.OpenGL) {
		import derelict.opengl: DerelictGL3;
		try {
			DerelictGL3.load();
		} catch(Throwable t) {
			fatal("Error loading OpenGL (mark I).  '%s'", t.msg);
		}
	}

	try {
		DerelictSDL2.load();
	} catch(Throwable t) {
		fatal("Error loading SDL2.  '%s'", t.msg);
	}

	try {
		DerelictASSIMP3.load();
	} catch (Throwable t) {
		fatal("Error loading ASSIMP 3.  '%s'", t.msg);
	}

	synchronized are_libraries_loaded = true;
}

// really, it returns errno_t, but that's the same as int
version (Windows) private extern (C) int _putenv_s(const char*, const char*);
// setup LD_LIBRARY_PATH (or equivalent) so derelict (or something else) can find libraries
void set_lib_path() {
	version (Windows) {
		void set_env(const char *key, const char *value) {
				_putenv_s(key, value);
		}
	} else {
		void set_env(const char *key, const char *value) {
			import core.sys.posix.stdlib: setenv;
			setenv(key, value, 1); // 1: overwrite value if it already exists
		}
	}

	const(char) *plat_libpath_name;
	const(char) *plat_lib_path;
	version (Windows) {
		plat_libpath_name = "PATH".cstr;
		plat_lib_path = r"lib\win\".cstr;
	} else version (OSX) {
		plat_libpath_name = "DYLD_LIBRARY_PATH".cstr;
		plat_lib_path = "lib/macos".cstr;
	} else version (linux) {
		plat_libpath_name = "LD_LIBRARY_PATH".cstr;
		plat_lib_path = "lib/linux".cstr;
	}

	set_env(plat_libpath_name, plat_lib_path);
}

// On laptops with hybrid graphics (one dedicated card, one integrated),
// setting these variables is a hint to the driver to give us the dedicated
// card.  Source: https://redd.it/bk7xbe
version (Windows) {
	import core.sys.windows.windows;

	//TODO: should this be extern (Windows)
	export { extern (C) {
		DWORD NvOptimusEnablement = 0x00000001;
		int AmdPowerXpressRequestHighPerformance = 1;
	}}
}

static if (build_type == BuildType.Release && build_target == OS.Windows) {
	import core.runtime;
	import core.sys.windows.windows;
	import std.conv: text;

	extern (Windows) int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
		try {
			int res;
			Runtime.initialize();
			wchar *args_unparsed = GetCommandLine();
			wchar **wargs = CommandLineToArgvW(args_unparsed, &res);
			string[] args;
			foreach (i; 0 .. res) {
				args ~= wargs[i].dstr.text;
			}
			LocalFree(wargs);

			res = real_main(args);

			Runtime.terminate();
			return res;
		} catch (Throwable e) {
			// ditto with (down there) (TODO)
			fatal(e.msg);
			return 1;
		}
	}
} else {
	int main(string[] args) {
		try {
			return real_main(args);
		// Don't fatal() on assertion errors; fatal has already been
		// called, we don't want a duplicate error message window
		} catch (FatalAssertionError) {
			return 1;
		}
	}
}
