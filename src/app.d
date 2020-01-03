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

bool done;

void dispatch(Event[] evs, GraphicsState gfx, Scriptlang script) {
	bool have_keyhandler = script.has_symbol("keyhandler");

	foreach (ev; evs) {
		final switch (ev.type) {
			case Evtype.Keydown:
				script.call("keyhandler", [ScriptVar(ev.key), ScriptVar(true)]);
				break;
			case Evtype.Keyup:
				script.call("keyhandler", [ScriptVar(ev.key), ScriptVar(false)]);
				break;
			case Evtype.Mousemove:
				//TODO: remove need for casts
				script.call("mousehandler", [ScriptVar(cast(long)ev.mouse.deltay), ScriptVar(cast(long)ev.mouse.deltax), ScriptVar(cast(long)ev.mouse.ypos), ScriptVar(cast(long)ev.mouse.xpos)]);
				break;
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
		string title = "FE2 - OpenGL";
	} else static if (gfx_backend == GfxBackend.Vulkan) {
		string title = "FE2 - Vulkan";
	}
	WindowSpec ws;
	string fs, vs;
	float physics_fps;
	float master_vol, music_vol, effect_vol;

	with (ws) Configure("prefs.toml",
		Table("Graphics"),
			"width", &win_width,
			"height", &win_height,
			"fullscreen", &fs,
			"borders", &borders,
			"vsync", &vs,
			"wireframe", &wireframe,
			"msaa", &aa_samples,
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
	switch (vs) {
		case "on": ws.vsync = Vsyncstate.On; break;
		case "off": ws.vsync = Vsyncstate.Off; break;
		case "undefined": ws.vsync = Vsyncstate.Undefined; break;
		default: error("config error: unable to load option 'fullscreen': invalid value '%s'.  Defaulting to let the driver do whatever it wants.", vs); ws.vsync = Vsyncstate.Undefined;
	}

	ws.render_width = ws.win_width;
	ws.render_height = ws.win_height;
	ws.title = title;


	scope GraphicsState gfx = new GraphicsState(ws);
	scope GorillaAudio audio = new GorillaAudio();
	//scope (exit) { destroy(gfx); destroy(audio); }
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
	faux.expose_fun("clear", (float r, float g, float b) => clear(gfx, r, g, b));

	{
		auto ww = ScriptVar(cast(long)ws.win_width);
		auto wh = ScriptVar(cast(long)ws.win_height);
		faux.expose_var("window-width", ww);
		faux.expose_var("window-height", wh);
	}

	faux.load("stdlib.scm");


	ulong frames;

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
			frames++;
		}
		if (something_worth_framing && !(frames % 5)) gfx.set_title(strfmt("%s %.f FPS (%.2fms)", title, 1/avg_frame_time, avg_frame_time * 1000));

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
	set_lib_path();

	{
		import bindbc.sdl;
		SDLSupport status = loadSDL();
		if (status == SDLSupport.noLibrary) {
			fatal("The SDL library file could not be found.  Have you moved (or removed) the DLL?");
		}

		if (status == SDLSupport.badLibrary) {
			fatal("The SDL library file appears to be corrupt");
		}

		static if (gfx_backend == GfxBackend.Vulkan) {
			if (status < SDLSupport.sdl206) {
				fatal("SDL 2.0.6 or better is required for Vulkan support");
			}
		}

		SDL_version ver;
		SDL_GetVersion(&ver);
		info("Successfully booted SDL %s.%s.%s", ver.major, ver.minor, ver.patch);
	}
	synchronized is_sdl_loaded = true;

	static if (gfx_backend == GfxBackend.Vulkan) {{
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
	}} else static if (gfx_backend == GfxBackend.OpenGL) {{
		import bindbc.opengl: GLSupport, loadOpenGL;
		GLSupport status = loadOpenGL();

		// No OpenGL context has been made yet (good!), this is just to
		// make sure the library is present and set up correctly.
		if (status != GLSupport.noContext) {
			if (status == GLSupport.noLibrary) {
				fatal("The OpenGL library file could not be found.  Have you moved (or removed) the DLL?");
			} else if (status == GLSupport.badLibrary) {
				fatal("The OpenGL library file appears to be corrupt");
			} else {
				// actually loaded opengl ???
				fatal("Something is horribly wrong with your system");
			}
		} else {
			info("Successfully booted OpenGL (mark I)");
		}
	}}

	{
		import bindbc.assimp: AssimpSupport, loadAssimp;
		AssimpSupport status = loadAssimp();
		final switch (status) {
			case AssimpSupport.noLibrary:
				fatal("The assimp library file could not be found.  Have you moved (or removed) the DLL?");
				assert(0);
			case AssimpSupport.badLibrary:
				fatal("The assimp library file appears to be corrupt");
				assert(0);
			case AssimpSupport.assimp500:
				// cool
				break;
		}
	}
}

// really, it returns errno_t, but that's the same as int
version (Windows) private extern (C) int _putenv_s(const char*, const char*);
// setup LD_LIBRARY_PATH (or equivalent) so bindbc (or something else) can find libraries
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
	int ret;
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

		ret = real_main(args);

		Runtime.terminate();
	} catch (Throwable e) {
		// ditto with (down there) (TODO)
		fatal(e.msg);
		ret = 1;
	}

	// ensure no other threads are still running
	global_pause_mutex.lock();

	return ret;
}
} else {
int main(string[] args) {
	int ret;
	try {
		ret = real_main(args);
	// Don't fatal() on assertion errors; fatal has already been
	// called, we don't want a duplicate error message window
	} catch (FatalAssertionError) {
		ret = 1;
	}

	// ensure no other threads are still running
	global_pause_mutex.lock();

	return ret;
}
}
