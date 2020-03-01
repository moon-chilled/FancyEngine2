import stdlib;
import cstdlib;
import stdmath;

import windowing.windows;
import windowing.key;

import graphics.fancy_model;
import graphics.font;
import graphics.model;
import graphics.shading;
import graphics.tex;

import asset;

import scripting;
import scripting.s7;
import scripting.moonjit;

import sound.gorilla;

import config;

import queue;

import scriptable;

import repl;

bool done;

void dispatch(Event[] evs, GraphicsState gfx, ScriptManager script) {
	foreach (ev; evs) {
		final switch (ev.type) {
			case Evtype.Keydown:
				script.keyhandler(ScriptVar(ev.key), ScriptVar(true));
				break;
			case Evtype.Keyup:
				script.keyhandler(ScriptVar(ev.key), ScriptVar(false));
				break;
			case Evtype.Mousemove:
				//TODO: remove need for casts
				script.mousehandler(ScriptVar(cast(long)ev.mouse.deltay), ScriptVar(cast(long)ev.mouse.deltax), ScriptVar(cast(long)ev.mouse.ypos), ScriptVar(cast(long)ev.mouse.xpos));
				break;
			case Evtype.Keypress: break;
			case Evtype.Quit:
				done = true;
		}
	}
}

struct G {
	vec3f clear_clr;
	bool am_grabbed;
}

int real_main(string[] args) {
	load_all_libraries();
	scope ScriptManager faux = new ScriptManager([
			"scm": new S7Script(),
			"lua": new MoonJitScript()]);

	//scope Repl repl = new Repl(faux, "scm", &done);
	//repl.start();

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
			"render_width", &render_width,
			"render_height", &render_height,
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

	ws.title = title;


	scope GraphicsState gfx = new GraphicsState(ws);
	scope GorillaAudio audio = new GorillaAudio();
	//scope (exit) { destroy(gfx); destroy(audio); }
	auto sound = audio.load_cache_sound("out.ogg");
	audio.set_volume(sound, master_vol * music_vol);
	audio.play(sound);

	gfx.grab_mouse();


	QueueManager queues = new QueueManager(5);

	faux.expose_fun("shader_set_and_blit", (ScriptVar[] vars) {
		if (vars.length < 2 || vars.length % 2 != 0) error("Asked to draw shader with bad params (%s)", vars);
		if (script_typeof(vars[0]) != ScriptVarType.Shader) {
			error("Asked to draw with shader, but passed %s object of type %s instead", vars[0], vars[0].script_typeof);
			return ScriptVar(false);
		}

		Shader s = vars[0].peek!Shader;

		if (script_typeof(vars[1]) != ScriptVarType.FancyModel) {
			error("Asked to draw model, but passed '%s' of type %s instead", vars[1], vars[1].script_typeof);
			return ScriptVar(false);
		}

		FancyModel model = vars[1].peek!FancyModel;

		vars = vars[2 .. $];
		Mat4fNamePair[] pairs;
		foreach (i; 0 .. vars.length/2) {
			if (script_typeof(vars[2*i]) != ScriptVarType.Str) { error("asked to set uniform on shader, but name was %s, not string", vars[2*i]); return ScriptVar(false); }
			if (script_typeof(vars[2*i+1]) != ScriptVarType.Matx4) { error("asked to set uniform matrix on shader, but given %s, not matrix", vars[2*i+1]); return ScriptVar(false); }
			Mat4fNamePair m;
			m.name = vars[2*i].peek!string;
			m.to = vars[2*i+1].peek!mat4f;

			pairs ~= m;
			//TODO: m.from
		}

		//TODO: bump allocator
		queues.enqueue(new ShaderSetMatricesAndDraw(s, pairs, model));

		return ScriptVar(true);
	}, cast(ScriptVarType[])[], true);

	G g_nminusone, g_n;

	faux.expose_fun("grab_mouse", { g_n.am_grabbed = true; queues.enqueue(new GfxGrabMouse(gfx, g_nminusone.am_grabbed)); });
	faux.expose_fun("ungrab_mouse", { g_n.am_grabbed = false; queues.enqueue(new GfxUngrabMouse(gfx, g_nminusone.am_grabbed)); });

	faux.expose_fun("clear", (float r, float g, float b) { g_n.clear_clr = vec3f(r, g, b); queues.enqueue(new GfxClear(gfx, g_n.clear_clr, g_nminusone.clear_clr)); });

	faux.expose_fun("make_fancy_model", (string s) => FancyModel(s));
	faux.expose_fun("make_shader", (string path) => Shader(fslurp(path ~ ".vert"), fslurp(path ~ ".frag"), gfx.gfx_context));

	faux.expose_fun("make_tex", (string path) => new Texture(path));
	faux.expose_fun("make_tex_from_data", (ScriptVar[] in_pixels, long depth) {
		uint height = cast(uint)in_pixels.length;
		ubyte[] pixels;
		uint width;
		if (depth != 1 && depth != 3 && depth != 4) {
			error("Bad colour depth %s; must be 1, 3, or 4", depth);
			return None;
		}

		foreach (in_row; in_pixels) {
			ScriptVar[] row;
			try {
				in_row.match!(
					(ScriptVar[] v) { row = v; },
					(_) { error("Bad pixel data"); assert(0); })();
			} catch (Throwable) return None;

			if (!width) {
				width = cast(uint)row.length;
			} else if (width != row.length) {
				error("Misaligned rows (%s != %s)", width, row.length);
				return None;
			}

			foreach (in_p; row) {
				long p;
				try {
				in_p.match!(
					(long l) { p = l; },
					(_) { error("Bad type for pixel"); assert(0); })();
				} catch (Throwable) return None;

				//foreach_reverse (i; 0 .. depth) pixels ~= cast(ubyte)((p & (0xff << (8*i))) >> (8*i));
				if (depth == 1) pixels ~= [p&0xff, p&0xff, p&0xff, 0xff];
				else if (depth == 3) pixels ~= [(p&0xff0000) >> 16, (p&0xff00) >> 8, p&0xff, 0xff];
				else if (depth == 4) pixels ~= [(p&0xff000000) >> 24, (p&0xff000) >> 16, (p&0xff) >> 8, p&0xff];
			}
		}

		// colour depth set to 4 in Texture{} because we manually re-pack everything to be rgba
		return ScriptVar(new Texture(pixels, width, height, 4));
	});

	Font[] fonts;
	scope(exit) foreach (f; fonts) f.destroy();
	faux.expose_fun("make_font", (string path) {
			Font f = Font(path, 24, ws.render_width, ws.render_height, gfx.gfx_context);
			fonts ~= f;
			return f;
	});


	Mesh tex_copy_mesh = Mesh([-1,+1, 0,1,
				   +1,-1, 1,0,
				   -1,-1, 0,0,

				   +1,-1, 1,0,
				   -1,+1, 0,1,
				   +1,+1, 1,1], [2, 2]);

	faux.expose_fun("draw_tex_ndc", (Texture t, vec2f loc) {
		vec2f loc2;
		loc2.x = loc.x + 2*cast(float)t.w/ws.render_width;
		loc2.y = loc.y + 2*cast(float)t.h/ws.render_height;
		queues.enqueue(new ShaderDraw2D(&gfx.gfx_extra.tex_copy, &tex_copy_mesh, [loc, loc2], t));
	});

	faux.expose_fun("draw_text_ndc", (Font f, string text, vec2f loc) => queues.enqueue(new FontDraw(f, loc, text)));


	faux.expose_fun("get_renderdims", () => vec2f(ws.render_width, ws.render_height));
	faux.expose_fun("get_texdims", (Texture t) => vec2f(t.w, t.h));

	ulong frames;

	faux.load_script("game.toml");
	faux.init();

	import std.datetime.stopwatch: StopWatch, AutoStart;

	float physics_frame = 1 / physics_fps;
	auto sw = StopWatch(AutoStart.yes);
	float time_so_far = 0;
	float avg_frame_time = physics_frame;
mainloop:
	while (!done) {
		global_pause_mutex.lock();

		bool something_worth_framing = false;

		float frame_time = sw.peek.total!"nsecs" / 1_000_000_000.0;
		sw.reset;
		time_so_far += frame_time;
		avg_frame_time = avg_frame_time*0.9 + 0.1*frame_time;

		if (time_so_far >= physics_frame) {
			time_so_far -= physics_frame;
			something_worth_framing = true;
			frames++;
		}
		if (something_worth_framing && !(frames % 5)) gfx.set_title(strfmt("%s %.2fms (%.f FPS)", title, avg_frame_time * 1000, 1/avg_frame_time));

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
			faux.update();
		}


		///////////////////////////////////
		////  RENDERING    ////////////////
		///               /////////////////
		//               /
		faux.graphics_update();
		queues.flush_current_frame();
		gfx.blit();


		///////////////////////////////////
		////    DUMB       ////////////////
		///     GLOBAL    /////////////////
		//      STATE    /
		g_nminusone = g_n;


		//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//
		// TODO: don't need to re-set volume every loop.  Put this into
		// sound manager, make it set a flag so that next time it's
		// updated, the volume is changed.
		audio.set_volume(sound, master_vol * music_vol);
		audio.update(frame_time);

		global_pause_mutex.unlock();
	}

	//repl.join();
	//destroy(repl);

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

		info("Successfully booted Vulkan (mark I)");
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

	{
		import bindbc.freetype: FTSupport, loadFreeType;
		FTSupport status = loadFreeType;
		final switch (status) {
			case FTSupport.noLibrary:
				fatal("The FreeType library file could not be found.  Have you moved (or removed) the DLL?");
				assert(0);
			case FTSupport.badLibrary:
				fatal("The FreeType library file appears to be corrupt");
				assert(0);
			case FTSupport.ft26: case FTSupport.ft27: case FTSupport.ft28: case FTSupport.ft29: case FTSupport.ft210:
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
	static if (build_type == BuildType.Release) {
	try {
		ret = real_main(args);
	// Don't fatal() on assertion errors; fatal has already been
	// called, we don't want a duplicate error message window
	} catch (Throwable t) {
		fatal(t.msg);
		ret = 1;
	}
	} else {
		ret = real_main(args);
	}

	// ensure no other threads are still running
	global_pause_mutex.lock();

	return ret;
}
}
