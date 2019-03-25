import stdlib;
import cstdlib;

import windowing.windows;
import windowing.key;

import graphics.shading;

import asset;

import scripting;
import scripting.ecl;

import sound.gorilla;

import derelict.opengl;
import derelict.sdl2.sdl;

bool done;
bool paused;

void dispatch(Event[] evs) {
	foreach (ev; evs) {
		final switch (ev.type) {
			case Evtype.Keydown:
				if (ev.key == Key.space)
					paused = !paused;
				break;
			case Evtype.Keyup: break;
			case Evtype.Mousemove: break;
			case Evtype.Keypress: break;
			case Evtype.Quit:
				done = true;
		}
	}
}

int real_main(string[] args) {
	load_all_libraries();
	init_ecl();
	scope(exit) shutdown_ecl();

	/+
	auto f = new ECLScript();
	f.expose_fun("traa", (ScriptVar[] args) { log("%s + %s => %s", args[0], args[1], args[0] ~ args[1]); return args[0] ~ args[1]; }, [ScriptVarType.Str, ScriptVarType.Str]);
	log("%s", f.eval("(traa \"hi┖\" \"therro\")"));
	+/

	scope GraphicsState gfx = new GraphicsState(WindowSpec("test", 640, 480, 640, 480, Fullscreenstate.None, true, true, false, 4));
	scope GorillaAudio audio = new GorillaAudio();
	auto f = audio.load_cache_sound("out.ogg");
	audio.play(f);


	float r = 0, g = 0, b = 0;
	void nice(ref float f) {
		if (f > 1)
			f -= 1;
		if (f < 0)
			f = 1 - (trunc(f) - f);
	}
	void nice2(size_t x, size_t y)(ref float[24] f) {
		f[x] = clamp(f[x], -.5, 0.5);
		f[y] = clamp(f[y], -.5, 0.5);

		if (f[x] >= 0.5 && f[y] < 0.5) {
			f[y] += 0.01;
		} else if (f[y] >= 0.5 && f[x] > -.5) {
			f[x] -= 0.01;
		} else if (f[x] <= -.5 && f[y] > -.5) {
			f[y] -= 0.01;
		} else {
			f[x] += 0.01;
		}
	}

	float[24] vertices = [
		-.5, -.5, 0.0, 1.0, 0.0, 0.0,
		0.5, -.5, 0.0, 0.0, 1.0, 0.0,
		0.5, 0.5, 0.0, 0.0, 0.0, 1.0,
		-.5, 0.5, 0.0, 0.0, 0.0, 0.0];


	Program prog = Program("#version 330 core
layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 clr;
out vec4 vertex_clr;
void main() {
	gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
	vertex_clr = vec4(clr, 1.0);

}", "#version 330 core
out vec4 frag_color;
in vec4 vertex_clr;

void main() {
	frag_color = vertex_clr;
	//frag_color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}");

mainloop:
	while (!done) {
		///////////////////////////////////
		////EVENT HANDLING ////////////////
		///               /////////////////
		//               /
		Event[] evs = poll_events();
		evs.dispatch();


		///////////////////////////////////
		////  PHYSICS      ////////////////
		///               /////////////////
		//               /
		r += 0.01;
		g += 0.02;
		b -= 0.01;
		nice(r);
		nice(g);
		nice(b);

		if (!paused) {
			nice2!(0, 1)(vertices);
			nice2!(6, 7)(vertices);
			nice2!(12, 13)(vertices);
			nice2!(18, 19)(vertices);
		}


		///////////////////////////////////
		////  RENDERING    ////////////////
		///               /////////////////
		//               /
		glClearColor(r, g, b, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT);


		prog.upload_vertices(vertices);
		prog.blit();
		gfx.blit();


		//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//
		audio.update(.0166666);
	}

	return 0;
}


void load_all_libraries() {
	import derelict.sdl2.sdl;
	import derelict.opengl;
	import scripting.ecl_lib_interface: DerelictECLLoader;

	set_lib_path();

	try {
		DerelictGL3.load();
	} catch(Throwable t) {
		fatal("Error loading OpenGL (mark I).  '%s'", t.msg);
	}
	try {
		DerelictSDL2.load();
	} catch(Throwable t) {
		fatal("Error loading SDL2.  '%s'", t.msg);
	}

	try {
		new DerelictECLLoader().load();
	} catch(Throwable t) {
		fatal("Error loading ECL.  '%s'", t.msg);
	}

	are_libraries_loaded = true;
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
			import core.stdc.stdlib: setenv;
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

version (release) { version (Windows) {
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
			fatal(e.msg);
			return 1;
		}
	}
} else {
	int main(string[] args) {
		return real_main(args);
	}
}} else {
	int main(string[] args) {
		return real_main(args);
	}
}
