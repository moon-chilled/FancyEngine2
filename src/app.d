import stdlib;

import windowing.windows;
import windowing.key;

import assets.asset;

import derelict.opengl;
import derelict.sdl2.sdl;

import scripting.ecl;

version (Windows) pragma(lib, "user32");

bool done;

void dispatch(Event[] evs) {
	foreach (ev; evs) {
		final switch (ev.type) {
			case Evtype.Keydown:
				log("Key %s down", ev.key);
				break;
			case Evtype.Keyup:
				log("Key %s is up", ev.key); break;
			case Evtype.Mousemove:
				log("Mouse moved by (%s, %s)", ev.mouse.deltay, ev.mouse.deltax); break;
			case Evtype.Keypress:
				log("Key %s was pressed", ev.key);
				break;
			case Evtype.Quit:
				done = true;
		}
	}
}

void main() {
	load_all_libraries();
	scope GraphicsState gfx = new GraphicsState(WindowSpec("test", 640, 480, 640, 480, Fullscreenstate.None, true, true));
	init_ecl();
	scope(exit) shutdown_ecl();

	float r = 0, g = 0, b = 0;
	void nice(ref float f) {
		if (f > 1)
			f -= 1;
		if (f < 0)
			f = 1 - (trunc(f) - f);
	}

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


		///////////////////////////////////
		////  RENDERING    ////////////////
		///               /////////////////
		//               /
		glClearColor(r, g, b, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT);
		gfx.blit();
	}
}


void load_all_libraries() {
	import derelict.sdl2.sdl;
	import derelict.opengl;
	import scripting.ecl: DerelictECLLoader;

	set_lib_path();

	try {
		DerelictGL3.load();
		log("hi");
	} catch(Throwable) {
		fatal("Error loading OpenGL (mark I)");
	}
	try {
		DerelictSDL2.load();
	} catch(Throwable) {
		fatal("Error loading SDL2");
	}

	try {
		new DerelictECLLoader().load();
	} catch(Throwable) {
		fatal("Error loading ECL");
	}

	are_libraries_loaded = true;
}

version (Windows) private extern (C) int _putenv_s(const char*, const char*);
// setup LD_LIBRARY_PATH (or equivalent) so derelict (or something else) can find libraries
void set_lib_path() {
	version (Windows) {
		// it returns errno_t, but that's an alias for int
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
		version (Win64) {
			plat_lib_path = r"lib\win\".cstr;
		} else version (Win32) {
			plat_lib_path = r"lib\win32\".cstr;
		} else {
			static assert(0);
		}
	} else version (OSX) {
		plat_libpath_name = "DYLD_LIBRARY_PATH".cstr;
		plat_lib_path = "lib/macos".cstr;
	} else version (linux) {
		plat_libpath_name = "LD_LIBRARY_PATH".cstr;
		version (X86_64) {
			plat_lib_path = r"lib/linux".cstr;
		} else version (X86) {
			plat_lib_path = r"lib/linux32".cstr;
		}
	}

	set_env(plat_libpath_name, plat_lib_path);
}
