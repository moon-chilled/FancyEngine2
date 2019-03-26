import stdlib;
import cstdlib;

import windowing.windows;
import windowing.key;

import graphics.shading;
import graphics.tex;

import asset;

import scripting;
import scripting.ecl;

import sound.gorilla;

import derelict.opengl;
import derelict.sdl2.sdl;

enum width = 1280, height = 720;
enum aspect_ratio = cast(double)width/cast(double)height;
enum fov = 90.0;

bool done;
bool paused = true;

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
	/*
	init_ecl();
	scope(exit) shutdown_ecl();

	auto faux = new ECLScript();
	faux.expose_fun("traa", (ScriptVar[] args) { log("%s + %s => %s", args[0], args[1], args[0] ~ args[1]); return args[0] ~ args[1]; }, [ScriptVarType.Str, ScriptVarType.Str]);
	log("%s", faux.eval("(traa \"hi┖\" \"therro\")"));
	*/

	scope GraphicsState gfx = new GraphicsState(WindowSpec("test", width, height, width, height, Fullscreenstate.None, true, true, false, 4));
	scope GorillaAudio audio = new GorillaAudio();


	float r = 0, g = 0, b = 0;
	void nice(ref float f) {
		if (f > 1)
			f -= 1;
		if (f < 0)
			f = 1 - (trunc(f) - f);
	}
	float[20] vertices = [
		0.5, 0.5, 0.0, 1., 1.,
		0.5, -0.5, 0.0, 1., -1,
		-0.5, -0.5, 0.0, -1, -1,
		-0.5, 0.5, 0.0, -1, 1.];
	import stdmath;

	struct State {
		mat4f projection = mat4f.perspective(to_rad(fov/2), aspect_ratio, 0.1, 100.0);
		mat4f view = mat4f.identity.translation(vec3f(0, 0, -3));
		mat4f model = mat4f.identity.rotateX(to_rad(-55));
	}

	State state;


	Program prog = Program(q{#version 330 core
layout (location = 0) in vec3 in_pos;
layout (location = 1) in vec2 in_tex_coord;
out vec2 tex_coord;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
	vec4 x = projection * view * model * vec4(in_pos, 1.0);
	if (x.z > 1) x.z = 1;
	if (x.z < 1) x.z = -1;
	gl_Position = x;
	//gl_Position = vec4(in_pos, 1.0);

	tex_coord = in_tex_coord;
}}, q{#version 330 core
out vec4 frag_colour;
in vec2 tex_coord;

uniform sampler2D wall_tex;
uniform sampler2D face_tex;

void main() {
	//frag_colour = texture(tex, tex_coord);
	frag_colour = mix(texture(wall_tex, tex_coord), texture(face_tex, vec2(-tex_coord.x, tex_coord.y)), 0.3);
	//frag_colour = vertex_clr;
	//frag_colour = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}});

	prog.upload_texture(0, new Texture("wall.jpg"));
	prog.upload_texture(1, new Texture("face.png"));
	prog.set_int("wall_tex", 0);
	prog.set_int("face_tex", 1);

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
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


		prog.upload_vertices(vertices);
		prog.set_mat4("projection", state.projection);
		prog.set_mat4("model", state.model);
		prog.set_mat4("view", state.view);
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
