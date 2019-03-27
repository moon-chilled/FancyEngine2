import stdlib;
import cstdlib;
import stdmath;

import windowing.windows;
import windowing.key;

import graphics.shading;
import graphics.tex;

import asset;

import scripting;
import scripting.ecl;

import sound.gorilla;

import derelict.opengl;

enum width = 640, height = 480;
enum aspect_ratio = cast(double)width/cast(double)height;
enum fov = 90.0;
enum speed = 0.1;

bool done;
bool paused = true;
bool grabbed;

struct ViewState {
	mat4f projection = mat4f.perspective(to_rad(fov/2), aspect_ratio, 100, 0.1);
	mat4f view = mat4f.identity.translation(vec3f(0, 0, -6));
	mat4f model = mat4f.identity.rotateX(to_rad(-55));

	vec3f cam_pos = vec3f(0, 0, 3), cam_target = vec3f(), cam_front = vec3f(0, 0, -1), cam_up = vec3f(0, 1, 0);
	vec3f velocity = vec3f(0, 0, 0);
	float pitch = 0, yaw = -90, roll = 0; // TODO: implement roll
}
void dispatch(Event[] evs, GraphicsState gfx, ref ViewState state) {
	foreach (ev; evs) {
		final switch (ev.type) {
			case Evtype.Keydown:
				switch (ev.key) {
					case Key.space: paused = !paused; break;
					case Key.enter: grabbed = !grabbed; break;
					case Key.w: state.velocity.z += speed; break;
					case Key.s: state.velocity.z -= speed; break;
					case Key.a: state.velocity.x -= speed; break;
					case Key.d: state.velocity.x += speed; break;
					default: break;
				}

				break;
			case Evtype.Keyup:
				switch (ev.key) {
					case Key.w: state.velocity.z -= speed; break;
					case Key.s: state.velocity.z += speed; break;
					case Key.a: state.velocity.x += speed; break;
					case Key.d: state.velocity.x -= speed; break;
					default: break;
				}

				break;

			case Evtype.Mousemove:
				float sense = 0.3;
				state.pitch = clamp(state.pitch - ev.mouse.deltay * sense, -89, 89);
				state.yaw += sense * ev.mouse.deltax;
				state.cam_front = vec3f(cos(state.pitch.to_rad) * cos(state.yaw.to_rad),
						sin(state.pitch.to_rad),
						cos(state.pitch.to_rad) * sin(state.yaw.to_rad));
				state.cam_front.normalize();


				break;
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

	gfx.grab_mouse();



	float r = 0, g = 0, b = 0;
	void nice(ref float f) {
		if (f > 1)
			f -= 1;
		if (f < 0)
			f = 1 - (trunc(f) - f);
	}
	float[180] vertices = [
		///FRONT
		0.5, 0.5, 0.5, 1., 1.,
		0.5, -0.5, 0.5, 1., -1,
		-0.5, 0.5, 0.5, -1, 1,

		0.5, -0.5, 0.5, 1., -1,
		-0.5, 0.5, 0.5, -1, 1,
		-0.5, -0.5, 0.5, -1, -1,



		///REAR
		0.5, 0.5, -0.5, 1, 1,
		0.5, -0.5, -0.5, 1, -1,
		-0.5, 0.5, -0.5, -1, 1,

		0.5, -0.5, -0.5, 1, -1,
		-0.5, 0.5, -0.5, -1, 1,
		-0.5, -0.5, -0.5, -1, -1,




		///TOP
		0.5, 0.5, 0.5, 1, 1,
		-0.5, 0.5, 0.5, -1, 1,
		0.5, 0.5, -0.5, 1, -1,

		-0.5, 0.5, 0.5, -1, 1,
		0.5, 0.5, -0.5, 1, -1,
		-0.5, 0.5, -0.5, -1, -1,

		///BOTTOM
		0.5, -0.5, 0.5, 1, 1,
		-0.5, -0.5, 0.5, -1, 1,
		0.5, -0.5, -0.5, 1, -1,

		-0.5, -0.5, 0.5, -1, 1,
		0.5, -0.5, -0.5, 1, -1,
		-0.5, -0.5, -0.5, -1, -1,



		///RIGHT
		0.5, 0.5, 0.5, 1, 1,
		0.5, 0.5, -0.5, -1, 1,
		0.5, -0.5, 0.5, 1, -1,

		0.5, 0.5, -0.5, -1, 1,
		0.5, -0.5, 0.5, 1, -1,
		0.5, -0.5, -0.5, -1, -1,



		///LEFT
		-0.5, 0.5, 0.5, 1, 1,
		-0.5, 0.5, -0.5, -1, 1,
		-0.5, -0.5, 0.5, 1, -1,

		-0.5, 0.5, -0.5, -1, 1,
		-0.5, -0.5, 0.5, 1, -1,
		-0.5, -0.5, -0.5, -1, -1,
	];

	ulong frames;

	ViewState state;



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
	frag_colour = mix(texture(wall_tex, tex_coord), texture(face_tex, vec2(-tex_coord.x, tex_coord.y)), 0.9);
	//frag_colour = vertex_clr;
	//frag_colour = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}});

	prog.upload_texture(0, new Texture("wall.jpg"));
	prog.upload_texture(1, new Texture("dickbutt.jpg"));
	prog.set_int("wall_tex", 0);
	prog.set_int("face_tex", 1);

mainloop:
	while (!done) {
		if (!paused) frames++;
		///////////////////////////////////
		////EVENT HANDLING ////////////////
		///               /////////////////
		//               /
		poll_events().dispatch(gfx, state);


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

		state.cam_pos += state.velocity.z * state.cam_front;
		state.cam_pos += state.cam_front.cross(state.cam_up).normalized * state.velocity.x;

		if (!paused) state.model = mat4f.identity.rotation(frames*.05, vec3f(0.5, 1, 1));
		state.view = mat4f.lookAt(state.cam_pos, state.cam_pos + state.cam_front, state.cam_up);


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
