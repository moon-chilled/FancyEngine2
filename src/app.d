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
import scripting.ecl;

import sound.gorilla;

import config;

enum WIDTH = 1280, HEIGHT = 720;
enum ASPECT_RATIO = cast(double)WIDTH/cast(double)HEIGHT;
enum FOV = 90.0;
enum SPEED = 0.1;
enum PHYSICS_FRAME = 0.016666666666;

bool done;
bool paused = true;
bool grabbed;

struct ViewState {
	mat4f projection = mat4f.perspective(to_rad(FOV/2), ASPECT_RATIO, 0.1, 100);
	mat4f view = mat4f.identity.translation(vec3f(0, 0, -6));
	mat4f model = mat4f.identity.rotateX(to_rad(90));

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
					case Key.w: state.velocity.z += SPEED; break;
					case Key.s: state.velocity.z -= SPEED; break;
					case Key.a: state.velocity.x -= SPEED; break;
					case Key.d: state.velocity.x += SPEED; break;
					default: break;
				}

				break;
			case Evtype.Keyup:
				switch (ev.key) {
					case Key.w: state.velocity.z -= SPEED; break;
					case Key.s: state.velocity.z += SPEED; break;
					case Key.a: state.velocity.x += SPEED; break;
					case Key.d: state.velocity.x -= SPEED; break;
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

	static if (gfx_backend == GfxBackend.OpenGL) {
		string title = "FE2—OpenGL";
	} else static if (gfx_backend == GfxBackend.Vulkan) {
		string title = "FE2—Vulkan";
	}
	WindowSpec ws;
	string fs;
	with (ws) Configure("prefs.toml",
		Table("Graphics"),
			"width", &win_width,
			"height", &win_height,
			"fullscreen", &fs,
			"borders", &borders,
			"vsync", &vsync,
			"wireframe", &wireframe,
			"aa", &aa_samples);
	ws.fullscreen =	fs == "fullscreen" ? Fullscreenstate.Fullscreen :
			fs == "desktop" ? Fullscreenstate.Desktop : Fullscreenstate.None;
	ws.render_width = ws.win_width;
	ws.render_height = ws.win_height;
	ws.title = title;

	scope GraphicsState gfx = new GraphicsState(ws);
	scope GorillaAudio audio = new GorillaAudio();
	//audio.play(audio.load_cache_sound("out.ogg"));

	gfx.grab_mouse();


	auto m = FancyModel("assets/model_nanosuit/nanosuit.obj");


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
		-0.5, -0.5, 0.5, -1, -1,
		-0.5, 0.5, 0.5, -1, 1,



		///REAR
		0.5, 0.5, -0.5, 1, 1,
		-0.5, 0.5, -0.5, -1, 1,
		0.5, -0.5, -0.5, 1, -1,

		0.5, -0.5, -0.5, 1, -1,
		-0.5, 0.5, -0.5, -1, 1,
		-0.5, -0.5, -0.5, -1, -1,




		///TOP
		0.5, 0.5, 0.5, 1, 1,
		-0.5, 0.5, 0.5, -1, 1,
		0.5, 0.5, -0.5, 1, -1,

		-0.5, 0.5, 0.5, -1, 1,
		-0.5, 0.5, -0.5, -1, -1,
		0.5, 0.5, -0.5, 1, -1,

		///BOTTOM
		0.5, -0.5, 0.5, 1, 1,
		0.5, -0.5, -0.5, 1, -1,
		-0.5, -0.5, 0.5, -1, 1,

		-0.5, -0.5, 0.5, -1, 1,
		0.5, -0.5, -0.5, 1, -1,
		-0.5, -0.5, -0.5, -1, -1,



		///RIGHT
		0.5, -0.5, 0.5, 1, -1,
		0.5, 0.5, 0.5, 1, 1,
		0.5, 0.5, -0.5, -1, 1,

		0.5, -0.5, 0.5, 1, -1,
		0.5, 0.5, -0.5, -1, 1,
		0.5, -0.5, -0.5, -1, -1,



		///LEFT
		-0.5, 0.5, 0.5, 1, 1,
		-0.5, -0.5, 0.5, 1, -1,
		-0.5, 0.5, -0.5, -1, 1,

		-0.5, 0.5, -0.5, -1, 1,
		-0.5, -0.5, 0.5, 1, -1,
		-0.5, -0.5, -0.5, -1, -1,
	];

	ulong frames;

	ViewState state;


static if (gfx_backend == GfxBackend.OpenGL) {
		Program prog = Program(q{#version 330 core
			layout (location = 0) in vec3 in_pos;
			layout (location = 1) in vec3 in_normal;
			layout (location = 3) in vec2 in_tex_coord;
			layout (location = 4) in vec3 in_tangent;
			layout (location = 5) in vec3 in_bitangent;

			out vec2 tex_coord;

			uniform mat4 model;
			uniform mat4 view;
			uniform mat4 projection;

			void main() {
				gl_Position = projection * view * model * vec4(in_pos, 1.0);
				tex_coord = in_tex_coord;
			}}, q{#version 330 core

			uniform sampler2D texture0;
			uniform sampler2D texture1;
			uniform sampler2D texture2;
			uniform sampler2D texture3;
			uniform sampler2D texture4;
			uniform sampler2D texture5;
			uniform sampler2D texture6;
			uniform sampler2D texture7;

			out vec4 FragColor;
			in vec2 out_tex_coord;
			void main() {
				FragColor = vec4(1, 1, 1, 1);
			}}, gfx.gfx_context);
	}
	//Mesh model = Mesh(vertices, [3, 2]);

	upload_texture(0, new Texture("dickbutt.png", gfx.gfx_context));
	prog.set_int("texture0", 0);

	import std.datetime.stopwatch: StopWatch, AutoStart;

	auto sw = StopWatch(AutoStart.yes);
	float time_so_far = 0;
	float avg_frame_time = PHYSICS_FRAME;
mainloop:
	while (!done) {
		bool something_worth_framing;

		float frame_time = sw.peek.total!"nsecs" / 1_000_000_000.0;
		sw.reset;
		time_so_far += frame_time;
		avg_frame_time = avg_frame_time*0.9 + 0.1*frame_time;

		if (time_so_far >= PHYSICS_FRAME) {
			time_so_far -= PHYSICS_FRAME;
			something_worth_framing = true;
		}
		if (something_worth_framing) gfx.set_title(format("%s %.f FPS", title, 1/avg_frame_time));

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
		if (something_worth_framing) {
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
		}


		///////////////////////////////////
		////  RENDERING    ////////////////
		///               /////////////////
		//               /
		clear(gfx, r, g, b);

		prog.set_mat4("projection", state.projection);
		prog.set_mat4("model", state.model);
		prog.set_mat4("view", state.view);
		//prog.blit(model);
		prog.blit(m);
		gfx.blit();


		//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//
		audio.update(frame_time);
	}

	return 0;
}


void load_all_libraries() {
	import derelict.sdl2.sdl: DerelictSDL2;
	import scripting.ecl_lib_interface: DerelictECLLoader;
	import derelict.assimp3.assimp: DerelictASSIMP3;

	set_lib_path();

	static if (gfx_backend == GfxBackend.OpenGL) {
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
		new DerelictECLLoader().load();
	} catch(Throwable t) {
		fatal("Error loading ECL.  '%s'", t.msg);
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

static if (build_type == BuildType.Release) { version (Windows) {
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
