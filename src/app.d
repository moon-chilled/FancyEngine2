import std.stdio;
import logging;
import windowing.windows;

import derelict.opengl;
import derelict.sdl2.sdl;

import std.math: abs, trunc;

version (Windows) pragma(lib, "user32");

void main() {
	auto gfx = GraphicsState(WindowSpec("test", 640, 480, 640, 480, Fullscreenstate.None, true, true));

	float r = 0, g = 0, b = 0;
	void nice(ref float f) {
		if (f > 1)
			f -= 1;
		if (f < 0)
			f = 1 - (trunc(f) - f);
	}

	while (true) {
		///////////////////////////////////
		////EVENT HANDLING ////////////////
		///               /////////////////
		//               /
		SDL_Event ev;
		if (SDL_PollEvent(&ev)) {
			if (ev.type == SDL_KEYDOWN) {
				break;
			} else if (ev.type == SDL_MOUSEBUTTONDOWN) {
				error("Hallooooo!");
			}
		}

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

	import core.stdc.stdlib: exit;
	exit(0);
}
