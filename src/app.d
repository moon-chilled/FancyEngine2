import std.stdio;

import logging;
import windowing.windows;
import windowing.key;

import derelict.opengl;
import derelict.sdl2.sdl;

import std.math: abs, trunc;

version (Windows) pragma(lib, "user32");

void main() {
	scope GraphicsState gfx = new GraphicsState(WindowSpec("test", 640, 480, 640, 480, Fullscreenstate.None, true, true));

	float r = 0, g = 0, b = 0;
	void nice(ref float f) {
		if (f > 1)
			f -= 1;
		if (f < 0)
			f = 1 - (trunc(f) - f);
	}

mainloop:
	while (true) {
		///////////////////////////////////
		////EVENT HANDLING ////////////////
		///               /////////////////
		//               /
		foreach (ev; poll_events) {
			final switch (ev.type) {
				case Evtype.Keydown:
					writefln("Key %s down", ev.key);
					break;
				case Evtype.Keyup:
					writefln("Key %s is up", ev.key); break;
				case Evtype.Mousemove:
					writefln("Mouse moved by (%s, %s)", ev.mouse.deltay, ev.mouse.deltax); break;
				case Evtype.Keypress:
					writefln("Key %s was pressed", ev.key);
					break;
				case Evtype.Quit:
					break mainloop;
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
}
