import std.stdio;
import logging;
import windowing.windows;

import derelict.opengl;
import derelict.sdl2.sdl;

version (Windows) pragma(lib, "user32");

void main() {
	GraphicsState gfx = new_winstate(WindowSpec("test", 640, 480, 640, 480, Fullscreenstate.None, true, true));
	glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);

	while (true) {
		SDL_Event ev;
		if (SDL_PollEvent(&ev)) {
			if (ev.type == SDL_KEYDOWN) {
				break;
			} else if (ev.type == SDL_MOUSEBUTTONDOWN) {
				error("Hallooooo!");
			}
		}

		gfx.blit();
	}

	import core.stdc.stdlib: exit;
	exit(0);
}
