import std.stdio;
import logging;
import windowing.windows;

void main() {
	GraphicsState gfx = new_winstate(WindowSpec("test", 640, 480, 640, 480, Fullscreenstate.None, true, true));
}
