module windowing.windows;
import stdlib;
import cstdlib;

import graphics.framebuffer;

import windowing.key;
static if (gfx_backend == GfxBackend.Vulkan) {
	import windowing.windows_vk;
} else static if (gfx_backend == GfxBackend.OpenGL) {
	import windowing.windows_gl;
}

import bindbc.sdl;

class GraphicsState {
	GfxContext gfx_context;
	GfxExtra gfx_extra;
	SDL_Window *window; // maybe I should allow for multiple windows.  But meh
	WindowSpec window_spec;

	this(WindowSpec window) {
		with (window) info("Opening %s window titled '%s' (%sx%s, render resolution %sx%s), in %s mode, with%s vsync", borders ? "bordered" : "borderless", title, win_width, win_height, render_width, render_height, [Fullscreenstate.None: "windowed", Fullscreenstate.Desktop: "borderless fullscreen", Fullscreenstate.Fullscreen: "true fullscreen"][window.fullscreen], [Vsyncstate.Off: "out", Vsyncstate.On: "", Vsyncstate.Undefined: " undefined"][vsync]);

		this.window_spec = window;

		if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_EVENTS) < 0) sdlerror;

		if (window.aa_samples > 1) {
			setup_aa(window.aa_samples);
		}

		pre_window_setup();

		SDL_WindowFlags win_flags = SDL_WINDOW_SHOWN;
		if (!window.borders) win_flags |= SDL_WINDOW_BORDERLESS;
		if (window.fullscreen == Fullscreenstate.Fullscreen) win_flags |= SDL_WINDOW_FULLSCREEN;
		else if (window.fullscreen == Fullscreenstate.Desktop) win_flags |= SDL_WINDOW_FULLSCREEN_DESKTOP;
		win_flags |= auxiliary_sdl_window_flags;
		set_fullscreen(window.fullscreen == Fullscreenstate.Fullscreen);

		int num_displays = SDL_GetNumVideoDisplays();
		if (num_displays < 0) {
			window.monitor_index = 0;
		} else if (window.monitor_index > num_displays-1) {
			warning("Tried to open on monitor #%s, but only have %s monitors; defaulting to monitor #1", window.monitor_index+1, num_displays);
			window.monitor_index = 0;
		}

		this.window = SDL_CreateWindow(window.title.cstr, SDL_WINDOWPOS_UNDEFINED_DISPLAY(window.monitor_index), SDL_WINDOWPOS_UNDEFINED_DISPLAY(window.monitor_index), window.win_width, window.win_height, win_flags);

		if (!this.window) sdlerror;

		// possibly redundant?
		SDL_ShowWindow(this.window);
		SDL_RaiseWindow(this.window);

		SDL_SetHint(SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS, "0");

		gfx_context = setup_context(this.window);

		post_window_setup(this.window);

		if (window.vsync == Vsyncstate.On) set_vsync(true);
		else if (window.vsync == Vsyncstate.Off) set_vsync(false);

		set_wireframe(window.wireframe);

		gfx_extra = setup_extra(gfx_context, window);
	}

	void grab_mouse() {
		if (SDL_SetRelativeMouseMode(SDL_TRUE) < 0) error("unable to grab mouse.  SDL says '%s'", SDL_GetError().dstr);
	}
	void ungrab_mouse() {
		if (SDL_SetRelativeMouseMode(SDL_FALSE) < 0) error("unable to ungrab mouse.  SDL says '%s'", SDL_GetError().dstr);
	}
	void set_title(string title) {
		SDL_SetWindowTitle(window, title.cstr);
	}

	~this() {
		gfx_end(gfx_context);
		SDL_DestroyWindow(window);
		SDL_Quit();
	}
}

enum Fullscreenstate {
	None,
	Desktop,
	Fullscreen
}
enum Vsyncstate {
	On,
	Off,
	Undefined, // let the driver do whatever it wants
}

struct WindowSpec {
	string title;
	uint win_width, win_height;
	uint render_width, render_height;
	Fullscreenstate fullscreen;
	bool borders;
	bool wireframe;
	Vsyncstate vsync;
	int aa_samples;
	uint monitor_index;
}
private void sdlerror() {
	fatal("SDL Error: %s", SDL_GetError().dstr);
}

pragma(inline, true) void blit(GraphicsState gs) {
	gfx_blit(gs.gfx_context, gs.gfx_extra, gs.window);
}
pragma(inline, true) void clear(GraphicsState gs, float r, float g, float b) {
	gfx_clear(gs.gfx_context, r, g, b);
}


Event[] poll_events() {
	Event[] ret;

	SDL_Event sdl_ev;

	while (SDL_PollEvent(&sdl_ev)) {
		switch (sdl_ev.type) {
			case SDL_KEYDOWN:
				if (!sdl_ev.key.repeat) ret ~= Event(Evtype.Keydown, sdltokey(sdl_ev.key.keysym.sym));
				break;
			case SDL_KEYUP:
				if (!sdl_ev.key.repeat) ret ~= Event(Evtype.Keyup, sdltokey(sdl_ev.key.keysym.sym));
				break;
			case SDL_MOUSEMOTION:
				Event x = Event(Evtype.Mousemove);
				x.mouse.deltay = sdl_ev.motion.yrel;
				x.mouse.deltax = sdl_ev.motion.xrel;
				x.mouse.ypos = sdl_ev.motion.y;
				x.mouse.xpos = sdl_ev.motion.x;
				ret ~= x;
				break;
			case SDL_MOUSEWHEEL:
				// if the mousewheel moved up by X, then add X instances of the mousewheelup keypress
				// (ditto for down, right, left)
				if (sdl_ev.wheel.y > 0) {
					while (sdl_ev.wheel.y--) {
						ret ~= Event(Evtype.Keypress, Key.mousewheelup);
					}
				} else if (sdl_ev.wheel.y < 0) {
					while (sdl_ev.wheel.y++) {
						ret ~= Event(Evtype.Keypress, Key.mousewheeldown);
					}
				}

				if (sdl_ev.wheel.x > 0) {
					while (sdl_ev.wheel.x--) {
						ret ~= Event(Evtype.Keypress, Key.mousewheelleft);
					}
				} else if (sdl_ev.wheel.x < 0) {
					while (sdl_ev.wheel.x++) {
						ret ~= Event(Evtype.Keypress, Key.mousewheelright);
					}
				}
				break;
			case SDL_MOUSEBUTTONDOWN:
				ret ~= Event(Evtype.Keydown, sdlmousetokey(sdl_ev.button.button));
				break;
			case SDL_MOUSEBUTTONUP:
				ret ~= Event(Evtype.Keyup, sdlmousetokey(sdl_ev.button.button));
				break;
			case SDL_QUIT:
				ret ~= Event(Evtype.Quit);
				break;
			case SDL_TEXTINPUT:
				//ignore; someday there will be a dedicated thing to read input
				break;
			default: trace("Unhandled event of type %s", sdl_ev.type);
		}
	}

	return ret;
}

Key sdlmousetokey(ubyte button) {
	return [SDL_BUTTON_LEFT: Key.mouse1,
	       SDL_BUTTON_MIDDLE: Key.mouse2,
	       SDL_BUTTON_RIGHT: Key.mouse3,
	       SDL_BUTTON_X1: Key.mouse4,
	       SDL_BUTTON_X2: Key.mouse5][button];
}
Key sdltokey(SDL_Keycode sdl) {
	Key[SDL_Keycode] lis = [SDLK_UNKNOWN: Key.unknown,
	       SDLK_RETURN: Key.enter,
	       SDLK_ESCAPE: Key.escape,
	       SDLK_BACKSPACE: Key.backspace,
	       SDLK_TAB: Key.tab,
	       SDLK_SPACE: Key.space,
	       SDLK_EXCLAIM: Key.exclaim,
	       SDLK_QUOTEDBL: Key.quotedbl,
	       SDLK_HASH: Key.hash,
	       SDLK_PERCENT: Key.percent,
	       SDLK_DOLLAR: Key.dollar,
	       SDLK_AMPERSAND: Key.ampersand,
	       SDLK_QUOTE: Key.quote,
	       SDLK_LEFTPAREN: Key.leftparen,
	       SDLK_RIGHTPAREN: Key.rightparen,
	       SDLK_ASTERISK: Key.asterisk,
	       SDLK_PLUS: Key.plus,
	       SDLK_COMMA: Key.comma,
	       SDLK_MINUS: Key.minus,
	       SDLK_PERIOD: Key.period,
	       SDLK_SLASH: Key.slash,
	       SDLK_0: Key.num_0,
	       SDLK_1: Key.num_1,
	       SDLK_2: Key.num_2,
	       SDLK_3: Key.num_3,
	       SDLK_4: Key.num_4,
	       SDLK_5: Key.num_5,
	       SDLK_6: Key.num_6,
	       SDLK_7: Key.num_7,
	       SDLK_8: Key.num_8,
	       SDLK_9: Key.num_9,
	       SDLK_COLON: Key.colon,
	       SDLK_SEMICOLON: Key.semicolon,
	       SDLK_LESS: Key.less,
	       SDLK_EQUALS: Key.equals,
	       SDLK_GREATER: Key.greater,
	       SDLK_QUESTION: Key.question,
	       SDLK_AT: Key.at,
	       SDLK_LEFTBRACKET: Key.leftbracket,
	       SDLK_BACKSLASH: Key.backslash,
	       SDLK_RIGHTBRACKET: Key.rightbracket,
	       SDLK_CARET: Key.caret,
	       SDLK_UNDERSCORE: Key.underscore,
	       SDLK_BACKQUOTE: Key.backquote,
	       SDLK_a: Key.a,
	       SDLK_b: Key.b,
	       SDLK_c: Key.c,
	       SDLK_d: Key.d,
	       SDLK_e: Key.e,
	       SDLK_f: Key.f,
	       SDLK_g: Key.g,
	       SDLK_h: Key.h,
	       SDLK_i: Key.i,
	       SDLK_j: Key.j,
	       SDLK_k: Key.k,
	       SDLK_l: Key.l,
	       SDLK_m: Key.m,
	       SDLK_n: Key.n,
	       SDLK_o: Key.o,
	       SDLK_p: Key.p,
	       SDLK_q: Key.q,
	       SDLK_r: Key.r,
	       SDLK_s: Key.s,
	       SDLK_t: Key.t,
	       SDLK_u: Key.u,
	       SDLK_v: Key.v,
	       SDLK_w: Key.w,
	       SDLK_x: Key.x,
	       SDLK_y: Key.y,
	       SDLK_z: Key.z,

	       SDLK_CAPSLOCK: Key.capslock,

	       SDLK_F1: Key.f1,
	       SDLK_F2: Key.f2,
	       SDLK_F3: Key.f3,
	       SDLK_F4: Key.f4,
	       SDLK_F5: Key.f5,
	       SDLK_F6: Key.f6,
	       SDLK_F7: Key.f7,
	       SDLK_F8: Key.f8,
	       SDLK_F9: Key.f9,
	       SDLK_F10: Key.f10,
	       SDLK_F11: Key.f11,
	       SDLK_F12: Key.f12,

	       SDLK_PRINTSCREEN: Key.printscreen,
	       SDLK_SCROLLLOCK: Key.scrolllock,
	       SDLK_PAUSE: Key.pause,
	       SDLK_INSERT: Key.insert,
	       SDLK_HOME: Key.home,
	       SDLK_PAGEUP: Key.pageup,
	       SDLK_DELETE: Key.key_delete,
	       SDLK_END: Key.end,
	       SDLK_PAGEDOWN: Key.pagedown,
	       SDLK_RIGHT: Key.right,
	       SDLK_LEFT: Key.left,
	       SDLK_DOWN: Key.down,
	       SDLK_UP: Key.up,

	       SDLK_NUMLOCKCLEAR: Key.numlockclear,
	       SDLK_KP_DIVIDE: Key.kp_divide,
	       SDLK_KP_MULTIPLY: Key.kp_multiply,
	       SDLK_KP_MINUS: Key.kp_minus,
	       SDLK_KP_PLUS: Key.kp_plus,
	       SDLK_KP_ENTER: Key.kp_enter,
	       SDLK_KP_1: Key.kp_1,
	       SDLK_KP_2: Key.kp_2,
	       SDLK_KP_3: Key.kp_3,
	       SDLK_KP_4: Key.kp_4,
	       SDLK_KP_5: Key.kp_5,
	       SDLK_KP_6: Key.kp_6,
	       SDLK_KP_7: Key.kp_7,
	       SDLK_KP_8: Key.kp_8,
	       SDLK_KP_9: Key.kp_9,
	       SDLK_KP_0: Key.kp_0,
	       SDLK_KP_PERIOD: Key.kp_period,

	       SDLK_APPLICATION: Key.application,
	       SDLK_POWER: Key.power,
	       SDLK_KP_EQUALS: Key.kp_equals,
	       SDLK_F13: Key.f13,
	       SDLK_F14: Key.f14,
	       SDLK_F15: Key.f15,
	       SDLK_F16: Key.f16,
	       SDLK_F17: Key.f17,
	       SDLK_F18: Key.f18,
	       SDLK_F19: Key.f19,
	       SDLK_F20: Key.f20,
	       SDLK_F21: Key.f21,
	       SDLK_F22: Key.f22,
	       SDLK_F23: Key.f23,
	       SDLK_F24: Key.f24,
	       SDLK_EXECUTE: Key.execute,
	       SDLK_HELP: Key.help,
	       SDLK_MENU: Key.menu,
	       SDLK_SELECT: Key.select,
	       SDLK_STOP: Key.stop,
	       SDLK_AGAIN: Key.again,
	       SDLK_UNDO: Key.undo,
	       SDLK_CUT: Key.cut,
	       SDLK_COPY: Key.copy,
	       SDLK_PASTE: Key.paste,
	       SDLK_FIND: Key.find,
	       SDLK_MUTE: Key.mute,
	       SDLK_VOLUMEUP: Key.volumeup,
	       SDLK_VOLUMEDOWN: Key.volumedown,
	       SDLK_KP_COMMA: Key.kp_comma,
	       SDLK_KP_EQUALSAS400: Key.kp_equalsas400,

	       SDLK_ALTERASE: Key.alterase,
	       SDLK_SYSREQ: Key.sysreq,
	       SDLK_CANCEL: Key.cancel,
	       SDLK_CLEAR: Key.clear,
	       SDLK_PRIOR: Key.prior,
	       SDLK_RETURN2: Key.return2,
	       SDLK_SEPARATOR: Key.separator,
	       SDLK_OUT: Key.key_out,
	       SDLK_OPER: Key.oper,
	       SDLK_CLEARAGAIN: Key.clearagain,
	       SDLK_CRSEL: Key.crsel,
	       SDLK_EXSEL: Key.exsel,

	       SDLK_KP_00: Key.kp_00,
	       SDLK_KP_000: Key.kp_000,
	       SDLK_THOUSANDSSEPARATOR: Key.thousandsseparator,
	       SDLK_DECIMALSEPARATOR: Key.decimalseparator,
	       SDLK_CURRENCYUNIT: Key.currencyunit,
	       SDLK_CURRENCYSUBUNIT: Key.currencysubunit,
	       SDLK_KP_LEFTPAREN: Key.kp_leftparen,
	       SDLK_KP_RIGHTPAREN: Key.kp_rightparen,
	       SDLK_KP_LEFTBRACE: Key.kp_leftbrace,
	       SDLK_KP_RIGHTBRACE: Key.kp_rightbrace,
	       SDLK_KP_TAB: Key.kp_tab,
	       SDLK_KP_BACKSPACE: Key.kp_backspace,
	       SDLK_KP_A: Key.kp_a,
	       SDLK_KP_B: Key.kp_b,
	       SDLK_KP_C: Key.kp_c,
	       SDLK_KP_D: Key.kp_d,
	       SDLK_KP_E: Key.kp_e,
	       SDLK_KP_F: Key.kp_f,
	       SDLK_KP_XOR: Key.kp_xor,
	       SDLK_KP_POWER: Key.kp_power,
	       SDLK_KP_PERCENT: Key.kp_percent,
	       SDLK_KP_LESS: Key.kp_less,
	       SDLK_KP_GREATER: Key.kp_greater,
	       SDLK_KP_AMPERSAND: Key.kp_ampersand,
	       SDLK_KP_DBLAMPERSAND: Key.kp_dblampersand,
	       SDLK_KP_VERTICALBAR: Key.kp_verticalbar,
	       SDLK_KP_DBLVERTICALBAR: Key.kp_dblverticalbar,
	       SDLK_KP_COLON: Key.kp_colon,
	       SDLK_KP_HASH: Key.kp_hash,
	       SDLK_KP_SPACE: Key.kp_space,
	       SDLK_KP_AT: Key.kp_at,
	       SDLK_KP_EXCLAM: Key.kp_exclam,
	       SDLK_KP_MEMSTORE: Key.kp_memstore,
	       SDLK_KP_MEMRECALL: Key.kp_memrecall,
	       SDLK_KP_MEMCLEAR: Key.kp_memclear,
	       SDLK_KP_MEMADD: Key.kp_memadd,
	       SDLK_KP_MEMSUBTRACT: Key.kp_memsubtract,
	       SDLK_KP_MEMMULTIPLY: Key.kp_memmultiply,
	       SDLK_KP_MEMDIVIDE: Key.kp_memdivide,
	       SDLK_KP_PLUSMINUS: Key.kp_plusminus,
	       SDLK_KP_CLEAR: Key.kp_clear,
	       SDLK_KP_CLEARENTRY: Key.kp_clearentry,
	       SDLK_KP_BINARY: Key.kp_binary,
	       SDLK_KP_OCTAL: Key.kp_octal,
	       SDLK_KP_DECIMAL: Key.kp_decimal,
	       SDLK_KP_HEXADECIMAL: Key.kp_hexadecimal,

	       SDLK_LCTRL: Key.lctrl,
	       SDLK_LSHIFT: Key.lshift,
	       SDLK_LALT: Key.lalt,
	       SDLK_LGUI: Key.lgui,
	       SDLK_RCTRL: Key.rctrl,
	       SDLK_RSHIFT: Key.rshift,
	       SDLK_RALT: Key.ralt,
	       SDLK_RGUI: Key.rgui,

	       SDLK_MODE: Key.mode,

	       SDLK_AUDIONEXT: Key.audionext,
	       SDLK_AUDIOPREV: Key.audioprev,
	       SDLK_AUDIOSTOP: Key.audiostop,
	       SDLK_AUDIOPLAY: Key.audioplay,
	       SDLK_AUDIOMUTE: Key.audiomute,
	       SDLK_MEDIASELECT: Key.mediaselect,
	       SDLK_WWW: Key.www,
	       SDLK_MAIL: Key.mail,
	       SDLK_CALCULATOR: Key.calculator,
	       SDLK_COMPUTER: Key.computer,
	       SDLK_AC_SEARCH: Key.ac_search,
	       SDLK_AC_HOME: Key.ac_home,
	       SDLK_AC_BACK: Key.ac_back,
	       SDLK_AC_FORWARD: Key.ac_forward,
	       SDLK_AC_STOP: Key.ac_stop,
	       SDLK_AC_REFRESH: Key.ac_refresh,
	       SDLK_AC_BOOKMARKS: Key.ac_bookmarks,

	       SDLK_BRIGHTNESSDOWN: Key.brightnessdown,
	       SDLK_BRIGHTNESSUP: Key.brightnessup,
	       SDLK_DISPLAYSWITCH: Key.displayswitch,
	       SDLK_KBDILLUMTOGGLE: Key.kbdillumtoggle,
	       SDLK_KBDILLUMDOWN: Key.kbdillumdown,
	       SDLK_KBDILLUMUP: Key.kbdillumup,
	       SDLK_EJECT: Key.eject,
	       SDLK_SLEEP: Key.sleep];
	return lis.get(sdl, Key.unknown);
}
