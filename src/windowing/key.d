module windowing.key;
import stdlib;

struct Event {
	Evtype type;
	union {
		Key key;
		private struct _msev {
			int deltay;
			int deltax;
			int ypos;
			int xpos;
		}
		_msev mouse;
	}
}

enum Evtype {
	// Stateful events.  Including mouse clicks even though it says 'key'
	Keydown, // type: Key
	Keyup, // type: Key

	// Stateless events
	Mousemove, // type: _msev
	Keypress, // type: Key.  Includes scroll wheel (actually currently it's just scroll wheel)
	Quit, // typeless
}

enum Key {
	unknown,

	enter,
	escape,
	backspace,
	tab,
	space,
	exclaim,
	quotedbl,
	hash,
	percent,
	dollar,
	ampersand,
	quote,
	leftparen,
	rightparen,
	asterisk,
	plus,
	comma,
	minus,
	period,
	slash,
	num_0,
	num_1,
	num_2,
	num_3,
	num_4,
	num_5,
	num_6,
	num_7,
	num_8,
	num_9,
	colon,
	semicolon,
	less,
	equals,
	greater,
	question,
	at,
	leftbracket,
	backslash,
	rightbracket,
	caret,
	underscore,
	backquote,
	a,
	b,
	c,
	d,
	e,
	f,
	g,
	h,
	i,
	j,
	k,
	l,
	m,
	n,
	o,
	p,
	q,
	r,
	s,
	t,
	u,
	v,
	w,
	x,
	y,
	z,

	capslock,

	f1,
	f2,
	f3,
	f4,
	f5,
	f6,
	f7,
	f8,
	f9,
	f10,
	f11,
	f12,

	printscreen,
	scrolllock,
	pause,
	insert,
	home,
	pageup,
	key_delete,
	end,
	pagedown,
	right,
	left,
	down,
	up,

	numlockclear,
	kp_divide,
	kp_multiply,
	kp_minus,
	kp_plus,
	kp_enter,
	kp_1,
	kp_2,
	kp_3,
	kp_4,
	kp_5,
	kp_6,
	kp_7,
	kp_8,
	kp_9,
	kp_0,
	kp_period,

	application,
	power,
	kp_equals,
	f13,
	f14,
	f15,
	f16,
	f17,
	f18,
	f19,
	f20,
	f21,
	f22,
	f23,
	f24,
	execute,
	help,
	menu,
	select,
	stop,
	again,
	undo,
	cut,
	copy,
	paste,
	find,
	mute,
	volumeup,
	volumedown,
	kp_comma,
	kp_equalsas400,

	alterase,
	sysreq,
	cancel,
	clear,
	prior,
	return2,
	separator,
	key_out,
	oper,
	clearagain,
	crsel,
	exsel,

	kp_00,
	kp_000,
	thousandsseparator,
	decimalseparator,
	currencyunit,
	currencysubunit,
	kp_leftparen,
	kp_rightparen,
	kp_leftbrace,
	kp_rightbrace,
	kp_tab,
	kp_backspace,
	kp_a,
	kp_b,
	kp_c,
	kp_d,
	kp_e,
	kp_f,
	kp_xor,
	kp_power,
	kp_percent,
	kp_less,
	kp_greater,
	kp_ampersand,
	kp_dblampersand,
	kp_verticalbar,
	kp_dblverticalbar,
	kp_colon,
	kp_hash,
	kp_space,
	kp_at,
	kp_exclam,
	kp_memstore,
	kp_memrecall,
	kp_memclear,
	kp_memadd,
	kp_memsubtract,
	kp_memmultiply,
	kp_memdivide,
	kp_plusminus,
	kp_clear,
	kp_clearentry,
	kp_binary,
	kp_octal,
	kp_decimal,
	kp_hexadecimal,

	lctrl,
	lshift,
	lalt,
	lgui,
	rctrl,
	rshift,
	ralt,
	rgui,

	mode,

	audionext,
	audioprev,
	audiostop,
	audioplay,
	audiomute,
	mediaselect,
	www,
	mail,
	calculator,
	computer,
	ac_search,
	ac_home,
	ac_back,
	ac_forward,
	ac_stop,
	ac_refresh,
	ac_bookmarks,

	brightnessdown,
	brightnessup,
	displayswitch,
	kbdillumtoggle,
	kbdillumdown,
	kbdillumup,
	eject,
	sleep,

	mouse1, //left
	mouse2, //middle
	mouse3, //right
	mouse4, // "x1" (close thumb button)
	mouse5, // "x2" (far thumb button)

	mousewheelup,
	mousewheeldown,
	mousewheelleft,
	mousewheelright,
}

private __gshared string[Key] key_to_string;
private __gshared Key[string] string_to_key;
shared static this() {
	static foreach (asstr; [__traits(allMembers, Key)]) {
		mixin("key_to_string[Key." ~ asstr ~ "] = \"" ~ asstr ~ "\";");
	}
	static foreach (asstr; [__traits(allMembers, Key)]) {
		mixin("string_to_key[\"" ~ asstr ~ "\"] = Key." ~ asstr ~ ";");
	}
}

pragma(inline, true) string tostr(Key key) {
	return key_to_str[key];
}

Key tokey(string str) {
	Key *ret = str in dict;

	return (ret !is null) ? *ret : Key.unknown;
}
