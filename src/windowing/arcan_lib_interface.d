module windowing.arcan_lib_interface;

import core.sys.posix.semaphore: sem_t;

extern (C):
public:

arcan_shmif_cont arcan_shmif_open(ARCAN_SEGID type, ARCAN_FLAGS flags, arg_arr**);

shmifext_setup_status arcan_shmifext_setup(arcan_shmif_cont* con, arcan_shmifext_setup arg);
int arcan_shmifext_isext(arcan_shmif_cont* con);
arcan_shmifext_setup arcan_shmifext_defaults(arcan_shmif_cont *con);
void arcan_shmif_drop(struct arcan_shmif_cont*);
bool arcan_shmifext_drop(struct arcan_shmif_cont* con);

alias shmif_pixel = uint; // VIDEO_PIXEL_TYPE
alias shmif_asample = short; // AUDIO_SAMPLE_TYPE
alias file_handle = int;
alias sem_handle = sem_t*
struct arcan_shmif_page;
struct shmif_hidden;
struct shmif_ext_hidden;

struct arcan_shmif_cont {
	arcan_shmif_page *addr;

	/* offset- pointers into addr, can change between calls to shmif_ functions so
	 * aliasing is not recommended, especially important if (default)
	 * connection-crash recovery-reconnect is enabled as the address >may< be
	 * changed. If that is a concern, define a handler using the shmif_resetfunc */
	union {
		shmif_pixel* vidp;
		float* floatp;
		ubyte* vidb;
	}
	union {
		shmif_asample* audp;
		ubyte* audb;
	}

	/*
	 * This cookie is set/kept to some implementation defined value and will be
	 * verified during integrity_check. It is placed here to quickly detect
	 * overflows in video or audio management from client side programming errors
	 */
	short oflow_cookie;

	/* use EITHER [audp, abufpos, abufcount] OR [audb, abufused, abufsize]
	 * to populate the current audio buffer depending on if you are working on
	 * SAMPLES or BYTES. abufpos != 0 will assume the latter */
	ushort abufused, abufpos;
	ushort abufsize, abufcount;

	/* updated on resize, provided to get feedback on an extended resize */
	ubyte abuf_cnt;

	/*
	 * the event handle is provided and used for signal event delivery
	 * in order to allow multiplexation with other input/output sources
	 */
	file_handle epipe;

	/*
	 * Maintain a connection to the shared memory handle in order to handle
	 * resizing (on platforms that support it, otherwise define
	 * ARCAN_SHMIF_OVERCOMMIT which will only recalc pointers on resize
	 */
	file_handle shmh;
	size_t shmsize;

	/*
	 * Used internally for synchronization (and mapped / managed outside
	 * the regular shmpage). system-defined but typically named semaphores.
	 */
	sem_handle vsem, asem, esem;

	/*
	 * Should be used to index vidp, i.e. vidp[y * pitch + x] = RGBA(r, g, b, a)
	 * stride and pitch account for padding, with stride being a row length in
	 * bytes and pitch a row length in pixels.
	 */
	size_t w, h, stride, pitch;

	/*
	 * acknowledged extended attributes in response to an shmif_resize_ext
	 * request. Affects addr->apad and addr->apad_type as well.
	 *
	 * Read only, SYNCH ON EXT_RESIZE
	 */
	uint adata;

	/*
	 * defaults to ARCAN_SHMIF_SAMPLERATE but may be renegotiated as part
	 * of an extended resize. A deviation between the constant samplerate
	 * and the negotiated one will likely lead to resampling server-side.
	 *
	 * Read only, SYNCH ON EXT_RESIZE
	 */
	size_t samplerate;

	/*
	 * Presentation and buffer content / format hints:
	 *
	 * SHMIF_RHINT_ORIGO_UL (or LL),
	 * SHMIF_RHINT_IGNORE_ALPHA
	 * SHMIF_RHINT_SUBREGION (only synch dirty region below)
	 * SHMIF_RHINT_SUBREGION_CHAIN (reserved, not in use)
	 * SHMIF_RHINT_CSPACE_SRGB (non-linear color space)
	 * SHMIF_RHINT_AUTH_TOK
	 * SHMIF_RHINT_VSIGNAL_EV (get frame- delivery notification via STEPFRAME)
	 * SHMIF_RHINT_TPACK (video buffer contents is packed in TPACK format)
	 *
	 * Write only, SYNCH on shmif_resize() calls.
	 */
	ubyte hints;

	/*
	 * IF the constraints:
	 *
	 * [Hints & SHMIF_RHINT_SUBREGION] and (X2>X1,(X2-X1)<=W,Y2>Y1,(Y2-Y1<=H))
	 * valid, [ARCAN] MAY synch only the specified region.
	 * Caller manipulates this field, will be copied to shmpage during synch.
	 *
	 * The [dx, dy] hints inside of the region indicates the number of pixels that
	 * are scrolled based on the previously synched buffer.
	 *
	 * The dirty region is reset on either calls to arcan_shmif_signal (video)
	 * or on shmif_resize calls that impose a size change.
	 */
	arcan_shmif_region dirty;

	/*
	 * The cookie act as overflow monitor and trigger for ABI incompatibilities
	 * between arcan main and program using the shmif library. Combined from
	 * shmpage struct offsets and type sizes. Periodically monitored (using
	 * arcan_shmif_integrity_check calls) and incompatibilities is a terminal
	 * state transition.
	 */
	ulong cookie;

	/*
	 * User-tag, primarily to support attaching ancilliary data to subsegments
	 * that are run and synchronized in separate threads.
	 */
	void* user;

	/*
	 * Opaque struct for implementation defined tracking (guard thread handles
	 * and related data).
	 */
	struct shmif_hidden* priv;
	struct shmif_ext_hidden* privext;

	/*
	 * Copy of the segment token identifier provided on the shmpage at setup
	 */
	uint segment_token;

	/*                                                                                                                                                                                                                                                                            
	 * Video buffers may, based on extended contents hints etc. have a size
	 * that does not match the normal formula of w * h * pitch bytes. Instead,
	 * and length validation for user-side programming is best done against
	 * this field. This represents the size of a single video buffer.
	 */
	size_t vbufsize;
}

/*
 * The following functions are simple lookup/unpack support functions for
 * argument strings usually passed on the command-line to a newly spawned
 * frameserver in a simple (utf-8) key=value\tkey=value type format.
 */
struct arg_arr {
	char* key;
	char* value;
}

enum ARCAN_FLAGS: int {
	SHMIF_NOFLAGS = 0,

	/* by default, the connection IPC resources are unlinked, this
	 * may not always be desired (debugging, monitoring, ...) */
	SHMIF_DONT_UNLINK = 1,

	/* a guard thread is usually allocated to monitor the status of
	 * the server, setting this flag explicitly prevents the creation of
	 * that thread */
	SHMIF_DISABLE_GUARD = 2,

	/* failure to acquire a segment should be exit(EXIT_FAILURE); */
	SHMIF_ACQUIRE_FATALFAIL = 4,

	/* if FATALFAIL, do we have a custom function? should be first argument */
	SHMIF_FATALFAIL_FUNC = 8,

	/* set to sleep- try spin until a connection is established */
	SHMIF_CONNECT_LOOP = 16,

	/* don't implement pause/resume management in backend, forward the
	 * events to frontend */
	SHMIF_MANUAL_PAUSE = 32,

	/* On crash or disconnect, wait and try to reconnect. If successful,
	 * a _RESET event will be enqueued locally with ioev[0].iv == 3.
	 * Subsegments will still be lost, and if the connection has been set-up
	 * inherited+anonymous, this will still exit like normally. Set this
	 * flag to disable RECONNECT attempts entirely. */
	SHMIF_NOAUTO_RECONNECT = 64,

	/* for use as flag input to shmif_migrate calls, the default behavior
	 * is to only permit migration of the primary segment as there are
	 * further client considerations when secondary segments run in different
	 * threads along with the problem if subsegment requests are rejected */
	SHMIF_MIGRATE_SUBSEGMENTS = 128,

	/*
	 * When a connection is initiated, a number of events are gathered until
	 * the connection is activated (see arcan_shmif_initial) to determine
	 * costly properties in advance. By default, this also sets the initial
	 * size of the segment. Set this flag during connection if this behavior
	 * would be ignored/overridden by a manual- "data-source controlled"
	 * size.
	 */
	SHMIF_NOACTIVATE_RESIZE = 256,

	/*
	 * Setting this flag disables the internal management preroll stage entirely
	 */
	SHMIF_NOACTIVATE = 512,

	/* Setting this flag will avoid sending the register event on acquire */
	SHMIF_NOREGISTER = 1024,
}

/*
 * Primarily hinting to the running appl, but can also dictate scheduling
 * groups, priority in terms of resource exhaustion, sandboxing scheme and
 * similar limitations (e.g. TITLEBAR / CURSOR should not update 1080p60Hz)
 * While not currently enforced, it is likely that (with real-world use data),
 * there will be a mask to queuetransfer+enqueue in ARCAN that will react
 * based on SEGID type (statetransfer for TITLEBAR, no..).
 *
 * Special flags (all not enforced yet, will be after hardening phase) :
 * [INPUT] data from server to client
 * [LOCKSTEP] signalling may block indefinitely, appl- controlled
 * [UNIQUE] only one per connection
 * [DESCRIPTOR_PASSING] for target commands where ioev[0] may be populated
 * with a descriptor. shmif_control retains ownership and may close after
 * it has been consumed, so continued use need to be dup:ed.
 *
 * [PREROLL, ACTIVATION] -
 * Preroll / activation is hidden by the shmif implementation and keeps a new
 * connection (not recovery/reset) in an event blocking/ pending state
 * until an activation event is received. This is to make sure that some events
 * that define client rendering properties, like language, dimensions etc. is
 * available from the start and is used to fill out the shmif_cont.
 */
enum ARCAN_SEGID: int {
	/*
	 * New / unclassified segments have this type until the first
	 * _EXTERNAL_REGISTER event has been received. aud/vid signalling is ignored
	 * in this state.
	 */
	SEGID_UNKNOWN = 0,

	/* LIGHTWEIGHT ARCAN (nested execution) */
	SEGID_LWA = 1,

	/* Server->Client exclusive --
	 * External Connection, 1:many */
	SEGID_NETWORK_SERVER = 2,

	/* Server->Client exclusive -- cannot be requested
	 * External Connection, 1:1 */
	SEGID_NETWORK_CLIENT = 3,

	/* External Connection, non-interactive data source. */
	SEGID_MEDIA = 4,

	/* Specifically used to indicate a terminal- emulator connection */
	SEGID_TERMINAL = 5,

	/* External client connection, A/V/latency sensitive */
	SEGID_REMOTING = 6,

	/* [INPUT] High-CPU, Low Latency, data exfiltration risk */
	SEGID_ENCODER = 7,

	/* High-frequency event input, little if any A/V use */
	SEGID_SENSOR = 8,

	/* High-interactivity, CPU load, A/V cost, latency requirements */
	SEGID_GAME = 9,

	/* Input reactive, user- sensitive data */
	SEGID_APPLICATION = 10,

	/* Networking, high-risk for malicious data, aggressive resource consumption */
	SEGID_BROWSER = 11,

	/* Virtual Machine, high-risk for malicious data,
	 * CPU load etc. guarantees support for state- management */
	SEGID_VM = 12,

	/* Head-Mounted display, buffer is split evenly left / right but updated
	 * synchronously */
	SEGID_HMD_SBS = 13,

	/* Head-Mounted display, should be mapped as LEFT view */
	SEGID_HMD_L = 14,

	/* Head-Mounted display, should be mapped as RIGHT view */
	SEGID_HMD_R = 15,

	/*
	 * [LOCKSTEP] Popup-window, use with viewport hints to specify
	 * parent-relative positioning. Labelhints on this message can be used
	 * to send a textual representation for server-side rendering or
	 * accessibility (text-to-speak friendly) data. Prefix message with:
	 * '-' to indicate a group separator.
	 * '*' prefix to indicate the presence of a sublevel
	 * '_' prefix to indicate inactive entry.
	 * '|' terminates / resets the message position.
	 * These can subsequently be activated as a DIGITAL input event with
	 * the subid matching the 0-based index of the item.
	 */
	SEGID_POPUP = 16,

	/*
	 * [UNIQUE] Used for statusbar style visual alert / identification
	 */
	SEGID_ICON = 17,

	/* [UNIQUE] Visual titlebar style for CSD, actual text contents is still
	 * server-side rendered and provided as message on this segment or through
	 * IDENT messages. Server-side rendering of global menus can also be
	 * enabled here by indicating labelhints (and then attaching popups when
	 * the label input arrives) */
	SEGID_TITLEBAR = 18,

	/* [UNIQUE] User- provided cursor, competes with CURSORHINT event. */                                                                  
	SEGID_CURSOR = 19,

	/*
	 * [UNIQUE]
	 * Indicates that this segment is used to propagate accessibility related data;
	 * High-contrast, simplified text, screen-reader friendly. Messages on this
	 * segment should be text-to-speech friendly.
	 *
	 * A reject on such a segment request indicates that no accessibility options
	 * have been enabled and can thus be used as an initial probe.
	 */
	SEGID_ACCESSIBILITY = 20,

	/*
	 * [UNIQUE] Clipboard style data transfers, for image, text or audio sharing.
	 * Can also have streaming transfers using the bchunk mechanism.  Distinction
	 * between Drag'n'Drop and Clipboard state uses the CURSORHINT mechanism.
	 */
	SEGID_CLIPBOARD = 21,

	/*
	 * [INPUT] Incoming clipboard data
	 */
	SEGID_CLIPBOARD_PASTE = 22,

	/*
	 * [UNIQUE] Not expected to have subwindows, no advanced input, no clipboards
	 * but rather a semi-interactive data source that can be rendered and managed
	 * outside the normal window flow.
	 */
	SEGID_WIDGET = 23,


	/*
	 * Used by the shmif_tui support library to indicate a monospaced text user
	 * interface, with known behavior for cut/paste (drag/drop), state transfers,
	 * resize response, font switching, ...
	 */
	SEGID_TUI = 24,

	/*
	 * Used in order to indicate system service integration, exposed as a control
	 * panel, tray or desktop style icon that expose abstract/simplified information
	 * and a control interface to the respective service.
	 */
	SEGID_SERVICE = 25,

	/*
	 * Used to indicate a protocol bridge and root windows
	 * (where applicable and no other segtype or subsegtype- can be spawned).
	 */
	SEGID_BRIDGE_X11 = 26,

	/*      
	 * The current semantics for wayland (subject to change):
	 * note that all subsegment allocations are made relative to the primary
	 * wayland subsegment and not as hierarchical subreq on a subseg.
	 * [ primary segment: SEGID_BRIDGE_WAYLAND ] ->
	 *        subsegment: APPLICATION (message to indicate shell)
	 *        subsegment: MEDIA (act as a subsurface)
	 *        subsegment: POPUP (VIEWPORT hint to show relationships)
	 *        subsegment: CURSOR (bound to a "seat", we always have 1:1)
	 *        subsegment: CLIPBOARD (container for DnD, ...)
	 *        subsegment: ICON (can reparent to application or clipboard for DnD)
	 *        subsegment: SEGID_BRIDGE_X11 (Xwayland surfaces)
	 */     
	SEGID_BRIDGE_WAYLAND = 27,

	/*
	 * Used as a forwarding primitive, meaning that the current connection will act
	 * as a mediator / router for a new connection, so the subsegment will not be a
	 * proper subsegment but internally promoted to a primary one. The management
	 * scripts should suspend the source segment if this is accepted, and keep it
	 * suspended until the new segment terminates. Other behavior should simply
	 * create a normal connection point and forward connection data there.
	 */
	SEGID_HANDOVER = 28,

	/* Can always be terminated without risk, may be stored as part of debug format
	 * in terms of unexpected termination etc. */
	SEGID_DEBUG = 255,

	SEGID_LIM = int.max
}

enum shmifext_setup_status: int {
	SHHIFEXT_UNKNOWN = 0,
	SHMIFEXT_NO_API,
	SHMIFEXT_NO_DISPLAY,
	SHMIFEXT_NO_EGL,
	SHMIFEXT_NO_CONFIG,
	SHMIFEXT_NO_CONTEXT,
	SHMIFEXT_ALREADY_SETUP,
	SHMIFEXT_OUT_OF_MEMORY,
	SHMIFEXT_OK
}


struct arcan_shmif_region {
        ushort x1, x2, y1, y2;
}

struct arcan_shmifext_setup {
	ubyte red, green, blue, alpha, depth;
	ubyte api, major, minor;
	ulong flags;
	ulong mask;

	/* 0 for self-managed fbo
	 * >0 for internal rendertarget that swaps out */
	ubyte builtin_fbo;

	ubyte supersample;
	ubyte stencil;
	ubyte no_context;
	ulong shared_context;

	/* deprecated members, but don't want to break abi, while still
	 * generating compiler visible errors for api break */
	ubyte deprecated_1;
	uint deprecated_2;

	/* workaround for versioning snafu with _setup not taking sizeof(...) */
	ubyte[6] uintfl_reserve;

	ulong[4] reserved;
}

enum shmifext_api {
	API_OPENGL = 0,
	API_GLES,
	API_VHK
}


/+
struct arcan_shmif_initial {
/* pre-configured primary font and possible fallback, remember to convert
 * to point size to account for density (interop macro
 * SHMIF_PT_SIZE(ppcm, sz_mm) */
        struct {
                int fd;
                int type;
                int hinting;
                float size_mm;
        } fonts[4];

/* output display density and LED- layout hint for subpixel hinting */
        float density;
        int rgb_layout;

/* maximum display output dimensions */
        size_t display_width_px;
        size_t display_height_px;
        ushort rate;

        ubyte lang[4], country[4], text_lang[4];
        float latitude, longitude, elevation;

/* device to use for 3D acceleration */
        int render_node;

/* UTC + */
        int timezone;
}
+/
