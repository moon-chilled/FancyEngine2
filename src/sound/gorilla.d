module sound.gorilla;
import stdlib;
import cstdlib;
import stdmath;
import asset;

version (Windows) {
	// gorilla wants ldexp, but microsoft doesn't provide it
	extern(C) double scalbn(double x, int exp);
	extern(C) double ldexp(double x, int exp) {
		return scalbn(x, exp);
	}
}

private extern (C) {
	struct SystemOps {
		void *function(uint size) alloc;
		void *function(void *ptr, uint size) realloc;
		void function(void *ptr) free;
	}
	enum ThreadPolicy {
		// there is an unusable 'unknown' with value 0.  For some reason...
		Unknown = 0,
		Single = 1,
		Multi = 2,
	}
	enum TellType {
		Current = 0,
		Total = 1,
	}
	enum DeviceType {
		Default = -1,
		Unknown = 0,
		OpenAL = 1,
		DirectSound = 2,
		XAudio2 = 3,
	}
	enum AudioType {
		Unknown = 1,
		Autodetect = 2,
		OGG = 3,
		WAV = 4,
	}
	enum HandleParam {
		Unknown = 0,
		Pan = 1,
		Pitch = 2,
		Gain = 3,
	}

	alias FinishCallback = void function(Handle *finished_handle, void *context);

	// Opaque typesafe pointers
	// Disallow creating or passing by value
	struct Manager { @disable this(); @disable this(this); }
	struct Mixer { @disable this(); @disable this(this); }
	struct StreamManager { @disable this(); @disable this(this); }
	struct Sound { @disable this(); @disable this(this); }
	struct Handle { @disable this(); @disable this(this); }
	struct Memory { @disable this(); @disable this(this); }
	struct SampleSourceLoop { @disable this(); @disable this(this); }

	int gc_initialize(SystemOps *ops);
	int gc_shutdown();

	Manager *gau_manager_create();
	Manager *gau_manager_create_custom(int dev_type, ThreadPolicy thread_policy, int num_buffers, int buffer_samples);
	void gau_manager_destroy(Manager *mgr);
	void gau_manager_update(Manager *mgr);
	Mixer *gau_manager_mixer(Manager *mgr);
	StreamManager *gau_manager_streamManager(Manager *mgr);
	Sound *gau_load_sound_file(const char *fname, const AudioType format);
	void gau_on_finish_destroy(Handle *finished_handle, void *context);
	int ga_handle_destroy(Handle* in_handle);
	Handle *gau_create_handle_memory(Mixer *mixer, Memory *memory, AudioType format, FinishCallback callback, void *context, SampleSourceLoop **loop_src);
	Handle *gau_create_handle_sound(Mixer *mixer, Sound *sound, FinishCallback callback, void *context, SampleSourceLoop **loop_src);
	Handle* gau_create_handle_buffered_file(Mixer *mixer, StreamManager *in_streamMgr, const char *filename, AudioType format, FinishCallback callback, void *in_context, SampleSourceLoop **loop_src);


	int ga_handle_play(Handle *handle);
	int ga_handle_playing(Handle *handle);
	int ga_handle_tell(Handle* in_handle, TellType in_param);
	int ga_handle_setParamf(Handle *handle, int param, float value);


	void ga_sound_release(Sound *sound);
}

shared static this() {
	gc_initialize(null);
}
shared static ~this() {
	gc_shutdown();
}


private abstract class ISound: Asset {
	package:
	Handle *handle;
	bool live = true;
}

class BufferedSound: ISound {
	AssetType asset_type = AssetType.BufferedSound;
}
class CachedSound: ISound {
	AssetType asset_type = AssetType.CachedSound;
}

class GorillaAudio {
	private Manager *mgr;
	private Mixer *mixer;
	private StreamManager *stream_mgr;
	private vec3f listener;

	this() {
		mgr = gau_manager_create_custom(DeviceType.Default, ThreadPolicy.Multi, 4, 512);
		if (!mgr) fatal("unable to initialize gorilla audio");
		mixer = gau_manager_mixer(mgr);
		if (!mixer) fatal("unable to create a mixer");
		stream_mgr = gau_manager_streamManager(mgr);
		if (!stream_mgr) fatal("unable to make a stream manager");
	}
	~this() {
		gau_manager_destroy(mgr);
	}
	void update(float delta) {
		// TODO: handle delta
		// and listener pos
		gau_manager_update(mgr);
	}

	// TODO: maybe have multiple listeners, 1 for each speaker (depends on speaker config) (ask Guy about this, maybe?)?
	// I sincerely apologize for the punctuation atrocity above
	void set_listener_position(vec3f pos) {
		listener = pos;
	}

	BufferedSound load_buf_sound(string fpath) {
		BufferedSound ret = new BufferedSound();

		Handle *handle = gau_create_handle_buffered_file(mixer, stream_mgr, fpath.cstr, AudioType.OGG, null, null, null);
		if (!handle) { error("unable to load sound '%s'", fpath); return null; }

		ret.handle = handle;
		return ret;
	}

	CachedSound load_cache_sound(string fpath) {
		CachedSound ret = new CachedSound();

		Sound *sound = gau_load_sound_file(fpath.cstr, AudioType.OGG);
		if (!sound) { error("unable to load sound '%s'", fpath); return null; }
		Handle *handle = gau_create_handle_sound(mixer, sound, null, null, null);
		if (!handle) error("unable to process sound '%s'", fpath);

		ret.handle = handle;
		return ret;
	}

	// TODO: use pos
	void play(ISound sound, vec3f pos = vec3f(0, 0, 0)) {
		if (sound is null) {
			error("tried to play null sound!");
			return;
		}
		if (!sound.live) {
			error("tried to play dead sound!");
		}

		int res = ga_handle_play(sound.handle);
		if (res == 0) error("error playing sound");
	}

	void kill(ISound snd) {
		snd.live = false;
		ga_handle_destroy(snd.handle);
	}

	void set_volume(ISound snd, float vol) {
		if (!snd.live) {
			error("tried to set volume of dead sound!");
		}

		ga_handle_setParamf(snd.handle, HandleParam.Gain, vol);
	}
}
