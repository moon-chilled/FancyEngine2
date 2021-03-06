module preload;
import stdlib;
import cstdlib;

import scriptable;

import threaded_script;

import std.concurrency;

import std.variant;

struct AckPreloadEnter{}
struct ReqPreloadExit{}
struct AckPreloadExit{}
struct PreloadScene{Scene s;}
struct UnloadScene{Scene s;}

class Preloader {
	Tid tid;
	shared uint working = 0; // number of tasks on this preloader's queue
}

shared uint thread_num = 0;

void preloader(Tid parent, shared Preloader self) {
	ThreadedScripter.add_this_thread();
	import core.atomic;
	uint my_thread = atomicOp!"+="(thread_num, 1) - 1;
	log("Initialized worker thread #%s", my_thread);
	send(parent, AckPreloadEnter());

	bool done = false;

	while (!done) {
		try {
		receive(
			(ReqPreloadExit r) {
				done = true;
				send(parent, AckPreloadExit());
			},

			(shared(PreloadScene) s) {
				log("asked to preload");
				s.s.loading = true;
				(cast()s.s).preload();
				s.s.loading = false;
				s.s.loaded = true;
				log("did preload");
				atomicOp!"-="(self.working, 1);
			},

			(shared(UnloadScene) s) {
				s.s.loading = true;
				(cast()s.s).unload();
				s.s.loaded = false;
				s.s.loading = false;
				atomicOp!"-="(self.working, 1);
			},
			
			(Variant v) => warning("Got unexpected message of type %s...", v));
		} catch (Throwable t) { log("fatal error in preload: %s", t); }
	}

	log("Out worker thread #%s", my_thread);
}
