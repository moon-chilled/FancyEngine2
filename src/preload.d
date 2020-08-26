module preload;
import stdlib;
import cstdlib;

import scriptable;

import std.concurrency;

import std.variant;

struct AckPreloadEnter{}
struct ReqPreloadExit{}
struct AckPreloadExit{}
struct AckPreloadScene{}
struct AckUnloadScene{}
struct PreloadScene{Scene s;}
struct UnloadScene{Scene s;}

class Preloader {
	Tid tid;
	shared uint working = 0; // number of tasks on this preloader's queue
}

shared uint thread_num = 0;

void preloader(Tid parent, shared Preloader self) {
	import core.atomic;
	uint my_thread = atomicOp!"+="(thread_num, 1) - 1;
	log("Initialized worker thread #%s", my_thread);

	bool done = false;

	send(parent, AckPreloadEnter());

	while (!done) {
		receive(
			(ReqPreloadExit r) {
				done = true;
				send(parent, AckPreloadExit());
			},

			(shared(PreloadScene) s) {
				log("asked to preload");
				s.s.loading = true;
				send(parent, AckPreloadScene());
				(cast()s.s).preload();
				s.s.loading = false;
				s.s.loaded = true;
				log("did preload");
				atomicOp!"-="(self.working, 1);
			},

			(shared(UnloadScene) s) {
				s.s.loading = true;
				send(parent, AckUnloadScene());
				(cast()s.s).unload();
				s.s.loaded = false;
				s.s.loading = false;
				atomicOp!"-="(self.working, 1);
			},
			
			(Variant v) => fatal("Got unexpected message of type %s...", v));
	}

	log("Out worker thread #%s", my_thread);
}
