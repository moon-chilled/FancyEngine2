module graphics.gl_thread;
import stdlib;

import std.concurrency;

struct GlReqStopthread{}
struct GlAckStopthread{}

struct GlReqSyncOp { shared void delegate() fn; Tid sender; }
struct GlAckSyncOp {}

private bool am_gl_thread = false;

private void opengl_thread(Tid parent) {
        import std.concurrency;

	am_gl_thread = true;

	bool done = false;

        while (!done) {
		try {
		receive(
			(GlReqStopthread _) => done = true,
                        (shared void delegate() fn) => fn(),
                        (shared void function() fn) => fn(),
			(GlReqSyncOp fn) {
				fn.fn();
				send(fn.sender, GlAckSyncOp());
			});
		} catch(Throwable t) {
			log("fatal error in glthread (%s)", t);
		}
        }

	send(parent, GlAckStopthread());
}

private __gshared Tid gl_tid;

/+
void gldo(void delegate() fn) {
	if (am_gl_thread)
		fn();
		//fatal("recursive gl");
	else
		send(gl_tid, cast(shared)fn);
}
void gldo(void function() fn) {
	if (am_gl_thread)
		fn();
		//fatal("recursive gl");
	else
		send(gl_tid, cast(shared)fn);
}
+/
void glwait(void delegate() fn) {
	//fn(); return;
	if (am_gl_thread)
		fn();
		//fatal("recursive gl");
	else {
		send(gl_tid, GlReqSyncOp(cast(shared)fn, thisTid));
		receiveOnly!GlAckSyncOp();
	}
}
void glwait(void function() fn) { glwait(toDelegate(fn)); }

void glthread_start() { gl_tid = spawn(&opengl_thread, thisTid); }
shared static ~this() { send(gl_tid, GlReqStopthread()); receiveOnly!GlAckStopthread(); }
