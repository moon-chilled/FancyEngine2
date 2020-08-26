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
		receive(
			(GlReqStopthread _) => done = true,
                        (shared void delegate() fn) => fn(),
                        (shared void function() fn) => fn(),
			(GlReqSyncOp fn) {
				fn.fn();
				send(fn.sender, GlAckSyncOp());
			});
        }

	send(parent, GlAckStopthread());
}

private __gshared Tid gl_tid;

void gldo(void delegate() fn) { if (am_gl_thread) fatal("recursive gl"); send(gl_tid, cast(shared)fn); }
void gldo(void function() fn) { if (am_gl_thread) fatal("recursive gl"); send(gl_tid, cast(shared)fn); }
void glwait(void delegate() fn) { if (am_gl_thread) fatal("recursive gl"); send(gl_tid, GlReqSyncOp(cast(shared)fn, thisTid)); receiveOnly!GlAckSyncOp(); }
void glwait(void function() fn) { glwait(toDelegate(fn)); }

void glthread_start() { gl_tid = spawn(&opengl_thread, thisTid); }
void glthread_stop() { send(gl_tid, GlReqStopthread()); receiveOnly!GlAckStopthread(); }
