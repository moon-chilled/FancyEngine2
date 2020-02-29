module queue;
import stdlib;

import windowing.key;

// TODO: compare-and-swap + lock on failure in undispatch (and dispatch?  Or is that superfluous?  Or should you generate the 'old' in dispatch?)
// TODO: (un)dispatch return bool of success/failure so they can be retried later?
interface Dispatchable {
	void dispatch();
	void undispatch();
}

class GfxClear: Dispatchable {
	import stdmath;
	import windowing.windows;
	GraphicsState gs;
	vec3f colour, old_colour;
	this(GraphicsState gs, vec3f colour, vec3f old_colour) { this.gs = gs; this.colour = colour; this.old_colour = old_colour; }
	void dispatch() { clear(gs, colour.r, colour.g, colour.b); }
	void undispatch() { clear(gs, old_colour.r, old_colour.g, old_colour.b); }
}

class GfxGrabMouse: Dispatchable {
	import windowing.windows;
	GraphicsState gs;
	bool was_grabbed;
	this(GraphicsState gs, bool was_grabbed) { this.gs = gs; this.was_grabbed = was_grabbed; }
	void dispatch() { gs.grab_mouse(); }
	void undispatch() { if (was_grabbed) gs.grab_mouse(); else gs.ungrab_mouse(); }
}
class GfxUngrabMouse: Dispatchable {
	import windowing.windows;
	GraphicsState gs;
	bool was_grabbed;
	this(GraphicsState gs, bool was_grabbed) { this.gs = gs; this.was_grabbed = was_grabbed; }
	void dispatch() { gs.ungrab_mouse(); }
	void undispatch() { if (was_grabbed) gs.grab_mouse(); else gs.ungrab_mouse(); }
}

class SetVar: Dispatchable {
	import scripting;
	ScriptVar[string][string] *dict;
	ScriptVar from, to;
	string namespace, name;

	this(ScriptVar[string][string] *dict, ScriptVar from, ScriptVar to, string namespace, string name) {
		this.dict = dict; this.from = from; this.to = to; this.namespace = namespace; this.name = name;
	}

	void dispatch() { (*dict)[namespace][name] = to; }
	void undispatch() { (*dict)[namespace][name] = from; }
}

struct Mat4fNamePair {
	import stdmath;
	string name;
	mat4f from, to;
}

class ShaderSetMatricesAndDraw: Dispatchable {
	import graphics.shading;
	import graphics.fancy_model;
	Shader shader;
	Mat4fNamePair[] matrices;
	FancyModel model;

	this(Shader shader, Mat4fNamePair[] matrices, FancyModel model) {
		this.shader = shader; this.matrices = matrices; this.model = model;
	}

	void dispatch() {
		shader.enter();
		foreach (m; matrices) {
			shader.set_mat4(m.name, m.to);
		}
		shader.blit(model);
	}

	void undispatch() {
		shader.enter();
		foreach_reverse (m; matrices) {
			shader.set_mat4(m.name, m.from);
		}
		shader.blit(model);
	}
}

private struct ActionQueue {
	Dispatchable[] actions;
	Key[] inputs;
}

class QueueManager {
	private ActionQueue[] past_frames;
	uint past_pointer = 0;
	private ActionQueue current_frame;

	// how many old frames to cache
	this(uint num_cached_frames) {
		past_frames = new ActionQueue[num_cached_frames];
	}

	void enqueue(Dispatchable a) {
		current_frame.actions ~= a;
	}
	void push_key(Key k) {
		current_frame.inputs ~= k;
	}
	void push_keys(Key[] k) {
		current_frame.inputs ~= k;
	}

	Key pop_key() {
		//TODO; push onto cache
		Key ret = current_frame.inputs[0];
		current_frame.inputs = current_frame.inputs[1 .. $];
		return ret;
	}

	void flush_current_frame() {
		// TODO: squash shader uniform settings and push into the cache
		foreach (a; current_frame.actions) a.dispatch();
		current_frame.actions.length = 0;
	}
}
