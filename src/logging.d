module logging;
import stdlib;
import cstdlib;

import std.stdio: File, stderr;


enum LogLevel: long {
	all, // pseudo-loglevel, just so you can set set_min_log_level(LogLevel.all)
	trace, info, log, warning, error, critical, fatal
}

shared static this() {
	static if (build_type == BuildType.Release) {
		import std.datetime.systime: Clock;
		set_logger_targets([File("fancy_log_" ~ Clock.currTime.toISOString ~ ".txt", "w")]);
	} else {
		set_logger_targets([stderr]);
	}
}

// strip off the final newline
private enum git_commit_hash = import(".commit_hash.txt")[0 .. $-1];
private File[] log_targets;
private LogLevel min_log_level = LogLevel.info; // logs must be >this to be logged
private LogLevel msg_log_level = LogLevel.warning; // if logs are >this, show a message box

void set_logger_targets(File[] files) {
	synchronized log_targets = files;
}
void set_min_log_level(LogLevel ll) {
	synchronized min_log_level = ll;
}
void set_msg_log_level(LogLevel ll) {
	synchronized msg_log_level = ll;
}

import std.exception: basicExceptionCtors;
class FatalAssertionError: Exception {
	mixin basicExceptionCtors;
}

void _real_push_log_msg(LogLevel ll, string str, string basic_str) {
	if (ll >= min_log_level) {
		foreach (target; log_targets) {
			target.write(str);
			target.flush();
		}
	}

	// TODO: globally pause everything?
	if (ll >= msg_log_level) {
		import core.thread: Thread;


		new Thread({
		global_pause_mutex.lock();
		import windowing.windows;

		if (is_sdl_loaded) {
			import bindbc.sdl;
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "ERROR", basic_str.cstr, null);
		} else {
			version(Windows) {
				import core.sys.windows.winuser: MessageBox;
				import std.conv: to;
				MessageBox(null, (basic_str.to!wstring.dup ~ '\0').ptr, null, 0);
			} else version (OSX) {
				import std.stdio: writeln;
				writeln("TODO: print a message on macos");
			} else {
				import core.stdc.stdlib;
				system(cstr("xmessage '" ~ basic_str ~ "'"));
			}
		}
		global_pause_mutex.unlock();
		}).start();
	}

	if (ll == LogLevel.fatal) {
		throw new FatalAssertionError("Fatal message logged: " ~ str);
	}
}


void _real_log(LogLevel ll, int line, string file, string func_name, string pretty_func_name, string module_name, string msg) {
	if (ll < min_log_level) return;

	import std.datetime.systime: Clock;

	auto now = Clock.currTime;

	string formatted_msg = strfmt("\033[34m%04d-%02d-%02dT%02d:%02d.%02dm%03d||%s||%s:%s\033[31m|$\033[0m %s\n", now.year, now.month, now.day, now.hour, now.minute, now.second, now.fracSecs.total!"msecs",
			git_commit_hash,
			file[7 .. $],
			line,
			msg);

	string basicformatted_msg = strfmt("<%s>%s:%s: %s", git_commit_hash, file[7 .. $], line, msg);

	_real_push_log_msg(ll, formatted_msg, basicformatted_msg);
}


// unabashadly stolen from phobos
template log_funf(LogLevel ll) {
	void log_funf(int line = __LINE__, string file = __FILE__,
			string func_name = __FUNCTION__,
			string pretty_func_name = __PRETTY_FUNCTION__,
			string module_name = __MODULE__, A...)(string fmt, A args) {
			_real_log(ll, line, file, func_name, pretty_func_name, module_name, strfmt(fmt, args));
		}
}

alias trace = log_funf!(LogLevel.trace);
alias info = log_funf!(LogLevel.info);
alias log = log_funf!(LogLevel.log);
alias warning = log_funf!(LogLevel.warning);
alias error = log_funf!(LogLevel.error);
alias critical = log_funf!(LogLevel.critical);
alias fatal = log_funf!(LogLevel.fatal);



template log_funs(LogLevel ll) {
	import std.conv: text;

	void log_funs(int line = __LINE__, string file = __FILE__,
			string func_name = __FUNCTION__,
			string pretty_func_name = __PRETTY_FUNCTION__,
			string module_name = __MODULE__, A...)(A args) {
			_real_log(ll, line, file, func_name, pretty_func_name, module_name, text(args));
		}
}

alias traces = log_funs!(LogLevel.trace);
alias infos = log_funs!(LogLevel.info);
alias logs = log_funs!(LogLevel.log);
alias warnings = log_funs!(LogLevel.warning);
alias errors = log_funs!(LogLevel.error);
alias criticals = log_funs!(LogLevel.critical);
alias fatals = log_funs!(LogLevel.fatal);
