module logging;
import stdlib;

import std.stdio: File, stderr;
import std.string: format;


enum LogLevel {
	all, // pseudo-loglevel, just so you can set set_min_log_level(LogLevel.all)
	trace, info, log, warning, error, critical, fatal
}

shared static this() {
	version (dev)
		set_logger_targets([stderr]);
	else version (release) {
		import std.datetime.systime: Clock;
		set_logger_targets([File("fancy_log_" ~ Clock.currTime.toISOString ~ ".txt", "w")]);
	}
}

private File[] log_targets;
private LogLevel min_log_level = LogLevel.trace; // logs must be >this to be logged
private LogLevel msg_log_level = LogLevel.error; // if logs are >this, show a message box

void set_logger_targets(File[] files) {
	synchronized log_targets = files;
}
void set_min_log_level(LogLevel ll) {
	synchronized min_log_level = ll;
}
void set_msg_log_level(LogLevel ll) {
	synchronized msg_log_level = ll;
}

void handle_log(LogLevel ll, int line, string file, string func_name, string pretty_func_name, string module_name, string msg) {
	import std.datetime.systime: Clock;

	string formatted_msg = format("\033[34m%s||%s||%s:%s\033[31m|$\033[0m %s\n", Clock.currTime.toISOExtString(),
			import(".commit_hash.txt")[0 .. $-1],
			file,
			line,
			msg);

	if (ll >= min_log_level) {
		foreach (target; log_targets) {
			target.write(formatted_msg);
			target.flush();
		}
	}

	// TODO: globally pause everything?
	if (ll >= msg_log_level) {
		import core.thread: Thread;


		new Thread({
		import windowing.windows;
		import std.string: toStringz;

		string msgformatted_msg = format("An error was encountered!  Please report this to the developers:\n<%s>%s:%s: %s", import(".commit_hash.txt")[0 .. $-1], file, line, msg);
		if (are_libraries_loaded) {
			import derelict.sdl2.sdl;
			SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "ERROR", msgformatted_msg.toStringz, null);
		} else {
			version(Windows) {
				import core.sys.windows.winuser: MessageBox;
				import std.conv: to;
				MessageBox(null, (msgformatted_msg.to!wstring.dup ~ '\0').ptr, null, 0);
			} else version (OSX) {
				import std.stdio: writeln;
				writeln("TODO: print a message on macos");
			} else {
				import core.stdc.stdlib;
				system(toStringz("xmessage '" ~ msgformatted_msg ~ "'"));
			}
		}
		}).start();
	}

	if (ll == LogLevel.fatal) {
		throw new Exception("Fatal message logged: " ~ formatted_msg);
	}
}


// unabashadly stolen from phobos
template log_funf(LogLevel ll) {
	void log_funf(int line = __LINE__, string file = __FILE__,
			string func_name = __FUNCTION__,
			string pretty_func_name = __PRETTY_FUNCTION__,
			string module_name = __MODULE__, A...)
		(string msg, A args) {
			handle_log(ll, line, file, func_name, pretty_func_name, module_name, format(msg, args));
		}

	void log_funf(int line = __LINE__, string file = __FILE__,
			string func_name = __FUNCTION__,
			string pretty_func_name = __PRETTY_FUNCTION__,
			string module_name = __MODULE__, A...)
		(bool condition, string msg, A args) {
			if (condition) {
				handle_log(ll, line, file, func_name, pretty_func_name, module_name, format(msg, args));
			}
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
			string module_name = __MODULE__, A...)
		(A args) if ((args.length > 0 && !is(Unqual!(A[0]) : bool)) || args.length == 0) {
			handle_log(ll, line, file, func_name, pretty_func_name, module_name, text(args));
		}

	void log_funs(int line = __LINE__, string file = __FILE__,
			string func_name = __FUNCTION__,
			string pretty_func_name = __PRETTY_FUNCTION__,
			string module_name = __MODULE__, A...)
		(bool condition, A args) {
			if (condition) {
				handle_log(ll, line, file, func_name, pretty_func_name, module_name, format(msg, args));
			}
		}
}

alias traces = log_funs!(LogLevel.trace);
alias infos = log_funs!(LogLevel.info);
alias logs = log_funs!(LogLevel.log);
alias warnings = log_funs!(LogLevel.warning);
alias errors = log_funs!(LogLevel.error);
alias criticals = log_funs!(LogLevel.critical);
alias fatals = log_funs!(LogLevel.fatal);
