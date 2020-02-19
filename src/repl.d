module repl;
import stdlib;

import core.thread;

import scriptable;

import std.socket;

enum port = 8844;

class Repl: Thread {
	ScriptManager man;
	string current_lang;
	TcpSocket listener;
	bool *done;

	this(ScriptManager man, string s, bool *done) {
		this.man = man;
		this.current_lang = s;
		this.done = done;

		listener = new TcpSocket();
		listener.blocking = true;
		listener.bind(new InternetAddress(port));
		listener.listen(1);

		log("Started repl on port %s ('telnet localhost %s')", port, port);


		super(&run);
	}

	void run() {
outerloop:	while (true && !*done) {
			log("ruckus");
			Socket sock = listener.accept();

			//TODO: readline
			//TODO: paren/brace matching
			while (true) {
				sock.send(current_lang ~ ": ");

				char[1024] buf;
				ptrdiff_t readb = sock.receive(buf);
				if (!readb || *done) {
					sock.shutdown(SocketShutdown.BOTH);
					sock.close();
					log("Closed remote connection");
					break;
				}
				string s = buf[0 .. readb].idup;

				if (s.split[0] == "@quit") break outerloop;

				//global_pause_mutex.lock();
				sock.send(man.languages[current_lang].eval_to_str(s) ~ '\n');
				//global_pause_mutex.unlock();
			}
		}

	}

	~this() {
		listener.shutdown(SocketShutdown.BOTH);
		listener.close();
		log("Shut down REPL server");
	}
}
