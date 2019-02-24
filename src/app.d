import std.stdio;

void main() {
	string commit_hash = import("commit_hash.txt");
	writefln("Currently running FancyEngine2 revision %s", commit_hash);
}
