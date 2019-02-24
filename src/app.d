import std.stdio;
import logging;

void main() {
	string commit_hash = import(".commit_hash.txt");
	log("Currently running FancyEngine2 revision %s", commit_hash);
}
