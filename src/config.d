module config;

import stdlib;
import toml;

struct Table {
	string table_name;
}

void Configure(S...)(string fname, S confs) {
	import std.file: read, exists;

	if (!exists(fname))
		fatal("File '%s' does not exist", fname);

	TOMLDocument toml;

	try {
		toml = parseTOML(cast(string)read(fname));
	} catch (TOMLParserException e) {
		fatal("Failed to parse toml file '%s'; '%s'", fname, e.msg);
	}

	string table, index;

	void proxyfunc(T)(T conf) {
		static if (is(typeof(conf) == Table)) {
			table = conf.table_name;
			if (!(table in toml)) {
				fatal("Table '%s' not in file %s", table, fname);
			}
		} else static if (is(typeof(conf) == string)) {
			index = conf;
			if (!(index in toml[table])) {
				fatal("Index '%s' not in table '%s'", index, table);
			}
		} else {
			_Configure!(typeof(*conf))(toml, table, index, conf);
			index = null;
		}
	}

	// loli was here
	template mmap(alias fun) {
		void mmap(T...)(T args) {
			foreach (arg; args)
				fun(arg);
		}
	}

	mmap!proxyfunc(confs);
}

private void _Configure(S)(TOMLDocument toml, string table, string index, S *conf) {
	import std.traits: isNumeric, isFloatingPoint;


	alias T = typeof(*conf);

	static if (is(T == bool)) {
		if (toml[table][index].type == TOML_TYPE.TRUE)
			*conf = true;
		// Pray the speculative execution bugs don't get me!
		else if (toml[table][index].type == TOML_TYPE.FALSE)
			*conf = false;
		else
			fatal("%s.%s was expected to be of boolean type, but was actually %s (%s)", table, index, toml[table][index].type, toml[table][index]);
	} else static if (is(T == string)) {
		if (toml[table][index].type == TOML_TYPE.STRING)
			*conf = toml[table][index].str;
		else
			fatal("%s.%s was expected to be of string type, but was actually %s (%s)", table, index, toml[table][index].type, toml[table][index]);
	} else static if (isNumeric!T) {
		if (toml[table][index].type == TOML_TYPE.INTEGER)
			// cast is because what if you pass in an int but .integer is a long
			*conf = cast(T)toml[table][index].integer;
		else
			fatal("%s.%s was expected to be of integer type, but was actually %s (%s)", table, index, toml[table][index].type, toml[table][index]);
	} else static if(isFloatingPoint!T) {
		if ((toml[table][index].type == TOML_TYPE.FLOAT) || (toml[table][index].type == TOML_TYPE.INTEGER))
			// ditto— float and double
			*conf = cast(T)toml[table][index].floating;
		else
			fatal("%s.%s was expected to be of numeric type, but was actually %s (%s)", table, index, toml[table][index].type, toml[table][index]);
	}
}
