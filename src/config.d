module config;

import stdlib;
import toml;

struct Table {
	string table_name;
}

void Configure(S...)(string fname, S confs) {
	import std.file: read;

	if (!fname.fexists)
		error("Configuration file '%s' does not exist", fname);

	TOMLDocument toml;

	try {
		toml = parseTOML(cast(string)read(fname));
	} catch (TOMLParserException e) {
		error("Failed to parse toml file '%s'; '%s'", fname, e.msg);
		return;
	}

	string table, index;

	void proxyfunc(T)(T conf) {
		static if (is(typeof(conf) == Table)) {
			table = conf.table_name;
			if (!(table in toml)) {
				error("Table '%s' not in file %s", table, fname);
				throw new Exception("");
			}
		} else static if (is(typeof(conf) == string)) {
			index = conf;
			if (!(index in toml[table])) {
				error("Index '%s' not in table '%s'", index, table);
				throw new Exception("");
			}
		} else {
			_Configure!(typeof(*conf))(toml[table][index], conf);
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

	try {
		mmap!proxyfunc(confs);
	} catch (Throwable t) {
		goto earlyreturn;
	}

earlyreturn:
}

private void _Configure(S)(TOMLValue toml, S *conf) {
	import std.traits: isDynamicArray;
	alias T = typeof(*conf);

	static if (is(T == bool)) {
		if (toml.type == TOML_TYPE.TRUE)
			*conf = true;
		// Pray the speculative execution bugs don't get me!
		else if (toml.type == TOML_TYPE.FALSE)
			*conf = false;
		else
			error("%s was expected to be of boolean type, but was actually %s", toml, toml.type);
	} else static if (is(T == string)) {
		if (toml.type == TOML_TYPE.STRING)
			*conf = toml.str;
		else
			error("%s was expected to be of string type, but was actually %s", toml, toml.type);
	} else static if (isNumeric!T) {
		if (toml.type == TOML_TYPE.INTEGER)
			// cast is because what if you pass in an int but .integer is a long
			*conf = cast(T)toml.integer;
		else
			error("%s was expected to be of integer type, but was actually %s", toml, toml.type);
	} else static if(isFloatingPoint!T) {
		if ((toml.type == TOML_TYPE.FLOAT) || (toml.type == TOML_TYPE.INTEGER))
			// ditto— float and double
			*conf = cast(T)toml.floating;
		else
			error("%s was expected to be of numeric type, but was actually %s", toml, toml.type);
	} else static if (isDynamicArray!T) {
		if (toml.type == TOML_TYPE.ARRAY) {
			TOMLValue[] v = toml.array;
			(*conf).length = v.length;
			foreach (i; 0 .. v.length) {
				_Configure(v[i], (*conf).ptr + i);
			}
		} else {
			error("%s was expected to be array type, but was actually %s", toml, toml.type);
		}
	}
}
