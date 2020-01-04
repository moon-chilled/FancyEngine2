function log(fmt, ...)
	msg = string.format(fmt, ...)
	formatted = "<lua>" .. msg .. "\n"
	 _real_push_log_msg(3, formatted, formatted)
end
