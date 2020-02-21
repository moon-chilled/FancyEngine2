--package.path = "dist/lua/?.lua"

--math = require "math"

function log(fmt, ...)
	msg = string.format(fmt, ...)
	formatted = "<lua>" .. msg .. "\n"
	 _real_push_log_msg(3, formatted, formatted)
end

function fe2_new_env()
	-- credit: http://lua-users.org/wiki/CopyTable
	function deepcopy(orig, copies)
		copies = copies or {}
		if type(orig) == 'table' then
			if copies[orig] then
				return copies[orig]
			else
				local copy = {}
				copies[orig] = copy
				for orig_key, orig_value in next, orig, nil do
					copy[deepcopy(orig_key, copies)] = deepcopy(orig_value, copies)
				end
				setmetatable(copy, deepcopy(getmetatable(orig), copies))
				return copy
			end
		else
			return orig
		end
		return copy
	end

	return deepcopy(_G)
end
