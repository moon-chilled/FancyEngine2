ffi = require("ffi")

ffi.cdef [[
struct vec3 { float v[3]; };
struct mat4 { float v[16]; };
]]

vec3f = ffi.metatype("struct vec3", {
	__add = function(s, o)
		ret = ffi.new("struct vec3")

		ret.v[0] = s.v[0] + o.v[0]
		ret.v[1] = s.v[1] + o.v[1]
		ret.v[2] = s.v[2] + o.v[2]

		return ret
	end,
	__sub = function(s, o)
		ret = ffi.new("struct vec3")

		ret.v[0] = s.v[0] - o.v[0]
		ret.v[1] = s.v[1] - o.v[1]
		ret.v[2] = s.v[2] - o.v[2]

		return ret
	end,
	__mul = function(s, v)
		ret = ffi.new("struct vec3")

		ret.v[0] = s.v[0] * v
		ret.v[1] = s.v[1] * v
		ret.v[2] = s.v[2] * v

		return ret
	end,
	__unm = function(s)
		ret = ffi.new("struct vec3")

		ret.v[0] = -s.v[0]
		ret.v[1] = -s.v[1]
		ret.v[2] = -s.v[2]

		return ret
	end,
	__index = {
		dot = function(s, o)
			return s.v[0]*o.v[0] + s.v[1]*o.v[1] + s.v[2]*o.v[2]
		end,

		cross = function(s, o)
			ret = ffi.new("struct vec3")
			ret.v[0] = s.v[1]*o.v[2] - s.v[2]*o.v[1]
			ret.v[1] = s.v[2]*o.v[0] - s.v[0]*o.v[2]
			ret.v[2] = s.v[0]*r.v[1] - s.v[1]*o.v[0]

			return ret
		end,
		
		normalize = function(s)
			sqmag = s.v[0]*s.v[0] + s.v[1]*s.v[1] + s.v[2]*s.v[2]
			invmag = 1 / math.sqrt(sqmag)

			s.v[0] = s.v[0] * invmag
			s.v[1] = s.v[1] * invmag
			s.v[2] = s.v[2] * invmag
			
			return s
		end,
		
		make = function(a, b, c)
			ret = ffi.new("struct vec3")
			ret.v[0] = a
			ret.v[1] = b
			ret.v[2] = c
			return ret
		end,
	}
})

function vec3f_new(a)
        o = ffi.cast("float(*)[3]", a)[0]
	return vec3f(o)
end


mat4f = ffi.metatype("struct mat4", {
	__index = {
		translate = function(s, v)
			r = ffi.new("struct mat4")

			r.v[0]=s.v[0]; r.v[1]=s.v[1]; r.v[2]=s.v[2]; r.v[3]=s.v[3]+v.v[0];
			r.v[4]=s.v[4]; r.v[5]=s.v[5]; r.v[6]=s.v[6]; r.v[7]=s.v[7]+v.v[1];
			r.v[8]=s.v[8]; r.v[9]=s.v[9]; r.v[10]=s.v[10]; r.v[11]=s.v[11]+v.v[2];
			r.v[12]=s.v[12]; r.v[13]=s.v[13]; r.v[14]=s.v[14]; r.v[15]=s.v[15];

			return r
		end,

		perspective = function(fov, aspect, near, far)
			r = ffi.new("struct mat4")

			f = 1/math.tan(fov/2)
			d = 1/(near-far)

			r.v[0] = f/aspect
			r.v[5] = f
			r.v[10] = d*(near+far)
			r.v[11] = 2*d*far*near
			r.v[14] = -1

			return r
		end,
		
		rotation = function(angle, axis)
			r = ffi.new("struct mat4")

			c = math.cos(angle)
			c_ = 1 - c
			s = math.sin(angle)

			axis:normalize()

			x = axis.v[0]
			y = axis.v[1]
			z = axis.v[2]

			r[0] = (x*x*c_) + c
			r[1] = (x*y*c_) - (z*s)
			r[2] = (x*z*c_) + (y*s)
			r[3] = 0

			r[4] = (y*x*c_) + (z*s)
			r[5] = (y*y*c_) + c
			r[6] = (y*z*c_) - (x*s)
			r[7] = 0

			r[8] = (z*x*c_) - (y*s)
			r[9] = (z*y*c_) + (x*s)
			r[10] = (z*z*c_) + c
			r[11] = 0

			r[12] = 0
			r[13] = 0
			r[14] = 0
			r[15] = 1

			return r
		end,
		lookat = function(eye, target, up)
			Z = eye - target; Z:normalize()
			X = (-up):cross(Z); X:normalize()
			Y = Z:cross(-X)

			r = ffi.new("struct mat4")
			r.v[0] = -X.v[0]
			r.v[1] = -X.v[1]
			r.v[2] = -X.v[2]
			r.v[3] = X:dot(eye)

			r.v[4] = Y.v[0]
			r.v[5] = Y.v[1]
			r.v[6] = Y.v[2]
			r.v[7] = -Y:dot(eye)

			r.v[8] = Z.v[0]
			r.v[9] = Z.v[1]
			r.v[10] = Z.v[2]
			r.v[11] = -Z:dot(eye)

			r.v[12] = 0
			r.v[13] = 0
			r.v[14] = 0
			r.v[15] = 1

			return r
		end,
		identity = function()
			ret = ffi.new("struct mat4")
			ret.v[0] = 1
			ret.v[5] = 1
			ret.v[10] = 1
			ret.v[15] = 1
			return ret
		end
	}
})
function mat4f_new(a)
        o = ffi.cast("float(*)[16]", a)[0]
	return vec3f(o)
end
