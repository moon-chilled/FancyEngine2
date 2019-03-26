public:
import gfm.math.vector: vec2f, vec3f, vec4f, dot, cross, reflect, angle_between = angleBetween;
import gfm.math.matrix: mat4f;

float to_rad(float deg) {
	import std.math: PI;
	return deg * PI / 180;
}
float to_deg(float rad) {
	import std.math: PI;
	return rad * 180 / PI;
}
