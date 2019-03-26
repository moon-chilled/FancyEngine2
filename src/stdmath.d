public:
import gfm.math.vector: vec2f, vec3f, vec4f, dot, cross, reflect, angle_between = angleBetween;
import gfm.math.matrix: mat4f;
import std.math: sin, cos, tan, PI;

pragma(inline, true) float sin(int x) {
	return sin(cast(float)x);
}
pragma(inline, true) float to_rad(float deg) {
	return deg * PI / 180;
}
pragma(inline, true) float to_deg(float rad) {
	return rad * 180 / PI;
}
