public:
import gfm.math.vector: vec3f;
import gfm.math.matrix: mat4f, Matrix;
import std.math: sin, cos, tan, PI, sqrt;

pragma(inline, true) float sin(int x) {
	return sin(cast(float)x);
}
pragma(inline, true) float to_rad(float deg) {
	return deg * PI / 180;
}
pragma(inline, true) float to_deg(float rad) {
	return rad * 180 / PI;
}
