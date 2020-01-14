public:
import gfm.math.vector: vec4f, vec3f, vec2f;
import gfm.math.matrix: mat4f, Matrix;
import std.math: sin, cos, tan, PI, sqrt;
mat4f ortho(float left, float right, float bottom, float top) {
	mat4f ret = mat4f.identity();
	//ret.v[] = 1;

	ret.c[0][0] = 2 / (right - left);
	ret.c[1][1] = 2 / (top - bottom);
	ret.c[2][2] = -1;
	ret.c[3][0] = -(right + left) / (right - left);
	ret.c[3][1] = -(top + bottom) / (top - bottom);

	return ret;
}



pragma(inline, true) float sin(int x) {
	return sin(cast(float)x);
}
pragma(inline, true) float to_rad(float deg) {
	return deg * PI / 180;
}
pragma(inline, true) float to_deg(float rad) {
	return rad * 180 / PI;
}
