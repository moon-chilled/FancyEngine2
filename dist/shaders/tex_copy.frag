#version 330 core
out vec4 frag_color;
in vec2 tex_coord;
uniform sampler2D screen_tex;

void main() {
	frag_color = texture(screen_tex, vec2(tex_coord.x, 1 - tex_coord.y));
}
