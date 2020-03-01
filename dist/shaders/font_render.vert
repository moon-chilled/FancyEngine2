#version 330 core
layout (location = 0) in vec2 in_position;
layout (location = 1) in vec2 in_tex_coord;

out vec2 tex_coord;

void main() {
	gl_Position = vec4(vec2(in_position.x, in_position.y), 1, 1);
	tex_coord = in_tex_coord;
}
