#version 330 core

uniform sampler2D font_atlas;

in vec2 tex_coord;

out vec4 FragColor;

void main() {
	FragColor = vec4(1, 1, 1, texture(font_atlas, tex_coord).r);
}
