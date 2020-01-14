#version 330 core

uniform sampler2D font_atlas;

in vec2 tex_coord;

out vec4 FragColor;

void main() {
	FragColor = vec4(1, 1, 1, texture(font_atlas, vec2(tex_coord.x, 1 - tex_coord.y)).r);
	//FragColor = vec4(1, 1, 1, 1) * texture(font_atlas, tex_coord).r;
	//if (tex_coord.x > 1 || tex_coord.x < -1) FragColor = vec4(0,1,1,1);
	//else if (tex_coord.y > 1 || tex_coord.y < -1) FragColor = vec4(1,0,1,1);
	//else FragColor = vec4(0, 0, .5, 1) + texture(font_atlas, vec2(tex_coord.x, 1 - tex_coord.y));
	//FragColor = vec4(1,1,1,1);
}
