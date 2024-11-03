#version 450
#extension GL_GOOGLE_include_directive : require
#include "inigo.glsl"

layout(location = 0) in vec2 pos;
layout(location = 0) out vec4 colour;

void main() {
  float d = sd_circle(pos, 0.5);
  vec3 c = inigo_debug(d);
  colour = vec4(c, 1);
}
