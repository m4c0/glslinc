// Based on routines created by Inigo Quilez
// https://iquilezles.org/

float sd_circle(vec2 p, float r) {
  return length(p) - r;
}

float sd_box(vec2 p, vec2 b) {
  vec2 d = abs(p) - b;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

float sd_cut_disk(vec2 p, float r, float h) {
  float w = sqrt(r * r - h * h); // constant for any given shape
  p.x = abs(p.x);

  float s = max(
    (h - r) * p.x * p.x + w * w * (h + r - 2.0 * p.y),
    h * p.x - w * p.y 
  );
  return (s < 0.0) 
    ? length(p) - r
    : (p.x < w)
      ? h - p.y
      : length(p - vec2(w, h));
}

float sd_rhombus(vec2 p, vec2 b) {
  p = abs(p);
  
  vec2 a = b - 2.0 * p;
  float ndot = a.x * b.x - a.y * b.y;

  float h = clamp(ndot / dot(b, b), -1.0, 1.0);
  float d = length(p - 0.5 * b * vec2(1.0 - h, 1.0 + h));
  return d * sign(p.x * b.y + p.y * b.x - b.x * b.y);
}

float sd_rnd_box(vec2 p, vec2 b, float r) {
  return sd_box(p, b) - r;
}

float sd_rnd_x(vec2 p, float w, float r) {
  p = abs(p);
  return length(p - min(p.x + p.y, w) * 0.5) - r;
}

float sd_tunnel(vec2 p, vec2 wh) {
  p.x = abs(p.x);
  p.y = -p.y;

  vec2 q = p - wh;

  vec2 v1 = vec2(max(q.x, 0.0), q.y);
  float d1 = dot(v1, v1);
  q.x = (p.y > 0.0) ? q.x : length(p) - wh.x;

  vec2 v2 = vec2(q.x, max(q.y, 0.0));
  float d2 = dot(v2, v2);
  float d = sqrt(min(d1, d2));
  return (max(q.x, q.y) < 0.0) ? -d : d;
}

vec3 inigo_debug(float d) {
  // this is 2.0 / resolution.y in Shader Toy. Impacts line thickness.
  const float px = 2.0 / 600.0;

  vec3 col = (d>0.0) ? vec3(0.9,0.6,0.3) : vec3(0.65,0.85,1.0);
  col *= 1.0 - exp2(-24.0*abs(d));
  col *= 0.8 + 0.2*cos(120.0*d);
  col = mix( col, vec3(1.0), 1.0-smoothstep(-px,px,abs(d)-0.005) );
  return pow(col, vec3(2.2));
}

