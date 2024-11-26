// Based on routines created by Inigo Quilez
// https://iquilezles.org/

float sd_circle(vec2 p, float r) {
  return length(p) - r;
}

float sd_box(vec2 p, vec2 b) {
  vec2 d = abs(p) - b;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
} 

float sd_box_3d(vec3 p, vec3 b, float r) {
  vec3 q = abs(p) - b + r;
  return length(max(q, 0)) + min(max(q.x, max(q.y, q.z)), 0.0) - r;
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

float sd_egg(vec2 p, float ra, float rb) {
  const float k = sqrt(3.0);
  p.x = abs(p.x);
  float r = ra - rb;
  return -rb + ((p.y < 0.0)
    ? length(p) - r
    : (k * (p.x + r) < p.y)
      ? length(p + vec2(0, -k * r))
      : length(p + vec2(r, 0)) - 2.0 * r);
}

float sd_iso_triangle(vec2 p, vec2 q) {
  p.x = abs(p.x);
  vec2 a = p - q * clamp(dot(p, q) / dot(q, q), 0.0, 1.0);
  vec2 b = p - q * vec2(clamp(p.x / q.x, 0.0, 1.0 ), 1.0);
  float s = -sign(q.y);
  vec2 d = min(
    vec2(dot(a, a), s * (p.x * q.y - p.y * q.x)),
    vec2(dot(b, b), s * (p.y - q.y)));
  return -sqrt(d.x) * sign(d.y);
}

float sd_oriented_box(vec2 p, vec2 a, vec2 b, float th) {
  float l = length(b - a);
  vec2 d = (b - a) / l;
  vec2 q = p - (a + b) * 0.5;
  q = mat2(d.x, -d.y, d.y, d.x) * q;
  q = abs(q) - vec2(l, th) * 0.5;
  return length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);
}

float sd_pie(vec2 p, float ap, float r) {
  ap *= 3.1415926535;
  vec2 c = vec2(sin(ap), cos(ap));
  p.x = abs(p.x);
  float l = length(p) - r;
  float m = length(p - c * clamp(dot(p, c), 0.0, r)); // c=sin/cos of aperture
  return max(l, m * sign(c.y * p.x - c.x * p.y));
}

float sd_rhombus(vec2 p, vec2 b) {
  p = abs(p);
  
  vec2 a = b - 2.0 * p;
  float ndot = a.x * b.x - a.y * b.y;

  float h = clamp(ndot / dot(b, b), -1.0, 1.0);
  float d = length(p - 0.5 * b * vec2(1.0 - h, 1.0 + h));
  return d * sign(p.x * b.y + p.y * b.x - b.x * b.y);
}

float sd_rnd_box(vec2 p, vec2 b, vec4 r) {
  r.xy = (p.x > 0.0) ? r.xy : r.zw;
  r.x  = (p.y > 0.0) ? r.x  : r.y;
  vec2 q = abs(p) - b + r.x;
  return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r.x;
}

float sd_rnd_x(vec2 p, float w, float r) {
  p = abs(p);
  return length(p - min(p.x + p.y, w) * 0.5) - r;
}

float sd_segment(vec2 p, vec2 a, vec2 b) {
  vec2 pa = p - a;
  vec2 ba = b - a;
  float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
  return length(pa - ba * h);
}

float sd_uneven_capsule(vec2 p, float r1, float r2, float h) {
  p.x = abs(p.x);
  float b = (r1 - r2) / h;
  float a = sqrt(1.0 - b * b);
  float k = dot(p, vec2(-b, a));
  if (k < 0.0) return length(p) - r1;
  if (k > a * h) return length(p - vec2(0.0, h)) - r2;
  return dot(p, vec2(a, b)) - r1;
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

