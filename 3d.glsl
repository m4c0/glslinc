mat4 translation(vec3 p) {
  return mat4(
    vec4(1, 0, 0, p.x),
    vec4(0, 1, 0, p.y),
    vec4(0, 0, 1, p.z),
    vec4(0, 0, 0, 1)
  );
}

mat4 projection_matrix(float fov_rad, float aspect, float near, float far) {
  const float f = 1 / atan(fov_rad / 2);
  const float p22 = (far + near) / (near - far);
  const float p32 = (2.0 * far * near) / (near - far);
  return mat4(
    f / aspect, 0.0, 0.0, 0.0,
    0.0, f, 0.0, 0.0,
    0.0, 0.0, p22, p32,
    0.0, 0.0, -1, 0.0
  );
}

mat4 view_matrix(vec3 cam, float a, vec3 up) {
  vec3 f = normalize(vec3(sin(a), 0, cos(a)));
  vec3 s = normalize(cross(f, up));
  vec3 u = cross(s, f);
  mat4 trn = translation(-cam);
  mat4 rot = mat4(
    vec4( s, 0),
    vec4( u, 0),
    vec4(-f, 0),
    vec4(0, 0, 0, 1)
  );
  return trn * rot;
}
