open Base

type t =
  { origin            : Vec3.t
  ; lower_left_corner : Vec3.t
  ; horizontal        : Vec3.t
  ; vertical          : Vec3.t
  }

let make ~look_from ~look_at ~v_up ~vfov ~aspect =
  let open Float.O in
  let theta             = vfov * Float.pi / 180.0 in
  let half_height       = Float.tan (theta*0.5) in
  let half_width        = aspect * half_height in
  let n                 = Vec3.(unit (look_from - look_at)) in
  let u                 = Vec3.(unit (cross v_up n)) in
  let v                 = Vec3.(cross n u) in
  let origin            = look_from in
  let lower_left_corner = Vec3.(origin - half_width *. u - half_height *. v - n) in
  let horizontal        = Vec3.(Float.O.(2.0 * half_width)  *. u) in
  let vertical          = Vec3.(Float.O.(2.0 * half_height) *. v) in
  { origin; lower_left_corner; horizontal; vertical }

let get_ray camera u v : Ray.t =
  { origin = camera.origin
  ; direction =
      Vec3.(
        camera.lower_left_corner
        + u *. camera.horizontal
        + v *. camera.vertical
        - camera.origin
      )
  }
