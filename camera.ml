open Base

type t =
  { origin            : Vec3.t
  ; lower_left_corner : Vec3.t
  ; horizontal        : Vec3.t
  ; vertical          : Vec3.t
  ; u                 : Vec3.t
  ; v                 : Vec3.t
  ; n                 : Vec3.t
  ; lens_radius       : float
  }

let make ~look_from ~look_at ~v_up ~vfov ~aspect ~aperture ~focus_dist =
  let open Float.O in
  let lens_radius       = aperture * 0.5 in
  let theta             = vfov * Float.pi / 180.0 in
  let half_height       = Float.tan (theta*0.5) in
  let half_width        = aspect * half_height in
  let n                 = Vec3.(unit (look_from - look_at)) in
  let u                 = Vec3.(unit (cross v_up n)) in
  let v                 = Vec3.(cross n u) in
  let origin            = look_from in
  let horizontal        = Vec3.(Float.O.(2.0 * focus_dist * half_width)  *. u) in
  let vertical          = Vec3.(Float.O.(2.0 * focus_dist * half_height) *. v) in
  let lower_left_corner =
    Vec3.(origin - Float.O.(half_width  * focus_dist) *. u
                 - Float.O.(half_height * focus_dist) *. v
                 - focus_dist *. n)
  in
  { origin; lower_left_corner; horizontal; vertical; u; v; n; lens_radius }

let rec random_in_unit_disk () =
  let x = Random.float_range (-1.0) 1.0 in
  let y = Random.float_range (-1.0) 1.0 in
  let open Float.O in
  if x*x + y*y < 1.0 then
    Vec3.make x y 0.0
  else
    random_in_unit_disk ()

let get_ray camera s t : Ray.t =
  let open Vec3 in
  let rd = camera.lens_radius *. random_in_unit_disk () in
  let offset = rd.x *. camera.u + rd.y *. camera.v in
  { origin = camera.origin + offset
  ; direction = camera.lower_left_corner
                + s *. camera.horizontal
                + t *. camera.vertical
                - (camera.origin + offset)
  }
