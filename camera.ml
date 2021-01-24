type t =
  { origin            : Vec3.t
  ; lower_left_corner : Vec3.t
  ; horizontal        : Vec3.t
  ; vertical          : Vec3.t
  }

let make () =
  let origin            = Vec3.make   0.0    0.0    0.0  in
  let lower_left_corner = Vec3.make (-2.0) (-1.0) (-1.0) in
  let horizontal        = Vec3.make   4.0    0.0    0.0  in
  let vertical          = Vec3.make   0.0    2.0    0.0  in
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
