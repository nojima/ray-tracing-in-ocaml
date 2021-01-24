type t =
  { origin : Vec3.t
  ; direction : Vec3.t
  }

let point_at_parameter ray t =
  Vec3.(ray.origin + t *. ray.direction)
