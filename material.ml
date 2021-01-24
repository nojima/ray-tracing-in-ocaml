open Base

type t =
  | Lambertian of Vec3.t
  | Metal of Vec3.t * float

type scattered =
  { ray : Ray.t
  ; attenuation : Vec3.t
  }

let rec random_in_unit_sphere () =
  let x = Random.float_range (-1.0) 1.0 in
  let y = Random.float_range (-1.0) 1.0 in
  let z = Random.float_range (-1.0) 1.0 in
  let open Float.O in
  if x*x + y*y + z*z < 1.0 then
    Vec3.make x y z
  else
    random_in_unit_sphere ()

let scatter material ray_in pos normal =
  match material with
  | Lambertian albedo ->
      let open Vec3 in
      let target = pos + normal + random_in_unit_sphere () in
      Some { ray = { origin = pos; direction = target - pos }
           ; attenuation = albedo
           }
  | Metal (albedo, fuzz) ->
      let open Vec3 in
      let open Ray in
      let reflected = reflect (unit ray_in.direction) normal in
      let scattered_direction = reflected + fuzz *. random_in_unit_sphere () in
      if Float.O.(Vec3.dot scattered_direction normal > 0.0) then
        Some { ray = { origin = pos; direction = scattered_direction }
             ; attenuation = albedo
             }
      else
        None
