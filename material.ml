open Base

type t =
  | Lambertian of Vec3.t
  | Metal of Vec3.t * float
  | Dialectric of float

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

let schlick cosine refractive_index =
  let open Float.O in
  let r0 = (1.0 - refractive_index) / (1.0 + refractive_index) in
  let r0 = r0 * r0 in
  r0 + (1.0 - r0) * (1.0 - cosine)**5.0

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
  | Dialectric refractive_index ->
      let open Vec3 in
      let open Ray in
      let attenuation = Vec3.one in
      let reflected = reflect ray_in.direction normal in
      let (outward_normal, ni_over_nt, cosine) =
        if Float.O.(dot ray_in.direction normal > 0.0) then
          ( -normal
          , refractive_index
          , Float.O.(refractive_index * dot ray_in.direction normal / length ray_in.direction)
          )
        else
          ( normal
          , Float.O.(1.0 / refractive_index)
          , Float.O.(-dot ray_in.direction normal / length ray_in.direction)
          )
      in
      let (reflect_prob, refracted) =
        match refract ray_in.direction outward_normal ni_over_nt with
        | Some refracted ->
            (schlick cosine refractive_index, refracted)
        | None ->
            (1.0, Vec3.zero)
      in
      if Float.O.(Random.float 1.0 <= reflect_prob) then
        Some { ray = { origin = pos; direction = reflected }; attenuation }
      else
        Some { ray = { origin = pos; direction = refracted }; attenuation }
