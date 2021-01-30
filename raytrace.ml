open Base

let background ray =
  let open Vec3 in
  let unit_direction = unit ray.Ray.direction in
  let t = Float.O.(0.5 * (unit_direction.y + 1.0)) in
  lerp (make 1.0 1.0 1.0) (make 0.5 0.7 1.0) t

let rec cast_ray ray world depth =
  match Hitable.hit world ray 0.001 Float.max_finite_value with
  | Some { p; normal; material; _ } ->
      if depth < 50 then
        match Material.scatter material ray p normal with
        | Some { ray; attenuation } ->
            Vec3.(attenuation * cast_ray ray world Int.(depth+1))
        | None ->
            Vec3.zero
      else
        Vec3.zero
  | None ->
      background ray

let sample_color camera world x y nx ny =
  let open Float.O in
  let u = (Float.of_int x + Random.float 1.0) / Float.of_int nx in
  let v = (Float.of_int y + Random.float 1.0) / Float.of_int ny in
  let ray = Camera.get_ray camera u v in
  cast_ray ray world 0

let sample_colors camera world x y nx ny n_samples =
  let rec go n_samples acc =
    if n_samples = 0 then
      acc
    else
      go (n_samples-1) Vec3.(acc + sample_color camera world x y nx ny)
  in
  Vec3.(go n_samples zero /. Float.of_int n_samples)

let gamma_correction c =
  Vec3.(make (Float.sqrt c.x) (Float.sqrt c.y) (Float.sqrt c.z))

let () =
  let (nx, ny) = (400, 225) in
  let n_samples = 300 in
  let camera = Scene.setup_camera nx ny in
  let world = Scene.setup_world () in
  Ppm.write nx ny (fun x y ->
    sample_colors camera world x y nx ny n_samples
    |> gamma_correction
  )
