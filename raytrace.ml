open Base
open Stdio

module PPM : sig

  val write : int -> int -> (int -> int -> Vec3.t) -> unit

end = struct

  let write_header nx ny =
    printf "P3\n";
    printf "%d %d\n" nx ny;
    printf "255\n"

  let write_body nx ny plotter =
    List.iter (List.range (ny-1) (-1) ~stride:(-1)) ~f:(fun y ->
      List.iter (List.range 0 nx) ~f:(fun x ->
        let color = plotter x y in
        let ir = Int.of_float (255.99 *. color.Vec3.x) in
        let ig = Int.of_float (255.99 *. color.Vec3.y) in
        let ib = Int.of_float (255.99 *. color.Vec3.z) in
        printf "%d %d %d\n" ir ig ib
      )
    )

  let write nx ny plotter =
    write_header nx ny;
    write_body nx ny plotter

end

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
      let open Vec3 in
      let unit_direction = unit ray.direction in
      let t = Float.O.(0.5 * (unit_direction.y + 1.0)) in
      lerp (make 1.0 1.0 1.0) (make 0.5 0.7 1.0) t

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
  let open Vec3 in
  make (Float.sqrt c.x) (Float.sqrt c.y) (Float.sqrt c.z)

let () =
  let (nx, ny) = (200, 100) in
  let n_samples = 100 in
  let camera = Camera.make () in
  let world = Hitable.Collection
    [ Hitable.Sphere (Vec3.make   0.0      0.0  (-1.0),   0.5, Material.Lambertian (Vec3.make 0.8 0.3 0.3))
    ; Hitable.Sphere (Vec3.make   0.0  (-100.5) (-1.0), 100.0, Material.Lambertian (Vec3.make 0.8 0.8 0.0))
    ; Hitable.Sphere (Vec3.make   1.0      0.0  (-1.0),   0.5, Material.Metal (Vec3.make 0.8 0.6 0.2, 1.0))
    ; Hitable.Sphere (Vec3.make (-1.0)     0.0  (-1.0),   0.5, Material.Metal (Vec3.make 0.8 0.8 0.8, 0.3))
    ]
  in
  PPM.write nx ny (fun x y ->
    sample_colors camera world x y nx ny n_samples
    |> gamma_correction
  )
