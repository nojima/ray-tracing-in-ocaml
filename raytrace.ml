open Base
open Stdio

module PPM : sig

  val write : int -> int -> (float -> float -> Vec3.t) -> unit

end = struct

  let write_header nx ny =
    printf "P3\n";
    printf "%d %d\n" nx ny;
    printf "255\n"

  let write_body nx ny plotter =
    List.iter (List.range (ny-1) (-1) ~stride:(-1)) ~f:(fun y ->
      List.iter (List.range 0 nx) ~f:(fun x ->
        let color = plotter (Float.of_int x /. Float.of_int nx)
                            (Float.of_int y /. Float.of_int ny)
        in
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

let lerp a b t =
  Vec3.((1.0 -. t) *. a + t *. b)

let hit_sphere center radius ray =
  let open Ray in
  let open Vec3 in
  let oc = ray.origin - center in
  let a = dot ray.direction ray.direction in
  let b = Float.O.(2.0 * dot oc ray.direction) in
  let c = Float.O.(dot oc oc - radius*radius) in
  let discriminant = Float.O.(b*b - 4.0*a*c) in
  Float.O.(discriminant > 0.0)

let color_of ray =
  if hit_sphere (Vec3.make 0.0 0.0 (-1.0)) 0.5 ray then
    Vec3.make 1.0 0.0 0.0
  else
    let unit_direction = Vec3.unit ray.Ray.direction in
    let t = unit_direction.Vec3.y +. 1.0 in
    lerp (Vec3.make 1.0 1.0 1.0) (Vec3.make 0.5 0.7 1.0) t

let () =
  let origin            = Vec3.make   0.0    0.0    0.0  in
  let lower_left_corner = Vec3.make (-2.0) (-1.0) (-1.0) in
  let horizontal        = Vec3.make   4.0    0.0    0.0  in
  let vertical          = Vec3.make   0.0    2.0    0.0  in
  PPM.write 200 100 (fun u v ->
    let ray = Ray.make origin Vec3.(lower_left_corner + u*.horizontal + v*.vertical) in
    color_of ray
  )
