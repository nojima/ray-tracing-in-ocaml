open Base
open Stdio

let write_ppm_header nx ny =
  printf "P3\n";
  printf "%d %d\n" nx ny;
  printf "255\n"

let write_ppm_body nx ny =
  List.iter (List.range (ny-1) (-1) ~stride:(-1)) ~f:(fun y ->
    List.iter (List.range 0 nx) ~f:(fun x ->
      let col = Vec3.make
        (Float.of_int x /. Float.of_int nx)
        (Float.of_int y /. Float.of_int ny)
        0.2
      in
      let ir = Int.of_float (255.99 *. col.x) in
      let ig = Int.of_float (255.99 *. col.y) in
      let ib = Int.of_float (255.99 *. col.z) in
      printf "%d %d %d\n" ir ig ib
    )
  )

let write_ppm nx ny =
  write_ppm_header nx ny;
  write_ppm_body nx ny

let () =
  write_ppm 200 100
