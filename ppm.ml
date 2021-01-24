open Base
open Stdio

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
