open Base

let setup_camera nx ny =
  Camera.make
    ~look_from:  (Vec3.make  10.0  2.0  3.0)
    ~look_at:    (Vec3.make   0.0  0.0  0.0)
    ~v_up:       (Vec3.make   0.0  1.0  0.0)
    ~vfov:       30.0
    ~aspect:     (Float.of_int nx /. Float.of_int ny)
    ~aperture:   0.1
    ~focus_dist: (Float.sqrt 113.0)

let random_spheres () =
  let open List.Monad_infix in
  List.range (-11) 11 >>= fun a ->
    List.range (-11) 11 >>= fun b ->
      let open Float.O in
      let (a, b) = (Float.of_int a, Float.of_int b) in
      let center = Vec3.make (a + Random.float 0.9)
                             0.2
                             (b + Random.float 0.9)
      in
      if Vec3.(length (center - Vec3.make 4.0 0.2 0.0)) <= 0.9 then
        []
      else
        let r = Random.float 1.0 in
        if r < 0.8 then (* diffuse *)
          let material = Material.Lambertian
            ( Vec3.make (Random.float 1.0 * Random.float 1.0)
                        (Random.float 1.0 * Random.float 1.0)
                        (Random.float 1.0 * Random.float 1.0)
            )
          in
          List.return (Hitable.Sphere (center, 0.2, material))
        else if r < 0.95 then (* metal *)
          let material = Material.Metal
            ( Vec3.make (0.5 * Random.float 0.5)
                        (0.5 * Random.float 0.5)
                        (0.5 * Random.float 0.5)
            , Random.float 0.5
            )
          in
          List.return (Hitable.Sphere (center, 0.2, material))
        else (* glass *)
          let material = Material.Dialectric 1.5 in
          List.return (Hitable.Sphere (center, 0.2, material))

let fixed_spheres () =
    [ Hitable.Sphere (Vec3.make   0.0  (-1000.0)   0.0, 1000.0, Material.Lambertian (Vec3.make 0.5 0.5 0.5))
    ; Hitable.Sphere (Vec3.make   0.0       1.0    0.0,    1.0, Material.Dialectric 1.5)
    ; Hitable.Sphere (Vec3.make (-4.0)      1.0    0.0,    1.0, Material.Lambertian (Vec3.make 0.4 0.2 0.1))
    ; Hitable.Sphere (Vec3.make   4.0       1.0    0.0,    1.0, Material.Metal (Vec3.make 0.7 0.6 0.5, 0.0))
    ]

let setup_world () =
  Hitable.Collection (List.rev_append (fixed_spheres ()) (random_spheres ()))
