open Base

type t =
  | Sphere of Vec3.t * float * Material.t
  | Collection of t list

type record =
  { t : float
  ; p : Vec3.t
  ; normal : Vec3.t
  ; material : Material.t
  }

let rec hit (hitable : t) (ray : Ray.t) (t_min : float) (t_max : float) : record option =
  match hitable with
  | Sphere (center, radius, material) ->
      hit_sphere center radius material ray t_min t_max
  | Collection hitables ->
      hit_collection hitables ray t_min t_max

and hit_sphere center radius material ray t_min t_max =
  let open Ray in
  let open Vec3 in
  let oc = ray.origin - center in
  let a = dot ray.direction ray.direction in
  let open Float.O in
  let b = dot oc ray.direction in
  let c = dot oc oc - radius*radius in
  let discriminant = b*b - a*c in
  if discriminant < 0.0 then
    None
  else
    let t = (-b - Float.sqrt discriminant) / a in
    if t_min < t && t < t_max then
      let p = point_at_parameter ray t in
      let normal = Vec3.((p - center) /. radius) in
      Some { t; p; normal; material }
    else
      let t = (-b + Float.sqrt discriminant) / a in
      if t_min < t && t < t_max then
        let p = point_at_parameter ray t in
        let normal = Vec3.((p - center) /. radius) in
        Some { t; p; normal; material }
      else
        None

and hit_collection hitables ray t_min t_max =
  List.fold hitables ~init:(None, t_max) ~f:(fun (ret, closest_so_far) hitable ->
    match hit hitable ray t_min closest_so_far with
    | None ->
        (ret, closest_so_far)
    | Some record ->
        (Some record, record.t)
  )
  |> fst
