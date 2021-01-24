open Base

type t = {
  x: float;
  y: float;
  z: float;
}

let make x y z =
  { x = x
  ; y = y
  ; z = z
  }

let (~-) v =
  { x = Float.neg v.x
  ; y = Float.neg v.y
  ; z = Float.neg v.z
  }

let (+) v w =
  { x = v.x +. w.x
  ; y = v.y +. w.y
  ; z = v.z +. w.z
  }

let (-) v w =
  { x = v.x -. w.x
  ; y = v.y -. w.y
  ; z = v.z -. w.z
  }

let ( * ) v w =
  { x = v.x *. w.x
  ; y = v.y *. w.y
  ; z = v.z *. w.z
  }

let (/) v w =
  { x = v.x /. w.x
  ; y = v.y /. w.y
  ; z = v.z /. w.z
  }

let ( *. ) f v =
  { x = f *. v.x
  ; y = f *. v.y
  ; z = f *. v.z
  }

let ( /. ) v f =
  { x = v.x /. f
  ; y = v.y /. f
  ; z = v.z /. f
  }

let length v =
  let open Float.O in
  Float.sqrt (v.x*v.x + v.y*v.y + v.z*v.z)

let squared_length v =
  let open Float.O in
  v.x*v.x + v.y*v.y + v.z*v.z

let dot v w =
  let open Float.O in
  v.x*w.x + v.y*w.y + v.z*w.z

let cross v w =
  let open Float.O in
  { x = v.y * w.z - v.z * w.y
  ; y = v.z * w.x - v.x * w.z
  ; z = v.x * w.y - v.y * w.x
  }

let unit v =
  v /. length v

let zero =
  make 0.0 0.0 0.0

let lerp a b t =
  (1.0 -. t) *. a + t *. b
