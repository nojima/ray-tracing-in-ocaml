type t = {
  x: float;
  y: float;
  z: float;
}

val make : float -> float -> float -> t

val (~-) : t -> t
val (+) : t -> t -> t
val (-) : t -> t -> t
val ( * ) : t -> t -> t
val (/) : t -> t -> t
val ( *. ) : float -> t -> t
val (/.) : t -> float -> t

val length : t -> float
val squared_length : t -> float
val dot : t -> t -> float
val cross : t -> t -> t
val unit : t -> t
val zero : t
val one : t
val lerp : t -> t -> float -> t
val reflect : t -> t -> t
val refract : t -> t -> float -> t option
