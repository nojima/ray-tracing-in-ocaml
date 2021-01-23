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
val muls : t -> float -> t
val divs : t -> float -> t

val length : t -> float
val squared_length : t -> float
val dot : t -> t -> float
val cross : t -> t -> t
val unit : t -> t
