module type S = sig
  type t
  type k1
  type k2
  val empty : t
  val extend : t -> k1 -> k2 -> t
  val lookupl : t -> k1 -> k2 option
  val lookupr : t -> k2 -> k1 option
end

module Make (T1 : Map.OrderedType) (T2 : Map.OrderedType) :
  S with type k1 = T1.t and type k2 = T2.t =
struct
  type k1 = T1.t
  type k2 = T2.t

  module M1 = Map.Make (T1)
  module M2 = Map.Make (T2)

  type t = {
    m1 : k2 M1.t;
    m2 : k1 M2.t
  }
  
  let empty = {
    m1 = M1.empty;
    m2 = M2.empty
  }
  
  let extend r e1 e2 = {
    m1 = M1.add e1 e2 r.m1;
    m2 = M2.add e2 e1 r.m2
  }

  let lookupl r k =
    try Some (M1.find k r.m1) with Not_found -> None

  let lookupr r k = 
    try Some (M2.find k r.m2) with Not_found -> None
end
