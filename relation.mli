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
  sig
    type t
    type k1 = T1.t
    type k2 = T2.t
    val empty : t
    val extend : t -> k1 -> k2 -> t
    val lookupl : t -> k1 -> k2 option
    val lookupr : t -> k2 -> k1 option
  end
