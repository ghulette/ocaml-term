module OrderedType = Map.OrderedType

module type S : sig
  
end

functor Make (Ord : OrderedType) -> S  with type key = Ord.t