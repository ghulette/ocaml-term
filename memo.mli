module type Generator = sig
  type t
  val gen : unit -> t
end

module GenSym : Generator with type t = Intern.t

module type Memo = sig
  type key
  type token
  type t
  val empty : t
  val memo : t -> key -> token
end

module Sigma (Ord : Map.OrderedType) (Gen : Generator) : Memo
  with type token = Gen.t 
  and type key = Ord.t
