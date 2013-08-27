(* Data structure for memoization of values and generation of unique
   variables *)
 
module type Generator = sig
  type t
  val gen : unit -> t
end

module GenSym = struct
  type t = Intern.t
  let s = ref 0
  let gen () = 
    let v = "gen" ^ (string_of_int !s) in
    incr s;
    Intern.intern v
end

module type Memo = sig
  type key
  type token
  type t
  val empty : t
  val memo : t -> key -> token
end

module Sigma (Ord : Map.OrderedType) (Gen : Generator) = struct
  module M = Map.Make (Ord)
  type key = Ord.t
  type token = Gen.t
  type t = token M.t ref
  let empty = ref M.empty
  let memo m k = 
    try
      M.find k !m
    with
      Not_found ->
	let v = Gen.gen () in
	m := M.add k v !m; v
end
