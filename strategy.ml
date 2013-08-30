type t = Term.t -> Term.t option

let apply s t = s t

let rule t1 t2 = 
  let r = Rewrite.make t1 t2 in
  fun t -> Rewrite.apply r t

let succeed = 
  fun t -> Some t

let fail = 
  fun _ -> None

let negate s = fun t -> 
  match s t with
  | Some _ -> None
  | None -> Some t

let test s = fun t ->
  match s t with
  | Some _ -> Some t
  | None -> None

let sequence s1 s2 = fun t ->
  match s1 t with
  | Some t' -> s2 t'
  | None -> None

let left_choice s1 s2 = fun t ->
  match s1 t with
  | Some t -> Some t
  | None -> s2 t

(*
type t = 
| Rule of Rewrite.t
| Succeed
| Fail
| Negate of t
| Test of t
| Sequence of t*t
| LeftChoice of t*t

let rec eval = function
  | Rule r -> fun t -> Rewrite.apply r t
  | Succeed -> fun t -> Some t
  | Fail -> fun _ -> None
  | Negate e -> fun t ->
    begin match eval e t with
    | Some _ -> None
    | None -> Some t
    end
  | Test e -> fun t ->
    begin match eval e t with
    | Some _ -> Some t
    | None -> None
    end
  | Sequence (e1,e2) -> fun t ->
    begin match eval e1 t with
    | Some t' -> eval e2 t'
    | None -> None
    end
  | LeftChoice (e1,e2) -> fun t ->
    begin match eval e1 t with
    | Some t' -> Some t'
    | None -> eval e2 t
    end
*)
