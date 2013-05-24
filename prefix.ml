type 'a t = (char,'a) Trie.t

let explode s = 
  let cs = ref [] in
  String.iter (fun c -> cs := c :: !cs) s;
  List.rev (!cs)

let empty = Trie.empty
let lookup k = Trie.lookup (explode k)
let insert k = Trie.insert (explode k)

