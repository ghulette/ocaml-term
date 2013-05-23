type 'a t = (char,'a) Trie.t

let explode s = 
  let cs = ref [] in
  for i = 0 to (String.length s - 1) do
    cs := s.[i] :: !cs
  done;
  List.rev (!cs)

let empty = Trie.empty
let lookup k = Trie.lookup (explode k)
let insert k = Trie.insert (explode k)

