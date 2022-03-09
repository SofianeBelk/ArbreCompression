module Tree =
struct

  type binary_tree =
    | Empty
    | Node of int * binary_tree * binary_tree;;

  type compressed_tree = 
    | Empty
    | Node of int * compressed_tree * compressed_tree * int
    | Pointer of compressed_tree ref * int array;;

  let rec insert (tree : binary_tree) (n : int) : binary_tree =
    match tree with
    | Empty -> (Node (n, Empty, Empty));
    | (Node (x, left, right)) -> 
      if n = x then
        tree
      else if n < x then
        (Node (x, (insert left n), right))
      else 
        (Node (x, left, (insert right n)))

  let construct (list : int list) : binary_tree = 
    let rec aux l bt = 
      match l with
      | [] -> bt
      | h::t -> aux t (insert bt h)
    in aux list Empty

  let rec get_size (node : compressed_tree) : int = 
    match node with
    | Empty -> 0
    | (Node (_, _, _, s)) -> s
    | (Pointer (ptr, _)) -> get_size (!ptr);;

  let rec search (tree : binary_tree) (n : int) : bool =
    match tree with
    | Empty -> false
    | (Node (x, left, right)) -> 
      if n = x then
        true
      else if n < x then
        search left n
      else 
        search right n

  let rec search_compressed (tree: compressed_tree) (n : int) : bool =
    match tree with 
    | Empty -> false
    | (Node (x, left, right, _)) -> 
      if n = x then
        true
      else if n < x then
        search_compressed left n
      else 
        search_compressed right n
    | (Pointer (ptr, array)) -> 
      let rec aux tree l i =
        match tree with
        | Empty -> false
        | (Node (x, left, right, s)) -> 
          if n = (Array.get l i) then 
            true
          else if n < (Array.get l i) then
            aux left l (i + 1)
          else 
            aux right l (i + (get_size left) + 1)
        | (Pointer (ptr, array)) -> aux !ptr l i
      in aux !ptr array 0

  let rec prefixe (tree : binary_tree) : int array = 
    match tree with
    | Empty -> [||]
    | (Node (x, left, right)) -> Array.append [|x|] (Array.append (prefixe left) (prefixe right))

  let rec phi (tree : binary_tree) : string =
    match tree with
    | Empty -> ""
    | (Node (x, Empty, Empty)) -> "()"
    | (Node (x, left, right)) -> "(" ^ (phi left) ^ ")" ^ (phi right)

  let compress (tree : binary_tree) = 
    let table = (Hashtbl.create 5 : (string, compressed_tree ref) Hashtbl.t) in
    let rec aux (tree : binary_tree) =
      match tree with
      | Empty -> Empty
      | (Node (x, left, right)) -> 
        let l = aux left in
        let r = aux right in 
        let s = (get_size l) + (get_size r) + 1 and p = phi tree in
        if (Hashtbl.mem table p) then 
          (Pointer ((Hashtbl.find table p), (prefixe tree)))
        else 
          let n = (Node (x, l, r, s)) in
            Hashtbl.add table p (ref n);
            n
    in aux tree;;
end;;