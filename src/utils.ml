open Random;;
Random.self_init ();;

module Utils =
struct
  let extraction_alea l p = 
    let len = (List.length l) in 
    let r = (Random.int len) in
    let rec aux c l1 l2 = 
      match c, l1 with
      | _, [] -> (l, p)
      | 0, h::t -> (List.rev_append l2 t, h::p)
      | _, h::t -> aux (c - 1) t (h::l2)
    in aux r l [];;

  let rec shuffle (l, p) =
    match l with 
    | [] -> p
    | _ -> shuffle (extraction_alea l p);;

  let gen_permutation n = 
    let rec aux count list =
      match count with
      | 0 -> shuffle  (list, [])
      | _ -> aux (count - 1) (count::list)
    in aux n [];; 

  let intercale l1 l2 =
    let rec loop acc l1 n1 l2 n2 =
      match l1, l2, n1 + n2 with
      | [], [], _ -> List.rev acc
      | [], hd::tl, n | hd::tl, [], n -> loop (hd::acc) tl (n - 1) [] 0
      | hd1::tl1, hd2::tl2, n ->
        if Random.int n < n1 then
          loop (hd1::acc) tl1 (n1 - 1) l2 n2
        else
          loop (hd2::acc) l1 n1 tl2 (n2 - 1)
    in loop [] l1 (List.length l1) l2 (List.length l2);;

  let rec gen_permutation2 p q = 
    if (p > q) then 
      []
    else if (p = q) then 
      [p]
    else
      let perm1 = gen_permutation2 p ((p + q) / 2) and perm2 = gen_permutation2 (((p + q) / 2) + 1) q in
      intercale perm1 perm2;;
end;;

