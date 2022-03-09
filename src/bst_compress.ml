open Tree;;
open Utils;;
open Random;;

Random.self_init ();;

let sizeof x : int = Obj.reachable_words (Obj.repr x);;

let random_keys max size = 
  let rec loop list size = 
    let r = (Random.int (max - 1)) + 1 in
    match size with
    | 0 -> r::list
    | _ -> loop (r::list) (size - 1)
  in loop [] size;;

let bench_time (nodes : int) (loops : int) : unit = 
  let keys = random_keys (nodes * 2) loops in

  Printf.printf "Time for %d Nodes\n" nodes;

  let gp = Utils.gen_permutation2 1 nodes in  
  let bst = Tree.construct gp in

  let start = Sys.time () in
  let rec loop keys =
    match keys with
    | [] -> Printf.printf "  | Uncompressed %.3f ms\n" ((Sys.time () -. start) *. 1000.0)
    | h::t -> 
      let _ = Tree.search bst h in 
      loop t;
  in loop keys;

  let cbst = Tree.compress bst in
  let start = Sys.time () in
  let rec loop keys =
    match keys with
    | [] -> Printf.printf "  | Compressed %.3f ms\n" ((Sys.time () -. start) *. 1000.0)
    | h::t -> 
      let _ = Tree.search_compressed cbst h in 
      loop t;
  in loop keys;;

let bench_size (nodes : int) : unit =
  let gp = Utils.gen_permutation2 1 nodes in  
  let bst = Tree.construct gp in
  let cbst = Tree.compress bst in

  Printf.printf "Size for %d Nodes\n" nodes;

  let words = (sizeof bst) in
  Printf.printf "  | Uncompressed %d words - %.3f KB\n" words ((float_of_int words) *. (float_of_int Sys.word_size /. 8.0) /. 1000.0);

  let words = (sizeof cbst) in
  Printf.printf "  | Compressed %d words - %.3f KB\n" words ((float_of_int words) *. (float_of_int Sys.word_size /. 8.0) /. 1000.0);;

bench_time 10 1000000;; 
bench_size 10;; 
Printf.printf "\n";;

bench_time 50 1000000;; 
bench_size 50;; 
Printf.printf "\n";;

bench_time 100 1000000;; 
bench_size 100;; 
Printf.printf "\n";;

bench_time 500 1000000;;
bench_size 500;;
Printf.printf "\n";;

bench_time 1000 1000000;; 
bench_size 1000;; 
Printf.printf "\n";;

bench_time 5000 1000000;; 
bench_size 5000;; 
Printf.printf "\n";;

bench_time 10000 1000000;;
bench_size 10000;;
Printf.printf "\n";;

bench_time 50000 1000000;; 
bench_size 50000;; 
Printf.printf "\n";;

bench_time 100000 1000000;; 
bench_size 100000;; 
Printf.printf "\n";;

bench_time 500000 1000000;;
bench_size 500000;;
Printf.printf "\n";;

bench_time 1000000 1000000;;
bench_size 1000000;;