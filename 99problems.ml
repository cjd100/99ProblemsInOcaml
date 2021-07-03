(** This is a file to do the 99 problems in OCaml **)

let rec last_list l = 
    match l with 
        | [] -> None
        | h::[] -> Some h
        | h :: t -> last_list t

let rec last_two l = 
    match l with 
        | [] -> None
        | a::b::[] -> Some (a, b)
        | h::t -> last_two t

(* find kth element *)
let rec find_k l n =
    let rec find_helper lst acc = 
        match l with 
            | [] -> None
            | h::t -> if acc = n then Some h else find_helper t (acc+1)
    in 
    find_helper l 0

(* find length of a list *)
let size lst = 
    let rec size_helper l acc = 
        match l with 
        | [] -> acc
        | h::t -> size_helper t (acc + 1)
    in 
    size_helper lst 0

(* reverse list *)
let rec reverse lst =
    match lst with 
    | [] -> []
    | h::t -> (reverse t) @ [h]

    let is_palindrome lst = lst = reverse lst

type 'a node =
    | One of 'a 
    | Many of 'a node list
let rec flatten nlist = 
    let rec flatten_helper acc lst = 
    match nlist with
     | [] -> acc
     | One h :: t -> flatten_helper (h::acc) t
     | Many h :: t -> flatten_helper (flatten_helper acc lst) t in
    List.rev (flatten_helper [] nlist)

(* eliminate consecutive duplicates of list elements *)
let rec elim_dups l = 
    match l with 
    | [] -> []
    | [a] -> [a]
    | a :: b :: t -> if a = b then elim_dups (b::t) else a :: elim_dups (b :: t)

(* pack consecutive duplicates of list elements into sublists *)




