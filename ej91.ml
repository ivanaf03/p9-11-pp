(* let rec to0from n =
if n < 0 then [] else n :: to0from (n-1);; *)
let to0from n=
    let rec to0from_in acc n =
        match n with 
        | -1->acc
        | _->n::to0from_in acc (n-1)
    in to0from_in [] n;;


(* let rec fromto m n =
if m > n then [] else m :: fromto (m+1) n;; *)
let fromto m n=
    let rec fromto_in acc m n=
        if m>n then acc 
        else m::fromto_in acc (m+1) n
    in fromto_in [] m n;;


(*let incseg l =
List.fold_right (fun x t -> x::List.map ((+) x) t) l [];; *)
let incseg l=
    let rec incseg_in l acc=
        match l with
        | []->acc
        | h::t->List.fold_right (fun x t -> x::List.map ((+) x) t) l acc
    in incseg_in l [];;

(* let rec remove x = function
[] -> []
| h::t -> if x = h then t else h :: remove x t;;*)
let remove x l=
    let rec remove_in x l acc =  
        match l with
        [] -> List.rev acc
        | h::t -> if x = h then List.rev_append acc t else remove_in x t (h::acc)
    in remove_in x l [];;

(*let rec compress = function
| h1::h2::t -> if h1 = h2 then compress (h2::t)
else h1 :: compress (h2::t)
| l -> l;;*)
let compress l=
    let rec compress_in acc l=
        match l with
        | h1::h2::t->if h1=h2 then compress_in acc t
                            else h1::compress_in acc (h2::t)
        | l->acc
    in compress_in [] l;;
