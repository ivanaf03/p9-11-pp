let rec divide l = match l with
    h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
    | _ -> l, [];;

let rec merge = function
    [], l | l, [] -> l
    | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
                                else h2 :: merge (h1::t1, t2);;

let rec msort1 l = 
    match l with
    | [] | _::[] -> l
    | _ -> let l1, l2 = divide l in 
        merge (msort1 l1, msort1 l2);;

(* Como pasaba con quicksort, al no ser terminal puede ocasionar looping recursions, por ejemplo, con esta lista: *)
let l2=  List.init 500000 (fun x-> Random.int 500);;

let divide' l =
  let rec divide_in d1 d2 = function
    | [] -> (List.rev d1, List.rev d2)
    | h::[] -> (List.rev (h::d1), List.rev d2)
    | h1::h2::t -> divide_in (h1::d1) (h2::d2) t
  in divide_in [] [] l;;

let merge' ord (l1, l2) =
  let rec merge_in (m1, m2) lm =
     match m1, m2 with
    | [], l | l, [] -> List.rev_append lm l
    | h1::t1, h2::t2 -> if ord h1 h2 then merge_in (t1, h2::t2) (h1::lm)
                        else merge_in (h1::t1, t2) (h2::lm)
  in merge_in (l1, l2) [];;

let rec msort2 ord l = 
    match l with
    | [] | _::[] -> l
    | _ -> let l1, l2 = divide' l
         in merge' ord (msort2 ord l1, msort2 ord l2);;

(* Esta función si puede ordenar l2 *)

(* Medición de tiempos:

let crono=
    let t= Sys.time() in 
       (msort1 (List.init 100000 (fun x-> Random.int 500))); Sys.time()-. t;;
val crono : float = 0.198884000000000061

let crono=
    let t= Sys.time() in 
       (msort2 (<) (List.init 100000 (fun x-> Random.int 500))); Sys.time()-. t;;
val crono : float = 0.186532000000000142

let crono=
    let t= Sys.time() in 
       (qsort2 (<) (rlist 500 100000)); Sys.time()-. t;;
val crono : float = 0.795993000000000173


Msort2 es un poco más rapida que Msort1 y ambas mucho más rapidas que Qsort2 

*)
