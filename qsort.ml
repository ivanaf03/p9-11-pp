(* QSORT 1 *)
let rec qsort1 ord = function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
        qsort1 ord before @ h :: qsort1 ord after;;

(* QSORT 2 *)
let rec qsort2 ord =
    let append' l1 l2 = List.rev_append (List.rev l1) l2 in
        function
        [] -> []
        | h::t -> let after, before = List.partition (ord h) t in
            append' (qsort2 ord before) (h :: qsort2 ord after);;

(* Creador de lista descendente *)
let rec dlist x=if x=0 then []
                              else x::dlist(x-1);;

(* Creador de lista acendente *)
let alist x=
    let sum n=n+1 
    in List.init x sum;;
      
(* Creador de lista aleatoria *)                                                                                                                                      
let rec rlist r n=
    if n<=0 then [] 
     else Random.int r::rlist r (n-1);;

(* 
Medidas de tiempos:
let crono=
    let t= Sys.time() in 
       (qsort1 (<) (dlist 10000)); Sys.time()-. t;;
val crono : float = 4.84681099999999532

let crono=
    let t= Sys.time() in 
       (qsort2 (<) (dlist 10000)); Sys.time()-. t;;
val crono : float = 5.07712099999999822

let crono=
    let t= Sys.time() in 
       (qsort1 (<) (alist 10000)); Sys.time()-. t;;
val crono : float = 3.97399100000000516

let crono=
    let t= Sys.time() in 
       (qsort2 (<) (alist 10000)); Sys.time()-. t;;
val crono : float = 3.99490000000000123

let crono=
    let t= Sys.time() in 
       (qsort1 (<) (rlist 500 10000)); Sys.time()-. t;;
val crono : float = 0.0241109999999906677

let crono=
    let t= Sys.time() in 
       (qsort2 (<) (rlist 500 10000)); Sys.time()-. t;;
val crono : float = 0.0252910000000099444

Esta implementación no es buena en casos en los que el vector esté ya ordenado. Es muchísimo mas eficiente para vectores totalmente aleatorios.
Qsort2 tiene la ventaja de que permite ordenar listas mayores a Qsort1, ya que al ser terminal evita loop recursion. Qsort 1 es un 5% más rápido aproximadamente. Esto se debe a que Qsort2 tiene que invertir la lista con la llamada a rev_append. Una lista que podría ejecutar Qsort2 y Qsort1 no sería, por ejemplo:
*)
 let l1 = List.init 500000 (fun x-> Random.int 500);;



