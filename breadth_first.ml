open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let breadth_first_t (Gt (x, children)) =
    let rec aux acc = function
      | [] -> acc
      | Gt (x, children) :: rest ->
          aux (x :: acc) (rest @ children)
    in
    aux [x] children;;

let rec inf_tree = Gt (0, [inf_tree]);;
let t2 = inf_tree;;
