type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

  let rec fold_tree f acc = function
    Empty -> acc
  | Node (x, l, r) -> f x (fold_tree f acc l) (fold_tree f acc r)

  let sum tree =
    fold_tree (fun x l r -> x + l + r) 0 tree
  
let rec prod = function
      Empty -> 1.0
   | Node (x, l, r) -> x *. (prod l) *. (prod r);; 
  
  let size tree =
  fold_tree (fun _ l r -> 1 + l + r) 0 tree

let height tree =
  fold_tree (fun _ l r -> 1 + max l r) 0 tree

let inorder tree =
  let rec aux acc = function
    | Empty -> acc
    | Node (x, l, r) -> aux (x :: aux acc r) l
  in aux [] tree

let mirror tree =
  fold_tree (fun x l r -> Node (x, r, l)) Empty tree

  let rec map_tree f = function
  Empty -> Empty
| Node (x,l,r) -> Node (f x, map_tree f l, map_tree f r);;
