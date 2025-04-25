(*Binary Tree*)
type 'a tree = Nil | Node of string * 'a * 'a tree * 'a tree

let create () = Nil

let rec insert(name, data) t =
  match t with
  | Nil -> Node (name, data, Nil, Nil)
  | Node(n, d, left, right) -> 
    if name < n then
      Node(n, d, insert (name, data) left, right)
    else if name > n then
      Node(n, d, left, insert (name, data) right)
    else
      raise Not_found;;

(*search*)
let rec search name t =
  match t with
  | Nil -> raise Not_found;
  | Node(n, d, tl, tr)-> 
    if name = n then d
    else if name < n then 
      search name tl
    else if name  > n then 
      search name tr
    else
      raise Not_found;;

(*delete*)
let rec delete name t =
  match t with
  | Nil -> raise Not_found;
  | Node (n, d, left, right) -> 
    if name < n then Node(n, d, delete name left, right)
    else if name > n then Node(n, d, left, delete name right)
    else
      let rec min t = 
        match t with
        Nil -> raise Not_found
        | Node (n, d, Nil, right) -> t 
        | Node (n, d, left, right) -> min left
      in
      match t with
      Nil -> raise Not_found
      |Node (n, d, Nil, Nil) -> Nil
      |Node (n, d, Nil, right) -> right
      |Node (n, d, left, Nil) -> left
      |Node (n, d, left, right) -> 
        let minNode = min right in
        match minNode with
        Node (n, d, l, r) ->
          let rightTree = delete n right in
          Node (n, d, left, rightTree)
          | _ -> raise Not_found

let t = create ();;
let t = insert("e", 5) t;;
let t = insert("c", 3) t;;
let t = insert("j", 10) t;;
let t = insert("a" , 1) t;;
let t = insert("g", 7) t;;
let t = insert("l", 12) t;;
let d = delete "e" t;;
