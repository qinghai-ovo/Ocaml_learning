(*Binary Tree*)
type 'a tree = Nil | Node of string * 'a * 'a tree * 'a tree
 
exception Empty;;

let create_que() = [];;

(*リストを用いて，queを作成しなさい．*)
 let enqueue n lst = n :: lst;;

 let front lst = match lst with
  | [] -> raise Empty
  | hd :: _ ->  hd;;
 
 let dequeue lst = match lst with
  | [] -> raise Empty
  | _ :: tl -> tl;;
 

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
  | Node(n, d, tl, tr)-> 
    if name = n then 
      match tl, tr with
      | Nil, Nil -> Nil
      | tl, Nil -> tl
      | Nil, tr -> tr
      | tl, tr -> 
        (*find min in tr (n, d, nil, _)*)
        let rec minsearch t = 
          match t with
          | Nil -> raise Not_found
          | Node(n, d, Nil, _) -> Node(n, d, Nil, Nil)
          | Node(_, _, tl, _) -> minsearch tl
        in
        let minNode = minsearch tr in
        match minNode with
        | Nil -> raise Not_found
        | Node(nmin, dmin, _, _) -> 
          let fxtr = delete nmin tr in 
          Node (nmin, dmin, tl, fxtr) 
    else if name < n then 
      Node(n, d, delete name tl, tr)
    else if name  > n then 
      Node(n, d, tl, delete name tr)
    else
      raise Not_found;;

let rec reoder t =
    match t with
    | Nil -> []
    | Node (n, d, Nil, Nil) -> [(n, d)]
    | Node (n, d, left, right) -> (reoder left) @ (reoder right) @ [(n, d)]

let bsearch t =
  let rec aux que =
  match que with
  |[] -> []
  |Nil :: tl -> aux tl
  |Node(n, d, left, right) :: tl -> [(n, d)] :: aux (tl @ [left; right])
  in
  aux [t]

let t = create ();;
let t = insert("e", 5) t;;
let t = insert("c", 3) t;;
let t = insert("j", 10) t;;
let t = insert("a" , 1) t;;
let t = insert("g", 7) t;;
let t = insert("l", 12) t;;
let t = insert("d", 4) t;;
let d = reoder t;;
