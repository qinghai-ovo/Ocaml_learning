type 'a queue = Nil | Cell of 'a * 'a queue;;
 
exception Empty;;

let create() = [];;

(*リストを用いて，queを作成しなさい．*)
 let enqueue n lst = n :: lst;;

 let front lst = match lst with
  | [] -> raise Empty
  | hd :: _ ->  hd;;
 
 let dequeue lst = match lst with
  | [] -> raise Empty
  | _ :: tl -> tl;;
 
  
 let v_creat() = Nil ;;
 
 let v_enqueue n st = Cell (n, st);;
 
 let rec v_dequeue st = match st with
 | Nil -> raise Empty
 | Cell(_, Nil) -> Nil
 | Cell(n, tl) -> Cell(n, v_dequeue tl);;
 
 let rec v_front st = match st with
  | Nil -> raise Empty
  | Cell(x, Nil) -> x
  | Cell(_, tl) -> v_front tl

(*test*)
let a = create();;
let b = enqueue 1 a;;
let c = enqueue 2 b;;
let d = enqueue 3 c;;
front d;;
let e = dequeue d;;
front e;;

