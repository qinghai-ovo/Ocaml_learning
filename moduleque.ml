module type Queue =
sig
  type 'a queue
  exception Empty

  val create : unit -> 'a queue 
  val enqueue : 'a -> 'a queue -> 'a queue
  val dequeue : 'a queue -> 'a queue
  val front : 'a queue -> 'a

end

module QueueV: Queue =
struct
  type 'a queue = Nil | Cell of 'a * 'a queue
  
  exception Empty
    
  let create() = Nil
  
  let enqueue n st = Cell (n, st)
  
  let rec dequeue st = match st with
  | Nil -> raise Empty
  | Cell(_, Nil) -> Nil
  | Cell(n, tl) -> Cell(n, dequeue tl)
  
  let rec front st = match st with
    | Nil -> raise Empty
    | Cell(x, Nil) -> x
    | Cell(_, tl) -> front tl
  
end

module QueueL: Queue = 
struct
  type 'a queue = 'a list 
  
  exception Empty

  let create() = []

  let enqueue n lst = n :: lst;;

  let front lst = match lst with
  | [] -> raise Empty
  | hd :: _ ->  hd;;

  let dequeue lst = match lst with
  | [] -> raise Empty
  | _ :: tl -> tl;;
  
end