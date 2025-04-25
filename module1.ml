module Stack:
sig
  exception Empty
  type 'a stack
  val create : unit -> 'a stack
  val push: 'a stack -> 'a -> 'a stack
  val top: 'a stack -> 'a
  val pop: 'a stack -> 'a stack
end
= struct
  exception Empty
  type 'a stack = Nil | Cell of 'a * 'a stack
  let create () = Nil
  let push st x = Cell (x, st)
  let top st = match st with
      Nil -> raise Empty
    | Cell (x, _) -> x
  let pop st = match st with
      Nil -> raise Empty
    | Cell (_, xs) -> xs
end;;
