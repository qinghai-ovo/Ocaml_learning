let rec fib n =
  if n < 2 then n
  else fib (n - 1 ) + fib (n - 2);;
  
let i = ref 0
let () =
    while !i <= 5 do
       print_int !i;
       i := !i + 1
    done;;

let fib n =
  if n  < 0  then raise Not_found
  else if n = 0 then 0
  else if n = 1 then 1 
  else 
    let fib = Array.make (n + 1) 0 in
    fib.(0) <- 0;
    fib.(1) <- 1;
    for i = 2 to n do
      fib.(i) <- fib.(i - 1) + fib.(i - 2)
    done;
    fib.(n);; 

let combination n k =
  if k < 0 || k > n then 0
  else if k = 0 then 1
  else
    let c = Array.make_matrix (n + 1) (k + 1) 0 in
    for i = 0 to n do
      for j = 0 to min i k do
        if j = 0 || j = i then
          c.(i).(j) <- 1
        else
          c.(i).(j) <- c.(i-1).(j-1) + c.(i-1).(j)
        done
      done;
    c.(n).(k);;