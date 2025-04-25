module S = String

let max(x, y, z) =
 if x > y then 
    if x > z then x
    else z
  else 
    if y > z then y
    else z;;

(*gap -2*)
let g() = -2

(*same +2
  diff -1*)
let q(c,d) =
 if c = d then 2
 else -1

(*align_dp, but return matirx a*)
let align_dp (s,t) =
    let m = S.length s
    and n = S.length t in
    let a = Array.make_matrix (m+1) (n+1) 0 in
    for j = 1 to n do
        a.(0).(j) <- a.(0).(j-1) + g()
    done;
    for i = 1 to m do
      a.(i).(0) <- a.(i-1).(0) + g()
    done;
    for i = 1 to m do
      for j = 1 to n do
        a.(i).(j) <- max(a.(i).(j-1) + g(),
                        a.(i-1).(j-1) + q(s.[i-1], t.[j-1]),
                        a.(i-1).(j) + g())
      done
    done;
    a;;

(*i = row
  j = columns
  j-1 = left
  i-1 = up
  j-1 && i-1 = upleft*)
let traceback (s1, s2) =
    let m = S.length s1
    and n = S.length s2 in
    let a = align_dp(s1, s2) in
    let rec aux i j rs1 rs2 =
      if i = 0 && j = 0 then (rs1, rs2)
      (*from upleft*)
      else if i > 0 && j > 0 && a.(i).(j) = a.(i-1).(j-1) + q(s1.[i-1], s2.[j-1]) then
        aux (i-1) (j-1) (S.sub s1 (i-1) 1 ^ rs1) (S.sub s2 (j-1) 1 ^ rs2)
      (*from up*)
      else if i > 0 && a.(i).(j) = a.(i-1).(j) + g() then
        aux (i-1) j (S.sub s1 (i-1) 1 ^ rs1) ("-" ^ rs2)
      (*from left*)
      else 
        aux i (j-1) ("-" ^ rs1) (S.sub s2 (j-1) 1 ^ rs2)
    in
  aux m n "" "";;
