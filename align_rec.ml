module S = String

let max(x,y) =
 if x > y then x else y

(*gap -2*)
let g() = -2

(*same +2
  diff -1*)
let q(c,d) =
 if c = d then 2
 else -1

(*align_rec*)
let rec align_sub (s,t,i,j) =
    if i = 0 || j = 0 then i * g() + j * g()
        else
            max(align_sub(s, t, i, j-1) + g(),
                max(align_sub(s, t, i-1, j-1)+q(s.[i-1],t.[j-1]),
                    align_sub(s, t, i-1,j) + g()))
let align_rec(s,t) = align_sub(s,t,S.length s, S.length t) ;;