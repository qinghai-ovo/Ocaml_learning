exception Error;;

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

(*align_dp*)
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
                            max(a.(i-1).(j-1) + q(s.[i-1], t.[j-1]),
                                a.(i-1).(j) + g()))
        done
    done;
    a.(m).(n);;

(*teisei*)
(*caculate socre and return with (score, word)*)
let rec caculate input lst = match lst with
    | [] -> []
    | hd :: tl -> (align_dp(input, hd), hd) :: (caculate input tl);;

(*find the bestmatch world and return world*)
let rec bestmatch lst = match lst with
    | [] -> ""
    | [(s, w)]-> w
    | (s1, w1)::(s2, w2)::tl -> 
        if s1 >= s2 then bestmatch ((s1, w1) :: tl)
        else bestmatch ((s2, w2) :: tl);;

let teisei (input, lst) =
    if lst = [] then raise Error
    else
        let calcu_lst = caculate input lst in
        let result = "Did you expect \"" ^ bestmatch calcu_lst ^ "\"?" in
        print_endline result;;