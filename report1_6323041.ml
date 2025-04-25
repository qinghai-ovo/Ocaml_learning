(**function for muultiuse*)
(**lst is the reverse one acc =[]*)
let rec reverse lst acc =
  match lst with
    | [] -> acc
    | hd :: tl -> reverse tl (hd :: acc);;

(**campare is there any same elem in lst*)
let rec contains elem lst =
  match lst with
  | [] -> false
  | hd :: tl -> hd = elem || contains elem tl;;


(**課題１*)
(**minimumはリストの中の一番小さいの要素を出した関数です、しかし、空リストのときErrorを表示する*)
let rec minimum lst =
  match lst with
  (**if empty then error*)
  | [] -> failwith "Error"
  (**only one element*)
  | [x] -> x
  (**more than one element*)
  | hd1 :: hd2 :: tl -> 
    (**first one is bigger*)
    if hd1 > hd2 then 
      minimum (hd2 ::tl)
    (**second one is bigger*)
    else 
      minimum (hd1 :: tl);;


(**課題２*)
(**extractはリストの中で条件を満たすの要素をそのままで、満たさない要素を削る関数です*)
let rec extract predicate lst =
   match lst with 
    (**if empty*)
    | [] -> []
    (**take the fisrt element*)
    | hd :: tl ->
      (**check hd with predicate*)
       if predicate hd then
        (**hd true then keep and check the rest*)
        hd :: extract predicate tl
      (**hd false then check the tl with hd*)
       else
        extract predicate tl;;


(**課題３*)
let distinct lst =
  let rec has_duplicate lst elem =
    match lst with
      | [] -> false
      | hd :: tl ->
        if hd = elem then
          true
        else
          has_duplicate tl elem 
  in
  let rec check_distinct lst =
    match lst with
    | [] -> true
    | hd :: tl ->
      if has_duplicate tl hd then
        false
      else 
        check_distinct tl
  in
    check_distinct lst;;


(**課題４*)
let rec combine lst1 lst2 =
  match lst1,lst2 with
  | [], [] -> []
  | hd1 :: tl1, hd2 :: tl2 -> (hd1, hd2) :: combine tl1 tl2 
  | _ -> failwith "Not_found";;


(**課題５*)
let rec revm lst1 lst2 =
  let reversed_lst2 = reverse lst2 [] in
  let rec append lst1 lst2 =
    match  lst1 with
    | [] -> lst2
    | hd :: tl -> hd :: append tl lst2
  in
  let rec check_length lst1 lst2 =
    match (lst1, lst2) with
    | ([], []) -> true
    | ([], _) | (_, []) -> false
    | (_ :: tl1, _ :: tl2) -> check_length tl1 tl2
  in
  if check_length lst1 reversed_lst2 then
    append lst1 reversed_lst2
  else
    failwith"Not_found";;


(**課題６*)
let split lst =
  let rec split_aux lst acc1 acc2 =
    match lst with
    | [] -> (acc1, acc2)
    | (x, y) :: tl -> split_aux tl (x :: acc1) (y :: acc2)
  in
  let temp_result1,temp_result2 =split_aux lst [] [] in
  reverse temp_result1 [], reverse temp_result2 [];;


(**課題７*)
let rec countElem lst =
  let rec countsublist lst = 
    match lst with
    | [] -> 0
    | _ :: tl -> 1 + countsublist tl
  in
  let rec countAux lst acc = 
    match lst with
    | [] -> acc
    | hd :: tl -> countAux tl (acc + countsublist hd)
  in
  countAux lst 0;;
     

(**課題８*)
let rec unique lst =
  match lst with
  | [] -> []
  | hd :: tl ->
    if contains hd tl then 
      unique tl
    else 
      hd :: unique tl


(**課題９*)
let rec merge2 lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> []
  | [], _ | _, []-> failwith "Not_found"
  | hd1 :: tl1, hd2 :: tl2 -> hd1 :: hd2 :: merge2 tl2 tl1


(**課題１０*)
let set_equal lst1 lst2 =
  let rec set_equalAux lst1 lst2 =
    match lst1 with
    | [] -> true
    | hd :: tl ->
      if contains hd lst2 then
        set_equalAux tl lst2 
      else
        false
  in
  if set_equalAux lst1 lst2 && set_equalAux lst2 lst1 then 
    true
  else  
    false
        


(**課題１１*)
let rec numOfRotes (m, n) =
  if m = 0 || n = 0 then 
    1
  else
    numOfRotes (m-1, n) + numOfRotes (m, n-1) 