(**4つの図形のヴァリアントを作成し，面積を求める関数を定義しなさい． ただし，円周率は3とする．
点（Point）: 点は大きさもない．
円（Circle）: 半径によって面積が変わる
長方形（Rectangle）：縦と横の長さによって面積が変わる
正方形（Square）：一辺の長さによって面積が変わる．*)
type zukei =
 Point of int
 | Circle of int
 | Rectangle of int * int
 | Square of int;;

let area_of_zukei n = match n with
 Point n -> 0;
 | Circle n -> 3 * n * n
 | Rectangle (n1, n2) -> n1 * n2
 | Square n -> n * n;;

(*リストを用いて，スタックを作成しなさい．*)
exception Empty;;

let create() = [];;

let push st n = n :: st;;

let pop st = match st with
 [] -> raise Empty
 | _ :: t1 -> t1;;

let top st = match st with
 [] -> raise Empty
 | hd :: _ -> hd;;
