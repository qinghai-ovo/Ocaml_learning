(* トークンの印字 *)
let print_token tk =
  match tk with
  | (ID i) -> P.printf "ID(%s)" i
  | (NUM n) -> P.printf "NUM(%d)" n
  | (LET) -> P.printf "LET"
  | (BE) -> P.printf "BE"
  | (IN) -> P.printf "IN"
  | (EOF) -> P.printf "EOF"
  | (ONE c) -> P.printf "ONE(%c)" c
  (* お試し実行関数 *)
  let rec run () =
  flush stdout;
  let rlt = gettoken () in
  match rlt with
  (ONE '$') -> raise End_of_system
  | _ -> (print_token rlt; P.printf "\n"; run())