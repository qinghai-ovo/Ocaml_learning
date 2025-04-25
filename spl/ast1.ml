module Lexer = struct
  type token = CID of string | VID of string | NUM of string |
              TO | IS | QUIT | OPEN | EOF | ONE of char

  module P = Printf
  
  exception End_of_system
  
  let _ISTREAM = ref stdin
  let ch = ref []
  (*q6 start*)
  let line_number = ref 1
    
  let read () = 
    let c = match !ch with 
      | [] -> input_char !_ISTREAM
      | h::rest -> (ch := rest; h)
    in
    if c = '\n' then incr line_number;
    c

  let unread c = 
    if c = '\n' then decr line_number;
    ch := c::!ch
   (*q6 voer*)

  let lookahead () = 
      try 
        let c = read () in unread c; 
        c 
      with 
        End_of_file -> '$'
  
    (* 文字列として数字を構成 *)
  let rec integer i =
    let c = lookahead () in
    if (c >= '0' && c <= '9') then
      integer (i^(Char.escaped (read ())))
    else i
  
  (*識別子の認識*)
  and identifier id =
    let c = lookahead () in
      if ((c >= 'a' && c <= 'z') || 
          (c >= 'A' && c <= 'Z') ||
          (c >= '0' && c <= '9') || 
          c == '_') then
        identifier (id^(Char.escaped (read ())))
        else id
  
  and native_token () =
    let c = lookahead () in
      (* CID に対する識別子および予約語 *)
      if (c >= 'a' && c <= 'z') then 
        let id = identifier "" in
        match id with
        | "quit" -> QUIT
        | "open" -> OPEN
        | "is" -> IS
        | _ -> CID id
      
      (* VID に対する識別子 *)
      else if (c >= 'A' && c <= 'Z') then
        VID (identifier "")
      
      else if (c >= '0' && c <= '9') then 
        NUM (integer "")
      
      (* :- を認識して TO を返す *)
      else if (c = ':') then
        let _ = read() in 
        if (lookahead() = '-') then
          let _ = read() in 
          TO
        else 
          ONE ':'

      else ONE (read ())
  
  and gettoken () =
    try
      let token = native_token () in
      match token with
      | ONE ' ' -> gettoken ()
      | ONE '\t' -> gettoken ()
      | ONE '\n' -> gettoken () 
      | _ -> token
    with End_of_file -> EOF  
  
  let print_token tk =
    match tk with
    | (CID i) -> P.printf "CID(%s)" i
    | (VID i) -> P.printf "VID(%s)" i
    | (NUM i) -> P.printf "NUM(%s)" i
    | (TO) -> P.printf ":-"
    | (QUIT) -> P.printf "quit"
    | (OPEN) -> P.printf "open"
    | (IS) -> P.printf "is"
    | (EOF) -> P.printf "eof"
    | (ONE c) -> P.printf "ONE(%c)" c
  
end

module Evaluator = struct
  (*Ast type*)
  type ast = Atom of string | Var of string | App of string * ast list

  (*Ast print*)
  module P = Printf

  let rec print_ast ast = match ast with
    | (App (s, hd::tl)) -> (P.printf "App(\"%s\",[" s;
                            print_ast hd; 
                            List.iter (fun x -> (print_string ";"; 
                                                  print_ast x)) tl;
                            print_string "])")
    | (App(s, [])) -> P.printf "App(\"%s\"" s
    | (Atom s) -> P.printf "Atom \"%s\"" s
    | (Var s) -> P.printf "Var \"%s\"" s
  
  let print_ast_list lst = match lst with
  | (hd::tl) -> (print_string "["; 
                  print_ast hd; 
                  List.iter (fun x -> print_string ";";
                              print_ast x) 
                  tl; 
                  print_string"]")
  | [] -> print_string "[]"

  let sub name term =
    let rec mapVar ast = match ast with
      | (Atom x) -> Atom(x)
      | (Var n) -> if n=name then term else Var n
      | (App(n, terms)) -> App(n, List.map mapVar terms)
    in mapVar

  let mgu (a,b) =
    let rec ut (one, another, unifier) = match (one, another) with
      | ([], []) -> (true, unifier)
      | (term::t1, Var(name)::t2) ->
          let r = fun x -> sub name term (unifier x) in
          ut(List.map r t1, List.map r t2, r)
      | (Var(name)::t1, term::t2) -> 
          let r = fun x -> sub name term (unifier x) in
          ut(List.map r t1, List.map r t2, r)
      | (Atom(n)::t1, Atom(m)::t2) ->
          if n=m then ut(t1,t2,unifier) else (false, unifier)
      | (App(n1,xt1)::t1, App(n2,xt2)::t2) ->
          if n1=n2 && List.length xt1 = List.length xt2 then
            ut(xt1@t1, xt2@t2, unifier)
          else (false, unifier)
      | (_,_) -> (false, unifier);
    in ut ([a],[b], (fun x -> x))

  let resolution (rule, question) = match (rule, question) with
    | (head::conds,goal::goals) ->
          let (unifiable, unifier) = mgu (head,goal)
          in List.map unifier (conds@goals)
    | ([], goals) -> goals
    | (_, []) -> []

  let rename ver term =
    let rec mapVar ast = match ast with
      | (Atom x) -> Atom(x)
      | (Var n) -> Var(n^"#"^ver)
      | (App(n, terms)) -> App(n, List.map mapVar terms)
    in mapVar term
    
  exception Compiler_error
  
  let succeed query = (print_ast query; true)

  let rec solve (program, question, result, depth) = match question with
      | [] -> succeed result
      | goal::goals ->
          let onestep _ clause =
            match List.map (rename (string_of_int depth)) clause with
              | [] -> raise Compiler_error
              | head::conds ->
                  let (unifiable, unifier) = mgu(head, goal) in
                    if unifiable then
                      solve (program, List.map unifier (conds@goals),
                              unifier result, depth+1)
                    else true
            in List.fold_left onestep true program

  let eval (program, question) = solve(program, [question], question, 1)
  
end

(*q1*)
module Parser = struct
  
  module E = Evaluator
  module L = Lexer
  
  let tok = ref (L.ONE ' ')

  let getToken () = L.gettoken()
  let advance () = (tok := getToken(); (*L.print_token(!tok)*))
  
  exception Syntax_error of int

  let error () = raise (Syntax_error !L.line_number)

  let check t = match !tok with
    | L.CID _ -> if(t = (L.CID "")) then () else error()
    | L.VID _ -> if(t = (L.VID "")) then () else error()
    | L.NUM _ -> if(t = (L.NUM "")) then () else error()
    | tk -> if(tk = t) then () else error()

  let eat t = (check t; advance())
  let prog = ref [[E.Var ""]]

  let rec clauses() = match !tok with
    | L.EOF -> []
    | _ -> let c = clause()in c :: clauses()

  and clause() = match !tok with
    | L.ONE '(' -> let t = term() in (eat(L.ONE '.'); [t])
    | _ -> (predicate(); 
            to_opt(); 
            eat(L.ONE '.'))

  and to_opt() = match !tok with
    | L.TO -> (eat L.TO; terms())
    | _ -> ()

  and command() = match !tok with
    | L.QUIT -> exit 0
    | L.OPEN -> (eat(L.OPEN);
      match !tok with
      | L.CID s -> (eat(L.CID ""); check (L.ONE '.');
                    L._ISTREAM := open_in (s ^ ".pl");
                    advance(); 
                    prog := clauses() ; close_in (!L._ISTREAM))
      | _ -> error())
    | _ -> let t = term() in (check(L.ONE '.'); let _ = E.eval(!prog, t) in ())

  and term() = match !tok with
    | L.ONE '(' -> (eat(L.ONE '('); term(); eat(L.ONE ')'))
    | _ -> predicate()

  and terms() = term(); terms_aux()

  and terms_aux() = match !tok with
    | L.ONE ',' -> (eat(L.ONE ','); term(); terms_aux())
    | _ -> ()

  and predicate() = match !tok with
    | L.CID s-> (eat(L.CID ""); eat(L.ONE '('); args(); eat(L.ONE ')'))
    | _ -> error()

  and args() = expr(); args_aux()

  and args_aux() = match !tok with
    | L.ONE ',' -> (eat(L.ONE ','); expr(); args_aux())
    | _ -> ()
  
  and expr() = match !tok with
    | L.ONE '(' -> (eat(L.ONE '('); expr(); eat(L.ONE ')'))
    | L.ONE '[' -> (eat(L.ONE '['); list(); eat(L.ONE ']'))
    | L.CID s -> (eat(L.CID ""); tail_opt())
    | L.VID s -> eat (L.VID "")
    | L.NUM n -> eat (L.NUM "")
    | _ -> error()

  and tail_opt() = match !tok with
  | L.ONE '(' -> (eat(L.ONE '('); args(); eat(L.ONE ')'))
  | _ -> ()
  
  and list() = match !tok with
    | L.ONE ']' -> ()
    | _ -> (expr(); list_opt())
  
  and list_opt() = match !tok with
    | L.ONE '|' -> (eat(L.ONE '|'); id())
    | L.ONE ',' -> (eat(L.ONE ','); list())
    | _ -> ()
  
  and id() = match !tok with
    | L.CID _ -> eat (L.CID "")
    | L.VID _ -> eat (L.VID "")
    | L.NUM _ -> eat (L.NUM "")
    | _ -> error()
end



let rec run() =
  print_string "?- ";
  while true do
    flush stdout; 
    Lexer._ISTREAM := stdin;
      try
        Parser.advance(); 
        Parser.command(); 
        print_string "\n?- "
      with
      | Parser.Syntax_error line_number -> Printf.printf "Syntax error at line %d \n?-" line_number;
  done

