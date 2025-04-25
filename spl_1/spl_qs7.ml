module Lexer = struct
  type token = CID of string | VID of string | NUM of string |
              TO | IS | QUIT | OPEN | EOF | ONE of char

  module P = Printf
  
  exception End_of_system
  
  let _ISTREAM = ref stdin
  let ch = ref []
  
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

  exception Syntax_error
  exception Compiler_error

 
  module P = Printf
  (*Q3 print as Prolog type*)
  let rec print_ast ast = match ast with
    | (App (s, hd::tl)) -> (P.printf "%s(" s;
                            print_ast hd; 
                            List.iter (fun x -> (print_string ","; 
                                                  print_ast x)) tl;
                            print_string ").")
    | (App(s, [])) -> P.printf "\"%s\"()." s
    | (Atom s) -> P.printf "%s" s
    | (Var s) -> P.printf "%s" s

  (*Q4 extract var from ast*)
  let ext_var = ref (Var "")
  let release_var() = ext_var := Var ""
  let rec extract_Var ast = match ast with
    | (Atom s) -> ()
    | (Var s) -> ext_var := Var(s)
    | (App( _ , terms)) -> let rec extract_Var_aux term = match term with
                            | [] -> ()
                            |hd::tl -> (extract_Var hd ; extract_Var_aux tl)
                           in 
                           extract_Var_aux terms    

  let print_ast_list lst = match lst with
    (hd::tl) -> (
      match !ext_var with
      | Var "" -> print_string "\nyes"
      | _ ->  print_ast !ext_var;
              print_string " = ";print_ast hd;
              List.iter (fun x -> (print_string ",";
              print_ast !ext_var;
              print_string " = ";
              print_ast x)) tl; 
              print_string "\nyes")
    | [] -> print_string "[]"

  (*Q7 calculate*)
  let calculate spcast =
    let rec calcu ast = match ast with
      | [] ->raise Compiler_error
      | hd::tl -> let rec onestep_calcu x = match x with
      | Atom(s) -> int_of_string s
      | App("+",Atom(s)::xr) -> int_of_string s +(calcu xr)
      | App("-",Atom(s)::xr) -> int_of_string s -(calcu xr)
      | App("*",Atom(s)::xr) -> int_of_string s *(calcu xr)
      | App("/",Atom(s)::xr) -> int_of_string s /(calcu xr)
      | _ -> raise Compiler_error
      in onestep_calcu hd in
    string_of_int (calcu spcast)
    

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
    (*Q7*)
      | (App("=",Var(name)::ari)::t1, term::t2) ->
          if (calculate ari = (calculate [term])) then let r = fun x -> sub name term (unifier x) in
          ut(List.map r t1, List.map r t2, r) else (false, unifier)
      | (term::t2, App("=",Var(name)::ari)::t1) ->
        if (calculate ari = (calculate [term])) then let r = fun x -> sub name term (unifier x) in
        ut(List.map r t1, List.map r t2, r) else (false, unifier)
      (*Q7END*)
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

  (*Q5*)
  let rec succeed query = 
    flush stdout; 
    let input = input_char stdin in
    (*P.printf "input is : %c" input;*)
    match input with
    | ';' -> print_ast_list [query];true
    | '.' -> false
    | _ -> (succeed query)

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

  (*Q6*)
  let rec eval (program, questions) = 
    match questions with
    | [] -> raise Compiler_error
    | qs :: [] ->
      release_var();
      solve(program, [qs], (let _ = extract_Var qs in !ext_var) ,1)
    | qshd :: qstl -> 
      release_var();
      let _ = solve(program, [qshd], (let _ = extract_Var qshd in !ext_var), 1) in ();
      eval (program, qstl)
    
end

module Parser = struct
  module P = Printf
  module E = Evaluator
  module L = Lexer
  
  let tok = ref (L.ONE ' ')

  let getToken () = L.gettoken()
  let advance () = (tok := getToken(); (*L.print_token(!tok);P.printf"\n"*))
  
  exception Syntax_error of int*string

  let error (s) = raise (Syntax_error (!L.line_number, s))

  let check t = match !tok with
    | L.CID _ -> if(t = (L.CID "")) then () else error("checkcid")
    | L.VID _ -> if(t = (L.VID "")) then () else error("checkvid")
    | L.NUM _ -> if(t = (L.NUM "")) then () else error("checknum")
    | tk -> if(tk = t) then () else error("checktk") 

  let eat t = (check t; advance();)
  let prog = ref [[E.Var ""]]

  let rec clauses() = match !tok with
    | L.EOF -> []
    | _ -> let c = clause() in c :: clauses()

  and clause() = match !tok with
    | L.ONE '(' -> (let t = term() in eat(L.ONE '.');[t])
    | _ -> (let p = predicate() in let t_opt = to_opt() in eat(L.ONE '.'); p :: t_opt;)

  and to_opt() = match !tok with
    | L.TO -> (eat L.TO; terms())
    | _ -> []

  and command() = match !tok with
    | L.QUIT -> exit 0
    | L.OPEN -> (eat(L.OPEN);
      match !tok with
      | L.CID s -> (eat(L.CID ""); check (L.ONE '.');
                    L._ISTREAM := open_in (s ^ ".pl");
                    advance(); 
                    prog := clauses(); close_in (!L._ISTREAM))
      | _ -> error("command"))
      (*Q6*)
    | _ -> let t = terms() in (check(L.ONE '.'); let _ = E.eval(!prog, t) in ())

  and term() = match !tok with
    | L.ONE '(' -> (eat(L.ONE '('); let t = term() in  eat(L.ONE ')'); t)
    (*Q7*)
    | L.VID s -> (eat(L.VID ""); eat(L.IS); E.App("=", E.Var(s)::[arithmexp()]))
    | _ -> predicate()

  and terms() = let t = term() in [t] @ terms_aux()

  and terms_aux() = match !tok with
    | L.ONE ',' ->(eat(L.ONE ','); let t = term() in [t] @ terms_aux())
    | _ -> []

  and predicate() = match !tok with
    | L.CID s -> eat(L.CID ""); 
                let args_list = match !tok with
                  | L.ONE '(' -> 
                    eat(L.ONE '('); 
                    let args = args() in
                    eat(L.ONE ')'); 
                    args
                  | _ -> error("predicate")
                in
                E.App(s, args_list) 
    | _ -> error("predicate")

  and args() = let exp = expr() in [exp] @ args_aux()

  and args_aux() = match !tok with
    | L.ONE ',' -> (eat(L.ONE ','); let exp = expr() in [exp] @ args_aux())
    | _ -> []
  
    (*Q7*)
  and expr() = arithmexp()

  and arithmfactor() = match !tok with
    | L.ONE '(' -> (eat(L.ONE '('); 
        let ae = arithmexp() in eat(L.ONE ')'); ae)
    | L.ONE '-' -> (eat(L.ONE '-'); 
        E.App("-",[arithmexp()]))
    | L.ONE '[' -> (eat(L.ONE '['); 
        let ls = list() in eat(L.ONE ']'); ls)
    | L.CID s -> (eat(L.CID ""); 
        let tail = tail_opt() in match tail with
          | None -> E.Atom s
          | Some a -> E.App(s, a))
    | L.VID s -> eat(L.VID ""); E.Var s 
    | L.NUM n -> eat(L.NUM ""); E.Atom n 
    | _ -> error("arithmfactor")

  and arithmterm() = let af = arithmfactor() in arithmterm_aux af

  and arithmterm_aux af = match !tok with
    | L.ONE '*' -> (eat(L.ONE '*'); 
        let af_aux = E.App("*" , [af]@[arithmfactor ()]) in arithmterm_aux af_aux)
    | L.ONE '/' -> (eat(L.ONE '/'); 
        let af_aux = E.App("*" , [af]@[arithmfactor ()]) in  arithmterm_aux af_aux)
    | _ -> af

  and arithmexp() = let at = arithmterm() in arithmexp_aux at
  and arithmexp_aux at = match !tok with
    | L.ONE '+' -> (eat(L.ONE '+'); let at_aux = E.App("+",[at]@[arithmterm()]) in arithmexp_aux at_aux)
    | L.ONE '-' -> (eat(L.ONE '-'); let at_aux = E.App("+",[at]@[arithmterm()]) in arithmexp_aux at_aux)
    | _ -> at
  (*Q7END*)

  and tail_opt() = match !tok with
  | L.ONE '(' -> (eat(L.ONE '('); let a = args() in eat(L.ONE ')'); Some a)
  | _ -> None
  
  and list() = match !tok with
    | L.ONE ']' -> E.Atom "nil"
    | _ -> E.App("cons", [expr(); list_opt()])
  
  and list_opt() = match !tok with
    | L.ONE '|' -> (eat(L.ONE '|'); id())
    | L.ONE ',' -> (eat(L.ONE ','); list())
    | _ -> E.Atom "nil"
  
  and id() = match !tok with
    | L.CID s -> let cid = E.Atom s in eat (L.CID ""); cid
    | L.VID s -> let vid = E.Var s in eat (L.VID ""); vid
    | L.NUM n -> let num = E.Atom n in eat (L.NUM ""); num
    | _ -> error("id")
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
      | Parser.Syntax_error (line_number,s) -> Printf.printf "Syntax error at line %d %s \n?-" line_number s;
  done

  let _ = run()

