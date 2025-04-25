module Lexer = struct
  type token = CID of string | VID of string | NUM of string | TO | IS | QUIT | OPEN | EOF
  | ONE of char
  module P = Printf
  exception End_of_system
  let _ISTREAM = ref stdin
  let ch = ref []
  let ci = ref 1
  let cl = ref 1
  let count_read () = match !ci with n -> ci := n+1
  let count_space () = match !ci with n -> ci := n-1
  let reset_count_ci () = ci := 1
  let count_line () = match !cl with n -> cl := n+1
  let reset_count_cl () = cl := 1
  let read () = match !ch with [] -> input_char !_ISTREAM
  | h::rest -> ( count_read () ;ch := rest; h)
  let unread c = ( ch := c::!ch)
  let lookahead () = try let c = read () in unread c; c with End_of_file -> '$'
  let rec integer i =
  let c = lookahead () in
   if (c>='0' && c <= '9' ) then
   integer (i^(Char.escaped(read())))
   else i
  (* 文字列として数字を構成 *)
  and identifier id =
  let c = lookahead () in
  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') || c == '_') then
  identifier (id^(Char.escaped (read ())))
  else id
  and native_token () =
  let c = lookahead () in
  if (c >= 'a' && c <= 'z') then let id = identifier "" in
  match id with
  "is" -> count_space ();IS
  |"quit" -> count_space ();QUIT
  |"open" -> count_space ();OPEN
  | _ -> CID (count_space ();id)
   (* CID に対する識別子および予約語 *)
  else if (c >= 'A' && c <= 'Z') then VID (count_space ();identifier "")(* VID に対する識別子 *)
  else if (c >= '0' && c <= '9') then NUM (count_space ();integer "")
  else if (c== ':') then let _ = read() in
   if lookahead() ='-' then let _ = read() in TO
   else ONE ':'
  (* :- を認識して TO を返す *)
  else ONE (count_space ();read ())
  and gettoken () =
  try
  let token = native_token () in
  match token with
  ONE ' ' -> gettoken ()
  | ONE '\t' -> gettoken ()
  | ONE '\n' -> reset_count_ci ();count_line ();gettoken ()
  | _ -> token
  with End_of_file -> EOF
  let print_token tk =
  match tk with
  (CID i) -> P.printf "CID(%s)" i
  | (VID i) -> P.printf "VID(%s)" i
  | (NUM i) -> P.printf "NUM(%s)" i
  | (TO) -> P.printf ":-"
  | (QUIT) -> P.printf "quit"
  | (OPEN) -> P.printf "open"
  | (IS) -> P.printf "is"
  | (EOF) -> P.printf "eof"
  | (ONE c) -> P.printf "ONE(%c)" c
  let rec run () =
  flush stdout;
  let rlt = gettoken () in
  match rlt with
  (ONE '$') -> raise End_of_system
  | _ -> (print_token rlt; P.printf "¥n"; run())
  end;;
  module Evaluator =
  struct
  type ast = Atom of string | Var of string | App of string * ast list
  module P = Printf
  module L = Lexer
  exception Compiler_error
  exception Syntax_error
  let tok = ref (L.ONE ' ')
  let reset_tok() = tok := L.ONE ' '
  let getToken () = L.gettoken ()
  let advance () = (tok := getToken()(*;L.print_token (!tok)*))
  let error () = raise Syntax_error
  let check t = match !tok with
   L.CID _-> if (t = (L.CID "")) then () else error()
   | L.VID _-> if (t = (L.VID "")) then () else error()
   | L.NUM _ -> if (t = (L.NUM "")) then () else error()
   | tk -> if (tk=t) then () else error()
  let eat t = (check t; advance())
  let _ISTREAM = ref stdin
  let varx = ref (Var "")
  let reset_varx () = varx := Var ""
  let rec print_ast ast = match ast with
  (App(s, hd::tl)) -> (P.printf "%s" s; print_string "(";
  print_ast hd; List.iter (fun x -> (print_string ","; print_ast x)) tl;
  print_string ")";print_string ".")
  | (App(s, [])) -> P.printf "\"%s\"[]" s
  | (Atom s) -> P.printf "%s" s
  | (Var s) -> (P.printf "%s" s)
  let print_ast_list lst = match lst with
  (hd::tl) -> ( print_ast hd;
  List.iter (fun x -> (print_string ",";
  print_ast x)) tl; print_string " yes. ¥n")
  | [] -> print_string "[]"
  (*let succeed query = (print_ast_list [query];true)*)
  let rec succeed_command var = L._ISTREAM := stdin;reset_tok(); advance (); match !tok with
  L.ONE ';' -> (check(L.ONE ';');print_ast_list [var];true)
  |L.ONE '.' -> (check(L.ONE '.');raise Compiler_error)
  | _ -> raise Syntax_error

  let calculate spcast =
  let rec calcu ast = match ast with
  [] ->raise Compiler_error
  |fi::re -> let rec onestep_calcu x = match x with
  |Atom(s) -> int_of_string s
  |App("+",Atom(s)::xr) -> int_of_string s +(calcu xr)
  |App("-",Atom(s)::xr) -> int_of_string s -(calcu xr)
  |App("*",Atom(s)::xr) -> int_of_string s *(calcu xr)
  |App("/",Atom(s)::xr) -> int_of_string s /(calcu xr)
  | _ -> raise Compiler_error
  in onestep_calcu fi in
  string_of_int (calcu spcast)

  let sub name term =
  let rec mapVar ast = match ast with
  (Atom x) -> Atom(x)
  | (Var n) -> if n=name then term else Var n
  | (App(n, terms)) -> App(n, List.map mapVar terms)
  in mapVar

  let mgu (a,b) =
  let rec ut (one, another, unifier) = match (one, another) with
  ([], []) -> (true, unifier)
  | (term::t1, Var(name)::t2) ->
   let r = fun x -> sub name term (unifier x) in
   ut(List.map r t1, List.map r t2, r)
  | (Var(name)::t1, term::t2) ->
   let r = fun x -> sub name term (unifier x) in
   ut(List.map r t1, List.map r t2, r)
  | (Atom(n)::t1, Atom(m)::t2) ->
   if n=m then ut(t1,t2,unifier) else (false, unifier)
  | (App("=",Var(name)::ari)::t1, term::t2) ->
   if (calculate ari = (calculate [term])) then let r = fun x -> sub name term (unifier x) in
  ut(List.map r t1, List.map r t2, r) else (false, unifier)
  | (term::t2, App("=",Var(name)::ari)::t1) ->
   if (calculate ari = (calculate [term])) then let r = fun x -> sub name term (unifier x) in
  ut(List.map r t1, List.map r t2, r) else (false, unifier)
  | (App(n1,xt1)::t1, App(n2,xt2)::t2) ->
   if n1=n2 && List.length xt1 = List.length xt2 then
   ut(xt1@t1, xt2@t2, unifier) else (false, unifier)
  | (_,_) -> (false, unifier);
  in ut ([a],[b], (fun x -> x))

  let rename ver term =
   let rec mapVar ast = match ast with
   (Atom x) -> Atom(x)
   | (Var n) -> Var(n^"#"^ver)
   | (App(n, terms)) -> App(n, List.map mapVar terms)
   in mapVar term
  let rec get_Var ast = match ast with
  (Atom x) -> ()
  | (Var n) -> varx := Var(n)
  | (App( _ , terms)) -> let rec into_app term = match term with
  [] -> ()
  |fi::re -> (get_Var fi ; into_app re)
  in into_app terms
  let rec solve (program, question, result, depth) = match question with
  [] -> succeed_command result
  | goal::goals ->
   let onestep _ clause =
   match List.map (rename (string_of_int depth)) clause with
   [] -> raise Compiler_error
   | head::conds ->
   let (unifiable, unifier) = mgu(head,goal) in
   if unifiable then
   solve (program, List.map unifier (conds@goals), unifier result, depth+1)
   else true
   in List.fold_left onestep true program
  let rec eval (program, questions) =
  match questions with
  [] -> raise Compiler_error
  |question::[] -> (reset_varx (); solve(program, [question], (let _ = get_Var question in !varx) ,
  1))
  |question::_questions -> (reset_varx (); let _ =solve(program, [question], (let _ = get_Var
  question in !varx) ,1) in () ; eval(program, _questions))
  end;;
  module Parser = struct
  module L = Lexer
  module E = Evaluator
  let tok = ref (L.ONE ' ')
  let prog = ref [[E.Var ""]]
  let getToken () = L.gettoken ()
  let advance () = (tok := getToken()(*;L.print_token (!tok)*))
  exception Syntax_error
  let error () = raise Syntax_error
  let check t = match !tok with
   L.CID _-> if (t = (L.CID "")) then () else error()
   | L.VID _-> if (t = (L.VID "")) then () else error()
   | L.NUM _ -> if (t = (L.NUM "")) then () else error()
   | tk -> if (tk=t) then () else error()
  let eat t = (check t; advance())
  let rec clauses () = match !tok with
   L.EOF -> []
   | _ -> (let c = clause () in c::clauses())
  and clause() =match !tok with
   L.ONE '(' -> (let t = term() in (); eat(L.ONE '.'); [t])
   | _ -> ( let pre = predicate() in (); let too = to_opt() in (); eat(L.ONE '.'); pre::too)
  and to_opt() = match !tok with
   L.TO -> (eat(L.TO) ; terms())
   | _ -> []
  and command() = match !tok with
  L.QUIT -> exit 0
  | L.OPEN -> (eat(L.OPEN);
   match !tok with
   L.CID s -> (eat(L.CID ""); check (L.ONE '.');
   L._ISTREAM := open_in (s^".pl"); advance();
   prog := clauses();Lexer.reset_count_cl ();close_in (!L._ISTREAM))
   | _ -> error())
   | _ -> let t = terms() in
  (check(L.ONE '.');Lexer.reset_count_cl (); let _ = E.eval(!prog, t) in ())

  and term() = match !tok with
   L.ONE '(' -> (eat( L.ONE '(') ; let t = term() in () ; eat(L.ONE ')');t)
   | L.VID t -> (eat(L.VID "") ; eat(L.IS) ; E.App("=",E.Var(t)::[arithmexp()]))
   | _ -> predicate()

  and terms() = (let t = term() in t::terms_opt() )
  and terms_opt () = match !tok with
   L.ONE ',' -> (eat(L.ONE ',') ; let t = term() in t::terms_opt())
   | _ -> []

  and predicate () = match !tok with
  L.CID s -> (eat(L.CID "") ; eat(L.ONE '(' ) ; let a = args() in () ; eat (L.ONE ')'); E.App(s,a))
  | _ -> error()

  and args () = ( let ae = arithmexp() in ae::args_opt ())
  and args_opt () = match !tok with
   L.ONE ',' -> (eat(L.ONE ',') ;(let ae = arithmexp() in ae::args_opt()))
   | _ -> []

  and arithmexp() = let at = arithmterm () in arithmexp_opt at
  and arithmexp_opt _t = match !tok with
   L.ONE '+' -> (eat(L.ONE '+') ; let plmi = E.App("+",[_t]@[arithmterm()]) in arithmexp_opt
  plmi)
   |L.ONE '-' -> (eat(L.ONE '-') ; let plmi = E.App("-",[_t]@[arithmterm()]) in arithmexp_opt
  plmi)
   |_ -> _t

  and arithmterm () = let af = arithmfactor () in arithmterm_opt af
  and arithmterm_opt _f = match !tok with
   L.ONE '*' -> (eat(L.ONE '*') ; let kawa = E.App("*" , [_f]@[arithmfactor ()]) in
  arithmterm_opt kawa)
   | L.ONE '/' -> (eat(L.ONE '/') ; let kawa = E.App("/" , [_f]@[arithmfactor ()]) in
  arithmterm_opt kawa)
   | _ -> _f

   and arithmfactor () =
   match !tok with
   L.ONE '(' -> (eat(L.ONE '('); let e = arithmexp() in () ;eat(L.ONE ')'); e)
   | L.ONE '-' -> (eat(L.ONE '-') ; E.App("-",[arithmexp()]))
   | L.ONE '[' -> (eat(L.ONE '['); let l = list() in ();eat(L.ONE ']'); l)
   | L.CID s -> (eat(L.CID "") ; tail_opt s)
   | L.VID s -> (eat(L.VID ""); E.Var(s))
   | L.NUM n -> (eat(L.NUM ""); E.Atom(n))
   | _ -> error()
  and tail_opt funct =match !tok with
   L.ONE '(' -> (eat(L.ONE '(') ; let a = args () in ();eat(L.ONE ')'); E.App(funct,a))
   | _ -> E.Atom (funct)
  and list() = match !tok with
   L.ONE ']' -> E.Atom("nil")
   | _ -> E.App("cons",[arithmexp();list_opt()])
  and list_opt() = match !tok with
   L.ONE '|' -> (eat(L.ONE '|'); id())
   | L.ONE ',' -> (eat(L.ONE ','); list())
   | _ -> E.Atom("nil")
  and id() = match !tok with
   L.CID s -> (eat(L.CID "");E.Atom(s))
   |L.VID s -> (eat(L.VID "");E.Var(s))
   |L.NUM n -> (eat(L.NUM "");E.Atom(n))
   | _ -> error()
  end
  let rec run() =
  print_string "?- ";
  while true do
  try
  flush stdout; Lexer._ISTREAM := stdin;
  Parser.advance(); Parser.command(); print_string "¥n?- "
  with Parser.Syntax_error -> Printf.printf "¥n";Printf.printf "Error with :";
  Lexer.print_token !Parser.tok ;Printf.printf " line";
  Printf.printf "%d" !Lexer.cl; Printf.printf "of character";Printf.printf "%d" !Lexer.ci ;
  Printf.printf "¥n"
  done;;
  let _ = run ();;