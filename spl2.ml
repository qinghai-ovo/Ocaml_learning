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
  
  let rec integer i =
    let c = lookahead () in
    if (c >= '0' && c <= '9') then
      integer (i^(Char.escaped (read ())))
    else i
  
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
      if (c >= 'a' && c <= 'z') then 
        let id = identifier "" in
        match id with
        | "quit" -> QUIT
        | "open" -> OPEN
        | "is" -> IS
        | _ -> CID id
      else if (c >= 'A' && c <= 'Z') then
        VID (identifier "")
      else if (c >= '0' && c <= '9') then 
        NUM (integer "")
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
      | ONE '\n' -> gettoken ()
      | _ -> token
    with
      End_of_file -> EOF
end

module Parser = struct
  exception Syntax_error of int
  module L = Lexer
  
  let tok = ref (L.gettoken ())
  
  let eat t = if !tok = t then tok := L.gettoken () else raise (Syntax_error !L.line_number)
  
  let check t = if !tok = t then () else raise (Syntax_error !L.line_number)
  
  let advance () = tok := L.gettoken ()
  
  let rec clauses() = clause(); clauses_aux()
  and clauses_aux() = match !tok with
    | L.ONE '.' -> advance(); clauses()
    | _ -> ()
  and clause() = term(); terms_aux(); check(L.ONE '.')
  and terms_aux() = match !tok with
    | L.ONE ',' -> (eat(L.ONE ','); term(); terms_aux())
    | _ -> ()
  and term() = match !tok with
    | L.CID s -> (eat(L.CID s); eat(L.ONE '('); args(); eat(L.ONE ')'))
    | _ -> error()
  and args() = expr(); args_aux()
  and args_aux() = match !tok with
    | L.ONE ',' -> (eat(L.ONE ','); expr(); args_aux())
    | _ -> ()
  and expr() = arithmexp()
  and arithmfactor() = match !tok with
    | L.ONE '(' -> (eat(L.ONE '('); arithmexp(); eat(L.ONE ')'))
    | L.ONE '-' -> (eat(L.ONE '-'); arithmexp())
    | L.ONE '[' -> (eat(L.ONE '['); list(); eat(L.ONE ']'))
    | L.CID s -> (advance(); tail_opt())
    | L.VID s -> advance()
    | L.NUM n -> advance()
    | _ -> error()
  and arithmterm() = arithmfactor(); arithmterm_aux()
  and arithmterm_aux() = match !tok with
    | L.ONE '*' -> (eat(L.ONE '*'); arithmfactor(); arithmterm_aux())
    | L.ONE '/' -> (eat(L.ONE '/'); arithmfactor(); arithmterm_aux())
    | _ -> ()
  and arithmexp() = arithmterm(); arithmexp_aux()
  and arithmexp_aux() = match !tok with
    | L.ONE '+' -> (eat(L.ONE '+'); arithmterm(); arithmexp_aux())
    | L.ONE '-' -> (eat(L.ONE '-'); arithmterm(); arithmexp_aux())
    | _ -> ()
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
    | L.CID _ -> advance()
    | L.VID _ -> advance()
    | L.NUM _ -> advance()
    | _ -> error()
  and error() = raise (Syntax_error !L.line_number)
  
  (* 解析 command *)
  and command() = match !tok with
    | L.QUIT -> exit 0
    | L.OPEN -> (eat(L.OPEN);
        match !tok with
        | L.CID s -> (eat(L.CID ""); check (L.ONE '.');
          L._ISTREAM := open_in (s ^ ".pl");
          advance(); clauses(); close_in (!L._ISTREAM))
        | _ -> error())
    (* 处理一般的谓词定义 *)
    | L.CID s -> (
        eat(L.CID s); 
        eat(L.ONE '('); 
        args(); 
        eat(L.ONE ')');
        (* 处理 :- 符号 *)
        eat(L.ONE ':'); 
        eat(L.ONE '-'); 
        expr(); 
        eat(L.ONE '.')
      )
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
