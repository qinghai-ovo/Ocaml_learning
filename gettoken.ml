module P = Printf
exception End_of_system
type token = LET | BE | IN | EOF | ID of string | NUM of int | ONE of char
(* デフォルト入力ファイル *)
let _ISTREAM = ref stdin
let ch = ref []
let read () = match !ch with [] -> input_char !_ISTREAM
| h::rest -> (ch := rest; h)
let unread c = ch := c::!ch
let lookahead () = try let c = read () in unread c; c with End_of_file -> '$'
(* 整数の認識 *)
let rec integer i =
let c = lookahead () in
if (c >= '0' && c <= '9') then
integer (10*i+(Char.compare (read ()) '0'))
else i
(* 識別子の認識 *)
and identifier id =
let c = lookahead () in
if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
(c >= '0' && c <= '9') || c == '_') then
identifier (id^(Char.escaped (read ())))
else id
(* 字句解析本体 *)
and native_token () =
let c = lookahead () in
if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c
== '_') then
let id = identifier "" in
match id with
"let" -> LET
| "be" -> BE
| "in" -> IN
| _ -> ID (id)
else if (c >= '0' && c <= '9') then NUM (integer 0)
else ONE (read ())
(* ホワイトスペースの読飛ばし含む *)
and gettoken () =
try
let token = native_token () in
match token with
ONE ' ' -> gettoken ()
| ONE '\t' -> gettoken ()
| ONE '\n' -> gettoken ()
| _ -> token
with End_of_file -> EOF