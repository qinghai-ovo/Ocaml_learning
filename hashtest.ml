let t = Hashtbl.create();;
let t = Hashtbl.insert ("hoge",1) t;;
let t = Hashtbl.insert ("boo",2) t;;
let t = Hashtbl.insert ("foo",3) t;;
Hashtbl.search "boo" t;;
let t = Hashtbl.delete "boo" t;;
Hashtbl.search "boo" t;;
     