module type HASHTBL =
sig
  type 'a tbl
  val create: unit -> 'a tbl
  val insert: string * 'a -> 'a tbl -> 'a tbl 
  val search : string -> 'a tbl -> 'a
  val delete : string -> 'a tbl -> 'a tbl
end

module Hashtbl: HASHTBL = 
struct
  exception Not_found  
  type 'a tbl = (string * 'a) list array
  
  let size = 1003

  (*str key to hashtable index k*)
  let hash key =
    let len = String.length key in
    let rec aux h i =
      if i >= len then (h mod size)
      else 
        (*hi = (a*h + code of Ci) mod size, a = 31*)
        let h_i = 31*h + Char.code key.[i] in
        (*aux hi i+1*)
        aux h_i (i + 1)
      in
    (*h0 = 0, i = 0*)
    aux 0 0

  let create() = Array.make size []
   
  let insert (key, value) tbl=   
    let index = hash key in 
    (*remove same key*)
    let rec remove_old key bucket =
      match bucket with
      | [] -> []
      | (k, v) :: rest ->
        (*if same then del,else remove rest*)
        if k = key then rest
        else (k, v) :: remove_old key bucket
    in
    tbl.(index) <- [(key, value)] @ remove_old key tbl.(index);
    tbl

  let search key tbl = 
    let index = hash key in 
    let rec search_aux key buscket =
      match buscket with
      | [] -> raise Not_found
      | (k, v) :: rest -> 
        (*if same then return v else search rest*)
        if k = key then v
        else search_aux key rest
      in
    search_aux key tbl.(index) 
      

  let delete key tbl = 
    let index = hash key in 
    let rec delete_aux key bucket =
      match bucket with
      | [] -> []
      | (k, v) :: rest ->
        (*if same then del and return rest, else del rest*)
        if k = key then rest
        else (k, v) :: delete_aux key bucket
      in
      tbl.(index) <- delete_aux key tbl.(index);
    tbl  

end
