type info = { mutable hash : int };;

let mk_info h = { hash = h };;

open Atom
;;

module Type_t = struct
  type t =
     | Bfalse
     | Btrue
     | Batom of info * atom
     (* begin
          rule Batom (p) -> Bp ((p, Btrue, Bfalse))
        end *)
     | Bnot of info * t
     (* begin
          involutive
          rule Bnot (Bp ((p, x, y))) -> Bp ((p, Bnot x, Bnot y))
          rule Bnot (Btrue) -> Bfalse
          rule Bnot (Bfalse) -> Btrue
        end *)
     | Band of info * t * t
     (* begin
          absorbent (Bfalse)
          neutral (Btrue)
          rule Band ((Bp ((p, x, y)), Bp ((q, z, w))))
            when Atom.compare p q = 0 ->
            Bp ((p, Band ((x, z)), Band ((y, w))))
          rule Band ((Bp ((p, x, y)), Bp ((q, z, w))))
            when Atom.compare p q < 0 ->
            Bp ((p, Band ((x, Bp ((q, z, w)))), Band ((y, Bp ((q, z, w))))))
          rule Band ((Bp ((q, x, y)), Bp ((p, z, w))))
            when Atom.compare p q < 0 ->
            Bp ((p, Band ((Bp ((q, x, y)), z)), Band ((Bp ((q, x, y)), w))))
        end *)
     | Bor of info * t * t
     (* begin
          absorbent (Btrue)
          neutral (Bfalse)
          rule Bor ((Bp ((p, x, y)), Bp ((q, z, w))))
            when Atom.compare p q = 0 ->
            Bp ((p, Bor ((x, z)), Bor ((y, w))))
          rule Bor ((Bp ((p, x, y)), Bp ((q, z, w))))
            when Atom.compare p q < 0 ->
            Bp ((p, Bor ((x, Bp ((q, z, w)))), Bor ((y, Bp ((q, z, w))))))
          rule Bor ((Bp ((q, x, y)), Bp ((p, z, w))))
            when Atom.compare p q < 0 ->
            Bp ((p, Bor ((Bp ((q, x, y)), z)), Bor ((Bp ((q, x, y)), w))))
        end *)
     | Bxor of info * t * t
     (* begin
          neutral (Bfalse)
          rule Bxor ((x, Btrue)) -> Bnot x
          rule Bxor ((Btrue, x)) -> Bnot x
          rule Bxor ((Bp ((p, x, y)), Bp ((q, z, w))))
            when Atom.compare p q = 0 ->
            Bp ((p, Bxor ((x, z)), Bxor ((y, w))))
          rule Bxor ((Bp ((p, x, y)), Bp ((q, z, w))))
            when Atom.compare p q < 0 ->
            Bp ((p, Bxor ((x, Bp ((q, z, w)))), Bxor ((y, Bp ((q, z, w))))))
          rule Bxor ((Bp ((q, x, y)), Bp ((p, z, w))))
            when Atom.compare p q < 0 ->
            Bp ((p, Bxor ((Bp ((q, x, y)), z)), Bxor ((Bp ((q, x, y)), w))))
        end *)
     | Bimplies of info * t * t
     (* begin
          rule Bimplies ((x, y)) -> Bor ((Bnot x, y))
        end *)
     | Bp of info * atom * t * t
     (* begin
          rule Bp ((_, x, y)) when x == y -> x
        end *)
  ;;
  
  
end
;;
include Type_t
;;
let rec equal_t moca_x moca_y =
  match (moca_x, moca_y) with
  | (Bp (_, moca_x1, moca_x2, moca_x3), Bp (_, moca_y1, moca_y2, moca_y3)) ->
    ((moca_x1 = moca_y1) && (moca_x2 == moca_y2)) && (moca_x3 == moca_y3)
  | (Bimplies (_, moca_x1, moca_x2), Bimplies (_, moca_y1, moca_y2)) ->
    (moca_x1 == moca_y1) && (moca_x2 == moca_y2)
  | (Bxor (_, moca_x1, moca_x2), Bxor (_, moca_y1, moca_y2)) ->
    (moca_x1 == moca_y1) && (moca_x2 == moca_y2)
  | (Bor (_, moca_x1, moca_x2), Bor (_, moca_y1, moca_y2)) ->
    (moca_x1 == moca_y1) && (moca_x2 == moca_y2)
  | (Band (_, moca_x1, moca_x2), Band (_, moca_y1, moca_y2)) ->
    (moca_x1 == moca_y1) && (moca_x2 == moca_y2)
  | (Bnot (_, moca_x1), Bnot (_, moca_y1)) -> moca_x1 == moca_y1
  | (Batom (_, moca_x1), Batom (_, moca_y1)) -> moca_x1 = moca_y1
  | _ -> false
;;

let rec get_hash_t moca_x =
  match moca_x with
  | Bp ({ hash = h }, _, _, _) -> h
  | Bimplies ({ hash = h }, _, _) -> h
  | Bxor ({ hash = h }, _, _) -> h
  | Bor ({ hash = h }, _, _) -> h
  | Band ({ hash = h }, _, _) -> h
  | Bnot ({ hash = h }, _) -> h
  | Batom ({ hash = h }, _) -> h
  | Btrue -> 2
  | Bfalse -> 1
;;

let rec hash_t moca_x =
  match moca_x with
  | Bp (_, moca_x1, moca_x2, moca_x3) ->
    get_hash_t moca_x3 + (get_hash_t moca_x2 + (Hashtbl.hash moca_x1 + 7))
  | Bimplies (_, moca_x1, moca_x2) ->
    get_hash_t moca_x2 + (get_hash_t moca_x1 + 6)
  | Bxor (_, moca_x1, moca_x2) ->
    get_hash_t moca_x2 + (get_hash_t moca_x1 + 5)
  | Bor (_, moca_x1, moca_x2) ->
    get_hash_t moca_x2 + (get_hash_t moca_x1 + 4)
  | Band (_, moca_x1, moca_x2) ->
    get_hash_t moca_x2 + (get_hash_t moca_x1 + 3)
  | Bnot (_, moca_x1) -> get_hash_t moca_x1 + 2
  | Batom (_, moca_x1) -> Hashtbl.hash moca_x1 + 1
  | Btrue -> 2
  | Bfalse -> 1
;;

module Hashing_t = struct
  type t = Type_t.t;;
  let equal = equal_t;;
  let hash = hash_t;;
end
;;

module Sharing_t = Weak.Make (Hashing_t);;

let htbl_t = Sharing_t.create 1009
;;
module Memo_htbl = struct
  type ('a, 'b) t = {
    hash : 'a -> int;
    eq : 'a -> 'a -> bool;
    mutable length : int;
    mutable values : ('a * 'b) list array;
  }
  ;;
  
  let create h eq len =
    if len <= 0 ||
      len >= Sys.max_array_length
    then invalid_arg (Printf.sprintf "Memo_tbl.create %d" len) else
    let hash x =
      let h_code = h x in
      if h_code < 0 then
        failwith (Printf.sprintf "Memo_tbl: bad hashing value %d" h_code) else
      h_code in
    { hash = hash;
      eq = eq;
      length = len;
      values = Array.make len []; }
  ;;
  
  let add t x y =
    let h = t.hash x mod t.length in
    t.values.(h) <- (x, y) :: t.values.(h)
  ;;
  
  let find t x =
    let h = t.hash x mod t.length in
    let bucket = t.values.(h) in
    let rec find_in_bucket x = function
      | [] -> raise Not_found
      | (x0, y0) :: rest -> if t.eq x x0 then y0 else find_in_bucket x rest in
    find_in_bucket x bucket
  ;;
  
end
;;

let positive_hash h =
  let h =
    if h < 0 then -h else h in
    if h < 0 then h - 10 else h
;;

let new_poly_memo_table size =
  let hash x = positive_hash (Hashtbl.hash x)
  and eq = ( = ) in
  Memo_htbl.create hash eq size
;;

let new_unary_memo_table size =
  let hash x = positive_hash (get_hash_t x)
  and eq = equal_t in
  Memo_htbl.create hash eq size
;;

let new_binary_memo_table size =
  let hash (x, y) =
    let h = get_hash_t x + get_hash_t y in
    positive_hash h
  and eq (x0, y0) (x1, y1) = equal_t x0 x1 && equal_t y0 y1 in
  Memo_htbl.create hash eq size
;;

let new_ternary_memo_table size =
  let hash (x, y, z) =
    let h = get_hash_t x + get_hash_t y + get_hash_t z in
    positive_hash h
  and eq (x0, y0, z0) (x1, y1, z1) =
    equal_t x0 x1 && equal_t y0 y1 && equal_t z0 z1 in
  Memo_htbl.create hash eq size
;;

let memo_hash_table_size = 1024
;;

let rec batom_memo_table = new_poly_memo_table 1024
;;

let rec bnot_memo_table = new_unary_memo_table 1024
;;

let rec band_memo_table = new_binary_memo_table 1024
;;

let rec bor_memo_table = new_binary_memo_table 1024
;;

let rec bxor_memo_table = new_binary_memo_table 1024
;;

let rec bimplies_memo_table = new_binary_memo_table 1024
;;

let new_a_t_t_memo_table size =
  let hash (x, y, z) =
    let h = Hashtbl.hash x + get_hash_t y + get_hash_t z in
    positive_hash h
  and eq (x0, y0, z0) (x1, y1, z1) =
    x0 = x1 && equal_t y0 y1 && equal_t z0 z1 in
  Memo_htbl.create hash eq size
;;

let rec bp_memo_table = new_a_t_t_memo_table 1024
;;

let rec mk_Batom moca_x1 =
  let info = { hash = 0 } in
  let v = Batom (info, moca_x1) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec mk_Bnot moca_x1 =
  let info = { hash = 0 } in
  let v = Bnot (info, moca_x1) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec mk_Band moca_x1 moca_x2 =
  let info = { hash = 0 } in
  let v = Band (info, moca_x1, moca_x2) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec mk_Bor moca_x1 moca_x2 =
  let info = { hash = 0 } in
  let v = Bor (info, moca_x1, moca_x2) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec mk_Bxor moca_x1 moca_x2 =
  let info = { hash = 0 } in
  let v = Bxor (info, moca_x1, moca_x2) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec mk_Bimplies moca_x1 moca_x2 =
  let info = { hash = 0 } in
  let v = Bimplies (info, moca_x1, moca_x2) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec mk_Bp moca_x1 moca_x2 moca_x3 =
  let info = { hash = 0 } in
  let v = Bp (info, moca_x1, moca_x2, moca_x3) in
  info.hash <- hash_t v;
  try Sharing_t.find htbl_t v with
  | Not_found -> Sharing_t.add htbl_t v;
                 v
;;

let rec compare_bimplies = Pervasives.compare

and bimplies moca_z =
  try Memo_htbl.find bimplies_memo_table moca_z with
  | Not_found ->
    let result =
  match moca_z with
  | (x, y) -> bor ((bnot x, y))
  | (moca_x, moca_y) -> mk_Bimplies moca_x moca_y in
    Memo_htbl.add bimplies_memo_table moca_z result;
    result

and bnot moca_x =
  try Memo_htbl.find bnot_memo_table moca_x with
  | Not_found ->
    (let result =
       match moca_x with
       | Bp ((_, p, x, y)) -> bp ((p, bnot x, bnot y))
       | Btrue -> bfalse
       | Bfalse -> btrue
       | Bnot (_, moca_x) -> moca_x
       | _ -> mk_Bnot moca_x in
     Memo_htbl.add bnot_memo_table moca_x result;
     result)

and bfalse = Bfalse

and compare_band = Pervasives.compare

and band moca_z =
  try Memo_htbl.find band_memo_table moca_z with
  | Not_found ->
    let result =
  match moca_z with
  | (Bp ((_, p, x, y)), Bp ((_, q, z, w))) when (Atom.compare p q = 0) ->
    bp ((p, band ((x, z)), band ((y, w))))
  | (Bp ((_, p, x, y)), Bp ((_, q, z, w))) when (Atom.compare p q < 0) ->
    bp ((p, band ((x, bp ((q, z, w)))), band ((y, bp ((q, z, w))))))
  | (Bp ((_, q, x, y)), Bp ((_, p, z, w))) when (Atom.compare p q < 0) ->
    bp ((p, band ((bp ((q, x, y)), z)), band ((bp ((q, x, y)), w))))
  | (moca_x, Btrue) -> moca_x
  | (Btrue, moca_x) -> moca_x
  | (_, Bfalse) -> bfalse
  | (Bfalse, _) -> bfalse
  | (moca_x, moca_y) -> mk_Band moca_x moca_y in
     Memo_htbl.add band_memo_table moca_z result;
     result

and batom moca_x =
  try Memo_htbl.find batom_memo_table moca_x with
  | Not_found ->
    (let result =
       match moca_x with
       | p -> bp ((p, btrue, bfalse))
       | _ -> mk_Batom moca_x in
     Memo_htbl.add batom_memo_table moca_x result;
     result)

and bp moca_z =
  try Memo_htbl.find bp_memo_table moca_z with
  | Not_found ->
    let result =
  match moca_z with
  | (_, x, y) when (x == y) -> x
  | (moca_x1, moca_x2, moca_x3) -> mk_Bp moca_x1 moca_x2 moca_x3 in
     Memo_htbl.add bp_memo_table moca_z result;
     result

and compare_bor = Pervasives.compare

and bor moca_z =
  try Memo_htbl.find bor_memo_table moca_z with
  | Not_found ->
    let result =
  match moca_z with
  | (Bp ((_, p, x, y)), Bp ((_, q, z, w))) when (Atom.compare p q = 0) ->
    bp ((p, bor ((x, z)), bor ((y, w))))
  | (Bp ((_, p, x, y)), Bp ((_, q, z, w))) when (Atom.compare p q < 0) ->
    bp ((p, bor ((x, bp ((q, z, w)))), bor ((y, bp ((q, z, w))))))
  | (Bp ((_, q, x, y)), Bp ((_, p, z, w))) when (Atom.compare p q < 0) ->
    bp ((p, bor ((bp ((q, x, y)), z)), bor ((bp ((q, x, y)), w))))
  | (moca_x, Bfalse) -> moca_x
  | (Bfalse, moca_x) -> moca_x
  | (_, Btrue) -> btrue
  | (Btrue, _) -> btrue
  | (moca_x, moca_y) -> mk_Bor moca_x moca_y in
     Memo_htbl.add bor_memo_table moca_z result;
     result

and btrue = Btrue

and compare_bxor = Pervasives.compare

and bxor moca_z =
  try Memo_htbl.find bxor_memo_table moca_z with
  | Not_found ->
    let result =
  match moca_z with
  | (x, Btrue) -> bnot x
  | (Btrue, x) -> bnot x
  | (Bp ((_, p, x, y)), Bp ((_, q, z, w))) when (Atom.compare p q = 0) ->
    bp ((p, bxor ((x, z)), bxor ((y, w))))
  | (Bp ((_, p, x, y)), Bp ((_, q, z, w))) when (Atom.compare p q < 0) ->
    bp ((p, bxor ((x, bp ((q, z, w)))), bxor ((y, bp ((q, z, w))))))
  | (Bp ((_, q, x, y)), Bp ((_, p, z, w))) when (Atom.compare p q < 0) ->
    bp ((p, bxor ((bp ((q, x, y)), z)), bxor ((bp ((q, x, y)), w))))
  | (moca_x, Bfalse) -> moca_x
  | (Bfalse, moca_x) -> moca_x
  | (moca_x, moca_y) -> mk_Bxor moca_x moca_y in
     Memo_htbl.add bxor_memo_table moca_z result;
     result

;;

external eq_t : t -> t -> bool = "%eq"
;;

