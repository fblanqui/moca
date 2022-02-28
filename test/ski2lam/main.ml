open Pprint;;
open Skilam;;
open Lam2ski;;
open Utils;;
open Type;;
open Format;;
open Skiv;;

let x = "x"
and y = "y"
and z = "z"
and u = "u"
;;

let lx = Var x
and ly = Var y
and lz = Var z
and lu = Var u
;;

let i = ski I
and k = ski K
and s = ski S
;;

let delta =  Abst (x, App (lx, lx));;
let deldelta = App (delta, delta);;
let c = Abst (x, (Abst (y, App (ly, lx))));;
let sd = Abst(x, App( App(lx, lx), lx));;
let threesd = App(App (sd, sd), sd);;
let myk = Abst(x, Abst(y, lx));;
let myi = Abst(x, lx);;
let mys = Abst (x, Abst(y, Abst (z, App( App(lx, lz), App(ly, lz)))))
(* X combinator *)
let damien = Abst (u, App ( App ( App (lu, myk), mys), myk));;
let myski= Sapp (Sapp (S, K), I);;
let mysk = Sapp(S, K);;
let sii = Sapp(Sapp (S, I), I) ;;
 

let l1 = Abst(x, Abst (y, ly))
and l2 = Abst (y, Abst (z, App(App(myk, lz), App(myi, lz))))
and l3 = App (App(mys, myk), myi);;

let rec tlam2tls = function
  | App (x, y) -> lapp (tlam2tls x, tlam2tls y)
  | Var x -> lvar x
  | Abst (x, y) -> labst (x, tlam2tls y)
;;

exception Notlam;;
let rec tls2tlam = function
  | Lapp (x, y) -> App (tls2tlam x, tls2tlam y)
  | Lvar x -> Var x
  | Labst (x, y) -> Abst(x, tls2tlam y)
  | _ -> raise Notlam
;;


let rec tski2tls = function
  | I -> si
  | K -> sk
  | S -> ss
  | Sapp (x, y) -> ssapp (tski2tls x, tski2tls y)
  | Svar x -> ssvar x
;;

exception Not_SKI;;


let rec tls2nski = function
  | Si -> ni
  | Sk -> nk
  | Ss -> ns
  | Ssapp (x, y) | Lapp (x,y) -> napp (tls2nski x, tls2nski y)
  | Ssvar x | Lvar x -> nvar x
  | Toski _ -> failwith "to ski remaining"
  | Tolam _ -> failwith "tolam?"
  | Labst _ -> failwith "abst?"
  | _ -> raise Not_SKI
;;

let rec tls2tski = function
  | Si -> I
  | Sk -> K  | Ss -> S
  | Ssapp (x, y) | Lapp (x,y)-> Sapp (tls2tski x, tls2tski y)
  | Ssvar x | Lvar x -> Svar x
  | Toski _ -> failwith "to ski remaining"
  | Tolam _ -> failwith "tolam?"
  | Labst _ -> failwith "abst?"
  | _ -> raise Not_SKI
;;


let rec nski2tski = function
  | Ni -> I
  | Nk -> K
  | Ns -> S
  | Napp (x, y) -> Sapp (nski2tski x, nski2tski y)
  | Nvar x -> Svar x
;;


let title ppf name =
   Format.fprintf ppf "@.@.******************@.";
   fprintf ppf "%s @." name;
   Format.fprintf ppf "******************@."
;;

let print_lam_and_ski name l =
  let ppf = Format.std_formatter in (
  title ppf name; 
  pout_lam l;
  fprintf ppf "@ ---->@ ";
  pout_ls (toski (tlam2tls l))
    )
;;


let print_ski_and_lam name s =
  let ppf = Format.std_formatter in (
  title ppf name; 
  pout_ski s;
  fprintf ppf "@ ---->@ ";
  pout_ls (tolam s)
    )
;;


(* This should be in type.mlm but it is not exported!!! *)

(* replace x with y in l *)
let rec replace_in_term x y t =
     if t = x then y
     else
       begin
         match t with
         | TyArrow (a, b) -> TyArrow (replace_in_term x y a,
                                      replace_in_term x y b)
         | _ -> t
       end

let rec replace_with x y l =
   List.map (fun (s,u) -> (replace_in_term x y s), replace_in_term x y
   u) l
;;       

let rec ty_fv = function
  | TyId x -> [x]
  | TyArrow (x, y) -> union (ty_fv x) (ty_fv y)
;;

exception Ufail of  ttype * ttype;;

let rec unify = function
  | [] -> []
  | (x, y) :: l ->
      begin
        match x, y with
        | x, y when x = y -> unify l
        | TyArrow (a, b), TyArrow (c, d) ->
            unify ((a, c) :: (b, d) :: l)

        | TyId z, _ when not (List.mem z (ty_fv y)) ->
           (unify (replace_with x y l))@[(x, y)]

        | _ , TyId z when not (List.mem z (ty_fv x)) ->
          (unify (replace_with y x l))@[(y, x)]

        | _, _ -> raise (Ufail (x, y))
      end  
;;                                          



let type_this lam =
  let ppf = Format.std_formatter in (
  Gensymb.reset_new_symb();
  match ltype (lam, [], None, []) with
  | Ltype (_, e, Some t, c) -> (
      fprintf ppf "Env@.";
       List.iter (
      fun (x, y) ->
        fprintf ppf "@[%s@,,@,%a@]@." x p_ty y
       ) e;
     fprintf ppf "Unification Constraints@.";
      List.iter (
      fun (x, y) ->
        fprintf ppf "@[%a@ ?=?@ %a@]@." p_ty x p_ty y
       ) c;
      fprintf ppf "Unification results@.";
      let subst = unify c in
      begin

       List.iter (
      fun (x, y) ->
        fprintf ppf "@[%a@ :=@ %a@]@." p_ty x p_ty y
       ) subst ;

      fprintf ppf "Type inference result@.";
      let unified_type =
        List.fold_left (fun te (x, y) -> replace_in_term x y te) t
          (List.rev subst)
       in
      pout_ty unified_type;
      pout_ty t
      end
      )
  | _ -> assert false
)
;;

let type_and_print name lam =
  let ppf = Format.std_formatter in (
  title ppf name; 
  pout_lam lam;
  fprintf ppf "@ ---->@ ";
  type_this lam;
    )


let from_ski_to_ski name ski =
  try
  title std_formatter name;
  (* To lambda for typing*)
  let ilam = tolam  ski  in
  (
   (*typing *)
   let lam = (tls2tlam ilam) in
    (type_this lam ;
   (* back to unnormalized ski *)
    let ski2 = tls2tski (toski ilam) in
   (* to normalize ski *)
    let nski2 = nski2tski (tls2nski ilam) in
    List.iter pout_ski [ski2; nski2]
   ) )
  with
  | Ufail (t1, t2) ->
      fprintf std_formatter "UF: %a !!! %a" p_ty t1 p_ty t2
  | Not_SKI -> print_string "Notski\n"
 
;;

let main () =
  Printf.printf "SKI and Lambda-calculus\n";
  List.iter pout_ls  (i :: k :: [s]);
 (*  List.iter *)
(*     (fun (x, y) -> print_lam_and_ski x y) *)
(*     [("Delta delta", deldelta); *)
(*      ("Delta", delta); *)
(*      ("C", c); *)
(*      ("Super Delta", sd); *)
(*      ("3 super delta", threesd); *)
(*      ("K", myk); *)
(*      ("S",mys); *)
(*      ("X", damien); *)
(*     ]; *)
   List.iter (fun (x, y) -> print_ski_and_lam x y)
    [("S", S); ("K", K);
     ("I", I);
     ("SII", sii);
    ];
     List.iter (fun (x, y) ->
       try  
       type_and_print x y
      with
       | Ufail _ -> print_string "Unification Failed\n"
       |  _ -> print_string "Unbound Var\n"
       )
    [
     ("var", Var "x");
     ("K", myk);
     ("S",mys);
     ("I",myi);
     ("Delta", delta);
     ("Delta delta", deldelta);
     ("L1",l1); ("L2",l2);
     ("L3", l3);
    ]
    ;

   List.iter (fun (x,y) -> from_ski_to_ski x y)
   [("SII", sii);
    ("SKI",
    myski);
    ("SK", mysk);
    
   ];
;; 

let exec_type () = ();;
let exec_l2s () = ();;


let umsg = "Usage: lam2ski [options]" ;;

let rec argspec = [
  ("-t", Arg.Unit exec_type, "");
  ("-l2s", Arg.Unit exec_l2s, "");
   ("-help", Arg.Unit print_usage,
   " print this option list and exit");
   ("--help", Arg.Unit print_usage,
   " print this option list and exit");
]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0;
;;

let new_main () =
  print_usage ();
  Arg.parse (Arg.align argspec) (fun s -> ());
  exit 0;
;;

main ();;
