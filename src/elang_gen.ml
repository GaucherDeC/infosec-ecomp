open Ast
open Elang
open Prog
open Report
open Options
open Batteries
open Elang_print
open Utils

let tag_is_unop = 
  function
  |Tneg -> true
  |_ -> false

let unop_of_tag =
  function
    Tneg -> Eneg
    | _ -> assert false
let tag_is_binop =
  function
    Tadd -> true
  | Tsub -> true
  | Tmul -> true
  | Tdiv -> true
  | Tmod -> true
  | Txor -> true
  | Tcle -> true
  | Tclt -> true
  | Tcge -> true
  | Tcgt -> true
  | Tceq -> true
  | Tne  -> true
  | _    -> false

let binop_of_tag =
  function
    Tadd -> Eadd
  | Tsub -> Esub
  | Tmul -> Emul
  | Tdiv -> Ediv
  | Tmod -> Emod
  | Txor -> Exor
  | Tcle -> Ecle
  | Tclt -> Eclt
  | Tcge -> Ecge
  | Tcgt -> Ecgt
  | Tceq -> Eceq
  | Tne -> Ecne
  | _ -> assert false

(* [make_eexpr_of_ast a] builds an expression corresponding to a tree [a]. If
   the tree is not well-formed, fails with an [Error] message. *)

let rec make_eexpr_of_ast (a: tree) : expr res =
  let res =
    match a with
    | StringLeaf(s) -> OK(Evar(s))
    | Node(Tint,[IntLeaf(i)]) -> OK(Eint(i))
    | Node(t, [e1; e2]) when tag_is_binop t -> 
      make_eexpr_of_ast e1 >>= fun expr1 -> 
        make_eexpr_of_ast e2 >>= fun expr2 -> 
          OK(Ebinop(binop_of_tag t,expr1,expr2))
    | Node(t,[e]) when tag_is_unop t-> make_eexpr_of_ast e >>= fun expr -> OK(Eunop(unop_of_tag t,expr))
    | _ -> Error (Printf.sprintf "Unacceptable ast in make_eexpr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_eexpr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let rec make_einstr_of_ast (a: tree) : instr res =
  let res =
    match a with
    | Node(Tassign,[Node(Tassignvar,[StringLeaf(i);e])]) -> 
      make_eexpr_of_ast e >>= (fun expr -> OK(Iassign(i,expr)))

    | Node(Tif,[e;i1;i2]) -> 
      make_eexpr_of_ast e >>= fun expr -> 
        make_einstr_of_ast i1 >>= fun instr1 -> 
        make_einstr_of_ast i2 >>= fun instr2 -> 
        OK(Iif(expr,instr1,instr2))
    
    | Node(Twhile,[e;i1]) -> 
      make_eexpr_of_ast e >>= fun expr -> 
        make_einstr_of_ast i1 >>= fun instr1 -> 
          OK(Iwhile(expr,instr1))
      
    | Node(Treturn,[e]) -> 
      make_eexpr_of_ast e >>= fun expr -> OK(Ireturn(expr))

    | Node(Tprint,[e]) -> 
      make_eexpr_of_ast e >>= fun expr -> OK(Iprint(expr))
    
    | Node(Tblock,l) -> 
      (list_map_res make_einstr_of_ast l )>>=(fun linstr -> OK(Iblock(linstr)))

    | _ -> Error (Printf.sprintf "Unacceptable ast in make_einstr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_einstr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let make_ident (a: tree) : string res =
  match a with
  | Node (Targ, [StringLeaf(s)]) ->
    OK (s)
  | a -> Error (Printf.sprintf "make_ident: unexpected AST: %s"
                  (string_of_ast a))

let make_fundef_of_ast (a: tree) : (string * efun) res =
  match a with
  | Node (Tfundef, [Node(Tfunname,[StringLeaf(fname)]); Node (Tfunargs, fargs); Node(Tfunbody,[fbody])]) ->
     (* TODO *)
     (list_map_res make_ident fargs) >>= (fun lvar -> make_einstr_of_ast fbody >>= (fun instr -> OK(fname,{funargs= lvar; funbody=instr})))

  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tfundef, got %s."
             (string_of_ast a))

let make_eprog_of_ast (a: tree) : eprog res =
  match a with
  | Node (Tlistglobdef, l) ->
    list_map_res (fun a -> make_fundef_of_ast a >>= fun (fname, efun) -> OK (fname, Gfun efun)) l
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tlistglobdef, got %s."
             (string_of_ast a))

let pass_elang ast =
  match make_eprog_of_ast ast with
  | Error msg ->
    record_compile_result ~error:(Some msg) "Elang";
    Error msg
  | OK  ep ->
    dump !e_dump dump_e ep (fun file () ->
        add_to_report "e" "E" (Code (file_contents file))); OK ep

