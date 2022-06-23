open Batteries
open Cfg
open Prog
open Utils

(* Analyse de vivacité *)

(* [vars_in_expr e] renvoie l'ensemble des variables qui apparaissent dans [e]. *)
let rec vars_in_expr (e: expr) =
   (* TODO *)
   match e with
   |Ebinop(_,expr1,expr2) -> Set.union (vars_in_expr expr1) (vars_in_expr expr2)
   |Eunop(_,expr) -> vars_in_expr expr
   |Eint(_) -> Set.empty
   |Evar(s) -> Set.singleton s

(* [live_cfg_node node live_after] renvoie l'ensemble des variables vivantes
   avant un nœud [node], étant donné l'ensemble [live_after] des variables
   vivantes après ce nœud. *)
let live_cfg_node (node: cfg_node) (live_after: string Set.t) =
   (* TODO *)
   match node with
   |Cassign(s,expr,_) -> Set.union (vars_in_expr expr) (Set.remove s live_after )
   |Creturn(expr) -> Set.union (vars_in_expr expr) live_after
   |Cprint(expr,_) -> Set.union (vars_in_expr expr) live_after
   |Ccmp(expr,_,_) -> Set.union (vars_in_expr expr) live_after
   |Cnop(_) -> live_after

(* [live_after_node cfg n] renvoie l'ensemble des variables vivantes après le
   nœud [n] dans un CFG [cfg]. [lives] est l'état courant de l'analyse,
   c'est-à-dire une table dont les clés sont des identifiants de nœuds du CFG et
   les valeurs sont les ensembles de variables vivantes avant chaque nœud. *)
let live_after_node cfg n (lives: (int, string Set.t) Hashtbl.t) : string Set.t =
   (* TODO *)
   Set.fold (fun elt acc -> Set.union acc (match Hashtbl.find_option lives elt with
                                          |None -> Set.empty
                                          |Some(sSet) -> sSet)) 
            (succs cfg n) Set.empty 
(* [live_cfg_nodes cfg lives] effectue une itération du calcul de point fixe.

   Cette fonction met à jour l'état de l'analyse [lives] et renvoie un booléen
   qui indique si le calcul a progressé durant cette itération (i.e. s'il existe
   au moins un nœud n pour lequel l'ensemble des variables vivantes avant ce
   nœud a changé). *)
let live_cfg_nodes (cfg:(int, cfg_node) Hashtbl.t) (lives : (int, string Set.t) Hashtbl.t) : bool =
   (* TODO *)
   Hashtbl.fold (fun n node b ->
      let former_in = Hashtbl.find_option lives n in (*! Option*)
      let out_n = live_after_node cfg n lives in 
      let new_in = live_cfg_node node out_n in
       match former_in with
      |None -> begin Hashtbl.replace lives n new_in; true end
      |Some(sSet) -> if Set.equal sSet new_in
                        then b
                        else begin Hashtbl.replace lives n new_in; true end ) cfg false

(* [live_cfg_fun f] calcule l'ensemble des variables vivantes avant chaque nœud
   du CFG en itérant [live_cfg_nodes] jusqu'à ce qu'un point fixe soit atteint.
   *)

let live_cfg_fun (f: cfg_fun) : (int, string Set.t) Hashtbl.t =
   (* TODO *)
  let lives = Hashtbl.create 17 in
  while live_cfg_nodes f.cfgfunbody lives do
   ();
  done;
  lives
 