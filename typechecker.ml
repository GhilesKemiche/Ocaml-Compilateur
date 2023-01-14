open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make(String)
type tenv = typ SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string
let error s = raise (Type_error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (typ_to_string ty_expected) (typ_to_string ty_actual))
let type_error_fun ty_actual =
  error (Printf.sprintf "expected a function but got %s"
           (typ_to_string ty_actual))
let type_error_struct ty_actual =
  error (Printf.sprintf "expected a functionstruct but got %s"
           (typ_to_string ty_actual))
(* vous pouvez ajouter d'autres types d'erreurs *)

(* Vérification des types d'un programme *)
let type_prog prog =

  (* Vérifie que l'expression [e] a le type [type] *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  (* Calcule le type de l'expression [e] *)
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool(b) -> TBool
    | Unit -> TUnit
            
    | Uop(Neg,e1) ->
       check e1 TInt tenv; TInt
    | Uop(Not,e1) ->
       check e1 TBool tenv; TBool
       
    | Bop((Add | Mul | Sub | Div | Mod), e1, e2) -> 
       check e1 TInt tenv; check e2 TInt tenv; TInt
    | Bop((And | Or), e1, e2) ->
       check e1 TBool tenv; check e2 TBool tenv; TBool
    | Bop((Lt | Le), e1, e2) ->
       check e1 TInt tenv; check e2 TInt tenv; TBool
    | Bop((Eq | Neq), e1, e2) ->
       let t1 = type_expr e1 tenv in check e2 t1 tenv; TBool

    | If(c, e1, e2) ->
       check c TBool tenv;
       let t1 = type_expr e1 tenv in check e2 t1 tenv; t1

    | Let(x,e1,e2) ->
       let t1 = type_expr e1 tenv in
       let t2 = type_expr e2 (SymTbl.add x t1 tenv) in t2
                                     
    | Var(e1) -> let t1 = (SymTbl.find e1 tenv) in t1

    
    | Fun(f,t,e1) -> (* *)
       TFun(t, (type_expr e1(SymTbl.add f t tenv)))

    | Fix(a,t,e1) ->
       let t1 = type_expr e1 (SymTbl.add a t tenv) in t1

    | App(e1,e2) ->
       let t1 = type_expr e1 tenv in
           begin match t1 with
           | TFun(t1,t2) -> check e2 t1 tenv; t2
           | t -> type_error_fun t
           end

    | Seq(e1,e2) ->
       let t1 = type_expr e1 tenv in
       let t2 = type_expr e2 tenv in 
       t2

    | GetF(e1,champ) ->
       let t1 = type_expr e1 tenv in
       begin match t1 with
       | TStrct(t) -> error(Printf.sprintf "Typage de GetF pas implémenté")
       | t -> type_error_struct t
       end
    | SetF(e1,champ,e2) ->      
       let t1 = type_expr e1 tenv in
       let t2 = type_expr e2 tenv in
       begin match t1 with
       | TStrct(t) -> error(Printf.sprintf "Typage de GetF pas implémenté")
       | t -> type_error_struct t
       end
    | Strct(l) -> error(Printf.sprintf "Typage de Struct pas implémenté")
    
    

  in

  type_expr prog.code SymTbl.empty
