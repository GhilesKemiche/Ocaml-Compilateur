(* Interprète Mini-ML *)

open Mml

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make(String)

(* Valeurs *)
type value =
  | VInt   of int
  | VBool  of bool
  | VUnit
  | VPtr   of int
(* Élements du tas *)
type heap_value =
  | VClos  of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t

let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p

(* Interprétation d'un programme complet *)
let eval_prog (p: prog): value =
  
  (* Initialisation de la mémoire globale *)
  let (mem: (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
  in

  (* Interprétation d'une expression, en fonction d'un environnement
     et de la mémoire globale *)
  let rec eval (e: expr) (env: value Env.t): value = 
    match e with
    | Int n  -> VInt n
    | Var x -> Env.find x env
    | Bool b -> VBool b
    | Unit -> VUnit
    | Bop(Add, e1, e2) -> VInt (evali e1 env + evali e2 env)
    | Bop(Mul, e1, e2) -> VInt (evali e1 env * evali e2 env)
    | Bop(op, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      begin match (v1, v2) with
       | (VInt i1, VInt i2) ->
          begin match op with
            | Add -> VInt  (i1  +  i2)
            | Sub -> VInt  (i1  -  i2)
            | Mul -> VInt  (i1  *  i2)
            | Div -> VInt  (i1  /  i2)
            | Mod -> VInt  (i1 mod i2)
            | Eq  -> VBool (i1 ==  i2)
            | Neq -> VBool (i1 !=  i2)
            | Le  -> VBool (i1 <=  i2)
            | Lt  -> VBool (i1  <  i2)
            | _   -> failwith "Opérateur binaire non supporté sur des entiers"
          end
       | (VBool b1, VBool b2) ->
          begin match op with
            | And -> VBool (b1 && b2)
            | Or  -> VBool (b1 || b2)
            | Eq  -> VBool (b1 == b2)
            | Neq -> VBool (b1 != b2)
            | _   -> failwith "Opérateur binaire non supporté sur des booléens"
          end
       | _ -> failwith "Opérandes de types incompatibles pour cet opérateur binaire"
       end
    | If (c, e1, e2) ->
      let vc = eval c env in
      (match vc with
      | VBool b -> if b then eval e1 env else eval e2 env
      | _ -> failwith "Condition de type incompatible pour l'instruction if")
        
    | Let (x, e1, e2) ->
      let v1 = eval e1 env in
      let env' = Env.add x v1 env in
      eval e2 env'

    | Uop(Neg, e) ->
        let v = eval e env in
        (match v with
        | VInt i -> VInt (-i)
        | _ -> failwith "Opérande de type incompatible pour l'opérateur négatif")
    | Uop(Not, e) ->
        let v = eval e env in
        (match v with
        | VBool b -> VBool (not b)
        | _ -> failwith "Opérande de type incompatible pour l'opérateur de négation logique")

    | Fun(x,t,e) ->
      let p = new_ptr() in
      let c = VClos(x, e, env) in
      Hashtbl.add mem p c;
      VPtr p
      
    | App(e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      (match v1 with
      | VPtr p ->
        let clos = Hashtbl.find mem p in
        (match clos with
        | VClos(x, e, env') ->
          let env'' = Env.add x v2 env' in
          eval e env''
        | _ -> failwith "Appel de fonction sur une valeur non fonctionnelle")
      | _ -> failwith "Appel de fonction sur une valeur non fonctionnelle")
    
    | GetF(e, f) ->
        let v = eval e env in
        (match v with
        | VPtr p ->
          let strct = Hashtbl.find mem p in
          (match strct with
          | VStrct ht ->
            (try Hashtbl.find ht f
            with Not_found -> failwith "Champ non trouvé")
          | _ -> failwith "Accès au champ d'une valeur non structurée")
        | _ -> failwith "Accès au champ d'une valeur non structurée")

    | SetF (strct, f, e) ->
      let v = eval strct env in
      (match v with
      | VPtr p ->
        let v' = Hashtbl.find mem p in
        (match v' with
        | VStrct ht ->
          let v'' = eval e env in
          Hashtbl.replace ht f v'';
          v''
        | _ -> failwith "La valeur n'est pas une structure")
      | _ -> failwith "La valeur n'est pas une adresse de tas")

    | Strct fields ->
      let ht = Hashtbl.create 16 in
      List.iter (fun (f, e) -> Hashtbl.add ht f (eval e env)) fields;
      let ptr =  (new_ptr()) in 
      Hashtbl.add mem ptr (VStrct ht);
      VPtr(ptr)
      
    | Seq (e1, e2) ->
      let _ = eval e1 env in
      eval e2 env
      
    | Fix (f, typ, e) ->
      let v = VClos (f, e, env) in
      let ptr = new_ptr() in
      Hashtbl.add mem ptr v;
      let env' = Env.add f (VPtr ptr) env in
      eval e env'

        
        
      
  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali (e: expr) (env: value Env.t): int = 
    match eval e env with
    | VInt n -> n
    | _ -> assert false
  in

  eval p.code Env.empty
