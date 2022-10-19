type typ = Bool | Int | Rat | Undefined | TypeN of string * typ | Pointeur of typ | Struct of (typ*string) list | Recurssive of string*int

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Undefined -> "Undefined"
  | TypeN (n, t) -> "TypeNomme( "^n^" , "^(string_of_type t)^")"
  | Pointeur t -> "Pointeur("^(string_of_type t)^")"
  | Recurssive (s,t) -> "type recurssive de "^s^" de taille : "^(string_of_int t)
  | Struct l -> "Struct{"^ (String.concat "," (List.map (fun (t,s)-> (string_of_type t) ^" : "^s) l))^"}"



let rec realType t = 
  match t with 
  | TypeN(_,tt) -> realType tt
  | Pointeur(tt) -> Pointeur(realType tt) 
  | Struct l -> let l2 = List.map (fun (a,b)->(realType a,b)) l in Struct(l2)
  | typeUsuelle -> typeUsuelle


let rec getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Pointeur _ -> 1
  | TypeN (_,t) ->getTaille t
  | Undefined -> 0
  | Recurssive( _,t)-> t
  | Struct l -> List.fold_right (fun e res -> res+ (getTaille (fst e))) l 0
  
let%test _ = getTaille Int = 1
let%test _ = getTaille Bool = 1
let%test _ = getTaille Rat = 2


let rec est_compatible t1 t2 =
  match ( t1),( t2) with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true 
  | TypeN (_,typ1) ,typ2 -> est_compatible  typ1 typ2
  | typ1, TypeN (_,typ2) -> est_compatible  typ1 typ2
  | Pointeur(_),Pointeur(Undefined)-> true
  | Pointeur(Undefined),Pointeur(_)-> true
  | Pointeur(t1), Pointeur(t2)->est_compatible  t1 t2
  | Undefined ,Undefined -> true
  | Struct l1,Struct l2 ->  est_compatible_list (List.map fst l1) (List.map fst l2)
  | Recurssive( _,t),typ -> t==(getTaille typ)
  | typ,Recurssive(_, t) -> t==(getTaille typ)
  | _ -> false 


and est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let%test _ = est_compatible Bool Bool
let%test _ = est_compatible Int Int
let%test _ = est_compatible Rat Rat
let%test _ = not (est_compatible Int Bool)
let%test _ = not (est_compatible Bool Int)
let%test _ = not (est_compatible Int Rat)
let%test _ = not (est_compatible Rat Int)
let%test _ = not (est_compatible Bool Rat)
let%test _ = not (est_compatible Rat Bool)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Int Undefined)
let%test _ = not (est_compatible Rat Undefined)
let%test _ = not (est_compatible Bool Undefined)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Undefined Rat)
let%test _ = not (est_compatible Undefined Bool)



let%test _ = est_compatible_list [] []
let%test _ = est_compatible_list [Int ; Rat] [Int ; Rat]
let%test _ = est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool]
let%test _ = not (est_compatible_list [Int] [Int ; Rat])
let%test _ = not (est_compatible_list [Int] [Rat ; Int])
let%test _ = not (est_compatible_list [Int ; Rat] [Rat ; Int])
let%test _ = not (est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool ; Int])


