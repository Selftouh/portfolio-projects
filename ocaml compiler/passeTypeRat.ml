(* Module de la passe de gestion des identifiants *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open Type

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme



let rec analyse_type_variable var =
(match var with
  | AstTds.Ident (infoAst) ->
         ( match info_ast_to_info infoAst with
                  |InfoVar(_,t,_,_) | InfoStruct(_,t,_,_,_) -> (AstType.Ident(infoAst),realType t)
                  | _ -> failwith "Erreur dans la passe tds"
          )

  | AstTds.Var(var2) ->
        let (var3,t2) = analyse_type_variable var2 in 
        (match t2 with
          |Pointeur(t3)-> (AstType.Var((getTaille t3),var3),t3)
          |_-> raise (TypeInattendu(t2,Pointeur(Undefined)))
        )

  |AstTds.Champ(var2,infoAst) -> 
      let (var3,t2) = analyse_type_variable var2 in 
      ( match info_ast_to_info infoAst with   
                  |InfoChamp(_,t,_,_,_) -> (AstType.Champ(var3,infoAst),realType t)
                  | _ -> failwith "Erreur dans la passe tds"
      )
 )

let rec analyse_type_expression  e = 
match e with
    | AstTds.AppelFonction(infoAst,el) ->
        let info = info_ast_to_info infoAst in
        (match info with
            |InfoFun (_,tr,tl2) ->

                let lp = List.map (analyse_type_expression ) el in
                let elT,tl1 = List.split lp in
                if (Type.est_compatible_list tl1 tl2) then
                      (AstType.AppelFonction(infoAst,elT),tr)
                else 
                      raise (TypesParametresInattendus(tl1,tl2))
            |_ -> failwith "Error"
        )


    | AstTds.ListeVal l -> 
        let l2a,l2b=List.split(List.map (fun e -> analyse_type_expression e ) l) in 
        let l3=(List.map (fun e -> (e,"") ) l2b) in
        (AstType.ListeVal(l2a),Struct(l3))

    |AstTds.AllocationDynamique t ->  (AllocationDynamique t,Pointeur(t))

    |AstTds.Adresse infoAst -> 
        (match info_ast_to_info infoAst with
          |InfoVar(_,t,_,_)| InfoStruct(_,t,_,_,_) -> (AstType.Adresse(infoAst),Pointeur(t))
          | _-> failwith "erreur dans la passe precedente (tds)"
        )

    | AstTds.Variable var ->  let (var2,typ)=analyse_type_variable var in (print_string((string_of_type typ)^" ligne 65\n"));(AstType.Variable(var2),typ) 
    | AstTds.Booleen (infoAst)-> (AstType.Booleen(infoAst),Bool) 
    | AstTds.Entier (infoAst)-> (AstType.Entier (infoAst),Int) 
    | AstTds.Unaire (op, expr) ->   
        let (expr1,typ) = (analyse_type_expression expr) in
        if est_compatible typ Rat then
    
          let opt = 
                (match op with
                    |AstSyntax.Numerateur -> AstType.Numerateur
                    |AstSyntax.Denominateur -> AstType.Denominateur
                    | _ -> failwith "Error, wrong unary op"
                ) 
          in
          (AstType.Unaire (opt,expr1), Int )
        else 
          raise (TypeInattendu(typ,Rat))

    |AstTds.Binaire (op, expr1,expr2) ->   
            let (exprT1,typ1) = (analyse_type_expression  expr1) in
            let (exprT2,typ2) = (analyse_type_expression  expr2) in
            (match realType typ1,realType typ2 with
                |Int,Int ->(match op with
                        |AstSyntax.Fraction -> (AstType.Binaire (AstType.Fraction,exprT1,exprT2), Rat) 
                        |AstSyntax.Plus ->  (AstType.Binaire (AstType.PlusInt,exprT1,exprT2), Int)  
                        |AstSyntax.Equ -> (AstType.Binaire (AstType.EquInt,exprT1,exprT2), Bool)  
                        |AstSyntax.Mult ->(AstType.Binaire (AstType.MultInt,exprT1,exprT2), Int)  
                        |AstSyntax.Inf -> (AstType.Binaire (AstType.Inf,exprT1,exprT2), Bool)
                        | _ -> raise (TypeBinaireInattendu(op,typ1,typ2)))
                |Rat,Rat ->(match op with
                        |AstSyntax.Plus ->  (AstType.Binaire(AstType.PlusRat,exprT1,exprT2), Rat)  
                        |AstSyntax.Mult ->(AstType.Binaire(AstType.MultRat,exprT1,exprT2), Rat)
                        | _ -> raise (TypeBinaireInattendu(op,typ1,typ2)))  
                       
                |Bool,Bool ->(match op with
                        |AstSyntax.Equ -> (AstType.Binaire (AstType.EquBool,exprT1,exprT2), Bool)
                        | _ -> raise (TypeBinaireInattendu (op,typ1,typ2)))
                |Pointeur(_),Pointeur(_) ->(match op with
                        |AstSyntax.Equ -> (AstType.Binaire (AstType.EquInt,exprT1,exprT2), Bool)
                        | _ -> raise (TypeBinaireInattendu (op,typ1,typ2)))
                  
                | _,_ ->    raise (TypeBinaireInattendu (op,typ1,typ2)))        
        

            
let rec modifier_type_info_struct lt infoAstL = 
  (match lt,infoAstL with
          |t::q1,infoAst::q2->
                    $let _= modifier_type_info (realType(fst t)) infoAst in (modifier_type_info_struct q1 q2)
          |[],[]->()
          | [],_|_,[]->failwith "Pas la meme taille"
  )
  
let rec analyse_type_instruction  typR i =
  match i with
  | AstTds.Declaration (t, infoAst, e) ->
  
     let (exprT,typ1)= analyse_type_expression e in
     if (est_compatible typ1 t) then
        let _=modifier_type_info (realType t) infoAst in
        let _=
        (match info_ast_to_info infoAst with
            |InfoStruct(_,Struct(l),_,_,infoAstL)->modifier_type_info_struct l infoAstL
            |_->()
        )
        in 
        AstType.Declaration( infoAst,exprT)
     else
        raise (TypeInattendu(typ1,t))

  | AstTds.Affectation (var,e) ->

      let (var1,t)= analyse_type_variable var in 
      let (exprT,typ1)= analyse_type_expression e in
        if (Type.est_compatible typ1 t) then
           AstType.Affectation(var1,exprT)
        else
           raise (TypeInattendu(typ1,t))
       

  | AstTds.AssignationAddition (var,e) ->
        let  (var1,t)= analyse_type_variable var in 
        let (exprT,typ1)= analyse_type_expression e in
        if (Type.est_compatible typ1 t) then
            (match typ1 with
                |Int -> AstType.AssignationAdditionInt(var1,exprT)
                |Rat -> AstType.AssignationAdditionRat(var1,exprT)
                |_ -> raise (TypeInattenduPourAssignatioAddition(typ1))
            )
        else
           raise (TypeInattendu(typ1,t))
       
      
  | AstTds.Typedef (_,_) -> Empty    

  | AstTds.Affichage(e) ->
    let (exprT,typ1)= analyse_type_expression e in
    ( match realType typ1 with 
      | Int -> AstType.AffichageInt(exprT)
      | Rat -> AstType.AffichageRat(exprT)
      | Bool ->AstType.AffichageBool(exprT)
      | _ -> failwith "Error, undefined type"
    )
     
  | AstTds.Conditionnelle (c,b1,b2) -> 
    let cT,cTyp= analyse_type_expression c in
    let b1T= analyse_type_bloc typR b1 in
    let b2T= analyse_type_bloc typR b2 in 
      if est_compatible cTyp Bool then
        AstType.Conditionnelle(cT,b1T,b2T)
      else
         raise (TypeInattendu(cTyp,Bool))

  | AstTds.TantQue (c,b) -> 
    let cT,cTyp= analyse_type_expression c in
    let bT= analyse_type_bloc typR b in 
      if est_compatible cTyp Bool then
        AstType.TantQue(cT,bT)
      else
        raise (TypeInattendu(cTyp,Bool))

  | AstTds.Retour (e) -> 
      let expr,typ1=analyse_type_expression e in
      (match typR with
      | Some t -> if est_compatible t typ1 then 
                    AstType.Retour( expr)
                  else
                    raise (TypeInattendu(typ1,t) )
      | None -> raise RetourDansMain)

  |AstTds.Empty -> Empty
  
    
      

and analyse_type_bloc typR li =List.map (analyse_type_instruction typR ) li




let analyse_type_fonction (AstTds.Fonction(t,infoAstF,lp,li))  =

  let _=List.map (fun a->modifier_type_info (realType(fst a)) (snd a) ) lp in
  let _,infoAstLp=List.split lp in 
  let tp= List.map fst lp in
  let _=modifier_type_fonction_info (realType t) tp infoAstF in
  AstType.Fonction(  infoAstF,infoAstLp,analyse_type_bloc (Some t) li)
  


let analyser (AstTds.Programme (_,fonctions,prog)) =
  let nf = List.map (analyse_type_fonction ) fonctions in 
  let nb = analyse_type_bloc None prog in
  AstType.Programme (nf,nb)

end