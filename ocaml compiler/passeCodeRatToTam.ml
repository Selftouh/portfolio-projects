
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open AstPlacement
  open Type
  open Code

  type t1 = Ast.AstPlacement.programme
  type t2 = string

let analyse_code_variable  v=

let rec aux var=
   (match var with
         | AstType.Ident (infoAst) ->
                  let info=info_ast_to_info infoAst in
                  (match info with
                        |InfoVar(_,t,d,r) |InfoStruct(_,t,d,r,_)-> ["LOAD ("^(string_of_int (getTaille t))^") "^(string_of_int d)^"["^r^"]\n"]
                        
                        | _ -> failwith "Erreur dans les passes précédentes"
                  )

         | AstType.Var(taille,var2) ->
                  let (t2) = aux var2 in 
                  ("LOADI ("^(string_of_int (taille))^")\n")::t2
         
         | AstType.Champ(var2,infoAst)->
            
                  let info=info_ast_to_info infoAst in
                  (print_string(string_of_info info));
                  (match info with
                  
                     |InfoChamp(p,t,d,_,taille)-> 
                              (match var2 with

                                    | AstType.Var(_,var3) ->  
                                             let subCode = aux var3 in
                                             ("LOADI ("^(string_of_int (getTaille t))^")\n")::("SUBR IAdd\n")::("LOADL "^(string_of_int d)^"\n")::subCode

                                    | AstType.Ident (infoAst) ->
                                             let info=info_ast_to_info infoAst in
                                             (match info with

                                                |InfoStruct(s,_,d2,r2,_)->  
                                                      (* (print_string(("info Struct "^s^"."^p^" alapos "^(string_of_int d2)^" p : "^(string_of_type t)^" alapos "^(string_of_int (d))^" dans le reg "^r2^"\n"))); *)
                                                      ["LOAD ("^(string_of_int (getTaille t))^") "^(string_of_int (d+d2))^"["^r2^"]\n"]
                                                
                                                | _ -> failwith "Erreur dans les passes précédentes 1"
                                             )

                                    |  AstType.Champ(var3,infoAst)-> 
                                                (match info_ast_to_info infoAst with

                                                   |InfoChamp(_,t2,d2,_,taille2)-> 
                                                      ("POP ("^string_of_int (getTaille t)^") "^(string_of_int (d))^"\n")::("POP (0) "^(string_of_int (taille-d-(getTaille t)))^"\n")::
                                                      ("POP ("^string_of_int (getTaille t2)^") "^(string_of_int (d2))^"\n")::("POP (0) "^(string_of_int (taille2-d2-(getTaille t2)))^"\n")::(aux var3)
                                             
                                                   | _->failwith "Erreur dans les passes précédentes 1"
                                                )
                           
                              ) 
                     
                     | _ -> failwith "Erreur dans les passes précédentes"
                  )
      )
  in
  String.concat "" (List.rev (aux v))
  
  
 

let rec analyse_code_expression  e = 
      (match e with
         | AstType.AppelFonction(infoAst,lp) ->
            let ls = List.map (analyse_code_expression) lp in
            let info=info_ast_to_info infoAst in
            (match info with
                  |InfoFun(s,_,_) -> (String.concat "" ls )^ "CALL (SB) "^s^"\n"
                  | _ -> failwith "Erreur"
            )
                  
         | AstType.Variable var ->   (analyse_code_variable var)

         |AstType.ListeVal l -> String.concat "" (List.map analyse_code_expression l)
 
         |AllocationDynamique t -> "LOADL "^(string_of_int (getTaille t))^"\n"^"SUBR MAlloc\n"
         
         |Adresse infoAst ->
            let info=info_ast_to_info infoAst in
            (match info with
                  |InfoVar(_,_,d,r) |InfoStruct(_,_,d,r,_)-> "LOADA "^(string_of_int d)^"["^r^"]\n"
                  | _ -> failwith "Erreur"
            )

         | AstType.Booleen (b)-> "LOADL "^(if b then "1" else "0")^"\n"

         | AstType.Entier (n)-> "LOADL "^(string_of_int n)^"\n"

         | AstType.Unaire (op, expr) -> 
            let codeExpr=analyse_code_expression expr in
            codeExpr^
            (match op with
                  |AstType.Numerateur -> "POP (0) 1\n"
                  |AstType.Denominateur -> "POP (1) 1\n"
                  | _ -> failwith "Error, wrong unary op"
            ) 

         |AstType.Binaire (op, expr1,expr2) ->   
            let codeExpr1=analyse_code_expression expr1 in
            let codeExpr2=analyse_code_expression expr2 in
            codeExpr1^codeExpr2^
            (match op with
               | Fraction ->""
               | PlusInt -> "SUBR IAdd\n"
               | PlusRat -> "CALL (SB) RAdd\n"
               | MultInt -> "SUBR IMul\n"
               | MultRat -> "CALL (SB) RMul\n"
               | EquInt | EquBool-> "SUBR IEq\n"
               | Inf -> "SUBR ILss\n"
               | _ -> failwith "Error, wrong unary op"
            )

      )
         
        
            
            
  
let rec analyse_code_instruction typR tailleParams tailleVarDeclare i =
            (match i with
            
                     | AstType.Declaration (infoAst, e)| AstType.DeclarationStruct (infoAst, e,_)  ->
                        let codeExpr=analyse_code_expression e in
                        let info = info_ast_to_info infoAst in
                        (match info with
                              | InfoVar(_,t,d,r) |InfoStruct(_,t,d,r,_)->   
                                    let tailleS=string_of_int (getTaille t) in 
                                    "PUSH "^tailleS^"\n"^
                                    codeExpr^
                                    "STORE ("^tailleS^") "^(string_of_int d)^"["^r^"]\n"

                              | _ -> failwith "Erreur code"
                        )

                     
                     | AstType.Affectation (var,e) ->
                        let codeExpr=analyse_code_expression e in 
                        codeExpr^
                        (match var with
                           |AstType.Ident (infoAst) ->
                              let info = info_ast_to_info infoAst in
                              (match info with
                                 | InfoVar(n,t,d,r) |InfoStruct(n,t,d,r,_)->  
                                       let tailleS=string_of_int (getTaille t) in 
                                       "STORE ("^tailleS^") "^(string_of_int d)^"["^r^"]\n"

                                 | _ -> failwith "Erreur code"
                              )

                           | AstType.Champ(var2,infoAst)-> 
                                 let info = info_ast_to_info infoAst in
                                 (match info with
                                       |InfoChamp(_,t,d,r,_)-> 
                                       
                                       
                                                   (match var2 with
                                                         | AstType.Var(taille,var3) ->  
                                                            let codeLoad=analyse_code_variable var3  in
                                                            codeLoad^
                                                            ("LOADL "^(string_of_int d)^"\n")^
                                                            ("SUBR IAdd\n")^
                                                            ("STOREI ("^(string_of_int (getTaille t))^")\n")
                                                         
                                                         | AstType.Ident (infoAst) ->
                                                                  let info=info_ast_to_info infoAst in
                                                                  (match info with
                                                                     |InfoVar(_,_,d2,_) |InfoStruct(_,_,d2,_,_)-> 
                                                                          "STORE ("^(string_of_int(getTaille t))^") "^(string_of_int (d2+d))^"["^r^"]\n"
                                                                     
                                                                     | _ -> failwith "Erreur dans les passes précédentes"
                                                                  )

                                                         |  AstType.Champ(_,_)-> failwith "Structure imbriqué sans référence"

                                                   ) 
                                                         
                                    
                                       
                                       | _ -> failwith "Erreur dans les passes précédentes"
                                )


                           |AstType.Var (taille,var2) ->  
                              let codeLoad=analyse_code_variable var2 in
                              codeLoad^"STOREI ("^(string_of_int taille)^")\n"
                           
                        )
                        
                        
                        | AstType.AssignationAdditionInt (var,e) ->
                           let codeExpr=analyse_code_expression e in
                           codeExpr^
                           (match var with
                                    |AstType.Ident (infoAst)-> 
                                       let info = info_ast_to_info infoAst in
                                       (match info with
                                          | InfoVar(_,t,d,r) |InfoStruct(_,t,d,r,_)->   
                                                let tailleS=string_of_int (getTaille t) in 
                                                "LOAD ("^tailleS^") "^(string_of_int d)^"["^r^"]\n"^
                                                "SUBR IAdd\n"^
                                                "STORE ("^tailleS^") "^(string_of_int d)^"["^r^"]\n"

                                          | _ -> failwith "Erreur code"
                                       )

                                    | AstType.Champ(var2,infoAst)-> 
                                          let info = info_ast_to_info infoAst in
                                          (match info with
                                                |InfoChamp(_,t,d,r,_)-> 
                                                
                                                
                                                            (match var2 with
                                                                  | AstType.Var(taille,var3) ->  
                                                                     let codeLoad=analyse_code_variable var3  in
                                                                     let codeValeur=analyse_code_variable var  in
                                                                     codeValeur^
                                                                     ("SUBR IAdd\n")^
                                                                     codeLoad^
                                                                     ("LOADL "^(string_of_int d)^"\n")^
                                                                     ("SUBR IAdd\n")^
                                                                     ("STOREI ("^(string_of_int (getTaille t))^")\n")
                                                                
                                                                  | AstType.Ident (infoAst) ->
                                                                        let info=info_ast_to_info infoAst in
                                                                        (match info with
                                                                           |InfoVar(_,_,d2,_) |InfoStruct(_,_,d2,_,_)-> 
                                                                                       "LOAD ("^(string_of_int(getTaille t))^") "^(string_of_int (d2+d))^"["^r^"]\n"^
                                                                                       "SUBR IAdd\n"^
                                                                                       "STORE ("^(string_of_int(getTaille t))^") "^(string_of_int (d2+d))^"["^r^"]\n"
                                                                           
                                                                           | _ -> failwith "Erreur dans les passes précédentes"
                                                                        )
                                                                  
                                                                  |  AstType.Champ(_,_)-> failwith "Structure imbriqué sans référence"

                                                            ) 
                                                                  
                                             
                                                
                                                | _ -> failwith "Erreur dans les passes précédentes"
                                          )
                                    |AstType.Var (taille,var2) ->  
                                    let codeLoad=analyse_code_variable var2 in
                                       let codeValeur=analyse_code_variable var in
                                       codeValeur^"SUBR IAdd\n"^codeLoad^"STOREI ("^(string_of_int taille)^")\n"
                              )

                        | AstType.AssignationAdditionRat (var,e) ->
                           let codeExpr=analyse_code_expression e in
                           codeExpr^
                           (match var with
                              |AstType.Ident (infoAst) -> 
                                 let info = info_ast_to_info infoAst in
                                 (match info with
                                    | InfoVar(_,t,d,r) |InfoStruct(_,t,d,r,_)->   
                                          let tailleS=string_of_int (getTaille t) in 
                                          "LOAD ("^tailleS^") "^(string_of_int d)^"["^r^"]\n"^"CALL (SB) RAdd\n"^"STORE ("^tailleS^") "^(string_of_int d)^"["^r^"]\n"
                                    | _ -> failwith "Erreur code")

                              | AstType.Champ(var2,infoAst)-> 
                                    let info = info_ast_to_info infoAst in
                                 (match info with
                                          |InfoChamp(_,t,d,r,_)-> 
                                                   (match var2 with
                                                            | AstType.Var(taille,var3) ->  
                                                               let codeLoad=analyse_code_variable var3  in
                                                               let codeValeur=analyse_code_variable var  in
                                                               codeValeur^
                                                               ("CALL (SB) RAdd\n")^
                                                               codeLoad^
                                                               ("LOADL "^(string_of_int d)^"\n")^("SUBR IAdd\n")^
                                                               ("STOREI ("^(string_of_int (getTaille t))^")\n")

                                                            | AstType.Ident (infoAst) ->
                                                               let info=info_ast_to_info infoAst in
                                                                  (match info with
                                                                  |InfoStruct(_,_,d2,r2,_)->
                                                                        "LOAD ("^(string_of_int(getTaille t))^") "^
                                                                        (string_of_int (d2+d))^"["^r^"]\n"^"CALL (SB) RAdd\n"^
                                                                        "STORE ("^(string_of_int(getTaille t))^") "^
                                                                        (string_of_int (d2+d))^"["^r^"]\n"
                                                                     
                                                                  | _ -> failwith "Erreur dans les passes précédentes"
                                                                  )

                                                            |  AstType.Champ(_,_)-> failwith "Structure imbriqué sans référence"

                                                      ) 
                                                            
                                       
                                          
                                          | _ -> failwith "Erreur dans les passes précédentes"
                                 )
                                 
                              |AstType.Var (taille,var2) ->  
                                 let codeLoad=analyse_code_variable var2 in
                                 let codeValeur=analyse_code_variable var in
                                 codeValeur^"CALL (SB) RAdd\n"^codeLoad^"STOREI ("^(string_of_int taille)^")\n"
                                 
                           )
                        
                     | AstType.AffichageInt (n) -> 
                        let codeExpr=analyse_code_expression n in codeExpr^"SUBR IOut\n"
                     | AstType.AffichageRat (r) -> 
                        let codeExpr=analyse_code_expression r in codeExpr^"CALL (SB) ROut\n"
                     | AstType.AffichageBool (b) -> 
                        let codeExpr=analyse_code_expression b in codeExpr^"SUBR BOut\n"
                        
                        
                     | AstType.Conditionnelle (c,b1,b2) -> 
                           let condition = analyse_code_expression c in 
                           let b1Code = analyse_code_bloc true typR tailleParams b1 in
                           let b2Code = analyse_code_bloc true typR tailleParams b2 in
                           let labelElse = getEtiquette () in
                           let labelFinIf = getEtiquette () in
                           condition^"JUMPIF (0) "^labelElse^"\n"^b1Code^"JUMP "^labelFinIf^"\n"^labelElse^"\n"^b2Code^labelFinIf^"\n"
                        

                     | AstType.TantQue (c,b) -> 
                        let condition = analyse_code_expression c in 
                           let blocCode = analyse_code_bloc true typR tailleParams b in
                           let debutBoucle = getEtiquette() in
                           let finBoucle = getEtiquette() in
                           debutBoucle^"\n"^condition^"JUMPIF (0) "^finBoucle^"\n"^blocCode^"JUMP "^debutBoucle^"\n"^finBoucle^"\n"

                     | AstType.Retour (e) -> 
                        let expr = analyse_code_expression e in
                        let tailleS= (match typR with
                           | Some t -> string_of_int (getTaille t)
                           | None -> "0")
                           in
                        let tailleParamS= string_of_int tailleParams in
                           let tailleTypR=(match typR with
                           | Some t -> string_of_int (getTaille t)
                           | None -> "0") 
                        in
                        expr^("POP ("^tailleTypR^") "^(string_of_int (tailleVarDeclare))^"\n")^"RETURN ("^tailleS^") "^tailleParamS^"\n"

                     |AstType.Empty -> ""
            )
    
      

and analyse_code_bloc endWithPop typR tailleParams li =
   let rec calculNombreOctect liste_i=   
      (match liste_i with 
         |[] -> 0

         |t::q ->
               (
                  (match t with 
                        | AstType.Declaration ( infoAst, _) ->
                              let info=(info_ast_to_info infoAst) in
                              (match info with
                                 | InfoVar(_,t,_,_) |InfoStruct(_,t,_,_,_)->  getTaille t
                                 | _ -> failwith "err"
                              ) 
                        | _ -> 0
                  ) + calculNombreOctect q
               )
      )
                     
   in
   let mainBloc=String.concat "" (List.map (analyse_code_instruction typR tailleParams (calculNombreOctect li)) li) in
   mainBloc^(if endWithPop then "POP (0) "^(string_of_int (calculNombreOctect li))^"\n" else "" )



let analyse_code_fonction (AstPlacement.Fonction(infoAstF,lp,li)) =

   let fonctionAux infoAst =
       let info = info_ast_to_info infoAst in
               (match info with
                     | InfoVar(_,_,pos,_)-> abs(pos)
                     | InfoStruct(_,_,pos,_,_)-> abs(pos)
                     | _-> failwith "Erreur" 
               )
   in 

   let tailleParams= 
         (match lp with 
         |t::_ -> fonctionAux t
         | _ -> 0)
   in
   let infoF = info_ast_to_info infoAstF in
   let name,typR=
               (match infoF with
                     | InfoFun(n,t,_)-> n,t
                     | _-> failwith "Erreur" )
   in
   name^"\n"^(analyse_code_bloc false (Some typR) tailleParams li)^"HALT\n\n"

 
  


let analyser (AstPlacement.Programme (fonctions,prog)) =
  let codeFonctions = String.concat "" ( List.map analyse_code_fonction fonctions )in 
  let codeMain = "main\n"^(analyse_code_bloc true None 0 prog)^"HALT" in
  (getEntete () ) ^codeFonctions^codeMain

  

end