(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open Type

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme

let rec analyse_tds_variable  tds var =
    (match var with
      | AstSyntax.Ident (s) ->
            let infoAst= (chercherGlobalement tds s) in
            ( match infoAst with
                |None  -> raise (IdentifiantNonDeclare s)
                |Some q ->            
                        (match (info_ast_to_info q) with
                              |InfoVar _ | InfoStruct _ -> AstTds.Ident(q)
                              | _ -> raise (MauvaiseUtilisationIdentifiant s)
                        )
              )
      | AstSyntax.Var(var2) -> AstTds.Var(analyse_tds_variable tds var2) 
      | AstSyntax.Champ (var2,s) ->
              let var3=analyse_tds_variable tds var2 in
              let infoAst= (chercherGlobalement tds s ) in
            ( match infoAst with
                |None  -> raise (IdentifiantNonDeclare s)
                |Some q ->            
                        (match (info_ast_to_info q) with
                              |InfoChamp _ -> AstTds.Champ (var3,q) 
                              | _ -> raise (MauvaiseUtilisationIdentifiant s)
                          )                
            )
                    
      )





let rec analyse_tds_struct tds l taille=
    (match l with
    | (t,n)::q ->
          (match chercherLocalement tds n with
                | None ->
                    (* Création de l'information associée à l'identfiant *)
                    let info = InfoChamp (n,t, 0, "",taille) in
                    (* Création du pointeur sur l'information *)
                    let ia = info_to_info_ast info in
                    (* Ajout de l'information (pointeur) dans la tds *)
                    ajouter tds n ia; 
                  
                    (ia)::(analyse_tds_struct tds q taille)
                | Some _ -> raise (DoubleDeclaration n)
          )
    | []-> []
    )


let rec analyse_tds_struct_instanciation s tds l taille =

        (match l with
        | (t,n1)::q ->
        let n=n1^"@"^s in

        let ia= afficher_locale tds;
        (match chercherGlobalement tds n1 with
                | None -> raise (IdentifiantNonDeclare n1)
                | Some c ->  c           
        )
        in
        (match chercherLocalement tds n with
                    | None ->
                        let info2 = InfoChamp (n,t, 0, "",taille) in
                        let ia2 = info_to_info_ast info2 in
                        ajouter tds n ia2;                      
                        (ia)::(analyse_tds_struct_instanciation s tds q taille)
                    | Some _ -> raise (DoubleDeclaration n)
        )

        | []-> []
        )




let estRecurssive n t =
    let rec aux n t k=
        (
        match t with
              |TypeN(s,t2)-> (k>0 && s==n) || (aux n t2 (k+1))
              |Struct l ->  
                          List.fold_right (fun (typ,s) res -> (k>0 && s==n) || (aux n typ (k+1))|| res) l false
              |Pointeur t3 ->aux n t3 (k+1)
              | _-> false
        )
    in 
    aux n t 0

let rec analyse_tds_typeN tds t =
      
        (match t with
          |TypeN (nt,_) -> 
          (match chercherGlobalement tds nt with 
              |None -> raise (IdentifiantNonDeclare nt)
              |Some infoAst -> let info = info_ast_to_info infoAst in
                (match info with
                    |InfoTypeN(_,tt) -> 
                    if (estRecurssive nt tt) then Type.TypeN(nt, Recurssive (nt,getTaille tt)) else Type.TypeN(nt, analyse_tds_typeN tds  tt)
                    | _ -> raise (MauvaiseUtilisationIdentifiant nt) 
                )
          )
          | Pointeur(t2)-> Pointeur(analyse_tds_typeN tds t2)
          | Struct l ->
                let l2 = List.map (fun (a,b)->(analyse_tds_typeN tds a,b)) l in Struct(l2)
               
          | typeUsuelle -> typeUsuelle
        )



(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =  
match e with


    | AstSyntax.AppelFonction(n,el) -> 
    
        let infoAst= (chercherGlobalement tds n) in
        (match infoAst with
            |None  -> raise (IdentifiantNonDeclare n)
            |Some q -> 
                (match (info_ast_to_info q) with
                  |InfoFun _ ->AstTds.AppelFonction(q,List.map (analyse_tds_expression tds) el)
                  | _ -> raise (MauvaiseUtilisationIdentifiant n)))
            
    | AstSyntax.ListeVal l -> 
                      let l2=List.map (analyse_tds_expression tds ) l in AstTds.ListeVal(l2)


    | AstSyntax.Variable(var) -> 
      (match var with
      |AstSyntax.Ident (s) -> 
      
         let infoAst= (chercherGlobalement tds s) in
         ( match infoAst with
            |None  -> raise (IdentifiantNonDeclare s)
            |Some q ->
            
             (match (info_ast_to_info q) with
                  |InfoVar _ | InfoStruct _-> AstTds.Variable(AstTds.Ident(q)) 
                  |InfoConst (_,value)  -> AstTds.Entier(value)
                  | _ -> raise (MauvaiseUtilisationIdentifiant s)
              )
          )


      |AstSyntax.Var (var2) ->  let varTds=analyse_tds_variable tds var2 in AstTds.Variable(AstTds.Var(varTds))

      |AstSyntax.Champ (var2,s) ->     
      let _= (* Cette partie verifie si dant le Champ s.p s est bien une infoStruct sinon mauvaise utilisation de s *)
      (match var2 with
      |AstSyntax.Champ (AstSyntax.Ident(s),_)->
         let infoAst= (chercherGlobalement tds s) in
         ( match infoAst with
            |None  -> raise (IdentifiantNonDeclare s)
            |Some q ->
            
             (match (info_ast_to_info q) with
                  | InfoStruct _-> ()
                  |__-> raise (MauvaiseUtilisationIdentifiant s)))
      |_-> () ) 
      in
        let var3=analyse_tds_variable tds var2 in
        let infoAst= (chercherGlobalement tds s) in
              ( match infoAst with
                  |None  -> raise (IdentifiantNonDeclare s)
                  |Some q ->
                  
                  (match (info_ast_to_info q) with
                        |InfoChamp _ -> AstTds.Variable(AstTds.Champ(var3,q) )
                        | _ -> raise (MauvaiseUtilisationIdentifiant s)))
      
      )
    
   
    | AstSyntax.AllocationDynamique (t) -> let t2=(analyse_tds_typeN tds t) in AstTds.AllocationDynamique(t2)
    | AstSyntax.Adresse (s) -> 
         let infoAst= (chercherGlobalement tds s) in
         ( match infoAst with
            |None  -> raise (IdentifiantNonDeclare s)
            |Some q ->
            
             (match (info_ast_to_info q) with
                  |InfoVar _ | InfoStruct _-> AstTds.Adresse(q)
                  | _ -> raise (MauvaiseUtilisationIdentifiant s)))
    | AstSyntax.Booleen q -> AstTds.Booleen q
    | AstSyntax.Entier q -> AstTds.Entier q
    | AstSyntax.Unaire (op,expr) -> AstTds.Unaire(op,analyse_tds_expression tds expr)
    | AstSyntax.Binaire (op,expr1,expr2) -> AstTds.Binaire(op,analyse_tds_expression tds expr1,analyse_tds_expression tds expr2)









(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)



let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
      let t2=  analyse_tds_typeN tds t in 
        match chercherLocalement tds n with
        | None ->
            
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
            
            (* Ajout de l'information (pointeur) dans la tds *)
            let ia=
            (match t2 with
            |Struct l ->  
            let taille=getTaille (Struct l) in
            let infoAstList =analyse_tds_struct tds l taille in
            
            let info = InfoStruct (n,Struct(l),0,"",infoAstList) in
            (* Création du pointeur sur l'information *)
             info_to_info_ast info
            (* Ajout de l'information (pointeur) dans la tds *)
            
             |TypeN(_,t)->  
                    (match realType t with
                    |Struct l ->  
                    let taille=getTaille (Struct l) in
                    let infoAstList =analyse_tds_struct_instanciation n tds l taille in
                    
                    let info = InfoStruct (n,Struct(l),0,"",infoAstList) in
                    (* Création du pointeur sur l'information *)
                    info_to_info_ast info
                      |_-> (* Création de l'information associée à l'identfiant *)
                    let info = InfoVar (n,Undefined, 0, "") in
                    (* Création du pointeur sur l'information *)
                    info_to_info_ast info )
            |_-> (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            info_to_info_ast info 
            )
            in
            ajouter tds n ia; 
            Declaration (realType t2,ia,ne)
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
         
            
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (v,e) ->
  let vTds= analyse_tds_variable tds v in
  let ne =analyse_tds_expression tds e in
  Affectation (vTds, ne)
  | AstSyntax.AssignationAddition (v,e)->
  let vTds= analyse_tds_variable tds v in
  let ne =analyse_tds_expression tds e in
  AssignationAddition (vTds, ne)


  | AstSyntax.Typedef(n,t) ->  
         
         
        
       (match chercherLocalement tds n with
        | None ->
                let info = InfoTypeN (n,t) in
                let ia = info_to_info_ast info in
                ajouter tds n ia;
                let t2 =analyse_tds_typeN tds t in
                Tds.modifier_type_info t2 ia;
                let _=
              (
                match realType t with
                      |Struct l -> 
                            let taille=getTaille (Struct l) in
                            let _ =analyse_tds_struct tds l taille in ()
                      | _ -> ()
              ) 
              in
              AstTds.Typedef(ia,t2)
        
        | Some _ ->
            raise (DoubleDeclaration n)
       )  
   
  | AstSyntax.Constante (n,v) -> 
      begin
        match chercherLocalement tds n with
        | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)
  | AstSyntax.Retour (e) -> 
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in
      Retour (ne)

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
  let infoAst =  chercherGlobalement maintds n in
  match infoAst with
    
    | None ->(let tds = creerTDSFille maintds in
            let lp2 = List.map (fun (a,b) ->( analyse_tds_typeN tds a, b)) lp in
            let tr2= analyse_tds_typeN tds t in
           let info = InfoFun(n,tr2,List.map fst lp2) in
           
            let ia = (info_to_info_ast info) in
            let _ = ajouter maintds n ia in
            (* InfoVar(s,typP,0," ") *)
            let ilp = List.map (fun (typP,s) -> 
                                  let info = 
                                  ( match typP with
                                       |Struct l ->  
                                            let taille=getTaille (Struct l) in
                                            let infoAstList =analyse_tds_struct tds l taille in
                                            InfoStruct(s,Struct(l),0,"",infoAstList)

                                        |TypeN(_,t)->  
                                                (match realType t with
                                                |Struct l ->  
                                                      let taille=getTaille (Struct l) in
                                                      let infoAstList =analyse_tds_struct_instanciation s tds l taille in
                                                        InfoStruct(s,Struct(l),0,"",infoAstList)
                                                  |_-> InfoVar(s,typP,0," "))
                                        |_->InfoVar(s,typP,0," ")
                                            ) 
                                  in
                                  let infoAst= info_to_info_ast info in 
                                  let _ = (match chercherLocalement tds s with
                                            |None -> ajouter tds s infoAst 
                                            |Some _ -> raise (DoubleDeclaration s))
                                   in
                                  (typP,infoAst)) lp2 in        
            let liTds = analyse_tds_bloc tds li in 
            
            AstTds.Fonction (tr2, ia, ilp,liTds) )
    | Some _ ->
            raise (DoubleDeclaration n)


let analyse_tds_typeNomme tds (AstSyntax.TypeNomme(n,t))=
let infoAst =  chercherGlobalement tds n in
  match infoAst with
    
    | None ->      
          let info = InfoTypeN (n,t) in
          let ia = info_to_info_ast info in
          ajouter tds n ia;
          let t2 =analyse_tds_typeN tds t in
          let _=
          (
            match realType t2 with
            |Struct l -> (print_string ("typedef "^(string_of_type t2)^"\n")); let taille=getTaille (Struct l) in let _ =analyse_tds_struct tds l taille in ()
            | _ -> ()
          ) 
          in 
          Tds.modifier_type_info t2 ia;
          AstTds.TypeNomme(ia,t2)
             
    | Some _ ->
            raise (DoubleDeclaration n)

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (typeN, fonctions,prog)) =
  let tds = creerTDSMere () in
  let td = List.map (analyse_tds_typeNomme tds) typeN in 
  let nf = List.map (analyse_tds_fonction tds) fonctions in 
  let nb = analyse_tds_bloc tds prog in
  Programme (td,nf,nb)

end
