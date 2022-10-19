
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open AstType
  open Type

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme



         
        
 let rec modifier_adresse_info_struct base reg lt infoAl =

  (match lt,infoAl with

        |(t,_)::q1,infoAst::q2->let _= 
            (print_string(string_of_info(info_ast_to_info infoAst)));
            (print_string(("modif addresse type : ")^(string_of_type t)^" de taille "^(string_of_int (getTaille t))^" dans le registre "^reg^"  base  "^(string_of_int base)^"\n"));
            (modifier_adresse_info base reg infoAst) in let _=(modifier_adresse_info_struct (base+(getTaille t )) reg q1 q2) in ()

        |[],[]->
            (print_string(("  fin \n")));()

        | [],_|_,[]->failwith "Pas la meme taille"
  )
  
            
  
let rec analyse_placement_instruction base reg i =
  (match i with
  
        | AstType.Declaration ( infoAst, _) ->
                  let info=(info_ast_to_info infoAst) in
                  (match info with

                        | InfoVar(_,t,_,_)->   
                        let taille=getTaille t in 
                        let _= modifier_adresse_info base reg infoAst in
                        taille

                        |InfoStruct(_,Struct(l),_,_,infoAstL)->
                        (print_string(("declaration ")));
                        let taille=getTaille (Struct(l)) in 
                        let _= modifier_adresse_info base reg infoAst in
                        let _=modifier_adresse_info_struct 0 reg l infoAstL in
                        taille

                        | _ -> failwith "Erreur placement1"
                  )
          
        | AstType.Conditionnelle (_,b1,b2) -> 
                  let _ = analyse_placement_bloc base reg b2 in 
                  let _ = analyse_placement_bloc base reg b1 in 0

        | AstType.TantQue (_,b) -> 
                   let _ = analyse_placement_bloc base reg b in 0

        | _ -> 0
  )
    

     
  
    
      

and analyse_placement_bloc base reg li =
          (match li with 
              |[] -> base
              |t::q ->
                  let taille=analyse_placement_instruction base reg t in
                  let _ = (analyse_placement_bloc (base+taille) reg q) in
                  base
          )


let analyse_placement_fonction base reg (AstType.Fonction(infoAstF,lp,li))  =
        (**fonctionAux :  *)
        let fonctionAux base2 infoAst =

        (let info= info_ast_to_info infoAst in
          let taille=
            (match info with
                  | InfoVar(_,t,_,_)-> getTaille t
                  | InfoStruct(_,Struct(l),_,_,infoAstL) ->
                            let _=modifier_adresse_info_struct 0 reg l infoAstL in getTaille (Struct(l))
                  | _-> failwith "Erreur" 
              ) 
          in
          let _ = modifier_adresse_info (base2-taille) reg infoAst in
          base2-taille
        )
          in

          let _= List.fold_right (fun e res -> fonctionAux res e) lp 0 in 
          let _=analyse_placement_bloc base reg li in 
          AstPlacement.Fonction(infoAstF,lp,li)

       
  let analyser (AstType.Programme (fonctions,prog)) =
          let nf = List.map (analyse_placement_fonction 3 "LB" ) fonctions in 
          let _  = analyse_placement_bloc 0 "SB" prog in
          AstPlacement.Programme (nf,prog)

end