/* Imports. */

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token <string> TID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token TYPEDEF
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL 
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token PLUSEGAL
%token MULT
%token INF
%token NEW
%token NULL
%token ET
%token PT
%token STRUCT
%token EOF

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction list> is
%type <instruction> i
%type <typ> typ
%type <(typ*string) list> dp
%type <expression> e 
%type <expression list> cp
%type <typeNomme> typeN
%type <variable> a
(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi = prog EOF     {lfi}

prog :
| tn = typeN  lfi = prog   {let (Programme (tn1,lf1,li))=lfi in (Programme (tn::tn1,lf1,li))}
| lf = fonc  lfi = prog {let (Programme (tn,lf1,li))=lfi in (Programme (tn,lf::lf1,li))}
| ID li = bloc            {Programme ([],[],li)}

fonc : t=typ n=ID PO p=dp PF AO li=is AF {Fonction(t,n,p,li)}

typeN : TYPEDEF n=TID EQUAL t=typ PV     {TypeNomme (n,t)}

bloc : AO li = is AF      {li}

is :
|                         {[]}
| i1=i li=is              {i1::li}

i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
| n=a EQUAL e1=e PV                 {Affectation (n,e1)}
| n=a PLUSEGAL e1=e PV              {AssignationAddition (n,e1)}
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| RETURN exp=e PV                   {Retour (exp)}
| TYPEDEF n=TID EQUAL t=typ PV      {Typedef (n,t)}

a :
| n=ID {Ident n}
| PO MULT var=a PF {Var var}
| PO en=a PT idt=ID PF {Champ(en,idt)}

dp :
|                         {[]}
| t=typ n=ID lp=dp        {(t,n)::lp}

typ :
| BOOL    {Bool}
| INT     {Int}
| RAT     {Rat}
| t=typ MULT {Pointeur t}
| n=TID    {TypeN (n,Undefined)}
| STRUCT AO l=dp AF  {Struct(l)}

e : 
| CALL n=ID PO lp=cp PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Binaire(Fraction,e1,e2)}
| n=a                     {Variable n}
| TRUE                    {Booleen true}
| FALSE                   {Booleen false}
| NULL                    {AllocationDynamique Undefined}
| PO NEW t=typ PF         {AllocationDynamique t}
| ET n=ID                 {Adresse n}
| e=ENTIER                {Entier e}
| NUM e1=e                {Unaire(Numerateur,e1)}
| DENOM e1=e              {Unaire(Denominateur,e1)}
| AO c=cp AF                {ListeVal(c)}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| PO exp=e PF             {exp}


cp :
|               {[]}
| e1=e le=cp    {e1::le}

