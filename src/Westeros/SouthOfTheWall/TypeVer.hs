module Westeros.SouthOfTheWall.TypeVer where


{-
Tipos -> Literales -> Expresiones -> Instrucciones -> Funciones
------------------------------------------------------------- EXPRESSIONS

Operators
    ops(int,float): 
        + | - | * | / | -. | 
    ops(int): 
        % 
    ops(int,bool,float,char): 
        == | /= | < | > | <= | >=
    ops(bool): 
        && | ||
    ops(all): 
        */type cast

Parser.y:413
EXPR :: { Ast.Expression }
    : EXPR '+' EXPR       
    | EXPR '-' EXPR       
    | EXPR '*' EXPR       
    | EXPR '/' EXPR       
    | EXPR '%' EXPR       
    | EXPR '=' EXPR       
    | EXPR '!=' EXPR      
    | EXPR '<' EXPR       
    | EXPR '>' EXPR       
    | EXPR '<=' EXPR      
    | EXPR '>=' EXPR      
    | EXPR and EXPR       
    | EXPR or EXPR        
    | EXPR '~'            

    | deref EXPR          
    | '[' EXPRLIST ']' EXP
    | id '<-' EXPR        
    | EXPR '->' id        
    | EXPR '?' id         

    | '[(' naturalLit ']' 
    | EXPR cast TYPE      
    | '(' EXPR ')'        
    
    | ARRAYLIT            
    | TUPLELIT            
    | FUNCTIONCALL        

    | intLit              
    | floatLit            
    | charLit             
    | atomLit             
    | stringLit           
    | true                
    | false               
    | null                
    | id                  

Parser.y:478
EXPRLIST :: { [Ast.Expression] }
    : EXPR                      
    | EXPRLIST ',' EXPR         
    
------------------------------------------------------------- DECLARATION

Parser.y:304
DECLARATION :: { Ast.Declaration }
    : SIMPLE_DECLARATION '.'                                                 
    | SIMPLE_DECLARATION ':=' EXPR '.'                                       
    | SIMPLE_DECLARATION ':==' EXPR '.'                                      
    | CONST_DECLARATION '.'                                                  

SIMPLE_DECLARATIONS :: { [Ast.VariableDeclaration] }
    : SIMPLE_DECLARATION                                                     
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                             

SIMPLE_DECLARATION :: { Ast.VariableDeclaration }
    : PRIMITIVE_DECLARATION                                                  
    | COMPOSITE_DECLARATION                                                  

PRIMITIVE_DECLARATION :: { Ast.VariableDeclaration }
    : var id type TYPE                                                       

COMPOSITE_DECLARATION :: { Ast.VariableDeclaration }
    : beginCompTypeId var id endCompTypeId TYPE                              
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz       
    | beginCompTypeId pointerVar id endCompTypeId TYPE                       
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz

CONST_DECLARATION :: { Ast.Declaration }
    : const id type TYPE constValue EXPR                                     
    | beginCompTypeId const id endCompTypeId TYPE constValue EXPR            

ALIAS_DECLARATION :: { Ast.AliasDeclaration }
    : beginAlias id ALIAS_TYPE TYPE '.'                                      

ALIAS_TYPE :: { Ast.AliasType }
    : strongAlias                                                            
    | weakAlias                                                              

------------------------------------------------------------- TYPES

Simples: Lanninteger/int | Freyt/float | Boolton/bool | Starkhar/char | Barathom/atom

Parser.y:273
TYPE :: { Ast.Type }
    : PRIMITIVE_TYPE                           
    | COMPOSITE_TYPE                           
    | id                                       

PRIMITIVE_TYPE :: { Ast.Type }
    : int                                      
    | float                                    
    | char                                     
    | bool                                     
    | atom                                     

COMPOSITE_TYPE :: { Ast.Type }
    : beginArray naturalLit TYPE endArray      
    | string                                   
    | pointerType TYPE                         
    | beginStruct SIMPLE_DECLARATIONS endStruct
    | beginUnion SIMPLE_DECLARATIONS endUnion  
    | beginTuple TUPLE_TYPES endTuple          

TUPLE_TYPES :: { [Ast.Type] }
    : {- empty -}                              
    | TYPES                                    

------------------------------------------------------------- INSTRUCTIONS

    assignment
    multiple assignnment
    simple selection
    multiple selection
    determinate repetition
    undeterminate repetition


Parser.y:348
INSTRUCTION :: { Ast.Instruction }
    : EXPR ':=' EXPR '.'                                                         
    | EXPRLIST ':==' EXPR '.'                                                    
    | void ':=' EXPR '.'                                                         
    | void ':==' EXPR '.'                                                        
    | pass '.'                                                                   
    | beginExit programName endExit '.'                                          
    | read EXPR '.'                                                              
    | print EXPR '.'                                                             
    | EXPR new '.'                                                               
    | EXPR free '.'                                                              
    | continue '.'                                                               
    | break '.'                                                                  
    | returnOpen EXPRLIST returnClose                                            
    | returnOpen returnClose                                                     
    | IF '.'                                                                     
    | SWITCHCASE '.'                                                             
    | FOR '.'                                                                    
    | WHILE '.'                                                                  
    | DECLARATION                                                                

IF :: { Ast.IfInst }
    : if EXPR then CODE_BLOCK endif                                              
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif                              

SWITCHCASE :: { Ast.Instruction }
    : switch EXPR switchDec '.' CASES endSwitch                                  

CASES :: { [Ast.Case] }
    : CASE                                                                       
    | CASES CASE                                                                 

CASE :: { Ast.Case }
    : case atomLit '.' CODE_BLOCK                                                
    | case nothing '.' CODE_BLOCK                                                

FOR :: { Ast.Instruction }
    : OPEN_SCOPE FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE                         

FOR_DEC :: { (Ast.Id, Ast.Expression, Ast.Expression) }
    : for id type int '.' forLB EXPR forUB EXPR '.'                              
                                                                                 
WHILE :: { Ast.Instruction }
    : while EXPR whileDec CODE_BLOCK endWhile                                    
                                                                                 
------------------------------------------------------------- FUNCTIONS

Function definition
Function call

Parser.y:219
FUNCTIONS :: { [Ast.FunctionDeclaration] }
    : {- empty -}                                                                
    | FUNCTIONS FUNCTION                                                         

FUNCTION :: { Ast.FunctionDeclaration }
    : id OPEN_SCOPE FUNCTION_PARAMETERS FUNCTION_RETURN FUNCTION_BODY CLOSE_SCOPE

FUNCTION_PARAMETERS :: { [Ast.Parameter] }
    : beginFuncParams PARAMETER_LIST endFuncParams                               

PARAMETER_LIST :: { [Ast.Parameter] }
    : void                                                                       
    | PARAMETERS                                                                 

PARAMETERS :: { [Ast.Parameter] }
    : PARAMETER                                                                  
    | PARAMETERS ',' PARAMETER                                                   

PARAMETER :: { Ast.Parameter }
    : PARAMETER_TYPE id type TYPE                                                

PARAMETER_TYPE :: { Ast.ParamType }
    : valueArg                                                                   
    | refArg                                                                     

FUNCTION_RETURN :: { [Ast.Type] }
    : beginReturnVals RETURN_TYPES endReturnVals                                 

RETURN_TYPES :: { [Ast.Type]}
    : void                                                                       
    | TYPES                                                                      

Parser.y:465
FUNCTIONCALL :: { Ast.Expression }
    : id '((' procCallArgs EXPRLIST '))'
    | id '((' procCallArgs void '))'    
    | id '(('  '))'                     



Compound:  .. later

-}