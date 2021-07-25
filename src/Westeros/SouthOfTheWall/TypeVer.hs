module Westeros.SouthOfTheWall.TypeVer where

import qualified Westeros.SouthOfTheWall.AST as AST
import qualified Westeros.SouthOfTheWall.Error as Err (TypeError(..))
import qualified Westeros.SouthOfTheWall.Symtable as ST 


data Type
    = IntT 
    | FloatT
    | CharT
    | BoolT
    | AtomT 

    | AliasT String 

    | StringT 
    | Array Type Int 
    | TupleT [Type]
    | StrucT Int
    | UnionT Int
    | PointerT Type 

    | TypeError
    deriving (Eq, Show)

-- This interface will provide type check consistency for their instances.
--
-- The propper way to use it is to implement exhaustive instances for everything in
-- the language susceptible of having a type and then performing type queries when
-- necessary.
class Typeable a where 
    typeQuery :: a -> ST.MonadParser Type

instance Typeable AST.Expr where
    typeQuery (AST.IntLit    _) = return IntT 
    typeQuery (AST.CharLit   _) = return CharT
    typeQuery (AST.FloatLit  _) = return FloatT 
    typeQuery (AST.AtomLit   _) = return AtomT 
    typeQuery (AST.StringLit _) = return StringT 
    typeQuery AST.TrueLit       = return BoolT 
    typeQuery AST.FalseLit      = return BoolT 

    typeQuery AST.NullLit       = undefined

    typeQuery (AST.ArrayLit a@(x:xs)) = do 
           let asExpr = map AST.getExpr a

           types   <- mapM typeQuery asExpr
           
           let arrType = head types
               cond1 = all notTypeError types
               cond2 = foldl (\acc b -> arrType == b && acc ) True types 

           if cond1 && cond2 
               then return $ Array (head types) (length a)
               else return TypeError 
    typeQuery (AST.ArrayLit [])    = error "Empty literal array not supported"

    typeQuery (AST.TupleLit parts)   = do 
        types <- mapM (typeQuery . AST.getExpr) parts

        let res = TupleT types

        if notTypeError res 
            then return res 
            else return TypeError

    typeQuery (AST.FuncCall   _ _) = undefined 

    typeQuery (AST.BinOp AST.Sum a b)  = arithmeticBinOpCheck a b
    typeQuery (AST.BinOp AST.Sub a b)  = arithmeticBinOpCheck a b
    typeQuery (AST.BinOp AST.Prod a b) = arithmeticBinOpCheck a b
    typeQuery (AST.BinOp AST.Mod a b)  = arithmeticBinOpCheck a b
    typeQuery (AST.BinOp AST.Div a b)  = arithmeticBinOpCheck a b

    typeQuery (AST.BinOp AST.Eq a b)  = comparisonBinOpCheck  a b
    typeQuery (AST.BinOp AST.Neq a b) = comparisonBinOpCheck a b
    typeQuery (AST.BinOp AST.Lt a b)  = comparisonBinOpCheck a b
    typeQuery (AST.BinOp AST.Gt a b)  = comparisonBinOpCheck a b
    typeQuery (AST.BinOp AST.Leq a b) = comparisonBinOpCheck a b
    typeQuery (AST.BinOp AST.Geq a b) = comparisonBinOpCheck a b
 
    typeQuery (AST.BinOp AST.And a b) = boolBinOpCheck a b
    typeQuery (AST.BinOp AST.Or a b)  = boolBinOpCheck a b

    typeQuery (AST.UnOp AST.Neg a) = do 
            x <- typeQuery (AST.getExpr a)
            if x `elem` [IntT, FloatT] 
               then return x
               else return TypeError 

    typeQuery (AST.UnOp AST.Deref p) = undefined

    typeQuery (AST.AccesField expr id) = undefined 
    typeQuery (AST.ActiveField _ _) = undefined 
    typeQuery (AST.AccesIndex _ _)  = undefined 

    typeQuery (AST.TupleIndex _ _) = undefined 
    typeQuery (AST.IdExpr _)       = undefined


-- ^ Use to check if recursive types have some leave with a type error.
notTypeError :: Type -> Bool 
notTypeError (TupleT xs)  = all notTypeError xs
notTypeError (PointerT e) = notTypeError e
notTypeError TypeError    = False
notTypeError _            = True


arithmeticBinOpCheck :: AST.Expression -> AST.Expression -> ST.MonadParser Type
arithmeticBinOpCheck a b = do 
      x <- typeQuery (AST.getExpr a) 
      d <- typeQuery (AST.getExpr b)
      if x == d && x `elem` [IntT, FloatT]
          then return x 
          else return TypeError 


comparisonBinOpCheck :: AST.Expression -> AST.Expression -> ST.MonadParser Type 
comparisonBinOpCheck a b = do 
     x <- typeQuery (AST.getExpr a) 
     d <- typeQuery (AST.getExpr b) 
     let typeList = [IntT, BoolT, FloatT, CharT] 
     if x == d && x `elem` typeList
        then return BoolT 
        else return TypeError 

boolBinOpCheck :: AST.Expression -> AST.Expression -> ST.MonadParser Type
boolBinOpCheck a b = do 
    x <- typeQuery (AST.getExpr a)
    d <- typeQuery (AST.getExpr b)
    if x == d && x == BoolT
        then return BoolT 
        else return TypeError

{-
data Expr
    = IntLit    Int
    | CharLit   Char
    | FloatLit  Float
    | StringLit String
    | AtomLit   String
    | TrueLit
    | FalseLit
    | NullLit
    | ArrayLit    [Expression]
    | TupleLit    [Expression]
    | FuncCall    Id [Expression]
    | BinOp       BinOp Expression Expression
    | UnOp        UnOp Expression
    | AccesField  Expression Id
    | ActiveField Expression Id
    | AccesIndex  Expression [Expression]
    | TupleIndex  Expression Int
    | Cast        Expression Type
    | IdExpr      Id
    deriving (Show, Eq)


-}

{-
AST.Expression
AST.Declaration 
AST.VariableDeclaration 

-}

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
DECLARATION :: { Ast.Declaration } -- # 
    : SIMPLE_DECLARATION '.'                                                 
    | SIMPLE_DECLARATION ':=' EXPR '.'                                       
    | SIMPLE_DECLARATION ':==' EXPR '.'                                      
    | CONST_DECLARATION '.'                                                  

SIMPLE_DECLARATIONS :: { [Ast.VariableDeclaration] }
    : SIMPLE_DECLARATION                                                     
    | SIMPLE_DECLARATIONS ',' SIMPLE_DECLARATION                             

SIMPLE_DECLARATION :: { Ast.VariableDeclaration } -- #
    : PRIMITIVE_DECLARATION                                                  
    | COMPOSITE_DECLARATION                                                  

PRIMITIVE_DECLARATION :: { Ast.VariableDeclaration } -- #
    : var id type TYPE                                                       

COMPOSITE_DECLARATION :: { Ast.VariableDeclaration } -- #
    : beginCompTypeId var id endCompTypeId TYPE                              
    | beginCompTypeId var id endCompTypeId TYPE beginSz EXPRLIST endSz       
    | beginCompTypeId pointerVar id endCompTypeId TYPE                       
    | beginCompTypeId pointerVar id endCompTypeId TYPE beginSz EXPRLIST endSz

CONST_DECLARATION :: { Ast.Declaration } -- #
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
    : EXPR ':=' EXPR '.'                    -- #                                     
    | EXPRLIST ':==' EXPR '.'               -- #                                  
    | void ':=' EXPR '.'                    -- # |                                   
    | void ':==' EXPR '.'                   -- # |                                  
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
    | IF '.'                                -- #
    | SWITCHCASE '.'                        -- #                                  
    | FOR '.'                               -- #                                     
    | WHILE '.'                             -- #                                     
    | DECLARATION                           -- #                                     

IF :: { Ast.IfInst }                        -- #
    : if EXPR then CODE_BLOCK endif                                              
    | if EXPR then CODE_BLOCK else CODE_BLOCK endif                              

SWITCHCASE :: { Ast.Instruction }           -- #
    : switch EXPR switchDec '.' CASES endSwitch                                  

CASES :: { [Ast.Case] }                     -- #
    : CASE                                                                       
    | CASES CASE                                                                 

CASE :: { Ast.Case }                        -- #
    : case atomLit '.' CODE_BLOCK                                                
    | case nothing '.' CODE_BLOCK                                                

FOR :: { Ast.Instruction }                  -- #
    : OPEN_SCOPE FOR_DEC INSTRUCTIONS endFor CLOSE_SCOPE                         

FOR_DEC :: { (Ast.Id, Ast.Expression, Ast.Expression) } -- #
    : for id type int '.' forLB EXPR forUB EXPR '.'                              
                                                                                 
WHILE :: { Ast.Instruction }                -- #
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
FUNCTIONCALL :: { Ast.Expression } -- #
    : id '((' procCallArgs EXPRLIST '))'
    | id '((' procCallArgs void '))'    
    | id '(('  '))'                     

-}

{-

data Type
    = IntT
    | FloatT
    | CharT
    | AtomT
    | BoolT
    | StringT
    | ArrayT    Type Int
    | StructT   [VariableDeclaration]
    | UnionT    [VariableDeclaration]
    | TupleT    [Type]
    | PointerT  Type
    | AliasT    Id
    deriving (Show, Eq)

data VariableDeclaration
    = SimpleVarDeclaration  Id Type
    | ArrayVarDeclaration   Id Type [Expression] <- this will have types and we can match them
    deriving (Show, Eq)

-}
