{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Def
%name parseStmts Defs
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='         { TEquals }
    ':'         { TColon }
    '\\'        { TAbs }
    '.'         { TDot }
    '('         { TOpen }
    ')'         { TClose }
    '->'        { TArrow }
    -- Ejercicio 3
    'let'       { TLet }
    'in'        { TIn }
    -- Ejercicio 4
    '0'         { TZero }
    'suc'       { TSuc }
    'R'         { TNatRec }
    'Nat'       { TTypeNat }
    -- Ejercicio 6
    'nil'       { TNil }
    'cons'      { TCons }
    'RL'        { TListRec }
    'List Nat'  { TTypeList }
    VAR         { TVar $$ }
    TYPEE       { TTypeE }
    DEF         { TDef }
    

%left '=' 
%right '->'
%right '\\' '.' 

%%

Def     :  Defexp                      { $1 }
        |  Exp	                       { Eval $1 }
Defexp  : DEF VAR '=' Exp              { Def $2 $4 } 

Exp     :: { LamTerm }
        -- Ejercicio 3
        : 'let' VAR '=' Exp 'in' Exp   { LLet $2 $4 $6 }
        | '\\' VAR ':' Type '.' Exp    { LAbs $2 $4 $6 }
        | NAbs                         { $1 }

NAbs    :: { LamTerm }
        : NAbs Atom                    { LApp $1 $2 }
        | 'suc' NAbs                   { LSuc $2 }
        | 'cons' NAbs NAbs             { LCons $2 $3 }
        | 'R' NAbs NAbs NAbs           { LRec $2 $3 $4 }
        | 'RL' NAbs NAbs NAbs          { LRec $2 $3 $4 }
        | Atom                         { $1 }

Atom    :: { LamTerm }
        : VAR                          { LVar $1 }  
        | '0'                          { LZero }
        | 'nil'                        { LNil }
        | '(' Exp ')'                  { $2 }

Type    : TYPEE                        { EmptyT }
        | Type '->' Type               { FunT $1 $3 }
        | 'Nat'                        { NatT }
        | 'List Nat'                   { ListT }
        | '(' Type ')'                 { $2 }

Defs    : Defexp Defs                  { $1 : $2 }
        |                              { [] }
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TTypeE
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TColon
               | TArrow
               | TEquals
               | TEOF
               -- Ejercicio 3
               | TLet
               | TIn
               -- Ejercicio 4
               | TZero
               | TSuc
               | TNatRec
               | TTypeNat
               -- Ejercicio 6
               | TNil
               | TCons
               | TListRec
               | TTypeList
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    -- Ejercicio 4
                    ('0':cs) -> cont TZero cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("E",rest)    -> cont TTypeE rest
                              ("def",rest)  -> cont TDef rest
                              -- Ejercicio 3
                              ("let", rest) -> cont TLet rest
                              ("in", rest) -> cont TIn rest
                              -- Ejercicio 4
                              ("suc", rest) -> cont TSuc rest
                              ("R", rest) -> cont TNatRec rest
                              ("Nat", rest) -> cont TTypeNat rest
                              -- Ejercicio 6
                              ("nil", rest) -> cont TNil rest
                              ("cons", rest) -> cont TCons rest
                              ("RL", rest) -> cont TListRec rest
                              ("List", rest) -> let
                                                  (_, rest') = span isSpace rest
                                                in case span isAlpha rest' of
                                                  ("Nat", rest'') -> cont TTypeList rest''
                                                  _ -> 
                                                    \ line -> Failed $ "Línea "++(show line)++": Solo se permiten listas de naturales"
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
