{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use $>" #-}
module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = try (do
  whiteSpace lis
  t <- p
  eof
  return t)

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , "++"
                        , "--"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

-- p *> q, aplica el parser p y luego aplica el parser q, retorna el resultado de q
-- f <$> p, aplica el parser p y luego aplica la función f al resultado
-- choice ps: equivalente a foldr1 (<|>) ps, aplica los parsers en ps hasta que uno tenga éxito

mulOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulOp = reservedOp lis "*" *> return Times <|> reservedOp lis "/" *> return Div

addOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addOp = reservedOp lis "+" *> return Plus <|> reservedOp lis "-" *> return Minus

varOp :: Parser (Variable -> Exp Int)
varOp = reservedOp lis "++" *> return VarInc <|> reservedOp lis "--" *> return VarDec <|> return Var

intexp:: Parser (Exp Int)
intexp = term `chainl1` addOp

term:: Parser (Exp Int)
term = factor `chainl1` mulOp

factor:: Parser (Exp Int)
factor = choice [ 
                  parens lis intexp,
                  (Const . fromIntegral) <$> natural lis,
                  do {
                    v <- identifier lis;
                    op <- varOp;
                    return (op v)
                  },
                  UMinus <$> (reservedOp lis "-" *> intexp)
                ]   
------------------------------------
--- Parser de expresiones booleanas
------------------------------------
boolOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolOp = reservedOp lis "&&" *> return And <|> reservedOp lis "||" *> return Or

compOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
compOp = choice [reservedOp lis "==" *> return Eq
         , reservedOp lis "!=" *> return NEq
         , reservedOp lis ">" *> return Gt
         , reservedOp lis "<" *> return Lt]

boolexp:: Parser (Exp Bool)
boolexp = boolterm `chainl1` boolOp

boolterm :: Parser (Exp Bool)
boolterm = choice [ parens lis boolexp,
                    Not <$> (reservedOp lis "!" *> boolexp),
                    try $ reserved lis "true" *> return BTrue,
                    try $ reserved lis "false" *> return BFalse,
                    do {e0 <- intexp; op <- compOp; e1 <- intexp; return (op e0 e1)}]
-----------------------------------
--- Parser de comandos
-----------------------------------
seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = reservedOp lis ";" *> return Seq

comm :: Parser Comm
comm = comm' `chainl1` seqOp

ifParser:: Parser Comm
ifParser = do {
  reserved lis "if";
  b <- boolexp;
  c <- braces lis comm;
  (
    do {
      reserved lis "else";
      c2 <- braces lis comm;
      return (IfThenElse b c c2);
    }
    <|>
    return (IfThenElse b c Skip)
  )
}

comm' :: Parser Comm
comm' = choice [try(do{v<-identifier lis; reservedOp lis "="; e<-intexp; return (Let v e)}),
                reserved lis "skip" *> return Skip,
                ifParser,
                do{reserved lis "repeat"; c<-braces lis comm; reserved lis "until"; b<-boolexp; return (RepeatUntil c b)}]

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
