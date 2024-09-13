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

mulOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulOp = reservedOp lis "*" *> return Times <|> reservedOp lis "/" *> return Div

addOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addOp = reservedOp lis "+" *> return Plus <|> reservedOp lis "-" *> return Minus

varOp :: Parser (Variable -> Exp Int)
varOp = reservedOp lis "++" *> return VarInc <|> reservedOp lis "--" *> return VarDec

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
boolexp :: Parser (Exp Bool)
boolexp = (do chainl1 boolexp' (try (do reservedOp lis "&&"; return (And))
                                <|> try (do reservedOp lis "||"; ; return (Or))))

boolexp' :: Parser (Exp Bool)
boolexp' = try (do reserved lis "true"; return (BTrue))
          <|> try (do reserved lis "false"; return (BTrue))
          <|> parens lis boolexp
          <|> try (do reservedOp lis "!"; b <- boolexp; return (Not b))
          <|> try (do b1 <- intexp; reservedOp lis "=="; b2 <- intexp; return (Eq b1 b2))
          <|> try (do b1 <- intexp; reservedOp lis "!="; b2 <- intexp; return (NEq b1 b2))
          <|> try (do b1 <- intexp; reservedOp lis ">"; b2 <- intexp; return (Gt b1 b2))
          <|> try (do b1 <- intexp; reservedOp lis "<"; b2 <- intexp; return (Lt b1 b2))

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = (do chainl1 comm' (do reservedOp lis ";"; return (Seq)))

comm' :: Parser Comm
comm' = try (do reserved lis "skip"; return (Skip))
       <|> try (do v <- identifier lis; reservedOp lis "="; expp <- intexp; return (Let v expp))
       <|> try (do reserved lis "if"; b <- boolexp; c <- braces lis comm; return (IfThenElse b c Skip))
       <|> try (do reserved lis "if"; b <- boolexp;  c <- braces lis comm; reserved lis "else"; c2 <- braces lis comm; return (IfThenElse b c c2))
       <|> try (do reserved lis "repeat"; c <- braces lis comm; reserved lis "until"; b <- boolexp; return (RepeatUntil c b))


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
