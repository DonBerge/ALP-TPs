{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import GHC.Num (integerToInt)

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
arithOp :: Parser (Exp Int -> Exp Int -> Exp Int)
arithOp = reservedOp lis "*" *> return Times
          <|> reservedOp lis "/" *>  return Div
          <|> reservedOp lis "+" *> return Plus
          <|> reservedOp lis "-" *> return Minus

intexp :: Parser (Exp Int)
intexp = intexp' `chainl1` arithOp


-- fun <%> p , aplica fun al resultado del parser p
-- p *> q, ejecuta el parser p y luego ejecuta el parser q 
intexp' :: Parser (Exp Int)
intexp' = try (do v <- identifier lis; ((reservedOp lis "++" *> return (VarInc v))
                                        <|> (reservedOp lis "--" *> return (VarDec v)))
                                        <|> return (Var v))
          <|> ((Const . fromIntegral) <$> natural lis)
          <|> (do reservedOp lis "-"; e <- intexp; return (UMinus e))
          <|> parens lis intexp

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
