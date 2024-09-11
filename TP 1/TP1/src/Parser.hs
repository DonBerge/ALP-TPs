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
intexp :: Parser (Exp Int)
intexp = (do chainl1 intexp' (try (do reservedOp lis "+"; return (Plus))
                              <|> try (do _ <- reservedOp lis "-"; return (Minus))
                              <|> try (do reservedOp lis "*"; return (Times))
                              <|> try (do reservedOp lis "/";  return (Div))))


intexp' :: Parser (Exp Int)
intexp' = try (do v <- identifier lis; (try (do reservedOp lis "++"; return (VarInc v))
                                        <|> try (do reservedOp lis "--"; return (VarDec v)))
                                        <|> return (Var v))
          <|> try (do i <- natural lis; return (Const (fromIntegral i)))
          <|> try (do reservedOp lis "-"; e <- intexp; return (UMinus e))
          <|> braces lis intexp

------------------------------------
--- Parser de expresiones booleanas
------------------------------------
boolexp :: Parser (Exp Bool)
boolexp = (do chainl1 boolexp' (try (do reservedOp lis "&&"; return (And))
                                <|> try (do reservedOp lis "||"; ; return (Or))))

boolexp' :: Parser (Exp Bool)
boolexp' = try (do reserved lis "true"; return (BTrue))
          <|> try (do reserved lis "false"; return (BTrue))
          <|> braces lis boolexp
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
