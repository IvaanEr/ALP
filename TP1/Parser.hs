module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "while","do"]
                                  , reservedOpNames = ["+","-","*","/","~", ">", "<","?",":"]
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
noMin :: Parser IntExp
noMin =    try (do x <- natural lis
                   return (Const x))
       <|> try (do x <- identifier lis
                   return (Var x))
       <|> try (do x <- parens lis intexp
                   return x)
       <|> try (do x <- boolexp
                   reservedOp lis "?"
                   y <- intexp
                   reservedOp lis ":"
                   z <- intexp
                   return (AltCond x y z))

factor  :: Parser IntExp
factor = try (do reservedOp lis "-"
                 x <- factor
                 return (UMinus x))
         <|> try (do x <- noMin
                     return x)
       

intexp :: Parser IntExp
intexp = do x <- noRS
            f <- intexp'
            return (f x)
            
intexp' :: Parser (IntExp -> IntExp)
intexp' =   try (do reservedOp lis "+"
                    x <- noRS
                    f <- intexp'
                    return (\i -> f (Plus i x)))
        <|> try (do reservedOp lis "-"
                    x <- noRS
                    f <- intexp'
                    return (\i -> f (Minus i x)))
        <|> return id
        
noRS :: Parser IntExp
noRS = do x <- factor
          f <- noRS'
          return (f x)
          
noRS' :: Parser (IntExp -> IntExp)
noRS' =     try (do reservedOp lis "*"
                    x <- factor
                    f <- noRS'
                    return (\i -> f (Times i x)))
        <|> try (do reservedOp lis "/"
                    x <- factor
                    f <- noRS'
                    return (\i -> f (Div i x)))
        <|> return id
          
-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = do x <- boolo
             f <- boolexp'
             return (f x)
             
boolexp' :: Parser (BoolExp -> BoolExp)           
boolexp' = try (do reservedOp lis "|" 
                   x <- boolo 
                   f <- boolexp'
                   return (\i -> f (Or i x)))
        <|> return id
       
boolo :: Parser BoolExp
boolo = do x <- factbool
           f <- boolo'
           return (f x)
           
boolo':: Parser (BoolExp -> BoolExp)           
boolo' = try (do reservedOp lis "&"
                 x <- factbool
                 f <- boolo'
                 return (\i -> f (And i x)))            
         <|> return id

factbool :: Parser BoolExp
factbool =  try (do {reserved lis "true"; return BTrue})
        <|> try (do {reserved lis "false"; return BFalse})
        <|> try (do {x <- parens lis boolexp; return x})
        <|> try (do {reservedOp lis "~"; x <- factbool; return (Not x)})
        <|> try (do {x <- boolint; return x})
                
boolint :: Parser BoolExp
boolint =  try (do {x <- intexp; operator lis; y <- intexp; return (Eq x y)})
       <|> try (do {x <- intexp; reservedOp  lis ">"; y <- intexp; return (Gt x y)})
       <|> try (do {x <- intexp; reservedOp  lis "<"; y <- intexp; return (Lt x y)})
       
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = do x <- noComm
          f <- comm'
          return (f x)
 
comm' :: Parser (Comm -> Comm)
comm' =  try (do reservedOp lis ";"
                 x <- noComm
                 f <- comm'
                 return (\i -> f (Seq i x)))
     <|> return id
     
noComm :: Parser Comm
noComm =    try (do x <- identifier lis 
                    reservedOp lis ":="
                    y <- intexp
                    return (Let x y))
        <|> try (do x <- other
                    return x)
                    
other :: Parser Comm
other =   try (do reserved lis "if"
                  x <- boolexp
                  reserved lis "then"
                  y <- comm
                  reserved lis "else"
                  z <- comm
                  reserved lis "end"
                  return (Cond x y z))
      <|> try (do reserved lis "while"
                  x <- boolexp
                  reserved lis "do"
                  y <- comm
                  reserved lis "end"
                  return (While x y))
      <|> try (do reserved lis "skip"
                  return Skip) 

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

  