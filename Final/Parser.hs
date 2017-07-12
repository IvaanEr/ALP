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
                                  , reservedNames = ["Contact","ph:", "n:", "addr:","@"]
                                  })
-- runP = parse  

nameParse :: Parser Name
nameParse = do reserved lis "n:"
               x <- identifier lis
               return x

addrParse :: Parser Address
addrParse = do reserved lis "addr:"
               street <- identifier lis
               num    <- natural lis
               return (Addr street num)

phoneParser :: Parser PhoneNum
phoneParser = do reserved lis "ph:"
                 pref <- natural lis
                 num <- natural lis
                 return (Phone pref num) 

contact :: Parser Contact
contact = do reserved lis "Contact:"
             n <- nameParse
             p <- phoneParser
             a <- addrParse
             return (Contact n p a)

