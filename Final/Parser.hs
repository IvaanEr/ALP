module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token as P
import Text.Parsec.Char
import Text.Parsec.Language (emptyDef)

import Types
import Data
import Data.Dates

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
                                  , reservedNames = ["contact","ph", "n", "addr","R","M","D","G"]
                                  , reservedOpNames = [":","[","]","-","$"]
                                  })

line :: Parser String
line = many $ noneOf "\n"

enter = reservedOp lis "\n"

dateP :: Parser DateTime
dateP = do reserved lis "date"
           day    <- natural lis
           (try (reservedOp lis "/") <|> (reservedOp lis "-"))
           month  <-  natural lis
           (try (reservedOp lis "/") <|> (reservedOp lis "-"))          
           year   <-  natural lis
           hour   <-  natural lis
           reservedOp lis ":"
           minute <-  natural lis
           return (DateTime (fromInteger year) (fromInteger month) 
                   (fromInteger day) (fromInteger hour) (fromInteger minute) 0)

nameParse :: Parser Name
nameParse = do reserved lis "n"
               reservedOp lis ":"
               x <- identifier lis
               return x

addrParse :: Parser Address
addrParse = do reserved lis "addr"
               reservedOp lis ":"
               street <- identifier lis
               num    <- natural lis
               return (Addr street num)

phoneParse :: Parser PhoneNum
phoneParse = do  reserved lis "ph"
                 reservedOp lis ":" 
                 pref <- natural lis
                 reservedOp lis "-"
                 num <- natural lis
                 return (Phone pref num) 

contactP :: Parser Contact
contactP = do reserved lis "contact"
              reservedOp lis ":"
              n <- nameParse
              p <- phoneParse
              a <- addrParse
              return (Contact n p a)

parseIO :: String -> Parser a -> String -> IO (Maybe a)
parseIO f p x = case parse (totParser p) f x of
                  Left e  -> putStrLn (show e) >> return Nothing
                  Right r -> return (Just r)

parseFile file = do f <- readFile file
                    case parse contactListP file f of
                      Left e  -> putStrLn (show e) >> return Nothing
                      Right r -> return (Just r)


contactListP :: Parser Contacts
contactListP = do reservedOp lis "["
                  xs <- sepBy1 contactP (reservedOp lis "$")
                  reservedOp lis "]"
                  return xs                  

reminderP :: Parser Reminder
reminderP = try (do reserved lis "R"
                    d <- dateP
                    description <- line
                    return (Remind d description))
            <|> (do reserved lis "M"
                    d <- dateP
                    description <- line
                    return (Meeting d description))

debtP :: Parser Debt
debtP = do reserved lis "D"
           who <- identifier lis
           mount <- natural lis
           why <- line
           return (Debt who mount why)

grocerieP :: Parser String
grocerieP = do reserved lis "G"
               line


schedP :: Parser Schedule
schedP = do reserved lis "Sched"
            reservedOp lis ":"
            many1 $ reservedOp lis "\n"

            return (Sched "yo" [] [] [] [])



