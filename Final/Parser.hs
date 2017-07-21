module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token as P
import Text.Parsec.Char
import Text.Parsec.Language (emptyDef)

import Data.Char (isSpace)
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
                                  , reservedNames = ["date","contact","ph", "n", "addr","R","M","D","G"]
                                  , reservedOpNames = [":","[","]","-",",","$"]
                                  })

line :: Parser String
line = many $ noneOf "\n"


removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else (x:xs)

-- Probably there is a better way to do this...
str :: Parser String
str = do sp <- many space
         x <- alphaNum
         sp2 <- many space
         xs <- many str
         let string = (sp)++[x]++(sp2)++(concat xs)
             string2 = reverse $ removeSpaces $ reverse string
         return string2

listP p = brackets lis (sepBy p (comma lis))

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

nameP :: Parser Name
nameP = do reserved lis "n"
           reservedOp lis ":"
           x <- identifier lis
           return x

addrP :: Parser Address
addrP = do reserved lis "addr"
           reservedOp lis ":"
           street <- identifier lis
           num    <- natural lis
           return (Addr street num)

phoneP :: Parser PhoneNum
phoneP = do  reserved lis "ph"
             reservedOp lis ":" 
             pref <- natural lis
             reservedOp lis "-"
             num <- natural lis
             return (Phone pref num) 

contactP :: Parser Contact
contactP = do reserved lis "contact"
              reservedOp lis ":"
              n <- nameP
              p <- phoneP
              a <- addrP
              return (Contact n p a)

contactListP :: Parser Contacts
contactListP = listP contactP

reminderP :: Parser Reminder
reminderP = try (do reserved lis "R"
                    d <- dateP
                    description <- str
                    return (Remind d description))
            <|> (do reserved lis "M"
                    d <- dateP
                    description <- str
                    return (Meeting d description))

reminderListP :: Parser Reminders
reminderListP = do listP reminderP


debtP :: Parser Debt
debtP = do reserved lis "D"
           who <- identifier lis
           reservedOp lis "$"
           mount <- natural lis
           why <- str
           return (Debt who mount why)

debtListP :: Parser Debts
debtListP = listP debtP

grocerieP :: Parser String
grocerieP = do reserved lis "G"
               str

grocerieListP :: Parser Groceries
grocerieListP = listP grocerieP

schedP :: Parser Schedule
schedP = do reserved lis "Sched"
            reservedOp lis ":"
            name <- identifier lis
            c <- contactListP
            r <- reminderListP
            d <- debtListP
            g <- grocerieListP
            return (Sched name c r d g)



