module ParserComm where

import AST
import Parser

import Data.Dates
import Text.ParserCombinators.Parsec
import Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

-----------------------
-- -- Funcion para facilitar el testing del parser.
-- totParser :: Parser a -> Parser a
-- totParser p = do 
--                   whiteSpace lis
--                   t <- p
--                   eof
--                   return t

-- Analizador de Tokens
lisComm :: TokenParser u
lisComm = makeTokenParser (emptyDef   { commentStart  = "/*"
                                      , commentEnd    = "*/"
                                      , commentLine   = "//"
                                      , reservedNames = ["newSched","newContact","newRemind","newMeeting","newDebt",
                                                         "addContact","addRemind","addContact","addRemind","addDebt","addGrocerie",
                                                         "delContact","delRemind","delDebt","delGrocerie",
                                                         "updPhone","updAddress","interval","thisWeek","thisMonth",
                                                         "allMeetings","allReminds","debtsHigher","debtsTo","allContacts",
                                                         "allDebts","allGroceries"]
                                      , reservedOpNames = ["/","-",":"]
                                      })


parseThis :: String -> Either ParseError ScheduleComm
parseThis s = parse parseComm "" s

parseComm :: Parser (ScheduleComm)
parseComm =     try newSchedP
            <|> try newContactP 
            <|> try newRemindP
            <|> try newMeetingP 
            <|> try newDebtP
            <|> try addContactP 
            <|> try addRemindP 
            <|> try addDebtP
            <|> try addGrocerieP 
            <|> try delContactP 
            <|> try delRemindP 
            <|> try delDebtP 
            <|> try delGrocerieP 
            <|> try updAddressP
            <|> try searchContactP
            <|> try allContactsP
            <|> try updPhoneP
            <|> try intervalP
            <|> try thisWeekP
            <|> try thisMonthP
            <|> try allMeetingsP
            <|> try allRemindsP
            <|> try debtsToP
            <|> try debtsHigherP
            <|> try allDebtsP
            <|> try allGroceriesP

newSchedP :: Parser (ScheduleComm)
newSchedP = do reserved lisComm "newSched"
               owner <- identifier lisComm
               return (NewSched owner)

newContactP :: Parser ScheduleComm
newContactP = do reserved lisComm "newContact"
                 name <- identifier lisComm
                 ph   <- phoneP
                 addr <- addrP
                 return (NewContact name ph addr)

newRemindP :: Parser ScheduleComm
newRemindP = do reserved lisComm "newRemind"
                day   <- natural lisComm
                (try (reservedOp lis "/") <|> (reservedOp lis "-"))
                month <-  natural lisComm
                (try (reservedOp lis "/") <|> (reservedOp lis "-"))
                year  <-  natural lisComm
                description <- line
                return (NewRemind (DateTime (fromInteger year) (fromInteger month) (fromInteger day) 0 0 0) description)

newMeetingP :: Parser ScheduleComm
newMeetingP = do reserved lisComm "newMeeting"
                 date <- dateP
                 description <- line
                 return (NewMeeting date description)

newDebtP :: Parser ScheduleComm
newDebtP = do reserved lisComm "newDebt"
              name <- identifier lis
              much <- natural lis
              why <- str
              return (NewDebt name much why)

addContactP :: Parser ScheduleComm
addContactP = do reserved lisComm "addContact"
                 c <- contactP
                 return (AddContact c)

addRemindP :: Parser ScheduleComm
addRemindP = do reserved lisComm "addRemind"
                r <- reminderP
                return (AddRemind r)

addDebtP :: Parser ScheduleComm
addDebtP = do reserved lisComm "addDebt"
              d <- debtP
              return (AddDebt d)

addGrocerieP :: Parser ScheduleComm
addGrocerieP = do reserved lisComm "addGrocerie"
                  g <- grocerieP
                  return (AddGrocerie g)

delContactP :: Parser ScheduleComm
delContactP = do reserved lisComm "delContact"
                 c <- identifier lis
                 return (DelContact c)

delRemindP :: Parser ScheduleComm
delRemindP = do reserved lisComm "delRemind"
                r <- reminderP
                return (DelRemind r)

delDebtP :: Parser ScheduleComm
delDebtP = do reserved lisComm "delDebt"
              d <- debtP
              return (DelDebt d)

delGrocerieP :: Parser ScheduleComm
delGrocerieP = do reserved lisComm "delGrocerie"
                  g <- str
                  return (DelGrocerie g)

updAddressP :: Parser ScheduleComm
updAddressP = do reserved lisComm "updAddress"
                 n <- nameP
                 addr <- addrP
                 return (UpdAddress n addr)

updPhoneP :: Parser ScheduleComm
updPhoneP = do reserved lisComm "updPhone"
               n <- nameP
               ph <- phoneP
               return (UpdPhone n ph)

searchContactP :: Parser ScheduleComm
searchContactP = do reserved lisComm "searchContact"
                    n <- nameP
                    return (SearchContact n)

intervalP :: Parser ScheduleComm
intervalP = do reserved lisComm "interval"
               n <- natural lis
               return (Interval n)

thisWeekP :: Parser ScheduleComm
thisWeekP = do reserved lisComm "thisWeek"
               return (ThisWeek)

thisMonthP :: Parser ScheduleComm
thisMonthP = do reserved lisComm "thisMonth"
                return (ThisMonth)

allContactsP :: Parser ScheduleComm
allContactsP = do reserved lisComm "allContacts"
                  return AllContacts

allMeetingsP :: Parser ScheduleComm
allMeetingsP = do reserved lisComm "allMeetings"
                  return (AllMeetings)

allRemindsP :: Parser ScheduleComm
allRemindsP = do reserved lisComm "allReminds"
                 return (AllReminds)

debtsToP :: Parser ScheduleComm
debtsToP = do reserved lisComm "debtsTo"
              n <- nameP
              return (DebtsTo n)

debtsHigherP :: Parser ScheduleComm
debtsHigherP = do reserved lisComm "debtsHigher"
                  reservedOp lis "$"
                  n <- natural lis
                  return (DebtsHigher n)

allDebtsP :: Parser ScheduleComm
allDebtsP = do reserved lisComm "allDebts"
               return AllDebts

allGroceriesP :: Parser ScheduleComm
allGroceriesP = do reserved lisComm "allGroceries"
                   return AllGroceries





