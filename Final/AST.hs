module AST where

import Data.Dates

type Name = String
-- type Email = String
type Owner = Name


data Address = Addr String Integer deriving Show

data PhoneNum = Phone Integer Integer deriving Show

-- data Date = Date { day :: Integer
--                  , month :: Integer
--                  , year :: Integer } deriving Show

-- data Hour = Houw { tHour :: Integer
--                  , tMinute :: Integer
--                  , tSecond :: Integer} deriving Show

-- data Email = Email String String deriving Show

data Contact = Contact {  name  :: Name
                       ,  phone :: PhoneNum
                       ,  addr  :: Address
                       } deriving Show 


data Reminder =   Remind DateTime String
                | Meeting DateTime String 
                deriving Show

data Debt = Debt Name Integer deriving Show



type Debts = [Debt]
type Contacts = [Contact]
type Reminders = [Reminder]
type Groceries = [String]

data Schedule = Sched Owner
                      Contacts
                      Reminders
                      Debts
                      Groceries deriving Show
                      
