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


data Reminder = Remind DateTime String
                | Meeting DateTime String 
                deriving Show

-- I debt Integer to Name because of String
data Debt = Debt Name Integer String deriving Show



type Debts = [Debt]
type Contacts = [Contact]
type Reminders = [Reminder]
type Groceries = [String]

data Schedule = Sched {  owner :: Owner
                      ,  contacts :: Contacts
                      ,  reminders :: Reminders
                      ,  debts :: Debts
                      ,  groceries :: Groceries 
                      } deriving Show

data Error =   Repeat
             | Unexist deriving Show

data LoadSched = Null | Schedule deriving Show

data State = State { file :: String -- Last schedule loaded 
                   , load_sched :: LoadSched
                   } deriving Show
