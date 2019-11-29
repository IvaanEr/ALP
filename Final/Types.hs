module Types where

import Data.Dates

type Name = String
-- type Email = String
type Owner = Name

data Address = Addr String Integer deriving Show

data PhoneNum = Phone Integer Integer deriving Show


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
type Grocerie = String
type Groceries = [Grocerie]

data Schedule = Sched {  owner :: Owner
                      ,  contacts :: Contacts
                      ,  reminders :: Reminders
                      ,  debts :: Debts
                      ,  groceries :: Groceries 
                      } deriving Show

data Error =   Repeat
             | Unexist deriving Show

data LoadSched = Null | LS Schedule deriving Show

data State = State { file :: String
                   , loadSched :: LoadSched
                   } deriving Show