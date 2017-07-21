module AST where

import Types
import Data.Dates

data InterpreterComm = Recompile
                       | Load String
                       | Browse
                       | Help
                       | Quit
                       | Operations
                       | Noop deriving Show

data ScheduleComm = NewSched Owner
                  
                  | NewContact Name PhoneNum Address
                  | NewRemind DateTime String -- needs only day-month-year
                  | NewMeeting DateTime String -- use "date day-manth-year hour-minute" to create a date
                  | NewDebt Name Integer String
                  -- | NewGrocerie String         -- no needed

                  | AddContact Contact
                  | AddRemind Reminder
                  | AddDebt Debt
                  | AddGrocerie Grocerie

                  | DelContact Name
                  | DelRemind Reminder
                  | DelDebt Debt
                  | DelGrocerie Grocerie

                  --operations on contacts
                  | UpdAddress Name Address
                  | UpdPhone Name PhoneNum 

                  -- operations on reminders
                  | Interval Integer
                  | ThisWeek
                  | ThisMonth
                  | AllMeetings
                  | AllReminds
                      --could add to filter meetings and reminders when u ask week and month

                  --operations on debts
                  | DebtsTo Name
                  | DebtsHigher Integer deriving Show