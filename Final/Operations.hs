module Operations where

import Data.Dates
import Data.Maybe

import System.IO.Unsafe (unsafePerformIO)

import AST


-- Creating a new Schedule
newSched :: Owner -> Schedule
newSched n = Sched n [] [] [] []

-- A new Contact
newContact :: Name -> PhoneNum -> Address -> Contact
newContact n ph addr = Contact n ph addr

-- Adding and removing contacts from a Schedule
isContact :: Name -> Contacts -> Bool
isContact n [] = False
isContact n (x:xs) = if n == name x then True else isContact n xs

delete :: Name -> Contacts -> Contacts
delete _ [] = []
delete n (x:xs) = if n == name x then xs else (x: delete n xs)

addContact :: Contact -> Schedule -> Either Error Schedule
addContact newCon (Sched owner contacts x y z) = case isContact (name newCon) contacts of
                                      True -> Left Repeat
                                      False -> Right (Sched owner (contacts++[newCon]) x y z)

deleteContact ::  Name -> Schedule -> Either Error Schedule
deleteContact n (Sched owner contacts x y z) = case isContact n contacts of
                                                True -> Right $ Sched owner (delete n contacts) x y z
                                                False -> Left Unexist

-- Updating a contact from a Schedule
updatePh :: Name -> PhoneNum -> Contacts -> Contacts
updatePh n newPh [] = []
updatePh n newPh (con@(Contact name ph addr ):xs) = if n == name
                                                     then ((Contact name newPh addr):xs) 
                                                     else (con:(updatePh n newPh xs))

updateAddr :: Name -> Address -> Contacts -> Contacts
updateAddr n newAddr [] = []
updateAddr n newAddr (con@(Contact name ph addr ):xs) = if n == name
                                                             then ((Contact name ph newAddr):xs)
                                                             else (con:(updateAddr n newAddr xs))

updateSchedPh :: Name -> PhoneNum -> Schedule -> Either Error Schedule
updateSchedPh n newPh (Sched owner contacts x y z) = case isContact n contacts of
                                          True -> Right (Sched owner (updatePh n newPh contacts) x y z)
                                          False -> Left Unexist

updateSchedAddr :: Name -> Address -> Schedule -> Either Error Schedule
updateSchedAddr n newAddr (Sched owner contacts x y z ) = case isContact n contacts of
                                            True -> Right (Sched owner (updateAddr n newAddr contacts) x y z)
                                            False -> Left Unexist

-- =================================================================================================================

-- Create a Remind
newRemind :: Int -> Int -> Int -> String -> Reminder
newRemind day month year st = Remind (DateTime year month day 0 0 0) st

newMeeting :: DateTime -> String -> Reminder
newMeeting date st = Meeting date st 

-- Adding and removing reminders from a schedule
-- There is no difference between the types Remind and Meeting
-- but we will have differents operations for these later
instance Eq Reminder where
  (Meeting d1 s1) == (Meeting d2 s2)   = d1 == d2 && s1 == s2
  (Remind d1 s1) == (Remind d2 s2)     = d1 == d2 && s1 == s2
  (Meeting _ _) == (Remind _ _)        = False
  x == y                               = y == x

-- Add a remind or a meeting as well
isReminder :: Reminder -> Reminders -> Bool
isReminder r [] = False
isReminder r (x:xs) = if r ==  x then True else isReminder r xs

isMeeting :: Reminder -> Bool
isMeeting (Meeting _ _) = True
isMeeting (Remind _ _) = False

isRemind :: Reminder -> Bool
isRemind x = not (isMeeting x)

addRemindSched :: Reminder -> Schedule -> Either Error Schedule
addRemindSched r (Sched own x rs y z) = case isReminder r rs of
                                          True -> Left Repeat
                                          False -> Right (Sched own x (r:rs) y z)

removeRemind :: Reminder -> Reminders -> Reminders 
removeRemind r [] = []
removeRemind r (x:xs) = if r == x then xs else (x:(removeRemind r xs))

removeRemindSched :: Reminder -> Schedule -> Either Error Schedule
removeRemindSched r (Sched own x rs y z) = case isReminder r rs of
                                            False -> Left Unexist
                                            True -> Right (Sched own x (removeRemind r rs) y z)

-- Usefull operations about Reminders

-- interval (today, one week after)
oneWeek :: (DateTime, DateTime)
oneWeek = (unsafePerformIO getCurrentDateTime,addInterval (unsafePerformIO getCurrentDateTime) (Days 7))

-- Is a meeting or a reminder this week?
isThisWeek :: Reminder -> Bool
isThisWeek (Remind d _) = fst oneWeek <= d && d <= snd oneWeek
isThisWeek (Meeting d _) = fst oneWeek <= d && d <= snd oneWeek

-- MORE GENERAL !!

-- From today, how much do u wanna see?
intervalDays :: Integer -> (DateTime,DateTime)
intervalDays days = (unsafePerformIO getCurrentDateTime, addInterval (unsafePerformIO getCurrentDateTime) (Days days))

intervalMonths :: Integer -> (DateTime,DateTime)
intervalMonths months = (unsafePerformIO getCurrentDateTime, addInterval (unsafePerformIO getCurrentDateTime) (Months months))

-- Is this reminder in this dates?
isInInterval :: Reminder -> (DateTime,DateTime)-> Bool
isInInterval (Remind d _) (today,add)  = today <= d && d <= add
isInInterval (Meeting d _) (today,add) = today <= d && d <= add

-- All meetings in interval (today, today + days)
intervalDaysMeetings :: Integer -> Schedule -> Reminders
intervalDaysMeetings days (Sched _ _ ys _ _) = intervalDaysMeetings' (intervalDays days) ys []

intervalDaysMeetings' :: (DateTime,DateTime) -> Reminders -> Reminders -> Reminders
intervalDaysMeetings' _ [] xs = xs
intervalDaysMeetings' int (y:ys) xs = if isInInterval y int && isMeeting y then intervalDaysMeetings' int ys (y:xs)
                                                                             else intervalDaysMeetings' int ys xs
-- All reminders in interval (today, today + days)
intervalDaysReminders :: Integer -> Schedule -> Reminders
intervalDaysReminders days (Sched _ _ ys _ _) = intervalDaysReminders' (intervalDays days) ys []

intervalDaysReminders' :: (DateTime,DateTime) -> Reminders -> Reminders -> Reminders
intervalDaysReminders' _ [] xs = xs
intervalDaysReminders' int (y:ys) xs = if isInInterval y int && isRemind y then intervalDaysReminders' int ys (y:xs)
                                                                           else intervalDaysReminders' int ys xs

-- All Meetings this week
thisWeekMeetings :: Schedule -> Reminders
thisWeekMeetings sched = intervalDaysMeetings 7 sched

-- All Reminders this week
thisWeekReminders :: Schedule -> Reminders
thisWeekReminders sched = intervalDaysReminders 7 sched