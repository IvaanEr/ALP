module Operations where

import Data.Dates
--import Data.Maybe

import System.IO.Unsafe (unsafePerformIO)

import Types


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
addContact newCon (Sched own contacts x y z) = case isContact (name newCon) contacts of
                                      True -> Left Repeat
                                      False -> Right (Sched own (contacts++[newCon]) x y z)

deleteContact ::  Name -> Schedule -> Either Error Schedule
deleteContact n (Sched own contacts x y z) = case isContact n contacts of
                                                True -> Right $ Sched own (delete n contacts) x y z
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
updateSchedPh n newPh (Sched own contacts x y z) = case isContact n contacts of
                                          True -> Right (Sched own (updatePh n newPh contacts) x y z)
                                          False -> Left Unexist

updateSchedAddr :: Name -> Address -> Schedule -> Either Error Schedule
updateSchedAddr n newAddr (Sched own contacts x y z ) = case isContact n contacts of
                                            True -> Right (Sched own (updateAddr n newAddr contacts) x y z)
                                            False -> Left Unexist

-- =================================================================================================================

-- Create a Remind
newRemind :: Int -> Int -> Int -> String -> Reminder
newRemind day month year st = Remind (DateTime year month day 0 0 0) st

newMeeting :: DateTime -> String -> Reminder
newMeeting date st = Meeting date st 

-- Adding and removing reminders from a schedule

instance Eq Reminder where
  (Meeting d1 s1) == (Meeting d2 s2)   = d1 == d2 && s1 == s2
  (Remind d1 s1) == (Remind d2 s2)     = d1 == d2 && s1 == s2
  (Meeting _ _) == (Remind _ _)        = False
  x == y                               = y == x

-- Add a remind or a meeting as well
isReminder :: Reminder -> Reminders -> Bool
isReminder _ [] = False
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
removeRemind _ [] = []
removeRemind r (x:xs) = if r == x then xs else (x:(removeRemind r xs))

removeRemindSched :: Reminder -> Schedule -> Either Error Schedule
removeRemindSched r (Sched own x rs y z) = case isReminder r rs of
                                            False -> Left Unexist
                                            True -> Right (Sched own x (removeRemind r rs) y z)

-- Usefull operations about Reminders

-- From today, how much do u wanna see?
intervalDays :: Integer -> (DateTime,DateTime)
intervalDays days = (unsafePerformIO getCurrentDateTime, addInterval (unsafePerformIO getCurrentDateTime) (Days days))

intervalMonths :: Integer -> (DateTime,DateTime)
intervalMonths months = (unsafePerformIO getCurrentDateTime, addInterval (unsafePerformIO getCurrentDateTime) (Months months))

-- Is this reminder in this dates?
isInInterval :: Reminder -> (DateTime,DateTime)-> Bool
isInInterval (Remind d _) (today,add)  = today <= d && d <= add
isInInterval (Meeting d _) (today,add) = today <= d && d <= add

-- All reminders in interval (today, today + days)
intervalDaysReminders :: Integer -> Schedule -> Reminders
intervalDaysReminders days sched = intervalDaysReminders' (intervalDays days) (reminders sched) []

intervalDaysReminders' :: (DateTime,DateTime) -> Reminders -> Reminders -> Reminders
intervalDaysReminders' _ [] xs = xs
intervalDaysReminders' int (y:ys) xs = if isInInterval y int then intervalDaysReminders' int ys (y:xs)
                                                             else intervalDaysReminders' int ys xs

-- All Reminders this week
thisWeekReminders :: Schedule -> Reminders
thisWeekReminders sched = intervalDaysReminders 7 sched

-- All Reminders one month ahead
thisMonthReminders :: Schedule -> Reminders
thisMonthReminders sched = intervalDaysReminders 30 sched

-- Filter reminders and meetings
getReminds :: Reminders -> Reminders
getReminds xs = filter isRemind xs

getMeetings :: Reminders -> Reminders
getMeetings xs = filter isMeeting xs

getRemindsSched :: Schedule -> Reminders
getRemindsSched (Sched _ _ xs _ _) = getReminds xs

getMeetingsSched :: Schedule -> Reminders
getMeetingsSched (Sched _ _ xs _ _) = getMeetings xs

-- =====================================================================================================================

-- Debts operations

-- Create a debt

instance Eq Debt where
  (Debt x1 y1 z1) == (Debt x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2 

newDebt :: Name -> Integer -> String -> Debt
newDebt who howmuch why = Debt who howmuch why

isDebt :: Debt -> Debts -> Bool
isDebt _ [] = False
isDebt d (x:xs) = if x == d then True else isDebt d xs

addDebtSched :: Debt -> Schedule -> Either Error Schedule
addDebtSched debt (Sched own x y [] z) = Right (Sched own x y [debt] z)
addDebtSched debt (Sched own x y ds z) = case isDebt debt ds of
                                           True -> Left Repeat
                                           False -> Right (Sched own x y (debt:ds) z)

removeDebt :: Debt -> Debts -> Debts
removeDebt _ [] = []
removeDebt d (x:xs) = if d == x then xs else (x:(removeDebt d xs))

removeDebtSched :: Debt -> Schedule -> Either Error Schedule
removeDebtSched d (Sched own x y ds z) = case isDebt d ds of
                                        False -> Left Unexist
                                        True -> Right (Sched own x y (removeDebt d ds) z)


-- Debts to someone specific
debtsTo :: Name -> Schedule -> Debts
debtsTo n sched = filter (\(Debt x _ _) -> x == n) (debts sched)

-- Debts that are higher or equal to...
debtsHigher :: Integer -> Schedule -> Debts
debtsHigher i sched = filter (\(Debt _ x _) -> x >= i) (debts sched)

-- ===================================================================================================0

-- Groceries operations

isGrocerie :: String -> [String] -> Bool
isGrocerie _ [] = False
isGrocerie st (x:xs) = if st == x then True else isGrocerie st xs

newGrocerie :: String -> String 
newGrocerie = id

addGrocerieSched :: String -> Schedule -> Either Error Schedule
addGrocerieSched st (Sched own x y z []) = Right $ Sched own x y z [st]
addGrocerieSched  st (Sched own x y z gs) = case isGrocerie st gs of
                                              True -> Left Repeat
                                              False -> Right (Sched own x y z (st:gs))

removeGrocerie :: String -> [String] -> [String]
removeGrocerie _ [] = []
removeGrocerie st (x:xs) = if st == x then xs else (x: removeGrocerie st xs)

removeGrocerieSched :: String -> Schedule -> Either Error Schedule
removeGrocerieSched st (Sched own x y z gs) = case isGrocerie st gs of
                                                False -> Left Unexist
                                                True -> Right (Sched own x y z (removeGrocerie st gs))
