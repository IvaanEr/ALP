module Operations where

import AST
import Data.Dates


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

addContact :: Contact -> Schedule -> Maybe Schedule
addContact newCon (Sched owner contacts x y z) = case isContact (name newCon) contacts of
                                      True -> Nothing
                                      False -> Just (Sched owner (contacts++[newCon]) x y z)

deleteContact ::  Name -> Schedule -> Schedule
deleteContact n (Sched owner contacts x y z) = Sched owner (delete n contacts) x y z


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

updateSchedPh :: Name -> PhoneNum -> Schedule -> Maybe Schedule
updateSchedPh n newPh (Sched owner contacts x y z) = case isContact n contacts of
                                          True -> Just (Sched owner (updatePh n newPh contacts) x y z)
                                          False -> Nothing

updateSchedAddr :: Name -> Address -> Schedule -> Maybe Schedule
updateSchedAddr n newAddr (Sched owner contacts x y z ) = case isContact n contacts of
                                            True -> Just (Sched owner (updateAddr n newAddr contacts) x y z)
                                            False -> Nothing

-- Create a Remind
newRemind :: Int -> Int -> Int -> String -> Reminder
newRemind day month year st = Remind (DateTime year month day 0 0 0) st

newMeeting :: DateTime -> String -> Reminder
newMeeting date st = Meeting date st 
-- Adding and removing reminders from a schedule
-- There is no difference between the types Remind and Meeting
-- but we will have differents operations for these later

-- Add a remind or a meeting as well
addRemind :: Reminder -> Schedule -> Maybe Schedule
addRemind r (Sched own x reminders y z) = Just (Sched own x (reminders++[r]) y z)


-- isRemind :: Reminder -> Bool
-- isRemind (Remind _ _) = True
-- isRemind (Meeting _ _) = False

-- isMeeting :: Reminder -> Bool
-- isMeeting r = not (isRemind r)

removeRemind :: Reminder -> Reminders -> Reminders
removeRemind _ [] = []
removeRemind (Remind d s) ((Meeting _ _):xs) = removeRemind (Remind d s) xs
removeRemind (Remind d s) ((Remind d' s'):xs) = if d == d' && s == s'
                                                then xs
                                                else ((Remind d' s'):(removeRemind (Remind d s) xs))