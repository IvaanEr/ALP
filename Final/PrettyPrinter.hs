module PrettyPrinter where

import Types
  
import Text.PrettyPrint.HughesPJ
import Data
import Data.Dates

printPhone :: PhoneNum -> Doc
printPhone (Phone pref num) = text "ph:" <+> 
                              integer pref <> text "-" <>
                              integer num

printAddr :: Address -> Doc
printAddr (Addr street num) = text "addr:" <+> 
                              text street <+>
                              integer num

printContact :: Contact -> Doc
printContact (Contact n p a) = text "contact:" <+>
                               text "n:" <+>
                               text n <+>
                               printPhone p <+>
                               printAddr a

printList :: [a] -> (a -> Doc) -> Doc
printList xs p = brackets (printList' xs p)

printList' :: [a] -> (a -> Doc) -> Doc
printList' [] _ = empty
printList' [x] p = p x
printList' (x:xs) p = (p x <> comma <+> printList' xs p)

printListContact :: Contacts -> Doc
printListContact xs = printList xs printContact 

printDate :: DateTime -> Doc
printDate date = let d   = day date
                     m   = month date
                     y   = year date
                     h   = hour date
                     min = minute date
                     s   = text "-"
                 in int d <> s <> int m <> s <> int y <+> 
                    int h <> colon <> int min  

printReminder :: Reminder -> Doc
printReminder (Remind d s) = text "R" <+> printDate d <+> text s 
printReminder (Meeting d s) = text "M" <+> printDate d <+> text s

printListReminder :: Reminders -> Doc
printListReminder xs = printList xs printReminder

printDebt :: Debt -> Doc
printDebt (Debt who n why) = text "D"  <+> 
                             text who  <+> 
                             integer n <+> 
                             text why

printListDebt :: Debts -> Doc
printListDebt xs = printList xs printDebt

printGrocerie :: Grocerie -> Doc
printGrocerie = text

printListGrocerie :: Groceries -> Doc
printListGrocerie xs = printList xs printGrocerie

printSched :: Schedule -> Doc
printSched (Sched own x y z t) = text "Sched:" <+>
                                 text own <+>
                                 printListContact x  <+>
                                 printListReminder y <+>
                                 printListDebt z     <+>
                                 printListGrocerie t

-- printTerm  :: Term -> Doc
-- printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t


