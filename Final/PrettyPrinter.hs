
module PrettyPrinter where

import Types
  
import Text.PrettyPrint.HughesPJ
import Data

printPhone :: PhoneNum -> Doc
printPhone (Phone pref num) = text "ph:" <+> 
                              integer pref <+> 
                              integer num

printAddr :: Address -> Doc
printAddr (Addr street num) = text "addr:" <+> 
                              text street <+>
                              integer num

printContact :: Contact -> Doc
printContact (Contact n p a) = text "Contact:" <+>
                               text "n:" <+>
                               text n <+>
                               printPhone p <+>
                               printAddr a






-- printTerm  :: Term -> Doc
-- printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t


