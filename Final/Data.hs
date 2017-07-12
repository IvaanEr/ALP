module Data where

import AST
import Operations


pedro :: Contact
pedro = Contact "pedro" (Phone 336 4289111) (Addr "Ayacucho" 1433)

juan :: Contact
juan = Contact "juan" (Phone 341 3509249) (Addr "9 de Julio" 728) 

luis :: Contact
luis = Contact "luis" (Phone 336 2906878) (Addr "Pellegrini" 702) 

mySched :: Schedule
mySched = Sched "Ivan" [pedro,juan,luis]

