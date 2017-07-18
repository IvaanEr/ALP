module Data where

import Types
import Operations
import Data.Dates
-- import System.IO.Unsafe (unsafePerformIO)


pedro :: Contact
pedro = Contact "pedro" (Phone 336 4289111) (Addr "Ayacucho" 1433)

juan :: Contact
juan = Contact "juan" (Phone 341 3509249) (Addr "9 de Julio" 728) 

luis :: Contact
luis = Contact "luis" (Phone 336 2906878) (Addr "Pellegrini" 702) 

mySched :: Schedule
mySched = Sched "Ivan" [pedro,juan,luis] rr [] []

today = DateTime 2017 07 15 14 24 00
oneDay = DateTime 2017 07 14 10 00 00
yesterday = DateTime 2017 07 16 0 0 0
anotherDay = DateTime 2017 07 01 0 0 0


r1 = Remind yesterday "Ir a la pelu"
r2 = Remind today "ser feliz"
r3 = Remind oneDay "pegarle a alguien"

m1 = Meeting yesterday "Junta abogados"
m2 = Meeting anotherDay "Junta perritos"

rr = [m1,r2,r3,m2]

