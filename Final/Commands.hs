{-# OPTIONS -XRecordWildCards #-}
module Commands where

import Data.List
import Data.Char
import Text.ParserCombinators.Parsec (parse)
import System.Console.Readline
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (catch,IOException)
import Text.PrettyPrint.HughesPJ (render)

import Types
import ParserComm
import AST
import Parser
import PrettyPrinter
import Operations

data Commands =  IComm InterpreterComm | SComm ScheduleComm

data InteractiveCommand = ICmd [String] String (String -> InterpreterComm) String

data ScheduleCommand = SCmd String [String] String

commands :: [InteractiveCommand]
commands = [ ICmd [":b",":browse"]  ""  (const Browse) "Names in scope",
             ICmd [":l",":load"]    "<file>" (Load) "Load a schedule from a file",
             --ICmd [":r",":reload"] "" (const Recompile) "Reload last file loaded",
             ICmd [":?",":help"]   "" (const Help) "Show this list of commands",
             ICmd [":q",":quit"]   "" (const Quit) "Exit interpreter",
             ICmd [":d",":display"]   "" (const Display) "Show the current schedule loaded",
             ICmd [":o",":operations"] "" (const Operations) "Show operations on schedulers",
             ICmd [":c",":close"]  "" (const Close) "Close current schedule, saving changes",
             ICmd [":s",":save"]        "" (const Save) "Save the changes on the schedule"
           ]

schedcommands :: [ScheduleCommand]
schedcommands = [ SCmd "newSched"   ["<owner>"] "Create a new schedule",
                  --SCmd "newContact" ["<name>", "<phone>","<addres>"] "Create a new contact",
                  --SCmd "newRemind"  ["<year>","<month>","<day>","<description>"] "Adding a remind",
                  --SCmd "newMeeting" ["<date>","<description>"] "Adding a meeting",
                  
                  SCmd "addContact" ["<contact>"]  "Add a contact", 
                  SCmd "addRemind"  ["<reminder>"] "Add a remind or a meeting",
                  SCmd "addDebt"    ["<debt>"]     "Add a debt",
                  SCmd "addGrocerie"["<grocerie>"] "Add a grocerie",

                  SCmd "delContact" ["<contact>"] "Remove a contact",
                  SCmd "delRemind"  ["<remind>"]  "Remove remind",
                  SCmd "delDebt"    ["<debt>"]      "Remove debt",
                  SCmd "delGrocerie"["<grocerie>"] "Remove grocerie",

                  SCmd "updAddress" ["<name>","<address>"] "Update contact address",
                  SCmd "updPhone"   ["<name>","phone"]     "Update contact phone",
                  SCmd "searchContact" ["<name>"]          "Search contact",
                  
                  SCmd "allContacts" [] "Show all contacts",
                  SCmd "allReminds"  [] "Show all reminds",
                  SCmd "allMeetings" [] "Show all meetings",
                  SCmd "allGroceries" [] "Show all groceries",
                  SCmd "allDebts"    [] "Show all debts",
                
                  SCmd "interval" ["<i>"] "Show reminders and meetings between today and i days forward",
                  SCmd "thisWeek" [] "Reminders and meetings this week",
                  SCmd "thisMonth" [] "Reminders and meetings this month",
                  SCmd "debtsTo" ["<name>"] "Debts to him/her",
                  SCmd "debtsHigher" ["<n>"] "Debts higher or equal to n"
                ]

interpretCommand :: String -> IO Commands
interpretCommand x 
  = if isPrefixOf ":" x then 
      do let (cmd,t') = break isSpace x
             t        = dropWhile isSpace t'
             matching = filter (\(ICmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
         case matching of
          []            -> do putStrLn ("Unknown command "++cmd++". Use :? or :op for help.")
                              return (IComm Noop)
          [ICmd _ _ f _] -> do return (IComm (f t))
          _             -> do putStrLn ("Ambiguous command, could refer to"++
                                        concat (intersperse ", " [head cs | (ICmd cs _ _ _) <- matching])++".")
                              return (IComm Noop)
    else case parseThis x of
           Left _  -> do putStrLn ("Unknown operation or invalid arguments. Use :o for help.") 
                         return (IComm Noop)
           Right c -> return (SComm c)


handleCommand :: State -> Commands -> IO (Maybe State)
handleCommand state comm = case comm of
                             (IComm x) ->handleInterpreter state x
                             (SComm x) ->handleSchedule state x

handleInterpreter :: State -> InterpreterComm -> IO (Maybe State)
handleInterpreter state (Load f)   = case loadSched state of
                                         Null -> do state' <- compileFile (state {file=(f++".sched")}) (f++".sched") 
                                                    return (Just state')
                                         (LS s) -> do x <- readline ("Want to close the actual schedule "++owner s++" and load a another one? [y/n]\n")
                                                      case x of
                                                        (Just "y") -> do putStrLn ("Saving schedule "++owner s)
                                                                         writeFile (file state) (render $ printSched s)
                                                                         putStrLn ("Save succesfull.")
                                                                         state' <- compileFile (state {file=(f++".sched")}) (f++".sched") 
                                                                         return (Just state')
                                                        (Just "n") -> do putStrLn ("Canceling load operation...")
                                                                         return (Just state)
                                                        (Just _)   -> do putStrLn ("Error: expecting \"y\" or \"n\".") >> return (Just state)
                                                        Nothing    -> do putStrLn "Nothing done." >> return (Just state)

-- handleInterpreter state Recompile  = do state' <- compileFile state (file state)
--                                         return (Just state')

handleInterpreter state Browse     = do putStrLn ("Load schedule from: "++getOwner (loadSched state)++".")
                                        return (Just state)
handleInterpreter state Help       = putStr (helpTxt commands) >> return (Just state)
handleInterpreter state Operations = putStr (opTxt schedcommands) >> return (Just state)
handleInterpreter state Quit       = case loadSched state of
                                       Null   -> do putStrLn "Good Bye!" >> return Nothing
                                       (LS s) -> do x <- readline "Do you want to save the current schedule first? [y/n]\n"
                                                    case x of
                                                      (Just "y") -> do putStrLn ("Saving schedule "++owner s)
                                                                       writeFile (file state) (render $ printSched s)
                                                                       putStrLn ("Save succesfull.")
                                                                       putStrLn ("Good bye "++(owner s)++".")
                                                                       return Nothing
                                                      (Just "n") -> do putStrLn ("Good bye "++(owner s)++".")
                                                                       return Nothing
                                                      (Just _)   -> do putStrLn ("Error: expecting \"y\" or \"n\".") >> return (Just state)
                                                      Nothing    -> do putStrLn "Nothing done." >> return (Just state)
handleInterpreter state Display     = case loadSched state of
                                        Null   -> do putStrLn "No schedule loaded." >> return (Just state)
                                        (LS s) -> do putStrLn (render (printSched s)) >> return (Just state)
handleInterpreter state Save        = case loadSched state of
                                       Null   -> do putStrLn "No schedule loaded." >> return (Just state)
                                       (LS s) -> do putStrLn ("Saving schedule "++owner s)
                                                    writeFile (file state) (render $ printSched s)
                                                    putStrLn ("Save succesfull.")
                                                    return (Just state)
handleInterpreter state Close       = case loadSched state of
                                        Null   -> do putStrLn "No schedule loaded." >> return (Just state)
                                        (LS s) -> do putStrLn ("Closing schedule "++owner s)
                                                     putStrLn ("Saving schedule "++owner s)
                                                     writeFile (file state) (render $ printSched s)
                                                     putStrLn ("Save succesfull.")
                                                     return (Just (state {loadSched = Null}))
handleInterpreter state Noop        = return (Just state)



handleSchedule ::  State -> ScheduleComm -> IO (Maybe State)
handleSchedule state (NewSched newOwner)
 = case loadSched state of
     (LS s) -> do x <- readline "Want to close the actual schedule and create a new one? [y/n]\n"
                  case x of
                    Nothing    -> do putStrLn "Nothing done."
                                     return (Just state)
                    (Just "y") -> do fileExist <- doesFileExist (newOwner++".sched")
                                     if fileExist 
                                     then do y <- readline "The file already exist, some data could be deleted\nDo you want to continue? [y/n]\n"
                                             case y of
                                               Nothing -> do putStrLn "Nothing done."
                                                             return (Just state)
                                               (Just "y") -> do let new = newSched newOwner
                                                                putStrLn ("Saving schedule: "++(owner $ s)++"...\n")
                                                                writeFile (file state) (render $ printSched s) -- save the old sched
                                                                writeFile (newOwner++".sched") (render $ printSched (new))        -- create the new one
                                                                return (Just (state {file=(newOwner++".sched"),loadSched=(LS $ new)}))
                                               (Just "n") -> do putStrLn ("Canceling newSched operation...\n")
                                                                return (Just state)
                                               (Just _)   -> do putStrLn ("Error: expecting \"y\" or \"n\".") >> return (Just state)
                                     else do let new = newSched newOwner
                                             putStrLn ("Saving schedule: "++file state++"...")
                                             writeFile (file state) (render $ printSched s) --save the old sched
                                             writeFile (newOwner++".sched") (render $ printSched new )     --create the new one
                                             return (Just (state {file=(newOwner++".sched"),loadSched=(LS $ new)}))         
                    (Just "n") -> do putStrLn ("Canceling newSched operation...")
                                     return (Just state)
                    (Just "_") -> putStrLn ("Error: expecting \"y\" or \"n\".") >> return (Just state)
     Null  -> do fileExist <- doesFileExist (newOwner++".sched")
                 if fileExist
                 then do y <- readline "The file already exist, some data could be deleted\nDo you want to continue? [y/n]\n"
                         case y of
                           Nothing -> do putStrLn "Nothing done."
                                         return (Just state)
                           (Just "y") -> do let new = newSched newOwner
                                            writeFile (newOwner++".sched") (render $ printSched new)
                                            return (Just (state {file=(newOwner++".sched"),loadSched=(LS $ new)}))
                           (Just "n") -> do putStrLn ("Canceling newSched operation...")
                                            return (Just state)
                           (Just _)   -> do putStrLn ("Error: expecting \"y\" or \"n\".") >> return (Just state)
                 else do let new = newSched newOwner
                         writeFile (newOwner++".sched") (render $ printSched new)
                         return (Just (state {file=(newOwner++".sched"),loadSched=(LS $ new)}))

handleSchedule state op = case loadSched state of
                         Null   -> do putStrLn "Error: No Schedule loaded for such operation.\n"
                                      return (Just state)
                         (LS s) -> handleSchedule' state s op


-- The schedule is pass as a parameter because if not we have to do this in every case...
  -- let (Ls s) = loadSched state
  -- in case Op s of

handleSchedule' :: State -> Schedule -> ScheduleComm -> IO (Maybe State)
handleSchedule' state s (AddContact con)
 = case addContact con s of
    (Left e) -> do putStrLn ("Error: "++name con++" "++show e)
                   return (Just state)
    (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (AddRemind r)
 = case addRemindSched r s of
     (Left e) -> do putStrLn ("Error: "++show e++"reminder.")
                    return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (AddDebt d)
 = case addDebtSched d s of
     (Left e) -> do putStrLn ("Error: "++show e++" debt.") >> return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (AddGrocerie g)
  = case addGrocerieSched g s of
      (Left e) -> do putStrLn ("Error: "++show e++" grocerie.") >> return (Just state)
      (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (DelContact n)
 = case deleteContact n s of
     (Left e) -> do putStrLn ("Error: "++show e++" contact.") >> return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (DelRemind r)
 = case removeRemindSched r s of
     (Left e) -> do putStrLn ("Error: "++show e++" reminder.") >> return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (DelDebt d)
 = case removeDebtSched d s of
     (Left e) -> do putStrLn ("Error: "++show e++" debt.") >> return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (DelGrocerie g)
 = case removeGrocerieSched g s of
    (Left e) -> do putStrLn ("Error: "++show e++" grocerie.") >> return (Just state)
    (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (UpdAddress n addr)
 = case updateSchedAddr n addr s of
     (Left e) -> do putStrLn ("Error: "++show e++" contact.") >> return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (UpdPhone n ph)
 = case updateSchedPh n ph s of
     (Left e) -> do putStrLn ("Error: "++show e++" contact.") >> return (Just state)
     (Right newS) -> return (Just (state {loadSched = (LS newS)}))

handleSchedule' state s (SearchContact n)
 = case searchContact n s of
         (Left e) -> do putStrLn ("Error: "++show e++" contact.") >> return (Just state)
         (Right c) -> do putStrLn ("Contact: "++ render (printContact c)) >>return (Just state)

handleSchedule' state s (AllContacts)
 = let n = contacts s
   in do putStrLn ("Contacts: \n"++ render (printListContact n))
         return (Just state)

handleSchedule' state s (Interval i)
  = let r = intervalDaysReminders i s
        (d1,d2) = intervalDays i
    in do putStrLn ("Reminders between "++render (printDateNoHour d1)++" - "++render (printDateNoHour d2)++"\n\n"++
                    render (printListReminder r)) >> return (Just state)

handleSchedule' state s (ThisWeek)
  = let r = thisWeekReminders s
    in do putStrLn ("For this week you have:\n\n"++
                    render (printListReminder r)++"\n")
          return (Just state)

handleSchedule' state s (ThisMonth)
  = let r = thisMonthReminders s
    in do putStrLn ("For this month you have:\n\n"++
                    render (printListReminder r)++"\n")
          return (Just state)

handleSchedule' state s (AllReminds)
 = let r = getRemindsSched s
   in do putStrLn ("All your reminds:\n\n"++
                  render (printListReminder r)++"\n")
         return (Just state)

handleSchedule' state s (AllMeetings)
  = let r = getMeetingsSched s
    in do putStrLn ("All your meetings:\n\n"++
                  render (printListReminder r)++"\n")
          return (Just state)

handleSchedule' state s (DebtsTo n)
 = let d = debtsTo n s
   in do putStrLn ("Debts to: "++n++"\n\n"++
                  render (printListDebt d)++"\n")
         return (Just state)

handleSchedule' state s (DebtsHigher n)
 = let d = debtsHigher n s
   in do putStrLn ("Debts higher than: "++show n++"\n\n"++
                  render (printListDebt d)++"\n")
         return (Just state)

handleSchedule' state s (AllDebts)
 = let d = debts s
   in do putStrLn ("Debts:\n"++ render (printListDebt d)++"\n")
         return (Just state)

handleSchedule' state s (AllGroceries)
 = do putStrLn ("Groceries:\n"++ render (printListGrocerie (groceries s))++"\n")
      return (Just state)

-- if there isn't a file in arguments then the state is the same.
-- the file is with .sched 
compileFile :: State -> String -> IO State
compileFile state@(State {..}) f =
  if f == ".sched" then return state     
  else do
    putStrLn ("Opening "++f)
    let f'= reverse(dropWhile isSpace (reverse f))
    x <- catch (readFile f')
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("Cannot open the file " ++ f' ++ ": " ++ err ++"\n")
                         return "")
    sched <- case parse (totParser schedP) f x of
              Left e -> putStrLn (show e) >> return Nothing
              Right s -> return (Just (LS s)) --return (Just (state {loadSched = s}))
    maybe (return state) (\s -> return (state {file=f,loadSched = s})) sched

getOwner :: LoadSched -> String
getOwner Null = "No Schedule loaded"
getOwner (LS sched) = owner sched

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "List of commands:Every command can be abbreviated with :c where 'c'\n"++
     "is the first character of the complete name\n\n" ++
     unlines (map (\ (ICmd c a _ d) -> 
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

opTxt :: [ScheduleCommand] -> String
opTxt cs
  = "List of operations to a Schedule:\n\n"++
    unlines (map (\ (SCmd c a d) -> let ct =  c ++ " " ++ (concat ((intersperse " ") a))
                                    in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d++"\n") cs)