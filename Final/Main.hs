{-# OPTIONS -XRecordWildCards #-}
module Main where

import System.Console.Readline
import System.Environment
--import Control.Exception (catch,IOException)
import System.Console.ANSI (clearScreen,setCursorPosition)
--import Control.Monad.Except

import Types
import Commands

---------------------
--- Interpreter
---------------------

-- Assume that only can get one argument, that is the name of a file to load
main :: IO ()
main = do arg <- getArgs
          case length arg of 
            0 -> readevalprint "" (State {file="",loadSched = Null})
            1 -> readevalprint (head arg) (State {file="",loadSched = Null})
            _ -> putStrLn "Error: too many arguments"

-- ioExceptionCatcher :: IOException -> IO (Maybe a)
-- ioExceptionCatcher _ = return Nothing

iname = "Scheduling interface"
iprompt = "Sched> "



--  read-eval-print loop
readevalprint :: String -> State -> IO ()
readevalprint args state =
  let rec state =
        do case loadSched state of
            Null -> do line <- readline iprompt
                       case line of
                        Nothing   ->  return ()
                        Just ""   ->  rec state
                        Just x    ->
                          do addHistory x
                             c  <- interpretCommand x
                             state' <- handleCommand state c
                             maybe (return ()) rec state'
            (LS s) -> do line <- readline (owner s++"> ")
                         case line of
                          Nothing   ->  return ()
                          Just ""   ->  rec state
                          Just x    ->
                            do addHistory x
                               c  <- interpretCommand x
                               state' <- handleCommand state c
                               maybe (return ()) rec state'
  in
    do setCursorPosition 0 0
       clearScreen
       state' <- compileFile state (args++".sched")
       putStrLn (iname ++ ".\n" ++ "Write :? to print help.")
       rec state'


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
