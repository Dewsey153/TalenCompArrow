module Main where

import ParseLib
import Algebra
import Model
import Interpreter
import Lexer
import Parser
import Data.Map
import qualified Data.HashSet as HashSet

-- Exercise 11

-- The interactive driver. After each step, print the board and
-- ask the user for confimation before continuing.
interactive :: Environment -> ArrowState -> IO ()
interactive env state = interactive' (Ok state) env state
   where
      interactive' :: Step -> Environment -> ArrowState -> IO ()
      interactive' oldStep env oldState =
         let
            newStep = step env oldState
         in
            do
               putStrLn $ message newStep
               getChar
               if isOk newStep 
                  then 
                     interactive' newStep env (getState newStep)
                  else
                     return ()

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
   putStrLn "\r\nGive filepath to space relative to the cabal file."
   spacePath <- getLine
   space <- readSpace spacePath
   putStrLn "Give filepath to the program relative to the cabal file."
   envPath <- getLine
   env <- readEnvironment envPath
   startPos <- getStartPosition
   initialHeading <- getInitialHeading
   interactive env (ArrowState space startPos initialHeading (Cmds [CRule (IIdent "start")]))

-- Provided the path to the file containing the space, parse
-- the Space and return it.
readSpace :: FilePath -> IO Space
readSpace path = 
   do
      fileContent <- readFile path
      return $ fst (head (parse parseSpace fileContent))

-- Given the file path to the program, parse the program.
readEnvironment :: FilePath -> IO Environment
readEnvironment path = 
   do
      fileContent <- readFile path
      return $ toEnvironment fileContent

-- Ask the user to provide a starting position
getStartPosition :: IO Pos
getStartPosition = 
   do
      putStrLn "Please provide starting position in the following format: [row]\\r\\n[column]\\r\\n"
      rowString <- getLine
      columnString <- getLine
      let row = read rowString
      let column = read columnString
      return (row, column)

-- Ask the user to provide a starting heading for Arrow
getInitialHeading :: IO Heading
getInitialHeading = 
   do
      putStrLn "Please provide the initial heading: North (n), East (e), South (s) or West (w)."
      stringToHeading <$> getLine

   where 
      stringToHeading :: String -> Heading
      stringToHeading "n" = North
      stringToHeading "e" = East
      stringToHeading "s" = South
      stringToHeading "w" = West

message :: Step -> String
message (Done space pos heading) 
   = printSpace space ++ "\r\nThe program has terminated without any errors."
      
message (Ok (ArrowState space pos heading stack)) 
   = printSpace space ++ "Arrow is on " ++ show pos ++ ". Stack contains" ++ show stack ++ "Current heading " ++ show heading ++ "\r\nPress any key to continue."
      
message (Fail errorMessage) 
   = errorMessage