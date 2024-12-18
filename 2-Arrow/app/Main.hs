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

-- Run the program in one go, only returning the final state
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env state =
   let
      -- evaluate next step
      nextStep = step env state
   in
      case nextStep of
         Ok newState
         -- recurs
            -> batch env newState
         -- recursion is done. Return final state.
         Done space pos heading 
            -> (space, pos, heading)
         -- Throw an error with the appropiate message.
         Fail errorMessage
            -> error $ makeErrorMessage errorMessage

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
   putStrLn ""
   space          <- getSpace
   env            <- getEnvironment
   startPos       <- getStartPosition
   initialHeading <- getInitialHeading
   mode           <- getMode

   let startingState = ArrowState space startPos initialHeading (Cmds [CRule (IIdent "start")])

   -- interactive mode
   if mode == 'i'
      then
         interactive env startingState
      -- batch mode
      else if mode == 'b' then
         let result = batch env startingState
         in putStrLn (printResult result ++ doneMessage)
      -- neither
      else error "Input character for mode selection is invalid."

-- Ask for the path to the file containing the space, parse
-- the Space and return it.
getSpace :: IO Space
getSpace = 
   do
      putStrLn "Give filepath to space relative to the cabal file."
      spacePath   <- getLine
      fileContent <- readFile spacePath
      return $ fst (head (parse parseSpace fileContent))

-- Ask for the file path to the program, parse the program.
getEnvironment :: IO Environment
getEnvironment = 
   do
      putStrLn "Give filepath to the program relative to the cabal file."
      envPath <- getLine
      fileContent <- readFile envPath
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

getMode :: IO Char
getMode =
   do
      putStrLn "Do you want to run interactive (i) or batch mode (b)?"
      getChar

-- Given a Step, return the message which interactive will display 
-- based on the step
message :: Step -> String
message (Done {}) 
   = doneMessage
      
message (Ok (ArrowState space pos heading stack)) 
   = printResult (space, pos, heading)
   ++ ".\r\n\r\nStack contains:\r\n" 
   ++ show stack 
   ++ "\r\n\r\nPress Enter to continue."
      
message (Fail errorMessage) 
   = makeErrorMessage errorMessage

-- Make a pretty string containing info on the space, position an heading.
printResult :: (Space, Pos, Heading) -> String
printResult (space, pos, heading) =
   printSpace space 
   ++ "Arrow position: " 
   ++ show pos 
   ++ "\r\nCurrent heading: " 
   ++ show heading

-- Message for when the program has terminated
doneMessage :: String
doneMessage = "\r\nThe program has terminated without any errors."

-- Add a fun "Runtime error: " string in front of the actual error message,
-- so the user knows for sure that an error occured.
makeErrorMessage :: String -> String
makeErrorMessage errorContent = "Runtime error: " ++ errorContent