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
interactive = undefined

-- 
-- interactive :: Step -> Environment -> ArrowState -> IO ()
-- interactive oldStep env oldState =
--    let
--       newStep = step env oldState
--    in
--       print $ message newStep
--       getChar
--       return $ case newStep of
--         Ok newState
--           -> interactive newStep env newState
--         _ 
--           -> () 

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
   chars <- readFile "examples/Add.arrow"
   putStrLn "Input program:"
   putStrLn ""
   putStrLn chars
   putStrLn ""
   let tokens = alexScanTokens chars
   putStrLn "Tokens:"
   putStrLn ""
   print tokens
   let arr = parser tokens
   putStrLn "Parsed program:"
   putStrLn ""
   print arr

-- Provided the path to the file containing the space, parse
-- the Space and return it.
readSpace :: FilePath -> IO Space

-- Given the file path to the program, parse the program.
readProgram :: FilePath -> IO Program

-- Ask the user to provide a starting position
getStartPosition :: IO Position

-- Ask the user to provide a starting heading for Arrow
getInitialHeading :: IO Heading

message :: Step -> String
message (Done space pos heading) 
   = printSpace space ++ "\r\nThe program has terminated without any errors."
      
message (Ok (ArrowState space pos heading stack)) 
   = printSpace space ++ "\r\nPress any key to continue."
      
message (Fail errorMessage) 
   = errorMessage