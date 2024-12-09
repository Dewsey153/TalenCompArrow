module Main where

import ParseLib
import Algebra
import Model
import Interpreter
import Lexer
import Parser
import Data.Map

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive = undefined

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
  chars <- readFile "examples/AddInput.space"
  let space = fst (head (parse parseSpace chars))
  let printedSpace = printSpace space
  let space1 = fst (head (parse parseSpace chars))
  print $ printSpace space1

  -- chars <- readFile "examples/Add.arrow"
  -- putStrLn "Input program:"
  -- putStrLn ""
  -- putStrLn chars
  -- putStrLn ""
  -- let tokens = alexScanTokens chars
  -- putStrLn "Tokens:"
  -- putStrLn ""
  -- print tokens
  -- let arr = parser tokens
  -- putStrLn "Parsed program:"
  -- putStrLn ""
  -- print arr

