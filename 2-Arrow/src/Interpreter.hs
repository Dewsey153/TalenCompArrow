module Interpreter where

import ParseLib

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra
import Data.Maybe


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace space = '(' : show (xRows space) ++ "," ++ show (xColumns space) ++ ")\r\n" 
  ++ positionsY space 0 
  where
    positionsY :: Space -> Int -> String
    positionsY s j | j <= xRows s = positionsX s 0 j ++ "\r\n" ++  positionsY s (j+1)
                   | otherwise = ""
    positionsX :: Space -> Int -> Int -> String
    positionsX s i j | i <= xColumns s = printContent (fromJust (L.lookup (j,i) s)) : positionsX s (i+1) j
                     | otherwise = ""


    maxKey :: Space -> Pos
    maxKey s = fst (L.findMax s)
    xRows :: Space -> Int
    xRows s = fst $ maxKey s
    xColumns :: Space -> Int
    xColumns s = snd $ maxKey s

    -- Get a char based on the input Contents
    printContent :: Contents -> Char
    printContent Empty    = '.'
    printContent Lambda   = '\\'
    printContent Debris   = '%'
    printContent Asteroid = 'O'
    printContent Boundary = '#'


-- These three should be defined by you
type Ident = IIdent
type Commands = Cmds
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
-- Parse the string into a new environment
toEnvironment :: String -> Environment
toEnvironment input = 
  let
    p = parser (alexScanTokens input)
  in
    if checkProgram p then programToEnvironment p else L.empty

-- Convert a Program into an Environment by placing each rule in the Map
programToEnvironment :: Program -> Environment
programToEnvironment (Program rules) = foldl addToEnvironment L.empty rules
  where
    addToEnvironment :: Environment -> Rule -> Environment
    addToEnvironment cenv (Rule ident cmds) = L.insert ident cmds cenv

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


