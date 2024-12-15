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
import Data.HashMap.Internal.Array (new)
import Control.Arrow (ArrowChoice(right))


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving Eq
data Cardinals = North | South | West | East
  deriving Eq


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
type Heading = Cardinals

-- Maps function names to command stacks
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
step env state@(ArrowState space position heading (Cmds commands)) =
  let
    noCommandLeft = null commands
    top = head commands
  in
    -- Return Done if there are no commands left
    if noCommandLeft then
      Done space position heading
    else
        case top of
          CGo -> stepGo state
          CTake -> stepTake state
          CMark -> stepMark state
          CNothing -> Ok state
          CTurn dir -> stepTurn state dir
          CCaseOfEnd dir alts -> stepCase state dir
          CRule ident -> stepRule env state ident

--Moves arrow if facing position is empty, lambda or debris
stepGo :: ArrowState -> Step
stepGo state@(ArrowState space position@(y,x) heading coms) | exists = Ok (ArrowState space facingPosition heading coms)
                                                            | otherwise = Fail "Not real position"
  where
    exists = L.member facingPosition space && ((space L.! facingPosition) `elem` [Empty, Lambda, Debris])
    facingPosition :: Pos
    facingPosition | heading == North = (y-1,x)
                   | heading == South = (y+1,x)
                   | heading == West = (y, x-1)
                   | heading == East = (y, x)

--Takes debris or lambda from position
stepTake :: ArrowState -> Step
stepTake state@(ArrowState space position heading coms) | filled = Ok (ArrowState newSpace position heading coms)
                                                        | otherwise = Fail "Empty position!"
  where
    filled = (space L.! position) `elem` [Lambda, Debris]
    newSpace = L.insert position Empty space

--Always replaces position with lambda as specified
stepMark :: ArrowState -> Step
stepMark state@(ArrowState space position heading coms) = Ok (ArrowState newSpace position heading coms)
  where newSpace = L.insert position Lambda space

--Turns Arrow in specified direction
stepTurn :: ArrowState -> Dir -> Step
stepTurn state@(ArrowState space position heading coms) d = Ok (ArrowState space position newHeading coms)
  where
    newHeading :: Heading
    newHeading | d == DLeft = leftFrom heading
               | d == DRight = rightFrom heading
               | otherwise = heading
    leftFrom heading | heading == North = West
                     | heading == West = South
                     | heading == South = East
                     | heading == East = North
    rightFrom heading | heading == North = East
                      | heading == West = North
                      | heading == South = West
                      | heading == East = South

stepCase :: ArrowState -> Dir -> Step
stepCase d as = undefined

-- Produces error when no rule is found, need to discuss
stepRule :: Environment -> ArrowState -> Ident -> Step
stepRule rules state@(ArrowState space position heading (Cmds coms)) i = Ok (ArrowState space position heading (Cmds (stackedComs newCommands)))
  where
    stackedComs (Cmds newCommands) = newCommands ++ coms
    newCommands = rules L.! i