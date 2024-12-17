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
import Data.List ( find )


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving Eq
data Cardinals = North | East | South | West
  deriving (Eq, Show)

-- | (dir == right) = oldFacing + 1
-- | (dir == left) = oldFacing - 1


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

isOk :: Step -> Bool
isOk (Ok _) = True
isOk _      = False

getState :: Step -> ArrowState
getState (Ok state) = state

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

-- Do one step of the simulation, popping one argument of the stack and
-- interpreting it.
step :: Environment -> ArrowState -> Step
step env state@(ArrowState space position heading stack) =
  let
    pop = popCommand stack
    noCommandLeft = isNothing pop
    top = fst (fromJust pop)
    poppedState = ArrowState space position heading (snd (fromJust pop))
  in
    -- Return Done if there are no commands left
    if noCommandLeft then
      Done space position heading
    else
    -- Else pattern match on the popped command.
        case top of
          CGo -> stepGo poppedState
          CTake -> stepTake poppedState
          CMark -> stepMark poppedState
          CNothing -> Ok poppedState
          CTurn dir -> stepTurn poppedState dir
          CCaseOfEnd dir alts -> stepCase poppedState dir alts
          CRule ident -> stepRule env poppedState ident

popCommand :: Stack -> Maybe (Cmd, Stack)
popCommand (Cmds [])        = Nothing
popCommand (Cmds (c : cs))  = Just (c, Cmds cs)

--Moves arrow if facing position is empty, lambda or debris
stepGo :: ArrowState -> Step
stepGo state@(ArrowState space position@(y,x) heading coms) 
  | exists = Ok (ArrowState space (nextPosition heading position) heading coms)
  | otherwise = Ok state
  where
    exists = 
      getContent space position `elem` [Empty, Lambda, Debris]

--Takes debris or lambda from position
stepTake :: ArrowState -> Step
stepTake state@(ArrowState space position heading coms) 
  | filled = Ok (ArrowState newSpace position heading coms)
  | otherwise = Ok state
  where
    filled = (space L.! position) `elem` [Lambda, Debris]
    newSpace = L.insert position Empty space

--Always replaces position with lambda as specified
stepMark :: ArrowState -> Step
stepMark state@(ArrowState space position heading coms) = 
  Ok (ArrowState newSpace position heading coms)
  where newSpace = L.insert position Lambda space

--Turns Arrow in specified direction
stepTurn :: ArrowState -> Dir -> Step
stepTurn state@(ArrowState space position heading coms) d = 
  Ok (ArrowState space position (newHeading heading d) coms)
    
-- Make sensor reading and push commands when pattern matches
-- TODO: must return fail if no pattern matches
stepCase :: ArrowState -> Dir -> Alts -> Step
stepCase as@(ArrowState space position heading stack) dir alts = 
  let
    scanPosition          = getNeighbouringPosition position heading dir
    contentOnScanPosition = getContent space scanPosition
    newCommands           = findPatternMatch alts contentOnScanPosition
  in
    Ok $ ArrowState space position heading (pushCommands newCommands stack) 

-- Given the alternatives some Contents, get the commands associated with the
-- content from the pattern matching.
-- The catch-all pattern will catch all patterns, so any pattern matches after
-- it will be ignored.
findPatternMatch :: Alts -> Contents -> Cmds
findPatternMatch (Alts alts) content = 
  let 
    Alt _ cmds = fromJust 
      $ find (\(Alt pat _) -> 
            pat == contentsToPat content 
        ||  pat == PUnderscore) 
        alts
  in cmds

-- Transform the Contents datatype in corresponding Pat data type
contentsToPat :: Contents -> Pat
contentsToPat Empty     = PEmpty
contentsToPat Lambda    = PLambda
contentsToPat Debris    = PDebris
contentsToPat Asteroid  = PAsteroid
contentsToPat Boundary  = PBoundary

-- Add the commands associated with a Rule to the stack.
-- This gives an error when an undefined Rule is called,
-- which is fine, because we check whether no undefined
-- Rules are called in checkProgram.
-- TODO: must return fail if the rule is undefined
stepRule :: Environment -> ArrowState -> Ident -> Step
stepRule rules state@(ArrowState space position heading stack) i = 
  Ok (ArrowState space position heading (pushCommands newCommands stack))
  where
    newCommands = rules L.! i

-- Pushes the commands from the first argument on top of the stack
pushCommands :: Commands -> Stack -> Stack
pushCommands (Cmds commands) (Cmds stack) = Cmds (commands ++ stack) 

-- Get the content on the position in the space.
-- Will return Boundary if the position is outside of the space.
getContent :: Space -> Pos -> Contents
getContent space pos
  | L.member pos space = space L.! pos
  | otherwise = Boundary

-- Given the current position, the current heading and a direction 
-- (left, front or right), returns the position to the left, front
--  or right of our beloved Arrow.
getNeighbouringPosition :: Pos -> Heading -> Dir -> Pos
getNeighbouringPosition pos@(row, column) heading dir =
  let
    altHeading = newHeading heading dir
  in
    nextPosition altHeading pos

-- Given a heading and a position, return the position you would walk to if 
-- possible.
nextPosition :: Heading -> Pos -> Pos
nextPosition heading (row, column) 
  | heading == North  = (row-1, column)
  | heading == South  = (row+1, column)
  | heading == West   = (row,   column-1)
  | heading == East   = (row,   column+1)

-- Given a heading and a direction to turn to, return the new heading
newHeading :: Heading -> Dir -> Heading
newHeading heading d | d == DLeft   = leftFrom heading
                     | d == DRight  = rightFrom heading
                     | otherwise    = heading

-- Get the heading to the left of the input heading
leftFrom :: Heading -> Heading
leftFrom heading  | heading == North = West
                  | heading == West = South
                  | heading == South = East
                  | heading == East = North

-- Get the heading to the right of the input heading
rightFrom :: Heading -> Heading
rightFrom heading | heading == North = East
                  | heading == West = North
                  | heading == South = West
                  | heading == East = South