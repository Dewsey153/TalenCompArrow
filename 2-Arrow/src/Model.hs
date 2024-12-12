{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model where

-- Exercise 1
data Token = 
      Token
    | TArrow 
    | TDot
    | TComma
    | TGo
    | TTake
    | TMark
    | TNothing
    | TTurn
    | TCase
    | TOf
    | TEnd
    | TLeft
    | TRight
    | TFront
    | TSemicolon
    | TEmpty
    | TLambda
    | TDebris
    | TAsteroid
    | TBoundary
    | TUnderscore
    | TIdent String
     deriving Show

-- Exercise 2
data Program = 
  Program [Rule]
    deriving Show

data Rule = Rule IIdent Cmds
    deriving Show

data Cmds = Cmds [Cmd]
    deriving Show

data Cmd = 
    CGo 
  | CTake 
  | CMark 
  | CNothing
  | CTurn Dir
  | CCaseOfEnd Dir Alts
  | CRule IIdent
    deriving Show

data Dir =
      DLeft
    | DRight
    | DFront
  deriving Show

data Alts = Alts [Alt]
  deriving Show

data Alt = Alt Pat Cmds
  deriving Show

data IIdent = IIdent String
  deriving Show

data Pat = 
      PEmpty
    | PLambda
    | PDebris
    | PAsteroid
    | PBoundary
    | PUnderscore
  deriving (Show, Eq)
