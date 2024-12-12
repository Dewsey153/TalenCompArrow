{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  x        { Token }
  '->'     { TArrow }
  '.'      { TDot }
  ','      { TComma }
  go       { TGo }
  take     { TTake }
  mark     { TMark }
  nothing  { TNothing }
  turn     { TTurn }
  case     { TCase }
  of       { TOf }
  end      { TEnd }
  left     { TLeft }
  right    { TRight }
  front    { TFront }
  ';'      { TSemicolon }
  Empty    { TEmpty }
  Lambda   { TLambda }
  Debris   { TDebris }
  Asteroid { TAsteroid }
  Boundary { TBoundary }
  '_'      { TUnderscore }
  Ident    { TIdent $$ }
%%

Program : Rules           { Program (reverse $1) }

Rules : {- empty -}       { [] }
  | Rules Rule            { $2 : $1 }

Rule : IIdent '->' Cmds '.'{Rule $1 $3}

Cmds : Cmds1              { Cmds (reverse $1) }

Cmds1 : {- Empty -}       { [] }
  | Cmd                   { [$1] }
  | Cmds1 ',' Cmd         { $3 : $1 }

Cmd : go                  { CGo }
  | take                  { CTake }
  | mark                  { CMark }
  | nothing               { CNothing }
  | turn Dir              { CTurn $2 }
  | case Dir of Alts end  { CCaseOfEnd $2 $4 }
  | IIdent                { CRule $1 }

IIdent : Ident            {IIdent $1}

Dir : left                {DLeft}
     |right               {DRight}
     |front               {DFront}

Alts : Alts1              { Alts (reverse $1) }

Alts1 : {- empty -}       { [] }
  | Alt                   { [$1] }
  | Alts1 ';' Alt         { $3 : $1 }

Alt : Pat '->' Cmds       {Alt $1 $3}

Pat : Empty               {PEmpty}
     |Lambda              {PLambda}
     |Debris              {PDebris}
     |Asteroid            {PAsteroid}
     |Boundary            {PBoundary}
     |'_'                 {PUnderscore}

{

happyError _ = error "parse error"

}