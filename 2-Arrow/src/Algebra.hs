module Algebra where

import Model

import Data.HashSet ( HashSet )
import qualified Data.HashSet as HashSet

type Env = HashSet String

-- Exercise 5
type Algebra program rule cmds cmd dir alts alt ident pat =
    (
        [rule] -> program,              -- program function
        ident -> cmds -> rule,          -- rule function
        [cmd] -> cmds,                  -- commands function
        CmdAlgebra cmd dir alts ident,  -- algebra for a single command
        DirAlgebra dir,                 -- algebra for a single direction
        [alt] -> alts,                  -- alts function
        pat -> cmds -> alt,             -- alt function
        String -> ident,                -- identifier function
        PatAlgebra pat                  -- algebra for pattern match
    )

type CmdAlgebra cmd dir alts ident =
    (
        cmd,                -- go function
        cmd,                -- take function
        cmd,                -- mark function
        cmd,                -- nothing function
        dir -> cmd,         -- turn function
        dir -> alts -> cmd, -- case of end function
        ident -> cmd        -- rule function
    )

type DirAlgebra dir =
    (
        dir,
        dir,
        dir
    )

type PatAlgebra pat =
    (
        pat,
        pat,
        pat,
        pat,
        pat,
        pat
    )

fold :: Algebra program rule cmds cmdAlg dirAlg alts alt ident patAlg
    -> Program -> program
fold (program, rule, cmds, cmdAlg, dirAlg, alts, alt, ident, patAlg)
    =
    fProgram
    where
        fProgram (Program rules) = program (map fRule rules)
        fRule (Rule rIdent rCmds) = rule (fIdent rIdent) (fCmds rCmds)
        fCmds (Cmds commands) = cmds (map fCmd commands)
        fCmd = foldCmd cmdAlg
        fDir = foldDir dirAlg
        fAlts (Alts as) = alts (map fAlt as)
        fAlt (Alt p c) = alt (fPat p) (fCmds c)
        fIdent (IIdent s) = ident s
        fPat = foldPat patAlg

        --foldCmd :: CmdAlgebra cmdAlg dirAlg alts ident -> Cmd -> cmdAlg
        foldCmd (go, take, mark, nothing, turn, caseOfEnd, rule)
            =
                fCmd'
            where
                fCmd' CGo = go
                fCmd' CTake = take
                fCmd' CMark = mark
                fCmd' CNothing = nothing
                fCmd' (CTurn dir) = turn (fDir dir)
                fCmd' (CCaseOfEnd dir alts) = caseOfEnd (fDir dir) (fAlts alts)
                fCmd' (CRule iident) = rule (fIdent iident)

        foldDir :: DirAlgebra dir -> Dir -> dir
        foldDir (left, right, front)
            =
                fDir'
            where
                fDir' DLeft = left
                fDir' DRight = right
                fDir' DFront = front

        foldPat :: PatAlgebra pat -> Pat -> pat
        foldPat (empty, lambda, debris, asteroid, boundary, underscore)
            =
                fPat'
            where
                fPat' PEmpty = empty
                fPat' PLambda = lambda
                fPat' PDebris = debris
                fPat' PAsteroid = asteroid
                fPat' PBoundary = boundary
                fPat' PUnderscore = underscore

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined

-- Algebra which checks whether there is at least one rule named "start".
ruleNamedStart :: Algebra Bool Bool () () () () () Bool ()
ruleNamedStart =
    (
        or,                 -- For the input list of booleans, one for each rule,
                            -- at least one must be true to return a 
                            -- True boolean.
        const,              -- for rule function, input is a Bool stating whether 
                            -- the title is "start", this must be passed on
        const (),           -- empty function
        cmdRuleNamedStart,  -- empty algebra
        dirRuleNamedStart,  -- empty algebra
        const (),           -- empty function
        \_ _ -> (),         -- empty function
        (== "start"),       -- for identifier, check whether the string is "start"
        patRuleNamedStart   -- empty algebra
    )
    where
        cmdRuleNamedStart :: CmdAlgebra () () () Bool
        cmdRuleNamedStart =
            (
                (),
                (),
                (),
                (),
                const (),
                \_ _ -> (),
                const ()
            )
        dirRuleNamedStart :: DirAlgebra ()
        dirRuleNamedStart =
            (
                (),
                (),
                ()
            )
        patRuleNamedStart :: PatAlgebra ()
        patRuleNamedStart =
            (
                (),
                (),
                (),
                (),
                (),
                ()
            )

noPatternMatchFailure :: Algebra Bool Bool Bool Bool () Bool Pat () Pat
noPatternMatchFailure =
    (
        and,            -- All pattern match cases must be fully covered
        \_ cs -> cs,    -- Just pass along cmds bool
        and,            -- All pattern match cases in cmds must be fully covered
        cmdNPMF,        -- Algebra checks whether all pattern match cases are fully covered
        dirNPMF,        -- Empty algebra
        \pats ->        -- For pattern matchings, Underscore must be present 
                        -- exactly once, or all other patterns must be 
                        -- present exactly once
                one (== PUnderscore) pats
            ||  (   one (== PEmpty)     pats
                &&  one (== PLambda)    pats
                &&  one (== PDebris)    pats
                &&  one (== PAsteroid)  pats
                &&  one (== PBoundary)  pats),
        const,          -- Just pass on the pattern matching, to be used by pats function
        const (),       -- Empty function
        patNPMF         -- Identity algebra
    )
    where
        -- If no pattern match is present, there is no problem with pattern 
        -- matching, so return True. Else, pass on the alts parameter,
        -- which indicates whether the pattern match is complete.
        cmdNPMF =
            (
                True,
                True,
                True,
                True,
                const True,
                \_ alts -> alts,
                const True
            )

        dirNPMF =
            (
                (),
                (),
                ()
            )

        -- Identity algeba consists of all constructors
        patNPMF =
            (
                PEmpty,
                PLambda,
                PDebris,
                PAsteroid,
                PBoundary,
                PUnderscore
            )

-- Algebra for getting environment of all rule names
envAlg :: Algebra Env String () () () () () String ()
envAlg = 
    (
        foldl (flip HashSet.insert) HashSet.empty,
        const,
        const (),
        cmdEnv,
        dirEnv,
        const (),
        \_ _ -> (),
        id,
        patEnv
    )
    where
        -- empty algebra
        cmdEnv = 
            (
                (),
                (),
                (),
                (),
                const (),
                \_ _ -> (),
                const ()
            )
        
        -- empty algebra
        dirEnv =
            (
                (),
                (),
                ()
            )

        -- empty algebra
        patEnv =
            (
                (),
                (),
                (),
                (),
                (),
                ()
            )

-- environment contains all rule names
noCallsToUndefinedRules :: Algebra Bool (Env -> Env -> (Env, Bool)) (Env -> Bool) (Env -> Bool) () (Env -> Bool) (Env -> Bool) String ()
noCallsToUndefinedRules =
    (
        -- all rules must not call any undefined rule
        undefined,
        -- ident :: String, cmds :: Env -> Bool, env :: Env
        -- Insert identifier into environment and fill it in to the cmds function
        \ident cmds cenv fenv -> (HashSet.insert ident cenv, cmds fenv),
        -- cmdList :: [Env -> Bool], env :: Env. 
        -- Return whether the list returns all true bools when given the environment
        \cmdList env -> all (\f -> f env) cmdList,
        -- cmdNCTUR ::  Env -> Bool
        -- True iff the command does not call an undefined rule
        cmdNCTUR,
        -- empty algebra
        dirNCTUR,
        -- altlist :: [Env -> Bool]
        \altList env -> all (\f -> f env) altList,
        -- pat :: (), cmds :: Env -> Bool, env :: Env
        -- cmds :: Env -> Bool, env :: Env
        -- For each command in cmds, True iff none call undefined rule
        \_ cmds env -> cmds env,
        -- Just pass along the string identifier
        id,
        -- Empty algebra
        patNCTUR
    )
    where
        -- Returns true iff there is no function call to an undefined function in the command
        cmdNCTUR :: CmdAlgebra (Env -> Bool) () (Env -> Bool) String
        cmdNCTUR = 
            (
                const True,
                const True,
                const True,
                const True,
                \_ _ -> True,
                \_ as env -> as env,
                HashSet.member
            )

        -- empty algebra
        dirNCTUR :: DirAlgebra ()
        dirNCTUR = 
            (
                (),
                (),
                ()
            )

        -- empty algebra
        patNCTUR :: PatAlgebra ()    
        patNCTUR = 
            (
                (),
                (),
                (),
                (),
                (),
                ()
            )

-- Return whether exactly one element of the list 
-- satisfies the predicate.
one :: (a -> Bool) -> [a] -> Bool
one p = (== 1) . length . filter p