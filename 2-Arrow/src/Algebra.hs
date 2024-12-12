module Algebra where

import Model


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
        cmd,
        cmd,
        cmd,
        cmd,
        dir -> cmd,
        dir -> alts -> cmd,
        ident -> cmd
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
        
        foldCmd (go, take, mark, nothing, turn, caseOfEnd, ident) 
            = 
                fCmd'
            where
                fCmd' CGo = go
                fCmd' CTake = take
                fCmd' CMark = mark
                fCmd' CNothing = nothing
                fCmd' (CTurn dir) = turn (fDir dir)
                fCmd' (CCaseOfEnd dir alts) = caseOfEnd (fDir dir) (fAlts alts)
                fCmd' (CIdent iident) = ident (fIdent iident)

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
                fpat' PUnderscore = underscore

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

-- Return whether exactly one element of the list 
-- satisfies the predicate.
one :: (a -> Bool) -> [a] -> Bool
one p = (== 1) . length . filter p