module Algebra where

import Model


-- Exercise 5
type Algebra program rule cmds cmd dir alts alt ident pat = 
    (
        [rule] -> program,
        ident -> cmds -> rule,
        [cmd] -> cmds,
        CmdAlgebra cmd dir alts ident,
        DirAlgebra dir,
        [alt] -> alts,
        pat -> cmds -> alt,
        String -> ident,
        PatAlgebra pat
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

ruleNamedStart :: Algebra Bool Bool () () () () () Bool ()
ruleNamedStart = 
    (
        or,
        const,
        const (),
        cmdRuleNamedStart,
        dirRuleNamedStart,
        const (),
        \_ _ -> (),
        (== "start"),
        patRuleNamedStart
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
        and,
        \_ cs -> cs,
        and,
        cmdNPMF,
        dirNPMF,
        \pats -> 
                elem PUnderscore pats 
            ||  (   one (== PEmpty)     pats 
                &&  one (== PLambda)    pats 
                &&  one (== PDebris)    pats 
                &&  one (== PAsteroid)  pats 
                &&  one (== PBoundary)  pats),
        const,
        const (),
        patNPMF
    )
    where
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