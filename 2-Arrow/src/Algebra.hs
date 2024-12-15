module Algebra where

import Model

import Data.HashSet ( HashSet )
import qualified Data.HashSet as HashSet
import qualified Data.Bifunctor as BF

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
        dir,                -- left
        dir,                -- right
        dir                 -- front
    )

type PatAlgebra pat =
    (
        pat,                -- empty
        pat,                -- lambda
        pat,                -- debris
        pat,                -- asteroid
        pat,                -- boundary
        pat                 -- underscore
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

-- Checks whether the input program is valid.
checkProgram :: Program -> Bool
checkProgram p = 
    let (noMatchFail, ((noDoubles, env), f)) = fold (combine noPatternMatchFailure (combine envAlg envCheckAlg)) p
    in noMatchFail && noDoubles && f env

-- Returns whether all cases are handled when using pattern matching.
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

-- Algebra for getting environment of all rule names.
-- The output boolean indicates whether one or more rules are defined multiple times.
envAlg :: Algebra (Bool, Env) String () () () () () String ()
envAlg =
    (
        \ss -> let set = foldl (flip HashSet.insert) HashSet.empty ss in (length set == length ss, set),
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

-- Given an environment, checks whether there are no calls to undefined rules 
-- in the program given to fold. Also checks whether there is a rule named "start".
envCheckAlg :: Algebra (Env -> Bool) (Env -> Bool) (Env -> Bool) (Env -> Bool) () (Env -> Bool) (Env -> Bool) String ()
envCheckAlg =
    (
        -- rules :: [Bool]
        -- Bool indicates whether Rule calls another undefined Rule
        \rules env -> HashSet.member "start" env && all (\f -> f env) rules,
        -- ident :: (), cmds :: Bool
        -- cmds indicates whether no command call undefined rules
        \ident cmds env -> cmds env,
        -- cmdList :: [Bool]
        -- Indicates whether all command do not call undefined rule
        \cmdList env -> all (\f -> f env) cmdList,
        envCheckCmd,
        envCheckDir,
        -- altList :: [Bool]
        -- Indicate for each alt whether it does not contain call to undefined 
        -- rule
        \altList env -> all (\f -> f env) altList,
        -- pat :: (), cmds :: Bool
        \pat cmds env -> cmds env,
        id,
        envCheckPat
    )
    where
        envCheckCmd :: CmdAlgebra (Env -> Bool) () (Env -> Bool) String
        envCheckCmd =
            (
                -- If no rule is called, no undefined rule is called, so return True.
                const True,
                const True,
                const True,
                const True,
                \dir env -> True,
                \dir alts env -> alts env,

                -- If a rule is called, check if the rule name is in the HashSet of defined rule names.
                HashSet.member
            )

        envCheckDir :: DirAlgebra ()
        envCheckDir =
            (
                (),
                (),
                ()
            )

        envCheckPat :: PatAlgebra ()
        envCheckPat =
            (
                (),
                (),
                (),
                (),
                (),
                ()
            )

-- combine two algebras, so you need a single fold to evaluate both.
-- The result is a tuple of the fold result.
combine :: Algebra program1 rule1 cmds1 cmd1 dir1 alts1 alt1 ident1 pat1
        -> Algebra program2 rule2 cmds2 cmd2 dir2 alts2 alt2 ident2 pat2
        -> Algebra (program1, program2) (rule1, rule2) (cmds1, cmds2) (cmd1, cmd2) (dir1, dir2) (alts1, alts2) (alt1, alt2) (ident1, ident2) (pat1, pat2)
combine
    (
        program1,
        rule1,
        cmds1,
        cmdAlg1,
        dirAlg1,
        alts1,
        alt1,
        ident1,
        patAlg1
    )
    (
        program2,
        rule2,
        cmds2,
        cmdAlg2,
        dirAlg2,
        alts2,
        alt2,
        ident2,
        patAlg2
    ) =
    (
        \rules -> (program1 (map fst rules), program2 (map snd rules)),
        \ident cmds -> BF.bimap (rule1 (fst ident)) (rule2 (snd ident)) cmds,
        \cmds -> (cmds1 (map fst cmds), cmds2 (map snd cmds)),
        combineCmd cmdAlg1 cmdAlg2,
        combineDir dirAlg1 dirAlg2,
        \alts -> (alts1 (map fst alts), alts2 (map snd alts)),
        \pat cmds -> BF.bimap (alt1 (fst pat)) (alt2 (snd pat)) cmds,
        \s -> (ident1 s, ident2 s),
        combinePat patAlg1 patAlg2
    )
    where
        combineCmd :: CmdAlgebra cmd1 dir1 alts1 ident1
            -> CmdAlgebra cmd2 dir2 alts2 ident2
            -> CmdAlgebra (cmd1, cmd2) (dir1, dir2) (alts1, alts2) (ident1, ident2)
        combineCmd
            (
                go1,
                take1,
                mark1,
                nothing1,
                turn1,
                case1,
                crule1
            )
            (
                go2,
                take2,
                mark2,
                nothing2,
                turn2,
                case2,
                crule2
            )
            =
            (
                (go1, go2),
                (take1, take2),
                (mark1, mark2),
                (nothing1, nothing2),
                BF.bimap turn1 turn2,
                \dir -> BF.bimap (case1 (fst dir)) (case2 (snd dir)),
                BF.bimap crule1 crule2
            )

        combineDir :: DirAlgebra dir1 -> DirAlgebra dir2 -> DirAlgebra (dir1, dir2)
        combineDir (left1, right1, front1) (left2, right2, front2) =
            ((left1, left2), (right1, right2), (front1, front2))

        combinePat :: PatAlgebra pat1 -> PatAlgebra pat2 -> PatAlgebra (pat1, pat2)
        combinePat (empty1, lambda1, debris1, asteroid1, boundary1, underscore1)
            (empty2, lambda2, debris2, asteroid2, boundary2, underscore2) =
            (
                (empty1, empty2),
                (lambda1, lambda2),
                (debris1, debris2),
                (asteroid1, asteroid2),
                (boundary1, boundary2),
                (underscore1, underscore2)
            )

-- Return whether exactly one element of the list 
-- satisfies the predicate.
one :: (a -> Bool) -> [a] -> Bool
one p = (== 1) . length . filter p