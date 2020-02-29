module Match where

import Language

-- Substitution maps variables to a term
-- Note: All Substitution are functions that map their domain (Identifier) to their range (Term)
--       Thus, applying an identifier to a substitution reveals what term it evaluates to.
type Substitution = Identifier -> Term

-- A special substitution is identity
-- does not change any variable. defined as follows.
identity :: Substitution
identity i = Var i

-- makeSubst takes an id and term and returns a subtitution that binds id to term.
-- This subst when applied to other will return:
-- 1. term if other is same as id, else
-- 2. Var other. Note that this allows us to parse in unknown identifiers as variables.
-- This illustrates how to return a function as result.
makeSubst :: Identifier -> Term -> Substitution
makeSubst id term other | id == other = term
                        | otherwise   = Var other

-- An example:
-- let sigma = makeSubst "sum_function" (Fn "sum" [Var "xs"]) :: Substitution
-- sigma "sum_function" == (Fn "sum" [Var "xs"])
-- sigma "g" == Var "g"

-- applying a Substitution (never fails)
applySubst :: Substitution -> Term -> Term
applySubst sigma (Var i)     = sigma i
applySubst sigma (Fn f args) = Fn f (map (applySubst sigma) args)

-- composing substitutions sigma1 and sigma2.
-- means apply sigma1 first and then sigma2
compose :: Substitution -> Substitution -> Substitution
compose sigma1 sigma2 = (applySubst sigma2) . sigma1

showSubst :: Substitution -> [Term] -> [(Term, Term)]
showSubst sigma = map (\t -> (t, applySubst sigma t))
