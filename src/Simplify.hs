module Simplify (simplify) where

import Data.List -- might need the `nub` function.
import Language
import Match
import Unify

-- Takes a Rule set and a ground term and computes the normalized form of the
-- term.
-- Applies rules (left to right) till no more rules can be applied.
-- Returns a list of normal forms.
simplify :: [Rule] -> Term -> [Term]
simplify rules term = simplify' rules [term]

-- This is a hint, a function that simplifies a list of terms as "deeply" as possible recursively till no rule applies.
simplify' :: [Rule] -> [Term] -> [Term]
simplify' rules ts | ts == (map (head. (deepApply rules)) ts) = ts  --simplify_helper rules ts []-- | t==ts = ts
				   | otherwise = simplify' rules (map (head. (deepApply rules)) ts)
				   -- where (t:_) = deepApply rules ts

-- simplify_helper :: [Rule] -> [Term] -> [Term] -> [Term]
-- simplify_helper rules [] tout = reverse tout
-- simplify_helper rules (t1:tr) tout | null (deepApply rules t1) = simplify_helper rules tr (t1:tout)
-- 								   | otherwise = simplify_helper rules tr (u1:tout)
-- 								   where (u1:_)= deepApply rules t1


-- This is a hint, a function that applies rules to a term, not it's sub-terms.
applyOnce :: [Rule] -> Term -> [Term]
applyOnce [] term = []
applyOnce (r:rs) term | null (unify (lhs r) term) = applyOnce rs term
					  | otherwise  = [(applySubst u (rhs r))]
					  where (u:_) = (unify (lhs r) term)

-- This is a hint, a function that can recusively apply rules on a term AND it's
-- sub-terms, and return their normal forms.
deepApply :: [Rule] -> Term -> [Term]
deepApply rules term@(Fn f args) | null (applyOnce rules term) = [Fn f (map (head.(deepApply rules)) args)]
								 | otherwise = (applyOnce rules term)
                                 

-- A function that the TA found to be useful
chunks :: [a] -> [([a], [a])]
chunks [] = [] -- not [([], [])]
chunks t@(x:xs) = ([],t):(map (\(h,t) -> (x:h,t)) $ chunks xs)
