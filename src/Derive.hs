module Derive (derive) where

import Data.List -- might need the `nub` function.
import Language
import Match
import Unify

-- Operates like `simplify` but additionally report the steps taken
-- to reach the normal form.
--
-- Each derivation (list of steps) is recorded in reverse, ie. most recent step
-- is at the head of the list.
derive :: [Rule] -> Term -> [Derivation]
derive rules term = []

-- This is a hint. Very similar to Simplify.
derive' :: [Rule] -> [Derivation] -> [Derivation]
derive' rules ds = []

deriveOnce :: [Rule] -> Derivation -> [Derivation]
deriveOnce rules ((term, r):steps) = []

-- This is a hint, a function that can recusively apply rules on a term AND it's
-- sub-terms, and return their normal forms.
deepDerive :: [Rule] -> Derivation -> [Derivation]
deepDerive rules d@((term@(Fn f args), r):steps) = []

-- A function that the TA found to be useful
chunks :: [a] -> [([a], [a])]
chunks [] = [] -- not [([], [])]
chunks t@(x:xs) = ([],t):(map (\(h,t) -> (x:h,t)) $ chunks xs)
