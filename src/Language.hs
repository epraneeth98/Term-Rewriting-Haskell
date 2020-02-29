module Language where

import Data.List

-- Identifier of variables are strings.
type Identifier = String

-- All operators are strings.
type Operator   = String

-- A term is either a variable or an application of an operator on a list of
-- terms.
data Term       = Var Identifier | Fn Operator [Term] deriving Eq

instance Show Term where
  show (Var v) = v
  show (Fn f args) | length args > 0 = f ++ "(" ++ (intercalate ", " $ map show args) ++ ")"
                   | otherwise = f

-- list of variables in a term
varsInTerm :: Term -> [Identifier]
varsInTerm (Var i)     = [i]
varsInTerm (Fn f args) = foldr (++) [] (map (varsInTerm) args)

--------------------------------------------------------------------------------
--                                                          Describing equations
--------------------------------------------------------------------------------

-- A rule is (a, b) where a and b are declared to be equivalent.
-- The Assume introduces any arbitrary initial term.
-- Each Rule has a name and two equivalent terms
data Rule = Assume
          | Rule { name :: String
                 , lhs :: Term
                 , rhs :: Term
                 }
          deriving (Show, Eq)
-- An example Rule set:
-- rules = [ Rule "add_zero" (Fn "+" [Fn "s" [Var "x"], Fn "zero" []]) (Fn "s" [Var "x"])
--         , Rule "add"      (Fn "+" [Var "y", Fn "s" [Var "x"]]) (Fn "+" [Fn "s" [Var "y"], Var "x"])]
--

-- A Derivation is a sequence of steps applied (along with the intermediate
-- term) that details steps taken by our algorithm.
-- **Note that you should keep the most recent step at the head of the list.**
-- Thus, the `Assume` rule is always the last element of a derivation.
type Derivation = [(Term, Rule)]

-- Renders a derivation on the terminal (in the correct order -- Assume first)
showDerivation :: Derivation -> IO ()
showDerivation = putStrLn . unlines . map render . reverse
  where render (term, Assume) = concat ["(a) ", show term]
        render (term, rule) = concat ["  ==> ", show term, "\t\t", name rule]
