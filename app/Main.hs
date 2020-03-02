module Main where

import Language
import Match
import Unify

vw = Var "w"
vx = Var "x"
vy = Var "y"
vz = Var "z"
symbols = [vw, vx, vy, vz]

zero = Fn "0" []
-- s1 = Fn "f" [vx, vy, vx]
-- s2 = Fn "f" [Fn "g" [vy, vy],
--              Fn "h" [vz],
--              Fn "g" [vw, Fn "h" [zero]]]


s1 = Fn "f" [vx ,vy]
s2 = Fn "f" [Fn "g" [vy, vy],
			Fn "h" [vz]]


-- changes x -> fog(y)
dummySubst :: Substitution
dummySubst "x" = Fn "fog" [Var "x"]
dummySubst x = identity x

main :: IO ()
main = do
  putStrLn $ show s1
  putStrLn $ show s2
  putStrLn $ show $ applySubst dummySubst s1
  putStrLn $ show $ applySubst (head $ unify s1 s2) s2
  --putStrLn $ show $ showSubst (head $ unify s1 s2) symbols
