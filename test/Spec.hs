import Test.Hspec

import Language
import Match
import Unify
import Simplify
import Derive

main :: IO ()
main = hspec $ do
  describe "language" $ do
    it "can show vars" $ do
      show (Var "x") `shouldBe` "x"
      
    it "can show funcs" $ do
      show (Fn "0" []) `shouldBe` "0"

    it "can show nested funcs" $ do
      show (Fn "f" [Var "x", Fn "neg" [Var "ab"]]) `shouldBe` "f(x, neg(ab))"

  describe "match/subst" $ do
    let sigma = makeSubst "sum_function" (Fn "sum" [Var "xs"])
    describe "makeSubst" $ do
      it "works" $ do
        sigma "sum_function" `shouldBe` (Fn "sum" [Var "xs"])

    it "substitutions can handle stuff not in domain" $ do
      sigma "g" `shouldBe` Var "g"

  describe "unify" $ do
    let
      vw = Var "w"
      vx = Var "x"
      vy = Var "y"
      vz = Var "z"

      zero = Fn "0" []
      s1 = Fn "f" [vx, vy, vx]
      s2 = Fn "f" [Fn "g" [vy, vy],
                   Fn "h" [vz],
                   Fn "g" [vw, Fn "h" [zero]]]

    it "unifier of s1 and s2 exists" $ do
      length (unify s1 s2) `shouldBe` 1

    it "unifies s1 and s2 correctly" $ do
      applySubst (head $ unify s1 s2) s1 `shouldBe` Fn "f" [Fn "g" [Fn "h" [zero],
                                                                    Fn "h" [zero]],
                                                            Fn "h" [zero],
                                                            Fn "g" [Fn "h" [zero],
                                                                    Fn "h" [zero]]]

  describe "simplify and derive" $ do
    let
      vx = Var "x"
      vy = Var "y"
      zero = Fn "0" []
      succ x = Fn "succ" [x]
      addFn a b = Fn "add" [a, b]
      mulFn a b = Fn "mul" [a, b]
      one = succ zero
      two = succ one
      three = succ two
      four = succ three
      
      rules = [ Rule "add_zero" (addFn vx zero) (vx)
              , Rule "add"      (addFn vy (succ vx)) (addFn (succ vy) vx)
              , Rule "mul_zero" (mulFn vx zero) (zero)
              , Rule "mul"      (mulFn vy (succ vx)) (addFn (mulFn vy vx) vy)]
    describe "simplification" $ do
      it "can compute 0 + 1 = 1" $ do
        simplify rules (addFn zero one) `shouldBe` [one]

      it "can compute 1 + 2 = 3" $ do
        simplify rules (addFn one two) `shouldBe` [three]

      it "can compute 1 * 0 = 0" $ do
        simplify rules (mulFn one zero) `shouldBe` [zero]

      it "can compute 0 * 1 = 0" $ do
        simplify rules (mulFn zero one) `shouldBe` [zero]

      it "can compute 1 * 2 = 2" $ do
        simplify rules (mulFn one two) `shouldBe` [two]

      it "can compute 2 * 2 = 4" $ do
        simplify rules (mulFn two two) `shouldBe` [four]

      it "can compute 1 + 1 * 2 = 3" $ do
        simplify rules (addFn one (mulFn one two)) `shouldBe` [three]

      it "can compute 2 * 1 + 1 + 1 = 4" $ do
        simplify rules (addFn (mulFn two one) (addFn one one)) `shouldBe` [four]

      it "can compute (1 + 1) * (1 + 1) = 4" $ do
        simplify rules (mulFn (addFn one one) (addFn one one)) `shouldBe` [four]

    describe "derivation" $ do
      it "can derive 0 + 1 = 1" $ do
        (fst . head . head) (derive rules (addFn zero one)) `shouldBe` one

      it "can derive 1 + 2 = 3" $ do
        (fst . head . head) (derive rules (addFn one two)) `shouldBe` three

      it "can derive 1 * 0 = 0" $ do
        (fst . head . head) (derive rules (mulFn one zero)) `shouldBe` zero

      it "can derive 0 * 1 = 0" $ do
        (fst . head . head) (derive rules (mulFn zero one)) `shouldBe` zero

      it "can derive 1 * 2 = 2" $ do
        (fst . head . head) (derive rules (mulFn one two)) `shouldBe` two

      it "can derive 2 * 2 = 4" $ do
        (fst . head . head) (derive rules (mulFn two two)) `shouldBe` four

      it "can derive 1 + 1 * 2 = 3" $ do
        (fst . head . head) (derive rules (addFn one (mulFn one two))) `shouldBe` three

      it "can derive 2 * 1 + 1 + 1 = 4" $ do
        (fst . head . head) (derive rules (addFn (mulFn two one) (addFn one one))) `shouldBe` four

      it "can derive 3 + (3 * 1) = 6" $ do
        (fst . head . head) (derive rules (addFn three (mulFn three one))) `shouldBe` succ (succ four)
