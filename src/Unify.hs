module Unify (unify, listUnify) where

import Language
import Match

find _ [] = False
find x1 (y1:xs) | x1==y1 = True
find x1 (_:xs) = find x1 xs

-- we return list of Substitution. why? (Failure!)
unify :: Term -> Term -> [Substitution]
unify (Var x) (Var y) | x == y    = [identity]
                      | otherwise = [makeSubst x (Var y)]

-- have to do occurs check, and look for already bound variables
-- since x does not unify with f(x)
unify (Var x) term     = (bindVar x term)
unify term    (Var y)  = (bindVar y term)
unify (Fn f fargs) (Fn g gargs) | f==g && ((length fargs) == (length gargs))= (listUnify fargs gargs)
                                | otherwise = []  

-- bindVar v t  checks if
-- var v occurs in t, else binds v to t
bindVar :: Identifier -> Term -> [Substitution]
bindVar v t | (find v (varsInTerm t)) = []
			| otherwise = [(makeSubst v t)]   

-- unify 2 lists of terms.
-- Do ts in list1 correspond with ts in list2
-- a simple recursive definition ...
listUnify :: [Term] -> [Term] -> [Substitution]
listUnify [] []     = [identity]
-- if either side has extra args, then fail.
listUnify [] (r:rs) = []
listUnify (t:ts) [] = []
-- unify t with r with some u1,
-- applySubst to ts' and rs' where ts' <- u1 ts and rs' <- u1 rs
listUnify (t:ts) (r:rs) | null (unify t r) = []
						| otherwise = map (compose head) (listUnify (map (applySubst head) ts) (map (applySubst head) rs)) 
							where (head:_) = (unify t r)  