# term-rewriter
_Mini project one CS467_

The project uses minimal features of stack, namely building and testing. I've
set everything up, so all you need to do to get started:

``` shell
stack setup

# to run the "main" of our program (see `app/Main.hs`),
stack run

# to run all tests defined in our suite (see `test/Spec.hs`),
stack test
```

If you run `stack test` you'll see our `Match` works, but `Unify` fails. Take a
look at the test and make it pass! Classic TDD.

> For reference material, look at the equations that describe `unify` and
> `narrow` on the [web page
> (lec-06)](https://www.cse.iitb.ac.in/~ananya/cs467/lec-06.html).

### Issue board is locked. Use Moodle
### Do not fork this repo, clone it.
### Do not cheat, [collaborate](https://www.cse.iitb.ac.in/~supratik/courses/copying-and-discussion.html).

---------

# `MATCH`
We've already implemented it for you (`src/Match.hs`).

> Start with `src/Language.hs`, it describes the data types we operate on and
> some utilities.

# Task one: `UNIFY`
This is your first task. Complete the function bodies. You are free to ignore
completing some suggested bodies, **but you must define the functions exported by
the module.**

### What's exported by a module? What is a module?
Each haskell file that starts with a `module` declaration is a module. By
default all top-level definitions are "exported" by the module (ie, modules that
import them get everything).

Modules can selectively export definitions (like describing public ones) by,

``` haskell
module Foo (publicOne, publicTwo) where
...
```

Importing modules can do selective imports using similar syntax, and can also do
qualified imports (much like the `from ___ import ___ as ___` idiom from
python).

# Task two: `SIMPLIFY`
Given a set of rules and a ground term (that has no variables), simplify it.

Complete the function bodies. You are free to ignore completing some suggested
bodies, **but you must define the functions exported by the module.**

Take a look at `Language.hs` there's a `Rule` definition. For examples look at `Spec.hs`.

# Task three (optional): `DERIVE`

This is probably ungraded. But is extremely fun to implement. While simplifying all you have to do is remember which rules are used in the derivation. See example below. The format is self-explanatory.

```
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

showDerivation $ head $ derive rules (mulFn two two)
---
(a) mul(suc(suc(0)), suc(suc(0)))
  ==> add(mul(suc(suc(0)), suc(0)), suc(suc(0)))		mul
  ==> add(suc(mul(suc(suc(0)), suc(0))), suc(0))		add
  ==> add(suc(suc(mul(suc(suc(0)), suc(0)))), 0)		add
  ==> suc(suc(mul(suc(suc(0)), suc(0))))		add_zer0
(a) suc(mul(suc(suc(0)), suc(0)))
(a) mul(suc(suc(0)), suc(0))
  ==> suc(suc(add(mul(suc(suc(0)), 0), suc(suc(0)))))		mul
(a) suc(add(mul(suc(suc(0)), 0), suc(suc(0))))
(a) add(mul(suc(suc(0)), 0), suc(suc(0)))
  ==> suc(suc(add(suc(mul(suc(suc(0)), 0)), suc(0))))		add
(a) suc(add(suc(mul(suc(suc(0)), 0)), suc(0)))
(a) add(suc(mul(suc(suc(0)), 0)), suc(0))
  ==> suc(suc(add(suc(suc(mul(suc(suc(0)), 0))), 0)))		add
(a) suc(add(suc(suc(mul(suc(suc(0)), 0))), 0))
(a) add(suc(suc(mul(suc(suc(0)), 0))), 0)
  ==> suc(suc(suc(suc(mul(suc(suc(0)), 0)))))		add_zer0
(a) suc(suc(suc(mul(suc(suc(0)), 0))))
(a) suc(suc(mul(suc(suc(0)), 0)))
(a) suc(mul(suc(suc(0)), 0))
(a) mul(suc(suc(0)), 0)
  ==> suc(suc(suc(suc(0))))		mul_zer0
```
