module PisigmaTal.Anf.Opt () where

-- TODO: optimization
-- 1. curried functions may create unnecessary closures
--    detect some inner lambda expressions. eg. \x -> \y -> (+) x y to \x y -> (+) x y
-- 2. inlining
--    - inline small functions
--      (\x y -> e) v w to e[x := v, y := w] if e is not big (e has no multi-arity constructor)
--    - inline functions that are only called once, then check the conditions below and inline.
--    - inline lambda expressions (beta reduction)
--      (\x -> e1) e2 to e1[x := e2] if x occurs only once in e1 and e2 is not big
-- 3. constant folding
--    - fold constant expressions
--    - eg. (+) 1 2 to 3
-- 4. dead code elimination
--    - remove unused bindings
--    - eg. let x = 1 in 2 to 2
-- 5. defunctionalization (option)
--    - replace higher-order functions with data types and pattern matching
--    id (id := \x -> x) to \x -> id x and make sure not to be closure-converted
--    f x (f := \x -> \y -> e) to \y -> f x y


