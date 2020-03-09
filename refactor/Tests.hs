module Tests where

import Core
import Sugar
import RunLibrary


-- |
--   >>> eval (Lit (I 5)) emptyContext
--   (fromList [],Valid (I 5))
--
--   >>> run (Fn [] [Lit (I 5)]) emptyContext
--   5
--   
--   >>> runFibonacci 10
--   55
--
--   >>> eval (ArithExp (Add (Lit (I 5)) (Lit (S "foo")))) emptyContext
--   (fromList [],Err (E (BadOperands "Add") []))
--
--   >>> eval (ArithExp (Divide (Lit (I 5)) (Lit (I 2)))) emptyContext
--   (fromList [],Valid (I 2))
--
--   >>> eval (ArithExp (Divide (Lit (S "foofoofoofoo")) (Lit (I 2)))) emptyContext
--   (fromList [],Valid (S "foofoo"))
--
--   >>> eval (ArithExp (Divide (Lit (I 10)) (Lit (I 0)))) emptyContext
--   (fromList [],Err (E DivideByZero []))
--
--   >>> eval (ArithExp (Divide (Lit (I 10)) (Lit (S "foo")))) emptyContext
--   (fromList [],Err (E (BadOperands "divide") []))
--
--   >>> eval (ArithExp (Divide (Lit (S "foo")) (Lit (S "bar"))) emptyContext
--   (fromList [],Err (E (BadOperands "divide") []))
