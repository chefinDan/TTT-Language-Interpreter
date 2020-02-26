module Sugar where

import Core

--User-useable boolean literals.
true :: Result
true = Valid (I 1)

false :: Result
false = Nil

--increment is sugar that rebinds a variable to that variable + 1.
increment :: Name -> Expression
increment n = Assign n (Add (Var n) (Val (I 1)))

--subtract is Sugar for, well, subtraction.
subtract :: Expression -> Expression -> Expression
subtract l r = Add l (Multiply r (Val (I (-1))))

--define is simply sugar for binding a function varaible.
define :: Name -> [Name] -> [Expression] -> Expression
define n ps es = Assign n (Val (Fn ps es))
