module Sugar where

import Core
import Prelude hiding (subtract)

--User-useable boolean literals.
true :: Expression
true = Lit (I 1)

false :: Expression
false = Lit (I 0)

--increment is sugar that rebinds a variable to that variable + 1.
increment :: Name -> Expression
increment n = Bind n (ArithExp (Add (Dereference n) (Lit (I 1))))

--subtract is Sugar for, well, subtraction.
subtract :: Expression -> Expression -> Expression
subtract l r = ArithExp (Add l (ArithExp (Multiply r (Lit (I (-1))))))

--define is simply sugar for binding a function varaible.
define :: Name -> [Name] -> [Expression] -> Expression
define n ps es = Bind n (Lit (Fn ps es))
