import Data.HashMap.Strict
import Debug.Trace
import Prelude hiding (subtract)

data Value =
    I Int
  | S String
  | Fn [Name] [Expression]
  deriving (Show)

--We hand define Eq to ensure our fungibility between float and int is handled, and
--to deal with Functions.
instance Eq Value where
  (I x) == (I y) = x == y
  (S x) == (S y) = x == y
--Because the only way of retrieving a function in Value form is by consulting the
--uniqueness enforcing Context, we can safely just compare names.
  (Fn x _) == (Fn y _) = x == y
  _ == _ = False

data Result = Valid Value | Error | Nil
  deriving (Show, Eq)

type List = [Value]
type Name = String

data Expression =
    Val Value
  | Var Name
  | Define Name [Name] [Expression] --Define a function.  Name of function, parameters, code.
  | Call Name [Expression] --Call a function.  Name of function, arguments.
  | Add Expression Expression
  -- | Subtract Expression Expression --Can be sugar using negate.
  | Multiply Expression Expression
  | Divide Expression Expression
  | Index Expression List
  | AssignIdx Expression Expression List
  | Append Expression List
  | Equ Expression Expression
  | If Expression [Expression] [Expression]
  | While Expression [Expression]
  | Print Expression
  | Assign Name Expression
  | Or Expression Expression
  | And Expression Expression
  | Not Expression
  deriving(Show, Eq)

type Context = HashMap Name Value

type Domain = Context -> Expression -> (Context, Result)

eval :: Domain
--Just a literal or a variable: evaluates to the data contained, or to an error
--if an attempt is made to evaluate an undefined variable.
eval c (Val v) = (c, Valid v)
eval c (Var s) = case Data.HashMap.Strict.lookup s c of
  Just x  -> (c, Valid x)
  Nothing -> (c, printError ("Undefined reference to variable " ++ s ++ ".") )
--Assignment of variable.  Assigning an Error produces another Error, assigning
--a real value binds the name to that value in the Context, and assigning Nil
--destroys a binding, if it exists.
eval c (Assign s ex) =
  let val = eval c ex in
    case val of
      (c', Valid v) -> (c'', Valid v)
        where c'' = Data.HashMap.Strict.insert s v c'
      (c', Error) -> (c', printError ("Could not assign non-value to variable " ++ s ++ "."))
      (c', Nil)     -> (c'', Nil)
        where c'' = Data.HashMap.Strict.delete s c'
--Calling a function.  Meat below in function def.
eval c (Call n e) = call c n e
--Equality
eval c (Equ l r)
  | l' == r'  = (c'', true)
  | otherwise = (c'', false)
    where (c', l')  = eval c l
          (c'', r') = eval c' r
--Logical Operators
eval c (Or e1 e2) =
  let (c', l)  = eval c e1
      (c'', r) = eval c' e2
  in case (extractTruth l, extractTruth r) of
    (False, False) -> (c'', false)
    _              -> (c'', true)
eval c (And e1 e2) =
  let (c', l)  = eval c e1
      (c'', r) = eval c' e2
  in case (extractTruth l, extractTruth r) of
    (True, True) -> (c'', true)
    _            -> (c'', false)
eval c (Not e) =
  let (c', r) = eval c e
  in if extractTruth r 
    then (c', false)
    else (c', true)
--If/Else
eval c (If cnd et ef) =
 let (c', r) = eval c cnd
  in if extractTruth r
    then foldExpressions c' et
    else foldExpressions c' ef

--While 
eval c (While cnd es) =
 let (c', r) = eval c cnd
  in if extractTruth r
    then let (c'', r') = (foldExpressions c' es) 
          in eval c'' (While cnd es)
    else (c', r)

    -- if test c s then stmt (While c b) (stmt b s) else s
--Addition.
eval c (Add (Val (I l)) (Val (I r))) = (c, Valid (I (l + r))) -- Int + Int
eval c (Add (Val (S l)) (Val (S r))) = (c, Valid (S (l ++ r))) -- String + String
eval c (Add (Val _) (Val _))         = (c, printError "Invalid operands to add.")        
eval c (Add l r)                     =
  let (c', l')  = eval c l
      (c'', r') = eval c' r
  in case (l', r') of
    (Nil, y)           -> (c'', y)
    (x, Nil)           -> (c'', x)
    (Error, _)         -> (c'', printError e)
    (_, Error)         -> (c'', printError e)
    (Valid a, Valid b) -> eval c'' (Add (Val a) (Val b))
    where e = "Invalid operands to add."
--Multiplication
eval c (Multiply (Val (I l)) (Val (I r))) = (c, Valid (I (l * r))) -- Int * Int
--Multiplying a string by an integer should, because it's fun, return that string concatenated that many times.
eval c (Multiply (Val (S l)) (Val (I r)))
  | r < 0     = (c, printError "Cannot multiply a string by a negative number.")
  | r == 0    = (c, Valid (S ""))
  | r == 1    = (c, Valid (S l))
  | otherwise = (c', Valid (S (l ++ l')) )
     where (c', Valid (S l') ) = eval c (Multiply (Val(S l)) (Val (I (r - 1) )))
--Rather than repeat the boilerplate, we'll just define I * S as S * I.
eval c (Multiply (Val (I l)) (Val (S r)))      =  eval c (Multiply (Val (S r)) (Val (I l)))
eval c (Multiply (Val _) (Val _))              = (c, printError "Invalid operands to multiply.")        
eval c (Multiply l r)                          =
  let (c', l')  = eval c l
      (c'', r') = eval c' r
  in case (l', r') of
    (Nil, y)           -> (c'', y)
    (x, Nil)           -> (c'', x)
    (Error, _)         -> (c'', printError e)
    (_, Error)         -> (c'', printError e)
    (Valid a, Valid b) -> eval c'' (Multiply (Val a) (Val b))
    where e = "Invalid operands to multiply."


-- Division
eval c (Divide _ (Val (I 0))) = (c, printError "Denominator cannot be 0")
eval c (Divide (Val (I n1)) (Val (I n2))) = (c, Valid (I (n1 `div` n2)))
eval c (Divide (Val (S s)) (Val (I n))) = (c, Valid (substring n (S s)))
eval c (Divide _ (Val (S s))) = (c, printError "Cannot divide string")
eval c (Divide (Val _) (Val _)) = (c, printError "Invalid operands to divide")
eval c (Divide l r) = 
  let (c', l')  = eval c l 
      (c'', r') = eval c' r 
  in case (l', r') of 
      (Nil, _) -> (c, printError "Cannot evaluate Nil")
      (_, Nil) -> (c, printError "Cannot evaluate Nil")
      (Error, _) -> (c, printError "Invalid operands to divide")
      (_, Error) -> (c, printError "Invalid operands to divide")
      (Valid n1, Valid n2) -> eval c'' (Divide (Val n1) (Val n2))


-- substring: for String division
substring :: Int -> Value -> Value
substring x (S text) = S (take n (text))
  where n = (length text) `div` x
--The trace thing for error reporting comes from: https://stackoverflow.com/questions/42700743/how-can-i-print-the-parameters-of-a-function-before-evaluation-in-haskell
printError :: String -> Result
printError s | trace s False = undefined
printError s = Error

extractTruth :: Result -> Bool
extractTruth (Valid (I 0))  = False
extractTruth (Valid (S "")) = False
extractTruth Nil            = False
extractTruth Error          = False
extractTruth _              = True


{- foldExpressions is the basic function for crunching a series of expressions
down to some final value.  The context is passed from expression to expression,
but intermediate results are basically rvalues and are discarded between lines; the
fold as a whole returns only the Result produced by the _final_ expression in the list.
(Or Nil, in the case of an empty list.)
Of course, if expressions are nested, then those rvalues have a use.  And for a
function or a program (which is a function itself,) the final rvalue produced
serves as the overall return value of the function. -}
foldExpressions :: Context -> [Expression] -> (Context, Result)
foldExpressions c (e:es) =
  let (c', r) = eval c e in
    case es of
      [] -> (c', r)
      _  -> foldExpressions c' es
foldExpressions c [] = (c, Nil)

{- valueIsFunc and transferFuncDefs are used to filter out everything except
function variables from a Context.  The purpose is to allow functions to have
global scope, by unioning a filtered inbound context with the bound argument list
of the function call itself.  It's also used to allow functions defined inside
a function to persist into the parent, because again, they're globals.-}
valueIsFunc :: Value -> Bool
valueIsFunc (Fn _ _) = True
valueIsFunc _ = False

transferFuncDefs :: Context -> Context
transferFuncDefs = Data.HashMap.Strict.filter valueIsFunc

--For bind arguments, we produce a tuple of two Contexts; the first is to preserve
--any modifications to the original context produced as side effects of evaluating
--the argument list; the second is the bound arguments for the new scope.

bindArguments :: (Context, Context) -> [Name] -> [Expression] -> (Context, Context)
bindArguments (ps, fs) [] _ = (ps, fs)
bindArguments (ps, fs) _ [] = (ps, fs)
bindArguments (ps, fs) (n:ns) (e:es) =
  let (ps', r) = eval ps e
    in case r of
      Valid v -> bindArguments (ps', Data.HashMap.Strict.insert n v fs) ns es
      _       -> bindArguments (ps', fs) ns es

{- call calls a function.  The general idea is that the list of expressions provided
as arguments is evaluated, and bound pairwise with the function's parameter name list
into variables in a new Context, the function's scope.  Any side effects of those 
expressions are preserved in the parent Context, and all function definitions
in the parent are transferred into the child scope.  After the call is complete,
any functions defined inside the call are transferred to the original scope,
overwriting in case of collision with an existing name.  The call itself
then evaluates down to the (possibly modifed) parent scope and the result of
  the last body expression.-}
call :: Context -> Name -> [Expression] -> (Context, Result)
call c fname e =
  let fn = Data.HashMap.Strict.lookup fname c
    in case fn of
      Just (Fn params body) ->
        let (c', scope) = bindArguments(c, transferFuncDefs c) params e
            (scope', r) = foldExpressions scope body
             in (Data.HashMap.Strict.union (transferFuncDefs scope') c', r)
      Nothing               -> (c, printError ("Function call to " ++ fname ++ " failed: no such function.") )
      _                     -> (c, printError ("Function call to" ++ fname ++ "failed: name is bound to non-function variable.") )

-- SUGAR

true :: Result
true = Valid (I 1)

false :: Result
false = Nil

increment :: Name -> Expression
increment n = Assign n (Add (Var n) (Val (I 1) ) )

subtract :: Expression -> Expression -> Expression
subtract l r = Add l (Multiply r (Val (I (-1) ) ) )

define :: Context -> Name -> [Name] -> [Expression] -> (Context, Result)
define = undefined

--LIBRARY and PROGRAM LAUNCHING

buildLibrary :: Context -> [(Name, Value)] -> Context
buildLibrary c [] = c
buildLibrary c ((n, fn):ts) = buildLibrary (Data.HashMap.Strict.insert n fn c) ts

run :: Context -> Value -> Result
run c (Fn n e) =
  let c' = Data.HashMap.Strict.insert "main" (Fn n e) c
      (_, r) = call c' "main" []
      in r
run _ _ =  printError "Could not launch program: second argument to run must be a function."

emptyContext :: Context
emptyContext = Data.HashMap.Strict.empty

library :: Context
<<<<<<< HEAD
library = buildLibrary emptyContext [("doubler", doubler), ("test", I 0), ("test2", I 4)]
=======
library = buildLibrary emptyContext [("doubler", doubler)
                                    ,("fib", fib)
                                    ]
>>>>>>> 2b3a5bc6bd73c876b976c09a0217771dfbb258e1

doubler :: Value
doubler = Fn ["x"] [Add (Var "x") (Var "x")]

<<<<<<< HEAD
whileTest :: (Context,Result)
whileTest = eval library (While (Not (Equ (Var "test") (Var "test2"))) [Assign "test" (Add (Var "test") (Val (I 1)))])
=======
fib :: Value
fib = Fn ["n"]
  [
    If (Equ (Var "n") (Val (I 0)))
    [--then
      Val (I 0)
    ]
    [--else
      If (Equ (Var "n") (Val (I 1)))
      [--then
        Val (I 1)
      ]
      [--else
        Add (Call "fib" [subtract (Var "n") (Val (I 1))]) (Call "fib" [subtract (Var "n") (Val (I 2))])
      ]
    ]
  ]

runFibonacci :: Int -> Result
runFibonacci n = run library (Fn [] [Call "fib" [Val (I n)] ])
>>>>>>> 2b3a5bc6bd73c876b976c09a0217771dfbb258e1
