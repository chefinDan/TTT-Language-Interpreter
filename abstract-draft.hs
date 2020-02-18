import Data.HashMap.Strict

data Value =
    I Int
  | F Float
  | S String
  | Fn [Name] [Expression]
  deriving (Eq, Show)
--  | L [Value]

type Function = Fn a

data Result = Valid Value | Error [String] | Nil
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
  | AssignIdx Expression List
  | Append Expression List
  | Equ Expression Expression
  | While Expression [Expression]
  | If Expression [Expression] [Expression]
  | Print Expression
  | Assign String Expression
  deriving(Show, Eq)

type Context = HashMap Name Value

type Domain = Context -> Expression -> (Context, Result)

parseOutput :: (Context, Result) -> IO ()
parseOutput (_, Valid v) = print v
parseOutput (_, Error e) = putStrLn (concat e)


eval :: Domain
--Just a literal or a variable: evaluates to the data contained, or to an error
--if an attempt is made to evaluate an undefined variable.
eval c (Val v) = (c, Valid v)
eval c (Var s) = case Data.HashMap.Strict.lookup s c of
  Just x -> (c, Valid x)
  Nothing -> (c, Error ["Undefined reference to variable " ++ s ++ ".\n"])
--Assignment of variable.  Assigning an Error produces another Error, assigning
--a real value binds the name to that value in the Context, and assigning Nil
--destroys a binding, if it exists.
eval c (Assign s ex) =
  let val = eval c ex in
    case val of
      (c', Valid v) -> (c'', Valid v)
        where c'' = Data.HashMap.Strict.insert s v c'
      (c', Error x) -> (c', Error (["Could not assign non-value to variable" ++ s ++ ".\n"] ++ Prelude.map ("  " ++) x))
      (c', Nil) -> (c'', Nil)
        where c'' = Data.HashMap.Strict.delete s c'
--Calling a function.  Meat below in function def.
eval c (Call n e) = call c n e
--Equality
eval c (Equ l r)
  | l' == r' = (c'', Valid (I 1))
  | otherwise = (c'', Valid (I 0))
    where (c', l') = eval c l
          (c'', r') = eval c' r
--If/Else
eval c (If cnd et ef) =
 let (c', r) = eval c cnd
  in case r of
    Valid (I 0) -> foldExpressions c' ef
    Valid (F 0) -> foldExpressions c' ef
    Valid (S "") -> foldExpressions c' ef
    Nil -> foldExpressions c' ef
    Error s -> (c', Error (["Error while evaluating If condition.\n"] ++ Prelude.map ("  " ++) s))
    _ -> foldExpressions c' et
--Addition.
eval c (Add (Val (I l)) (Val (I r))) = (c, Valid (I (l + r))) -- Int + Int
eval c (Add (Val (F l)) (Val (F r))) = (c, Valid (F (l + r))) -- Float + Float
eval c (Add (Val (F l)) (Val (I r))) = (c, Valid (F (l + fromIntegral r))) -- Float + Int
eval c (Add (Val (I l)) (Val (F r))) = (c, Valid (F (fromIntegral l + r))) -- Int + Float
eval c (Add (Val (S l)) (Val (S r))) = (c, Valid (S (l ++ r))) -- String + String
eval c (Add l r) =
  let (c', l') = eval c l
      (c'', r') = eval c' r
  in case (l', r') of
    (Nil, y) -> (c'', y)
    (x, Nil) -> (c'', x)
    (Error x, Error y) -> (c, Error (e ++ Prelude.map ("   " ++) x ++ Prelude.map ("   " ++) y ))
    (Error x, _) -> (c'', Error (e ++ Prelude.map ("   " ++) x))
    (_, Error x) -> (c'', Error (e ++ Prelude.map ("   " ++) x))
    (Valid a, Valid b) -> eval c'' (Add (Val a) (Val b))
    where e = ["Invalid operands to add:\n"]

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
      _ -> foldExpressions c' es
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
      _ -> bindArguments (ps', fs) ns es

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
      Nothing -> (c, Error ["Function call to " ++ fname ++ " failed: no such function.\n"])
      _ -> (c, Error ["Function call to" ++ fname ++ "failed: name is bound to non-function variable.\n"])

-- SUGAR

buildLibrary :: Context -> [(Name, Value)] -> Context
buildLibrary c [] = c
buildLibrary c ((n, fn):ts) = buildLibrary (Data.HashMap.Strict.insert n fn c) ts

run :: Context -> Value -> Result
run c (Fn n e) =
  let c' = Data.HashMap.Strict.insert "main" (Fn n e) c
      (_, r) = call c' "main" []
      in r
run _ _ =  Error ["Could not launch program: second argument to run must be a function.\n"]

emptyContext :: Context
emptyContext = Data.HashMap.Strict.empty

library :: Context
library = buildLibrary emptyContext [("doubler", doubler)]

doubler :: Value
doubler = Fn ["x"] [Add (Var "x") (Var "x")]