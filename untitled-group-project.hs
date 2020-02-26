import           Data.HashMap.Strict
import           Debug.Trace
import           Prelude                 hiding ( subtract )
import           Data.List

data Value =
    I Int
  | S String
  | Fn [Name] [Expression]
  | List [Value]
  deriving (Show)

type Name = String

--We hand define Eq to ensure our fungibility between float and int is handled, and
--to deal with Functions.
instance Eq Value where
  (I x   ) == (I y   ) = x == y
  (S x   ) == (S y   ) = x == y
--Because the only way of retrieving a function in Value form is by consulting the
--uniqueness enforcing Context, we can safely just compare names.
  (Fn x _) == (Fn y _) = x == y
  _        == _        = False

data Result = Valid Value | Error | Nil
  deriving (Show, Eq)

data Expression =
    Val Value
  | Var Name
  | Call Name [Expression] --Call a function.  Name of function, arguments.
  | Add Expression Expression
  | Multiply Expression Expression
  | Divide Expression Expression
  | Index Expression Expression
  | AssignIdx Expression Expression Expression
  | Append Expression Expression
  | AddLists Expression Expression
  | Prepend Expression Expression
  | Equ Expression Expression
  | If Expression [Expression] [Expression]
  | While Expression [Expression]
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
  Nothing -> (c, printError ("Undefined reference to variable " ++ s ++ "."))
--Assignment of variable.  Assigning an Error produces another Error, assigning
--a real value binds the name to that value in the Context, and assigning Nil
--destroys a binding, if it exists.
eval c (Assign s ex) =
  let val = eval c ex
  in  case val of
        (c', Valid v) -> (c'', Valid v)
          where c'' = Data.HashMap.Strict.insert s v c'
        (c', Error) ->
          ( c'
          , printError ("Could not assign non-value to variable " ++ s ++ ".")
          )
        (c', Nil) -> (c'', Nil) where c'' = Data.HashMap.Strict.delete s c'
--Calling a function.  Meat below in function def.
eval c (Call n e) = call c n e
--Equality
eval c (Equ l r) | l' == r'  = (c'', true)
                 | otherwise = (c'', false)
 where
  (c' , l') = eval c l
  (c'', r') = eval c' r
--Logical Operators
eval c (Or e1 e2) =
  let (c' , l) = eval c e1
      (c'', r) = eval c' e2
  in  case (extractTruth l, extractTruth r) of
        (False, False) -> (c'', false)
        _              -> (c'', true)
eval c (And e1 e2) =
  let (c' , l) = eval c e1
      (c'', r) = eval c' e2
  in  case (extractTruth l, extractTruth r) of
        (True, True) -> (c'', true)
        _            -> (c'', false)
eval c (Not e) =
  let (c', r) = eval c e in if extractTruth r then (c', false) else (c', true)
--If/Else
eval c (If cnd et ef) =
  let (c', r) = eval c cnd
  in  if extractTruth r then foldExpressions c' et else foldExpressions c' ef
--While
eval c (While cnd es) =
  let (c', r) = eval c cnd
  in  if extractTruth r
        then let (c'', r') = foldExpressions c' es in eval c'' (While cnd es)
        else (c', r)
--Addition.
eval c (Add (Val (I l)) (Val (I r))) = (c, Valid (I (l + r))) -- Int + Int
eval c (Add (Val (S l)) (Val (S r))) = (c, Valid (S (l ++ r))) -- String + String
eval c (Add (Val _) (Val _)) = (c, printError "Invalid operands to add.")
eval c (Add l r) =
  let (c' , l') = eval c l
      (c'', r') = eval c' r
  in  case (l', r') of
        (Nil    , y      ) -> (c'', y)
        (x      , Nil    ) -> (c'', x)
        (Error  , _      ) -> (c'', printError e)
        (_      , Error  ) -> (c'', printError e)
        (Valid a, Valid b) -> eval c'' (Add (Val a) (Val b))
  where e = "Invalid operands to add."
--Multiplication
eval c (Multiply (Val (I l)) (Val (I r))) = (c, Valid (I (l * r))) -- Int * Int
--Multiplying a string by an integer should, because it's fun, return that string concatenated that many times.
eval c (Multiply (Val (S l)) (Val (I r)))
  | r < 0     = (c, printError "Cannot multiply a string by a negative number.")
  | r == 0    = (c, Valid (S ""))
  | r == 1    = (c, Valid (S l))
  | otherwise = (c', Valid (S (l ++ l')))
  where (c', Valid (S l')) = eval c (Multiply (Val (S l)) (Val (I (r - 1))))
--Rather than repeat the boilerplate, we'll just define I * S as S * I.
eval c (Multiply (Val (I l)) (Val (S r))) =
  eval c (Multiply (Val (S r)) (Val (I l)))
eval c (Multiply (Val _) (Val _)) =
  (c, printError "Invalid operands to multiply.")
eval c (Multiply l r) =
  let (c' , l') = eval c l
      (c'', r') = eval c' r
  in  case (l', r') of
        (Nil    , y      ) -> (c'', y)
        (x      , Nil    ) -> (c'', x)
        (Error  , _      ) -> (c'', printError e)
        (_      , Error  ) -> (c'', printError e)
        (Valid a, Valid b) -> eval c'' (Multiply (Val a) (Val b))
  where e = "Invalid operands to multiply."


-- Division
eval c (Divide _ (Val (I 0))) = (c, printError "Denominator cannot be 0")
eval c (Divide (Val (I n1)) (Val (I n2))) = (c, Valid (I (n1 `div` n2)))
eval c (Divide (Val (S s)) (Val (I n))) = (c, Valid (substring n (S s)))
eval c (Divide _ (Val (S s))) = (c, printError "Cannot divide string")
eval c (Divide (Val _) (Val _)) = (c, printError "Invalid operands to divide")
eval c (Divide l r) =
  let (c' , l') = eval c l
      (c'', r') = eval c' r
  in  case (l', r') of
        (Nil     , _       ) -> (c, printError "Cannot evaluate Nil")
        (_       , Nil     ) -> (c, printError "Cannot evaluate Nil")
        (Error   , _       ) -> (c, printError "Invalid operands to divide")
        (_       , Error   ) -> (c, printError "Invalid operands to divide")
        (Valid n1, Valid n2) -> eval c'' (Divide (Val n1) (Val n2))
eval c (Index e l) =
  let (c' , e') = eval c e
      (c'', l') = eval c' l
  in  case (e', l') of
        (Valid (I a), Valid (List xs)) -> grabIndex c'' a xs
        _ -> (c'', printError "Invalid Arguments to Index")
eval c (Append e l) =
  let (c' , e') = eval c e
      (c'', l') = eval c' l
  in  case (e', l') of
        (Valid a, Valid (List xs)) -> (c'', Valid (List (xs ++ [a])))
        _ -> (c'', printError "Invalid Arguments to Append")
eval c (Prepend e l) =
  let (c' , e') = eval c e
      (c'', l') = eval c' l
  in  case (e', l') of
        (Valid a, Valid (List xs)) -> (c'', Valid (List (a : xs)))
        _ -> (c'', printError "Invalid Arguments to Prepend")
eval c (AssignIdx i e l) =
  let (c'  , e') = eval c e
      (c'' , l') = eval c' l
      (c''', i') = eval c'' i
  in  case (i', e', l') of
        (Valid (I a), Valid d, Valid (List xs)) -> if a > length xs || a < 0
          then (c''', printError "Out of Bounds")
          else (c''', Valid (List (changeIndex a d xs)))
        _ -> (c, printError "Invalid Arguments to AssignIdx")
eval c (AddLists e l) =
  let (c' , e') = eval c e
      (c'', l') = eval c' l
  in  case (e', l') of
        (Valid (List a), Valid (List xs)) -> (c'', Valid (List (a ++ xs)))
        _ -> (c'', printError "Invalid Arguments to AddLists")
changeIndex :: Int -> Value -> [Value] -> [Value]
changeIndex i d [] = if i == 0 then [d] else []
changeIndex i d (x : xs) =
  if i == 0 then d : xs else x : changeIndex (i - 1) d xs

grabIndex :: Context -> Int -> [Value] -> (Context, Result)
grabIndex c i [] = (c, Nil)
grabIndex c i xs = if length xs > i then (c, Valid (xs !! i)) else (c, Nil)

-- substring: for String division
substring :: Int -> Value -> Value
substring x (S text) = S (take n text) where n = length text `div` x
--The trace thing for error reporting comes from: https://stackoverflow.com/questions/42700743/how-can-i-print-the-parameters-of-a-function-before-evaluation-in-haskell
printError :: String -> Result
printError s | trace s False = undefined
printError s                 = Error

extractTruth :: Result -> Bool
extractTruth (Valid (I 0 )) = False
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
foldExpressions c (e : es) =
  let (c', r) = eval c e
  in  case es of
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
valueIsFunc _        = False

transferFuncDefs :: Context -> Context
transferFuncDefs = Data.HashMap.Strict.filter valueIsFunc

--For bind arguments, we produce a tuple of two Contexts; the first is to preserve
--any modifications to the original context produced as side effects of evaluating
--the argument list; the second is the bound arguments for the new scope.

bindArguments
  :: (Context, Context) -> [Name] -> [Expression] -> (Context, Context)
bindArguments (ps, fs) [] _  = (ps, fs)
bindArguments (ps, fs) _  [] = (ps, fs)
bindArguments (ps, fs) (n : ns) (e : es) =
  let (ps', r) = eval ps e
  in  case r of
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
  in  case fn of
        Just (Fn params body) ->
          let (c'    , scope) = bindArguments (c, transferFuncDefs c) params e
              (scope', r    ) = foldExpressions scope body
          in  (Data.HashMap.Strict.union (transferFuncDefs scope') c', r)
        Nothing ->
          ( c
          , printError
            ("Function call to " ++ fname ++ " failed: no such function.")
          )
        _ ->
          ( c
          , printError
            (  "Function call to"
            ++ fname
            ++ "failed: name is bound to non-function variable."
            )
          )

-- SUGAR

true :: Result
true = Valid (I 1)

false :: Result
false = Nil

increment :: Name -> Expression
increment n = Assign n (Add (Var n) (Val (I 1)))

subtract :: Expression -> Expression -> Expression
subtract l r = Add l (Multiply r (Val (I (-1))))

define :: Name -> [Name] -> [Expression] -> Expression
define n ps es = Assign n (Val (Fn ps es))


--LIBRARY and PROGRAM LAUNCHING

buildLibrary :: Context -> [(Name, Value)] -> Context
buildLibrary c [] = c
buildLibrary c ((n, fn) : ts) =
  buildLibrary (Data.HashMap.Strict.insert n fn c) ts

run :: Context -> Value -> Result
run c (Fn n e) =
  let c'     = Data.HashMap.Strict.insert "main" (Fn n e) c
      (_, r) = call c' "main" []
  in  r
run _ _ = printError
  "Could not launch program: second argument to run must be a function."

emptyContext :: Context
emptyContext = Data.HashMap.Strict.empty

library :: Context
library = buildLibrary
  emptyContext
  [("doubler", doubler), ("fib", fib), ("list", listtest), ("maplist", maplist)]


listtest :: Value
listtest = Fn ["list"] [Val (List [I 5, I 2])]

doubler :: Value
doubler = Fn ["x"] [Add (Var "x") (Var "x")]

--Simple naive Fibonacci implementation.
fib :: Value
fib = Fn
  ["n"]
  [ If
      (Equ (Var "n") (Val (I 0)))
      [--then
       Val (I 0)]
      [--else
        If
          (Equ (Var "n") (Val (I 1)))
          [--then
           Val (I 1)]
          [--else
            Add (Call "fib" [subtract (Var "n") (Val (I 1))])
                (Call "fib" [subtract (Var "n") (Val (I 2))])
          ]
      ]
  ]


--Maplist takes as arguments a function and a list, and maps that function over
--each item in the list, returning the new, modified list.
maplist :: Value
maplist = Fn
  ["fn", "input"]
  [ Assign "i" (Val (I 0))
  , While
    (Index (Var "i") (Var "input"))
    [ Assign
      "input"
      (AssignIdx (Var "i")
                 (Call "fn" [Index (Var "i") (Var "input")])
                 (Var "input")
      )
    , increment "i"
    ]
  , Var "input"
  ]

--mapdemo is a demo program.  It defines a list of integers, then
--calls maplist, passing the doubler function in as an argument.  It then
--defines a list of strings, and calls maplist on that as well, this time
--passing in a function literal that multiplies its argument by three.
--Finally, it concatenates the two lists and returns them.
mapdemo :: Value
mapdemo = Fn
  []
  [ Assign "ints"    (Val (List [I 10, I 20, I 30]))
  , Assign "output"  (Call "maplist" [Var "doubler", Var "ints"])
  , Assign "strings" (Val (List [S "foo", S "bar", S "baz"]))
  , AddLists
    (Var "output")
    (Call "maplist"
          [Val (Fn ["str"] [Multiply (Var "str") (Val (I 3))]), Var "strings"]
    )
  ]


runFibonacci :: Int -> Result
runFibonacci n = run library (Fn [] [Call "fib" [Val (I n)]])

runMapDemo :: Result
runMapDemo = run library mapdemo


-- | Examples of bad programs that produce error results
---- 1. Attempts to add a string to an int, result is Error
baddemo1 :: Value
baddemo1 = Fn
  []
  [
    Assign "val1" (Val (I 2)),
    Assign "val2" (Val (S "bad")),
    Add (Var "val1") (Var "val2")
  ]
---- 2. Attempts to multiply an int literal by an undefined variable,  
baddemo2 :: Value
baddemo2 = Fn
  ["val"]
  [
    Multiply (Val (I 2)) (Var "val")
  ]
---- 3. Attempts to Multiply a string by a negative number 
baddemo3 :: Value
baddemo3 = Fn
  []
  [
    Multiply (Val (S "oops")) (Val (I (-2)))
  ]
---- 4. Division by zero 
baddemo4 :: Value
baddemo4 = Fn
  []
  [
    Assign "zero" (Val (I 0)),
    Divide (Val (I 2)) (Var "zero")
  ]
baddemo5 :: Value
baddemo5 = Fn
  []
  [
    Assign "idx" (Val (I 3)),
    Assign "list" (Val (List [I 2, I 3, I 4])),
    Assign "val" (Val (I 9)),
    While (Not (Equ (Var "idx") (Val (I 4))))
    [
      Assign "list" (AssignIdx (Var "idx") 
                (Var "val") 
                (Var "list")),
      increment "idx"
    ],
    Var "list"
  ]
baddemo6 :: Value
baddemo6 = Fn
  []
  [
    Assign "list" (Val (List [I 2])),
    AssignIdx (Val (I 3))
              (Val (I 3))
              (Var "list")
  ]

runBadDemos :: [Value] -> [Result]
runBadDemos []     = []
runBadDemos (v:vs) = (run library v):(runBadDemos vs)