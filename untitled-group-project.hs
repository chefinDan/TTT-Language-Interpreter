import           Data.HashMap.Strict
import           Debug.Trace
import           Prelude                 hiding ( subtract )
import           Data.List


{- Values are the basic data types available as literals.  Note that boolean
 - values are expressible via syntactic sugar. -}
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

--We're defining Ord only in terms of less than, because that is all our
--semantics require to bootstrap the other operations in sugar.
instance Ord Value where
  (I x) < (I y) = x < y
  (S x) < (S y) = x < y
  _     < _     = False
  (I x) <= (I y) = x <= y
  (S x) <= (S y) = x <= y
  _     <= _     = False


{- In our language, all Expressions evaluate down to a result: Results can
 - be any of a Value, Nil, or an Error.  Nil and Error are distinct, with the
 - former being used for, among other things, an out of bounds index on an
 - array. -}
data Result = Valid Value | Error | Nil
  deriving (Show, Eq)

{- Expressions are the basic unit of execution.  Each expression can take
 - other expressions as arguments, and arguments will be recursively evaluated
 - left to right to produce Results on which the top level expression will
 - operate. -}
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
  | LessThan Expression Expression
  deriving(Show, Eq)

{- Context is used to store the state.  State in our language consists of
 - all variable bindings visible in the current scope. -}
type Context = HashMap Name Value

{- Domain is the semantic domain for evaluating Expressions.  Each Expression
 - takes a Context, and returns a tuple of a Context and a Result.  The context
 - returned by an Expression is then passed to the next Expression to be
 - evaluated, so that state is preserved and updated as the program executes. -}
type Domain = Context -> Expression -> (Context, Result)


{- eval is the basic Haskell function for evaluating Expressions.  We intend
 - to refactor some of these cases out into sub-types later on, to shorten
 - the sheer number of cases. -}
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
-- List Operations
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
        (Valid (I a), Valid d, Valid (List xs)) -> if a >= length xs || a < 0
          then (c''', printError ("AssignIdx: Out of Bounds"))
          else (c''', Valid (List (changeIndex a d xs)))
        _ -> (c, printError "Invalid Arguments to AssignIdx")
eval c (AddLists e l) =
  let (c' , e') = eval c e
      (c'', l') = eval c' l
  in  case (e', l') of
        (Valid (List a), Valid (List xs)) -> (c'', Valid (List (a ++ xs)))
        _ -> (c'', printError "Invalid Arguments to AddLists")

-- A helper function for AssignIdx
changeIndex :: Int -> Value -> [Value] -> [Value]
changeIndex i d [] = if i == 0 then [d] else []
changeIndex i d (x : xs) =
  if i == 0 then d : xs else x : changeIndex (i - 1) d xs

--A helper function for Index.
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

--exctractTruth is used to assign some truth value to a Result.  This allows
--things like If and While structures to operate, of course.
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

--transferFuncDefs is used in scope creation; it takes a Context and returns
--a new Context from which all non-function variable bindings have been
--excised.
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


--LIBRARY and PROGRAM LAUNCHING


--Just an empty context, useful for various purposes.
emptyContext :: Context
emptyContext = Data.HashMap.Strict.empty

--Since our library is implemented as a Context containing bindings for
--various library functions, we have this function to pre-populate it.
buildLibrary :: Context -> [(Name, Value)] -> Context
buildLibrary c [] = c
buildLibrary c ((n, fn) : ts) =
  buildLibrary (Data.HashMap.Strict.insert n fn c) ts

--The actual library; functions to be added to the library cna be placed
--in the list.
library :: Context
library = buildLibrary
  emptyContext
  [("doubler", doubler), ("fib", fib), ("list", listtest), ("maplist", maplist)]

--run is the function that actually launched a program.  It is passed a context,
--which will generally be the library, and a function, which it will bind to
--the name "main" in that context, and execute.
run :: Context -> Value -> Result
run c (Fn n e) =
  let c'     = Data.HashMap.Strict.insert "main" (Fn n e) c
      (_, r) = call c' "main" []
  in  r
run _ _ = printError
  "Could not launch program: second argument to run must be a function."

--Library function that just adds an argument to itself and returns the new
--value.
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

--Helper function to run the fibonacci demo; takes an int as an argument.
runFibonacci :: Int -> Result
runFibonacci n = run library (Fn [] [Call "fib" [Val (I n)]])

--Helper function to run the mapdemo demo.
runMapDemo :: Result
runMapDemo = run library mapdemo
