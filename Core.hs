module Core where

import           Data.HashMap.Strict
import           Debug.Trace
import           Prelude                 hiding ( subtract
                                                , not
                                                , and
                                                , or
                                                )
--import           Data.List


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
  | Nand Expression Expression
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
eval c (Equ l r) | l' == r'  = (c'', Valid (I 1))
                 | otherwise = (c'', Valid (I 0))
 where
  (c' , l') = eval c l
  (c'', r') = eval c' r

--Logical Operator
eval c (Nand e1 e2) =
  let (c' , l) = eval c e1
      (c'', r) = eval c' e2
  in  case (extractTruth l, extractTruth r) of
        (True, True) -> (c'', Valid (I 0))
        _            -> (c'', Valid (I 1))
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
          then (c''', printError "AssignIdx: Out of Bounds")
          else (c''', Valid (List (changeIndex a d xs)))
        _ -> (c, printError "Invalid Arguments to AssignIdx")
eval c (AddLists e l) =
  let (c' , e') = eval c e
      (c'', l') = eval c' l
  in  case (e', l') of
        (Valid (List a), Valid (List xs)) -> (c'', Valid (List (a ++ xs)))
        _ -> (c'', printError "Invalid Arguments to AddLists")

eval c (LessThan (Val (I n1)) (Val (I n2)))
  | I n1 < I n2 = (c, Valid (I 1))
  | otherwise = (c, Valid (I 0))
eval c (LessThan (Val (S s1)) (Val (S s2)))
  | S s1 < S s2 = (c, (Valid (I 1)))
  | otherwise = (c, (Valid (I 0)))
eval c (LessThan (Val (I n)) (Val (S s))) = (c, printError "Error: Mismatched type when using '<' operator")
eval c (LessThan (Val (S s)) (Val (I n))) = (c, printError "Error: Mismatched type when using '<' operator")
eval c (LessThan l r) =
  let (c' , l') = eval c l
      (c'', r') = eval c' r in case (l', r') of 
        (Valid (I n1), Valid (I n2))
          | (I n1) < (I n2) -> (c, Valid (I 1))
          | otherwise -> (c, (Valid (I 0)))
        (Valid (S s1), Valid (S s2)) 
          | S s1 < S s2 -> (c, (Valid (I 1)))
          | otherwise -> (c, (Valid (I 0)))  
        (_) -> (c, printError "Error: Mismatched type when using '<' operator") 




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
