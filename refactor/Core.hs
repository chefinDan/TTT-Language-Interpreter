module Core where

import           Data.Map.Strict
import           Prelude

{- Values are the basic data types available as literals.  Note that boolean
 - values are expressible via syntactic sugar. -}
data Value =
    I Int
  | S String
  | Fn [Name] [Expression]
  | List [Value]
  deriving (Show, Eq)

--Just types to make it clear what strings are being used for.
type Name = String

data Expression =
    Lit Value
  | Dereference Name
  | Call Name [Expression] --Call a function.  Name of function, arguments.
  | ArithExp ArithOp
  | ListExp ListOp
  | Equ Expression Expression
  | If Expression [Expression] [Expression]
  | While Expression [Expression]
  | Bind Name Expression
  | Nand Expression Expression
  | LessThan Expression Expression
  deriving(Show, Eq)


--Helper expression, just to keep function definitions reasonable.
data ArithOp = Add Expression Expression
             | Multiply Expression Expression
             | Divide Expression Expression
             deriving(Show, Eq)
--As above; helper expression.
data ListOp = Index Expression Expression
            | AssignIdx Expression Expression Expression
            | Append Expression Expression
            | Prepend Expression Expression
            | AddLists Expression Expression
            deriving(Show, Eq)

{- Context is used to store the state.  State in our language consists of
 - all variable bindings visible in the current scope. -}
type Context = Map Name Value


--Result is use to hold the results of expression evaluations.
--Nil is a special case used in list operations: it represents
--something which is neither a value nor an error.  In
--particular, we use it to indicate than an attempt to index
--into a list is Out of Bounds, which we don't view as an error:
--it provides a convenient way to iterate over a list without
--knowing the size.  We can't simply return an empty list or
--other Value tombstone, because a list could in fact contain
--an empty list.  Because Lists can legitimately contain any
--Value, no Value can be used to represent a non-Value.
data Result = Valid Value | Nil | Err Error
  deriving (Show, Eq)

{- Error is a recursive data type used for storing information
 - about runtime errors.  ErrType is defined at the bottom, it
 - has a constructor for each category of error.  By including
 - a list of Errors in Error, we can nest errors, such that we
 - can print a hierarchically indented list of all errors that
 - pertain to the Expression that halted execution.  Because of
 - course an Expression has Expressions as arguments, even
 - though we halt execution there can be a deep nest of errors
 - involved.  See the 'errornesting' demo program for a simple
 - example. -}
data Error = E ErrType [Error]
  deriving (Show, Eq)

{- Domain is the semantic domain for evaluating expressions. -}
type Domain = Context -> (Context, Result)

{- eval is the function for evaluating expressions. -}
eval :: Expression -> Domain

--LITERAL
eval (Lit         v) c = (c, Valid v)
--VARIABLE DEREFERENCING
eval (Dereference s) c = case Data.Map.Strict.lookup s c of
  Just x  -> (c, Valid x)
  Nothing -> (c, Err (E (DerefUnbound s) []))
--FUNCTION CALLING: see helper function call.
eval (Call s args) c = call s args c

--ARITHMETIC: uses a helper function.
eval (ArithExp op) c = arithHelper op c

--EQUALITY
eval (Equ l r) c | l' == r'  = (c'', Valid (I 1))
                 | otherwise = (c'', Valid (I 0))
 where
  (c' , l') = eval l c
  (c'', r') = eval r c'

--IF/ELSE
eval (If cnd et ef) c =
  let (c', r) = eval cnd c
  in
    case r of
      (Err e) -> (c', Err (E (BadConditional "If") [e]))
      _ ->
        if extractTruth r then foldExpressions et c' else foldExpressions ef c'

--WHILE
eval (While cnd es) c =
  let (c', r) = eval cnd c
  in  case r of
        (Err err) -> (c', Err (E (BadConditional "While") [err]))
        _         -> if extractTruth r
          then let (c'', r') = foldExpressions es c' in eval (While cnd es) c''
          else (c', r)

--VARIABLE BINDING
eval (Bind s ex) c =
  let val = eval ex c
  in  case val of
        (c', Valid v) -> (c'', Valid v)
          where c'' = Data.Map.Strict.insert s v c'
        (c', Err err) -> (c', Err (E (BindNotValue s) [err]))
        (c', Nil    ) -> (c'', Nil) where c'' = Data.Map.Strict.delete s c'

--LIST OPERATIONS
eval (ListExp op) c = listHelper op c

--COMPARATORS
eval (LessThan (Lit (I n1)) (Lit (I n2))) c | n1 < n2   = (c, Valid (I 1))
                                            | otherwise = (c, Valid (I 0))
eval (LessThan (Lit (S s1)) (Lit (S s2))) c | s1 < s2   = (c, Valid (I 1))
                                            | otherwise = (c, Valid (I 0))
eval (LessThan (Lit (I n)) (Lit (S s))) c =
  (c, Err (E (BadOperands "comparator") []))
eval (LessThan (Lit (S s)) (Lit (I n))) c =
  (c, Err (E (BadOperands "comparator") []))
eval (LessThan l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Nil  , _    ) -> (c'', Err (E (BadOperands "comparator") []))
        (_    , Nil  ) -> (c'', Err (E (BadOperands "comparator") []))
        (Err l, Err r) -> (c'', Err (E (BadOperands "comparator") [l, r]))
        (Err l, _    ) -> (c'', Err (E (BadOperands "comparator") [l]))
        (_    , Err r) -> (c'', Err (E (BadOperands "comparator") [r]))

        (Valid (I n1), Valid (I n2)) | n1 < n2   -> (c'', Valid (I 1))
                                     | otherwise -> (c'', Valid (I 0))
        (Valid (S s1), Valid (S s2)) | s1 < s2   -> (c'', Valid (I 1))
                                     | otherwise -> (c'', Valid (I 0))

--NAND
eval (Nand l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Err l, Err r) -> (c'', Err (E (BadOperands "Nand") [l, r]))
        (Err l, _    ) -> (c'', Err (E (BadOperands "Nand") [l]))
        (_    , Err r) -> (c'', Err (E (BadOperands "Nand") [r]))

        (n1   , n2   ) -> case (extractTruth n1, extractTruth n2) of
          (True, True) -> (c'', Valid (I 0))
          _            -> (c'', Valid (I 1))


{- foldExpressions is the basic function for crunching a series of expressions
down to some final value.  The context is passed from expression to expression,
but intermediate results are basically rvalues and are discarded between lines;
the fold as a whole returns only the Result produced by the _final_ expression
in the list. (Or Nil, in the case of an empty list.) Of course, if expressions
are nested, then those rvalues have a use.  And for a function or a program
(which is a function itself,) the final rvalue produced serves as the overall
return value of the function. -}

foldExpressions :: [Expression] -> Domain
foldExpressions (e : es) c =
  let (c', r) = eval e c
  in  case (r, es) of
        (Err err, _ ) -> (c', Err err)
        (_      , []) -> (c', r)
        _             -> foldExpressions es c'
foldExpressions [] c = (c, Nil)

{- arithHelper is simply a helper function to handle arithmetic Expression
 - evaluations. -}
arithHelper :: ArithOp -> Domain
--ADDITION
arithHelper (Add l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Err errL, Err errR) -> (c'', Err (E (BadOperands "Add") [errL, errR]))
        (Err errL   , _          ) -> (c'', Err (E (BadOperands "Add") [errL]))
        (_          , Err errR   ) -> (c'', Err (E (BadOperands "Add") [errR]))
        (Valid (I a), Valid (I b)) -> (c'', Valid (I (a + b)))
        (Valid (S a), Valid (S b)) -> (c'', Valid (S (a ++ b)))
        _                          -> (c'', Err (E (BadOperands "Add") []))
--MULTIPLICATION
arithHelper (Multiply (Lit (I l)) (Lit (I r))) c = (c, Valid (I (l * r)))
--Multiplying a string by an integer should, because it's fun, return that
--string concatenated that many times.
arithHelper (Multiply (Lit (S l)) (Lit (I r))) c
  | r < 0     = (c, Err (E MultiplyStringByNegative []))
  | r == 0    = (c, Valid (S ""))
  | r == 1    = (c, Valid (S l))
  | otherwise = (c', Valid (S (l ++ l')))
 where
  (c', Valid (S l')) = arithHelper (Multiply (Lit (S l)) (Lit (I (r - 1)))) c
--Rather than repeat the boilerplate, we'll just define I * S as S * I.
arithHelper (Multiply (Lit (I l)) (Lit (S r))) c =
  arithHelper (Multiply (Lit (S r)) (Lit (I l))) c
arithHelper (Multiply (Lit _) (Lit _)) c =
  (c, Err (E (BadOperands "Multiply") []))
arithHelper (Multiply l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Nil    , y      ) -> (c'', y)
        (x      , Nil    ) -> (c'', x)
        (Err l  , Err r  ) -> (c'', Err (E (BadOperands "Multiply") [l, r]))
        (Err l  , _      ) -> (c'', Err (E (BadOperands "Multiply") [l]))
        (_      , Err r  ) -> (c'', Err (E (BadOperands "Multiply") [r]))
        (Valid a, Valid b) -> arithHelper (Multiply (Lit a) (Lit b)) c

-- Division
arithHelper (Divide _ (Lit (I 0))) c = (c, Err (E DivideByZero []))
arithHelper (Divide _ (Lit (S s))) c = (c, Err (E (BadOperands "divide") []))
arithHelper (Divide (Lit (S s)) (Lit (I n))) c = (c, Valid (substring n (S s)))
arithHelper (Divide (Lit (I n)) (Lit (I d))) c = (c, Valid (I (n `div` d)))
arithHelper (Divide (Lit _) (Lit _)) c = (c, Err (E (BadOperands "divide") []))

arithHelper (Divide l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Nil    , _      ) -> (c, Err (E (BadOperands "divide") []))
        (_      , Nil    ) -> (c, Err (E (BadOperands "divide") []))
        (Err l  , Err r  ) -> (c, Err (E (BadOperands "divide") [l, r]))
        (Err l  , _      ) -> (c, Err (E (BadOperands "divide") [l]))
        (_      , Err r  ) -> (c, Err (E (BadOperands "divide") [r]))
        (Valid n, Valid d) -> arithHelper (Divide (Lit n) (Lit d)) c


-- substring for string division
substring :: Int -> Value -> Value
substring x (S text) = S (Prelude.take n text) where n = length text `div` x


{-listHelper implements List operations.-}
listHelper :: ListOp -> Domain
--INDEX INTO LIST
listHelper (Index e l) c =
  let (c' , e') = eval e c
      (c'', l') = eval l c'
  in  case (e', l') of
        (Valid (I a), Valid (List xs)) -> grabIndex a xs c''
        (Err errL, Err errR) ->
          (c'', Err (E (BadOperands "Index") [errL, errR]))
        (Err errL, _       ) -> (c'', Err (E (BadOperands "Index") [errL]))
        (_       , Err errR) -> (c'', Err (E (BadOperands "Index") [errR]))
        _                    -> (c'', Err (E (BadOperands "Index") []))


--ASSIGN TO LIST INDEX
listHelper (AssignIdx i e l) c =
  let (c'  , e') = eval e c
      (c'' , l') = eval l c'
      (c''', i') = eval i c''
  in  case (i', e', l') of
        (Valid (I a), Valid d, Valid (List xs)) -> if a >= length xs || a < 0
          then (c''', Err (E (IdxOutOfBounds a) []))
          else (c''', Valid (List (changeIndex a d xs)))
        _ -> (c, Err (E (BadOperands "Index Assignment") []))

-- ADD TO END OF LIST
listHelper (Append e l) c =
  let (c' , e') = eval e c
      (c'', l') = eval l c'
  in  case (e', l') of
        (Valid (List xs), Valid (List ys)) -> (c'', Valid (List (ys ++ xs)))
        (Valid a, Valid (List xs)) ->
          eval (ListExp $ AddLists (Lit (List xs)) (Lit (List [a]))) c''
        (_, _) -> (c'', Err (E (BadOperands "Append to List") []))


-- ADD TO START OF LIST
listHelper (Prepend e l) c =
  let (c' , e') = eval e c
      (c'', l') = eval l c'
  in  case (e', l') of
        (Valid (List xs), Valid (List ys)) -> (c'', Valid (List (xs ++ ys)))
        (Valid a, Valid (List xs)) ->
          eval (ListExp $ AddLists (Lit (List [a])) (Lit (List xs))) c''
        (_, _) -> (c'', Err (E (BadOperands "Prepend to List") []))

--CONCATENATE LISTS
listHelper (AddLists e l) c =
  let (c' , e') = eval e c
      (c'', l') = eval l c'
  in  case (e', l') of
        (Valid (List a), Valid (List xs)) -> (c'', Valid (List (a ++ xs)))
        _ -> (c'', Err (E (BadOperands "List Concatenation") []))

-- A helper function for AssignIdx
changeIndex :: Int -> Value -> [Value] -> [Value]
changeIndex i d [] = if i == 0 then [d] else []
changeIndex i d (x : xs) =
  if i == 0 then d : xs else x : changeIndex (i - 1) d xs

--A helper function for Index.
grabIndex :: Int -> [Value] -> Context -> (Context, Result)
grabIndex i [] c = (c, Nil)
grabIndex i xs c = if length xs > i then (c, Valid (xs !! i)) else (c, Nil)


--extractTruth is used to assign some truth value to a Result.  This allows
--things like If and While structures to operate, of course.
extractTruth :: Result -> Bool
extractTruth (Valid (I 0 )) = False
extractTruth (Valid (S "")) = False
extractTruth Nil            = False
extractTruth _              = True


{- valueIsFunc and transferFuncDefs are used to filter out everything except
function variables from a Context.  The purpose is to allow functions to have
global scope, by unioning a filtered inbound context with the bound argument
list of the function call itself.  It's also used to allow functions defined
inside a function to persist into the parent, because again, they're globals.-}
valueIsFunc :: Value -> Bool
valueIsFunc (Fn _ _) = True
valueIsFunc _        = False

--transferFuncDefs is used in scope creation; it takes a Context and returns
--a new Context from which all non-function variable bindings have been
--excised.

--Nonce types for clarity.
type ParentScope = Context
type FunctionScope = Context

transferFuncDefs :: ParentScope -> FunctionScope
transferFuncDefs = Data.Map.Strict.filter valueIsFunc

--For bind arguments, we produce a tuple of two Contexts; the first is to preserve
--any modifications to the original context produced as side effects of evaluating
--the argument list; the second is the bound arguments for the new scope.

type BindResult = Either (ParentScope, FunctionScope) (ParentScope, Result)

bindArguments
  :: (ParentScope, FunctionScope) -> [Name] -> [Expression] -> BindResult
bindArguments (ps, fs) [] [] = Left (ps, fs)
bindArguments (ps, fs) _  [] = Right (ps, Err (E ParameterMismatch []))
bindArguments (ps, fs) [] _  = Right (ps, Err (E ParameterMismatch []))
bindArguments (ps, fs) (n : ns) (e : es) =
  let (ps', r) = eval e ps
  in  case r of
        Valid v -> bindArguments (ps', Data.Map.Strict.insert n v fs) ns es
        Nil     -> bindArguments (ps', fs) ns es
        Err err -> Right (ps', Err (E ParameterBind [err]))


{- call calls a function.  The general idea is that the list of expressions
provided as arguments is evaluated, and bound pairwise with the function's
parameter name list into variables in a new Context, the function's scope.  Any
side effects of those expressions are preserved in the parent Context, and all
function definitions in the parent are transferred into the child scope.  After
the call is complete, any functions defined inside the call are transferred to
the original scope, overwriting in case of collision with an existing name.  The
call itself then evaluates down to the (possibly modifed) parent scope and the
result of the last body expression.-}
call :: Name -> [Expression] -> Context -> (Context, Result)
call fname e c =
  let fn = Data.Map.Strict.lookup fname c
  in  case fn of
        Just (Fn params body) ->
          let bindings = bindArguments (c, transferFuncDefs c) params e
          in  case bindings of
                Left (c', fnScope) ->
                  let (_, r) = foldExpressions body fnScope in (c', r)
                Right (c', Err err) -> (c', Err err)
        Nothing -> (c, Err (E (CallUnboundName fname) []))
        _       -> (c, Err (E (CallNotAFunc fname) []))

{- ErrType is used to specify the exact nature of an error.  RunLibrary has
 - functions for rendering human-readable strings from this. -}

data ErrType =
    BadOperands String
  | BadConditional String
  | MultiplyStringByNegative
  | CallUnboundName String
  | CallNotAFunc String
  | DerefUnbound String
  | BindNotValue String
  | UnhandledEval String
  | AssignIdxOOB
  | ParameterMismatch
  | ParameterBind
  | DivideByZero
  | IdxOutOfBounds Int

  deriving (Eq, Show)
