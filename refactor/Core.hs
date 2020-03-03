module Core where

import           Data.Map.Strict

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
type Error = String

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

data ArithOp = Add Expression Expression
             | Multiply Expression Expression
             | Divide Expression Expression
             deriving(Show, Eq)

data ListOp = Index Expression Expression
            | AssignIdx Expression Expression Expression
            | Append Expression Expression
            | Prepend Expression Expression
            | AddLists Expression Expression
            deriving(Show, Eq)

{- Context is used to store the state.  State in our language consists of
 - all variable bindings visible in the current scope. -}
type Context = Map Name Value


--Result is use to hold the results of expression evaluations, other than
--errors.
data Result = Valid Value | Nil | Error String
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
  Nothing -> (c, Error ("Undefined reference to variable " ++ s ++ "."))
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
      (Error err) ->
        (c', Error ("Error in evaluating If condition:" ++ indent err))
      _ ->
        if extractTruth r then foldExpressions et c' else foldExpressions ef c'

--WHILE
eval (While cnd es) c =
  let (c', r) = eval cnd c
  in  case r of
        (Error err) ->
          (c', Error ("Error in evaluating While condition:" ++ indent err))
        _ -> if extractTruth r
          then let (c'', r') = foldExpressions es c' in eval (While cnd es) c''
          else (c', r)

--VARIABLE BINDING
eval (Bind s ex) c =
  let val = eval ex c
  in
    case val of
      (c', Valid v) -> (c'', Valid v)
        where c'' = Data.Map.Strict.insert s v c'
      (c', Error err) ->
        ( c'
        , Error
          ("Could not assign non-value to variable " ++ s ++ "." ++ indent err)
        )
      (c', Nil) -> (c'', Nil) where c'' = Data.Map.Strict.delete s c'

--LIST OPERATIONS
eval (ListExp op  ) c = listHelper op c

--COMPARATORS
eval (LessThan l r) c = undefined --TODO

--Emergency error handling.
eval e              c = (c, Error ("UNHANDLED EVAL CASE: " ++ show e))

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
        (Error err, _ ) -> (c', Error err)
        (_        , []) -> (c', r)
        _               -> foldExpressions es c'
foldExpressions [] c = (c, Nil)

{- arithHelper is simply a helper function to handle arithmetic Expression
 - evaluations. -}
arithHelper :: ArithOp -> Domain
--ADDITION
arithHelper (Add l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Error errL, Error errR) ->
          (c'', Error (errString ++ indent errL ++ indent errR))
        (Error errL , _          ) -> (c'', Error (errString ++ indent errL))
        (_          , Error errR ) -> (c'', Error (errString ++ indent errR))
        (Valid (I a), Valid (I b)) -> (c'', Valid (I (a + b)))
        (Valid (S a), Valid (S b)) -> (c'', Valid (S (a ++ b)))
        _ -> (c'', Error (errString ++ "Operands are of non-addable types."))
  where errString = "Invalid operands to add: "
--MULTIPLICATION
arithHelper (Multiply (Lit (I l)) (Lit (I r))) c = (c, Valid (I (l * r)))
--Multiplying a string by an integer should, because it's fun, return that 
--string concatenated that many times.
arithHelper (Multiply (Lit (S l)) (Lit (I r))) c
  | r < 0     = (c, Error "Cannot multiply a string by a negative number.")
  | r == 0    = (c, Valid (S ""))
  | r == 1    = (c, Valid (S l))
  | otherwise = (c', Valid (S (l ++ l')))
 where
  (c', Valid (S l')) = arithHelper (Multiply (Lit (S l)) (Lit (I (r - 1)))) c
--Rather than repeat the boilerplate, we'll just define I * S as S * I.
arithHelper (Multiply (Lit (I l)) (Lit (S r))) c =
  arithHelper (Multiply (Lit (S r)) (Lit (I l))) c
arithHelper (Multiply (Lit _) (Lit _)) c =
  (c, Error "Invalid operands to multiply.")
arithHelper (Multiply l r) c =
  let (c' , l') = eval l c
      (c'', r') = eval r c'
  in  case (l', r') of
        (Nil    , y      ) -> (c'', y)
        (x      , Nil    ) -> (c'', x)
        (Error l, Error r) -> (c'', Error (errStr ++ indent l ++ indent r))
        (Error l, _      ) -> (c'', Error (errStr ++ indent l))
        (_      , Error r) -> (c'', Error (errStr ++ indent r))
        (Valid a, Valid b) -> arithHelper (Multiply (Lit a) (Lit b)) c
  where errStr = "Invalid operands to multiply:"
arithHelper (Divide l r) c = undefined --TODO

{-listHelper implements List operations.-}
listHelper :: ListOp -> Domain
--INDEX INTO LIST
listHelper (Index e l) c =
  let (c' , e') = eval e c
      (c'', l') = eval l c'
  in  case (e', l') of
        (Valid (I a), Valid (List xs)) -> grabIndex a xs c''
        (Error errL, Error errR) ->
          (c'', Error (errStr ++ indent errL ++ indent errR))
        (Error errL, _         ) -> (c'', Error (errStr ++ indent errL))
        (_         , Error errR) -> (c'', Error (errStr ++ indent errR))
        _                        -> (c'', Error errStr)
  where errStr = "Invalid operands to Index."

listHelper (Append  e l) c = undefined --TODO
listHelper (Prepend e l) c = undefined --TODO

--ASSIGN TO LIST INDEX
listHelper (AssignIdx i e l) c =
  let (c'  , e') = eval e c
      (c'' , l') = eval l c'
      (c''', i') = eval i c''
  in  case (i', e', l') of
        (Valid (I a), Valid d, Valid (List xs)) -> if a >= length xs || a < 0
          then (c''', Error "AssignIdx: Out of Bounds")
          else (c''', Valid (List (changeIndex a d xs)))
        _ -> (c, Error "Invalid Arguments to AssignIdx")

--CONCATENATE LISTS

listHelper (AddLists e l) c =
  let (c' , e') = eval e c
      (c'', l') = eval l c'
  in  case (e', l') of
        (Valid (List a), Valid (List xs)) -> (c'', Valid (List (a ++ xs)))
        _ -> (c'', Error "Invalid Arguments to AddLists")

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
bindArguments (ps, fs) _ [] =
  Right (ps, Error "Error: Mismatch between parameter and argument counts.")
bindArguments (ps, fs) [] _ =
  Right (ps, Error "Error: Mismatch between parameter and argument counts.")
bindArguments (ps, fs) (n : ns) (e : es) =
  let (ps', r) = eval e ps
  in
    case r of
      Valid v -> bindArguments (ps', Data.Map.Strict.insert n v fs) ns es
      Nil     -> bindArguments (ps', fs) ns es
      Error err ->
        Right
          ( ps'
          , Error ("Error in binding scope for function call:" ++ indent err)
          )


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
  in
    case fn of
      Just (Fn params body) ->
        let bindings = bindArguments (c, transferFuncDefs c) params e
        in  case bindings of
              Left (c', fnScope) ->
                let (_, r) = foldExpressions body fnScope in (c', r)
              Right (c', Error err) -> (c', Error err)
      Nothing ->
        ( c
        , Error
          ("Error :Function call to " ++ fname ++ " failed: no such function.")
        )
      _ ->
        ( c
        , Error
          (  "Error: Function call to"
          ++ fname
          ++ "failed: name is bound to non-function variable."
          )
        )

indent :: String -> String
indent s = "\n\t" ++ s
