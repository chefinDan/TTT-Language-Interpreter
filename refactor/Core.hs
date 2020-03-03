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

--Literal Expression.
eval (Lit         v) c = (c, Valid v)
--Variable dereferencing.
eval (Dereference s) c = case Data.Map.Strict.lookup s c of
  Just x  -> (c, Valid x)
  Nothing -> (c, Error ("Undefined reference to variable " ++ s ++ "."))
--Function calling: see helper function call.
eval (Call s args) c = call s args c

eval (ArithExp op) c   = arithHelper op c

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

arithHelper :: ArithOp -> Domain
arithHelper (Add l r) c =
  let
    (c' , l') = eval l c
    (c'', r') = eval r c'
  in
    case (l', r') of
      (Error errL, Error errR) ->
        (c'', Error (errString ++ indent errL ++ indent errR))
      (Error errL, _         ) -> (c'', Error (errString ++ indent errL))
      (_         , Error errR) -> (c'', Error (errString ++ indent errR))
      (Valid (I a), Valid (I b)) ->
        (c'', Valid (I (a + b)))
      (Valid (S a), Valid (S b)) ->
        (c'', Valid (S (a ++ b)))
      _ -> (c'', Error (errString ++ "Operands are of non-addable types."))
  where errString = "Invalid operands to add:"
arithHelper (Multiply l r) c = undefined
arithHelper (Divide   l r) c = undefined


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
      Valid v ->
        bindArguments (ps', Data.Map.Strict.insert n v fs) ns es
      Nil -> bindArguments (ps', fs) ns es
      Error err ->
        Right (ps', Error ("Error in binding scope for function call:" ++ indent err))


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
              Left  (c', fnScope) -> foldExpressions body fnScope
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
