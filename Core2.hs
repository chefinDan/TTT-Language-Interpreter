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

type Name = String

type Error = String

data Result = Valid Value | Nil
  deriving (Show, Eq)

type Return = Either Result Error



data Expression =
    Lit Value
  | Dereference Name
  | Call Name [Expression] --Call a function.  Name of function, arguments.
  | ArithOp
  | ListOp
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

data ListOp = Index Expression Expression
            | AssignIdx Expression Expression Expression
            | Append Expression Expression
            | Prepend Expression Expression
            | AddLists Expression Expression

{- Context is used to store the state.  State in our language consists of
 - all variable bindings visible in the current scope. -}
type Context = Map Name Value

type Domain = Context -> (Context, Return)

eval :: Expression -> Domain
eval = undefined


{- foldExpressions is the basic function for crunching a series of expressions
down to some final value.  The context is passed from expression to expression,
but intermediate results are basically rvalues and are discarded between lines;
the fold as a whole returns only the Result produced by the _final_ expression
in the list. (Or Nil, in the case of an empty list.) Of course, if expressions
are nested, then those rvalues have a use.  And for a function or a program
(which is a function itself,) the final rvalue produced serves as the overall
return value of the function. -}


foldExpressions :: [Expression] -> Context -> (Context, Return)
foldExpressions (e : es) c =
  let (c', r) = eval e c
  in  case (r, es) of
        (Right err, _ ) -> (c', Right err)
        (_        , []) -> (c', r)
        _               -> foldExpressions es c'
foldExpressions [] c = (c, Left Nil)


--exctractTruth is used to assign some truth value to a Result.  This allows
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

bindArguments
  :: (ParentScope, FunctionScope)
  -> [Name]
  -> [Expression]
  -> (ParentScope, FunctionScope)
bindArguments (ps, fs) [] _  = (ps, fs)
bindArguments (ps, fs) _  [] = (ps, fs)
bindArguments (ps, fs) (n : ns) (e : es) =
  let (ps', r) = eval e ps
  in  case r of
        Left (Valid v) -> bindArguments (ps', Data.Map.Strict.insert n v fs) ns es
        _       -> bindArguments (ps', fs) ns es


{- call calls a function.  The general idea is that the list of expressions
provided as arguments is evaluated, and bound pairwise with the function's
parameter name list into variables in a new Context, the function's scope.  Any
side effects of those expressions are preserved in the parent Context, and all
function definitions in the parent are transferred into the child scope.  After
the call is complete, any functions defined inside the call are transferred to
the original scope, overwriting in case of collision with an existing name.  The
call itself then evaluates down to the (possibly modifed) parent scope and the
result of the last body expression.-}
call :: Name -> [Expression] -> Context -> (Context, Return)
call fname e c =
  let fn = Data.Map.Strict.lookup fname c
  in  case fn of
        Just (Fn params body) ->
          let (c'    , scope) = bindArguments (c, transferFuncDefs c) params e
              (scope', r    ) = foldExpressions body scope
          in  (Data.Map.Strict.union (transferFuncDefs scope') c', r)
        Nothing ->
          (c, Right ("Error :Function call to " ++ fname ++ " failed: no such function."))
        _ ->
          ( c
          , Right ("Error: Function call to"
            ++ fname
            ++ "failed: name is bound to non-function variable.")
          )
