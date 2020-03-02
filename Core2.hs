module Core where

{- Values are the basic data types available as literals.  Note that boolean
 - values are expressible via syntactic sugar. -}
data Value =
    I Int
  | S String
  | Fn [Name] [Expression]
  | List [Value]
  deriving (Show)

type Name = String


data Result = Valid Value | Nil
  deriving (Show, Eq)

data Error = String


{- Context is used to store the state.  State in our language consists of
 - all variable bindings visible in the current scope. -}
type Context = Map Name Value

type Domain = Context -> (Context, Either Result Error)

eval :: Expr -> Domain
eval = undefined


{- foldExpressions is the basic function for crunching a series of expressions
down to some final value.  The context is passed from expression to expression,
but intermediate results are basically rvalues and are discarded between lines; the
fold as a whole returns only the Result produced by the _final_ expression in the list.
(Or Nil, in the case of an empty list.)
Of course, if expressions are nested, then those rvalues have a use.  And for a
function or a program (which is a function itself,) the final rvalue produced
serves as the overall return value of the function. -}
foldExpressions :: [Expression] -> Context -> (Context, Result)
foldExpressions c (e : es) =
  let (c', r) = eval c e
  in  case es of
        [] -> (c', r)
        _  -> foldExpressions c' es
foldExpressions c [] = (c, Nil)
