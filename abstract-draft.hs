import Data.HashMap.Strict

data Value =
    I Int
  | F Float
  | C Char
  | S String
  deriving (Eq, Show, Ord)
--  | L [Value]

data Result = Valid Value | Error [String]
  deriving Show

type List = [Value]
type Variable = String

data Expression =
    Val Value
  | Var Variable
  | Function Context [Expression] --First list is arguments, second list is code.
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

type Context = HashMap Variable Value

emptyContext :: Context
emptyContext = Data.HashMap.Strict.empty

type Domain = Context -> Expression -> (Context, Result)

parseOutput :: (Context, Result) -> IO ()
parseOutput (_, Valid v) = print v
parseOutput (_, Error e) = putStrLn (concat e)

eval :: Domain
--Just a literal or a variable.
eval c (Val v) = (c, Valid v)
eval c (Var s) = case Data.HashMap.Strict.lookup s c of
  Just x -> (c, Valid x)
  Nothing -> (c, Error ["Undefined reference to variable " ++ s ++ ".\n"])
--Assignment of variable.
eval c (Assign s ex) =
  let val = eval c ex in
    case val of
      (c', Valid v) -> (c'', Valid v)
        where c'' = Data.HashMap.Strict.insert s v c'
      (c', Error x) -> (c', Error (["Could not assign non-value to variable" ++ s ++ ".\n"] ++ Prelude.map ("  " ++) x))
--Addition.
eval c (Add (Val (I l)) (Val (I r))) = (c, Valid (I (l + r))) -- Int + Int
eval c (Add (Val (F l)) (Val (F r))) = (c, Valid (F (l + r))) -- Float + Float
eval c (Add (Val (F l)) (Val (I r))) = (c, Valid (F (l + fromIntegral r))) -- Float + Int
eval c (Add (Val (I l)) (Val (F r))) = (c, Valid (F (fromIntegral l + r))) -- Int + Float
eval c (Add (Val (S l)) (Val (S r))) = (c, Valid (S (l ++ r))) -- String + String
eval c (Add l r) =
  let (_, l') = eval c l
      (_, r') = eval c r
  in case (l', r') of
    (Error x, Error y) -> (c, Error (e ++ Prelude.map ("   " ++) x ++ Prelude.map ("   " ++) y ))
    (Error x, _) -> (c, Error (e ++ Prelude.map ("   " ++) x))
    (_, Error x) -> (c, Error (e ++ Prelude.map ("   " ++) x))
    (Valid a, Valid b) -> eval c (Add (Val a) (Val b))
    where e = ["Invalid operands to add:\n"]