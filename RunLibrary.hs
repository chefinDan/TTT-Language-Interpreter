module RunLibrary where

import Core
import Sugar
import Data.HashMap.Strict
import Prelude hiding (subtract, and, or, not)

--This module holds the library, demo programs, and the logic for launching
--programs.


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
  [("doubler", doubler), ("fib", fib), ("maplist", maplist),
   ("not", not), ("and", and), ("or", or), ("xor", xor), 
   ("nor", nor), ("xnor", xnor)]

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

not :: Value
not = Fn ["p"] [Nand (Var "p") (Var "p")]

and :: Value
and = Fn ["p", "q"] 
   [Nand (Nand (Var "p") (Var "q")) (Nand (Var "p") (Var "q")) ]
or :: Value
or = Fn ["p", "q"] 
  [Nand (Call "not" [Var "p"]) (Call "not" [Var "q"])]
nor :: Value 
nor = Fn ["p", "q"] 
  [Call "not" [Call "or" [Var "p", Var "q"]]]

xor :: Value
xor = Fn ["p", "q"] 
  [Call "and" 
    [Call "or" [Var "p", Var "q"], Nand (Var "p") (Var "q")]]

xnor :: Value
xnor = Fn ["p", "q"] [Call "not" [Call "xor" [Var "p", Var "q"]]]

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

--DOCTESTS
--
--
