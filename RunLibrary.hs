module RunLibrary where

import           Core
import           Sugar
import           Data.HashMap.Strict
import           Prelude                 hiding ( subtract
                                                , and
                                                , or
                                                , not
                                                )
--LAUNCHER

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


--LIBRARY

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
  [ ("doubler", doubler)
  , ("fib"    , fib)
  , ("maplist", maplist)
  , ("not"    , not)
  , ("and"    , and)
  , ("or"     , or)
  , ("xor"    , xor)
  , ("nor"    , nor)
  , ("xnor"   , xnor)
  ]



--Library function that just adds an argument to itself and returns the new
--value.
doubler :: Value
doubler = Fn ["x"] [Add (Var "x") (Var "x")]

--Logical operation functions, all deriving from the Core Nand.
not :: Value
not = Fn ["p"] [Nand (Var "p") (Var "p")]

and :: Value
and =
  Fn ["p", "q"] [Nand (Nand (Var "p") (Var "q")) (Nand (Var "p") (Var "q"))]

or :: Value
or = Fn ["p", "q"] [Nand (Call "not" [Var "p"]) (Call "not" [Var "q"])]

nor :: Value
nor = Fn ["p", "q"] [Call "not" [Call "or" [Var "p", Var "q"]]]

xor :: Value
xor = Fn
  ["p", "q"]
  [Call "and" [Call "or" [Var "p", Var "q"], Nand (Var "p") (Var "q")]]

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

--DEMO PROGRAMS

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

  -- | Examples of bad programs that produce error results or unexpected behavior
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
---- 5. Accessing out of bounds element in list via while loop
baddemo5 :: Value
baddemo5 = Fn
  []
  [
    Assign "idx" (Val (I 0)),
    Assign "badLen" (Val (I 4)),
    Assign "list" (Val (List [I 2, I 3, I 4])),
    Assign "val" (Val (I 9)),
    While (Call "not" [Equ (Var "idx") (Var "badLen")])
    [
      Assign "list" (AssignIdx (Var "idx") 
                (Var "val") 
                (Var "list")),
      increment "idx"
    ]
  ]
---- 6. Assigning non-value to variable and calling undefined function 
baddemo6 :: Value
baddemo6 = Fn
  []
  [
    Assign "result" (Call "func" [Val (I 2)]) 
  ]

---- 7. Args to functions are passed by value, this demo defines a variable
--      and a function to increment the variable. The variable is then "returned"
--      after calling the function and te value has no changed.  
baddemo7 :: Value 
baddemo7 = Fn
  []
  [
    Assign "num" (Val (I 5)),
    define "add1" ["val"] [increment ":rval"],
    Call "add1" [Var "num"],
    Var "num"
  ]


--Helper function to run the baddemo progs 
runBadDemo :: Int -> Result
runBadDemo n = run library
  (if n == 1 then baddemo1
  else if n == 2 then baddemo2
  else if n == 3 then baddemo3
  else if n == 4 then baddemo4
  else if n == 5 then baddemo5
  else if n == 6 then baddemo6
  else if n == 7 then baddemo7
  else noProg n)                  
noProg :: Int -> Value
noProg n = Fn [] [Val (S ("runBadDemo Error: Cannot find program baddemo" ++ (show n)))]


--Helper function to run the fibonacci demo; takes an int as an argument.
runFibonacci :: Int -> Result
runFibonacci n = run library (Fn [] [Call "fib" [Val (I n)]])

--Helper function to run the mapdemo demo.
runMapDemo :: Result
runMapDemo = run library mapdemo
