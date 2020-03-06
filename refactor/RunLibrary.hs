module RunLibrary where

import           Core
import           Sugar
import           Data.Map.Strict
import           Prelude                 hiding ( subtract
                                                , and
                                                , or
                                                , not
                                                )
--LAUNCHER

--run is the function that actually launched a program.  It is passed a context,
--which will generally be the library, and a function, which it will bind to
--the name "main" in that context, and execute.
run :: Value -> Context -> IO ()
run (Fn n e) c =
  let c'     = Data.Map.Strict.insert "main" (Fn n e) c
      (_, r) = call "main" [] c'
  in  unwrapReturn r
run _ _ = putStrLn
  "Could not launch program: second argument to run must be a function."

unwrapReturn :: Result -> IO ()
unwrapReturn (Err err)    = printErrors (stringifyErrors err)
unwrapReturn Nil          = putStrLn "Nil"
unwrapReturn (Valid rslt) = case rslt of
  (I    i ) -> print i
  (S    s ) -> putStrLn s
  (List l ) -> print l
  (Fn n fn) -> print fn


errTypeToString :: ErrType -> String
errTypeToString (BadOperands    s) = "Invalid operands to " ++ s ++ "."
errTypeToString (BadConditional s) = "Faulty conditional in " ++ s ++ "."
errTypeToString (CallUnboundName s) =
  "Function call failed: Name \""
    ++ s
    ++ "\" is not bound to a value in current scope."
errTypeToString (CallNotAFunc s) =
  "Function call failed: Name \""
    ++ s
    ++ "\" is bound to a non-function value.\n"
errTypeToString (DerefUnbound s) =
  "Could not defererence name \""
    ++ s
    ++ "\" not bound to any value in current scope."
errTypeToString ParameterMismatch =
  "Mismatch between parameter and argument counts.\n"
errTypeToString ParameterBind     = "Error while binding function parameters."
errTypeToString (UnhandledEval s) = "UNHANDLED EVAL CASE: " ++ s ++ "\n"
errTypeToString MultiplyStringByNegative = "Cannot multiply a string by a negative number." 
errTypeToString (BindNotValue s) = "Error in binding \"" ++ s ++ "\": error in expression to be bound."
--Catch-all:
errTypeToString x =
  "Error in reporting error: ErrorType \"" ++ show x ++ "\" has no defined string."

stringifyErrors :: Error -> [String]
stringifyErrors (E e []) = [errTypeToString e]
stringifyErrors (E e xs) =
  errTypeToString e : Prelude.map ("  " ++) (concatMap stringifyErrors xs)

printErrors :: [String] -> IO ()
printErrors [e     ] = putStrLn e
printErrors (e : es) = putStrLn (e ++ concatMap ("\n" ++) es)

--LIBRARY

--This module holds the library, demo programs, and the logic for launching
--programs.


--Just an empty context, useful for various purposes.
emptyContext :: Context
emptyContext = Data.Map.Strict.empty

--Since our library is implemented as a Context containing bindings for
--various library functions, we have this function to pre-populate it.

buildLibrary :: Context -> [(Name, Value)] -> Context
buildLibrary c []             = c
buildLibrary c ((n, fn) : ts) = buildLibrary (Data.Map.Strict.insert n fn c) ts



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
doubler = Fn ["x"] [ArithExp (Add (Dereference "x") (Dereference "x"))]

runDoubler :: Int -> IO ()
runDoubler n = run (Fn [] [Call "doubler" [Lit (I n)]]) library

--Simple naive Fibonacci implementation.
fib :: Value
fib = Fn
  ["n"]
  [ If
      (Equ (Dereference "n") (Lit (I 0)))
      [--then
       Lit (I 0)]
      [--else
        If
          (Equ (Dereference "n") (Lit (I 1)))
          [--then
           Lit (I 1)]
          [--else
            ArithExp
              (Add (Call "fib" [subtract (Dereference "n") (Lit (I 1))])
                   (Call "fib" [subtract (Dereference "n") (Lit (I 2))])
              )
          ]
      ]
  ]


runFibonacci :: Int -> IO ()
runFibonacci n = run (Fn [] [Call "fib" [Lit (I n)]]) library

--Logical operation functions, all deriving from the Core Nand.
not :: Value
not = Fn ["p"] [Nand (Dereference "p") (Dereference "p")]

and :: Value
and = Fn
  ["p", "q"]
  [ Nand (Nand (Dereference "p") (Dereference "q"))
         (Nand (Dereference "p") (Dereference "q"))
  ]

or :: Value
or = Fn ["p", "q"]
        [Nand (Call "not" [Dereference "p"]) (Call "not" [Dereference "q"])]

nor :: Value
nor = Fn ["p", "q"] [Call "not" [Call "or" [Dereference "p", Dereference "q"]]]

xor :: Value
xor = Fn
  ["p", "q"]
  [ Call
      "and"
      [ Call "or" [Dereference "p", Dereference "q"]
      , Nand (Dereference "p") (Dereference "q")
      ]
  ]

xnor :: Value
xnor =
  Fn ["p", "q"] [Call "not" [Call "xor" [Dereference "p", Dereference "q"]]]

--Maplist takes as arguments a function and a list, and maps that function over
--each item in the list, returning the new, modified list.
maplist :: Value
maplist = Fn
  ["fn", "input"]
  [ Bind "i" (Lit (I 0))
  , While
    (ListExp (Index (Dereference "i") (Dereference "input")))
    [ Bind
      "input"
      (ListExp
        (AssignIdx
          (Dereference "i")
          (Call "fn" [ListExp (Index (Dereference "i") (Dereference "input"))])
          (Dereference "input")
        )
      )
    , increment "i"
    ]
  , Dereference "input"
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
  [ Bind "ints"    (Lit (List [I 10, I 20, I 30]))
  , Bind "output"  (Call "maplist" [Dereference "doubler", Dereference "ints"])
  , Bind "strings" (Lit (List [S "foo", S "bar", S "baz"]))
  , ListExp
    (AddLists
      (Dereference "output")
      (Call
        "maplist"
        [ Lit (Fn ["str"] [ArithExp (Multiply (Dereference "str") (Lit (I 3)))])
        , Dereference "strings"
        ]
      )
    )
  ]


--Helper function to run the mapdemo demo.
runMapDemo :: IO ()
runMapDemo = run mapdemo library

{- errornesting demonstrates our error handling:  It's a two line
 - function with an error on the first line, but that first line 
 - is a complicated nested call.  The output is a nested series
 - of errors about invalid operands to add, terminating in an
 - error complaining that "Boo" is undefined.  The second line
 - of the function is never executed. -}
errornesting :: Value
errornesting = Fn
  []
  [ ArithExp (Add (Lit (I 1)) 
     (ArithExp (Add (Lit (I 1))
       (ArithExp (Add (Lit (I 1))
         (ArithExp (Add (Lit (I 1)) (Dereference "BOO!"))))))))
  , Dereference "The program should never get here!"
  ]


--  Examples of bad programs that produce error results or unexpected behavior
---- 1. Attempts to add a string to an int, result is Error

baddemo1 :: Value
baddemo1 = Fn
  []
  [ Bind "val1" (Lit (I 2))
  , Bind "val2" (Lit (S "bad"))
  , ArithExp (Add (Dereference "val1") (Dereference "val2"))
  ]


---- 2. Attempts to multiply an int literal by an undefined variable,  
baddemo2 :: Value
baddemo2 = Fn
  []
  [
    ArithExp (Multiply (Lit (I 2)) (Dereference "val"))
  ]
---- 3. Attempts to Multiply a string by a negative number 
baddemo3 :: Value
baddemo3 = Fn
  []
  [
    ArithExp (Multiply (Lit (S "oops")) (Lit (I (-2))))
  ]
---- 4. Division by zero 

{-
baddemo4 :: Value
baddemo4 = Fn
  []
  [
    Bind "zero" (Lit (I 0)),
    Divide (Lit (I 2)) (Dereference "zero")
  ]
-}

---- 5. Accessing out of bounds element in list via while loop
baddemo5 :: Value
baddemo5 = Fn
  []
  [
    Bind "idx" (Lit (I 0)),
    Bind "badLen" (Lit (I 4)),
    Bind "list" (Lit (List [I 2, I 3, I 4])),
    Bind "val" (Lit (I 9)),
    While (Call "not" [Equ (Dereference "idx") (Dereference "badLen")])
    [
      Bind "list" (ListExp (AssignIdx (Dereference "idx") 
                (Dereference "val") 
                (Dereference "list"))),
      increment "idx"
    ]
  ]
---- 6. Assigning non-value to variable and calling undefined function 
baddemo6 :: Value
baddemo6 = Fn
  []
  [
    Bind "result" (Call "func" [Lit (I 2)]) 
  ]

---- 7. Args to functions are passed by value, this demo defines a variable
--      and a function to increment the variable. The variable is then "returned"
--      after calling the function and te value has no changed.  
baddemo7 :: Value 
baddemo7 = Fn
  []
  [
    Bind "num" (Lit (I 5)),
    define "add1" ["val"] [increment ":rval"],
    Call "add1" [Dereference "num"],
    Dereference "num"
  ]


--Helper function to run the baddemo progs 

{-
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
noProg n = Fn [] [Lit (S ("runBadDemo Error: Cannot find program baddemo" ++ (show n)))]
-}
