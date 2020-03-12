# Project: A programming language interpreter
## Overview

Michael Andrews andrmich
Daniel Green greendan
Nicholas Matsumoto matsumon
Quan Nguyen nguyenq2

## Introduction

Our language, Untitled Group Project (abbreviated TTT and pronounced 'French',)
is a pass-by-value imperative language.  TTT's most interesting features include
first-class functions and list operations.

## Executing Programs

TTT programs, at present, are run inside of ghci.  The functions to run programs
reside in the RunLibrary module.

The command would be:
	ghci RunLibrary.hs

Programs in this language are functions, so the 'run' command is used to pass
in a starting context and bootstrap execution.  Functions to simplify this
are provided for the demo programs.

## Demo Programs

### Fibonacci calculator

This is a simple, naive recursive Fibonacci implementation.  It demonstrates,
among other things, recursion, the use of the if-then-else construct, the equality
operator, and the use of the context/scope: it relies both on local variables and
the globally-scoped function variables.

To run it:
	runFibonacci n
		where n is some positive integer.

The expected output is:
	Valid (I x)
		where x is the nth number in the Fibonacci series

Note: Because this is a naive implementation, be mindful of the time.  30 is a
good upper limit before patience starts being a requirement.

### List Mapping Demo.

This program takes advantage of a library function that maps some function over
a list.  (This is directly analogous to mapping over a list in Haskell.)  
It creates two lists, one of integers and one of strings, and maps
a function over each list, eventually returning the modified resulting lists in
a list (of lists.)  It demonstrates, among other things, While loops, defining a
function from a literal, passing a function as an argument to a function,
reassignment of variables, list operations, arithmetic operators, and string
operators.

Specifically, it creates a list [10, 20, 30], and maps a function over that list
which doubles its argument, to produce [20, 40, 60].

It then creates a list ["foo", "bar", "baz] and maps a function over it which
multiplies its argument by three, producing 
["foofoofoo", "barbarbar", "bazbazbaz"].

The function passed to map in the first case is a bound variable, and in the
second case an unbound function literal is passed in, and bound inside the mapper.

To run it:
	runMapDemo

The expected output is:
	Valid (List [I 20,I 40,I 60,S "foofoofoo",S "barbarbar",S "bazbazbaz"])

### "Bad program" Demos.
There are 7 defined programs that demonstrate various erroneous or 
unexpected behavior. The programs are defined in the same way as mapdemo, 
and as such can be executed in the same way.

#### baddemo1
This program assigns an integer value to one variable, and a string value 
to another, then attempts to perform addition on the two variables. 
This operation produces an error because an integer cannot be 
arithmetically added to a string.

To run it:  
  runBadDemo1

The expected output is:
  Invalid operands to add.
  Error

#### baddemo2
This program defines a main function with one parameter. The parameter is undefined and is used in Multiply with an integer literal value.  
This operation produces an error because the variable used in the multiplication expression is undefined.

To run it:  
  runBadDemo2

The expected output is:
  Undefined reference to variable val.
  Invalid operands to multiply.
  Error

#### baddemo3
This program is similar to baddemo1 in that it uses a string as an operand in arithmetic operation. But in this case we have made the decision to support "string multiplication". This allows a user to multiply a string by a non-negative integer n and produce a new string that is concatenated with itself n-times.   
This operation produces an error because the integer used in the multiplication expression is negative.

To run it:  
  runBadDemo3

The expected output is:
  Cannot multiply a string by a negative number.
  Error

#### baddemo4
This program assigns the integer value 0 to a variable and attempts to use said variable as the denominator in a Divide Expression. This operation produces an error because division by zero is undefined.

To run it:  
  runBadDemo4

The expected output is:
  Denominator cannot be 0
  Error

#### baddemo5
This program declares a list of integers with 3 elements, a start index value of 0, and a value "val" to add to each element. Then the program iterates through the list using a while loop, adding val to each element.
This operation produces an error because the condition in the while loop allows the loop to iterate past the end of the list. Producing an "index out of bounds" error.
The last line in the program output is Valid (I 0); this indicates a "false" value. This is printed because the condition in the while loop is the last Expression evaluated, hence it is the function's return value.     

To run it:  
  runBadDemo5

The expected output is:
  AssignIdx: Out of Bounds
  Valid (I 0)

#### baddemo6
This program produces two different errors. First it attempts to call a undefined function, and then assigns the value of the undefined function to a variable.
This program produces errors because you cannot call an undefined function, nor can you assign a variable a non-value, in this cae "nil".    

To run it:  
  runBadDemo6

The expected output is:
  Function call to func failed: no such function.
  Could not assign non-value to variable result.
  Error

#### baddemo7
This program does not produce a error in the literal sense, but demonstrates possibly unexpected behavior due to the user not understanding that arguments to functions are pass-by-value only. A function is defined that takes a single parameter and increments that value by one. After calling the function, the value of the variable passed to the function has not changed. 

To run it:  
  runBadDemo7

The expected output is:
  Valid (I 5)

What the user might have expected:
  Valid (I 6)

#### baddemo8
List concatenation can only be done on List values. While in Haskell a string is just a list of chars, it is not so in our language. This program produces an invalid operands to List concatenation error.

To run it:  
  runBadDemo8

The expected output is:
  "Invalid operands to List Concatenation."

#### baddemo9
Because our Lists can be mixed type the only error that will be encountered is by passing a non-list value when a List is expected. This demo illustrates calling Append on two strings and this type error causing program execution to stop. The third line of the function is never evaluated. 
To run it:  
  runBadDemo9

The expected output is:
  "Error in binding "list": error in expression to be bound.
     Invalid operands to Append to List."
