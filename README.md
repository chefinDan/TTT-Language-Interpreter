# Project: A programming language interpreter
## Overview

Michael Andrews andrmich
Daniel Green greendan
Nicholas Matusmoto matsumon
Quan Nguyen nguyenq2

##Introduction

Our language, Untitled Group Project (abbreviated TTT and pronounced 'French',)
is a pass-by-value imperative language.  TTT's most interesting features include
first-class functions and list operations.

##Executing Programs

###TTT programs, at present, are run inside of ghci.  The language implementation
consists only of one file; untitled-group-project.hs.  There are no modules to load.

The command would be:
	ghci untitled-group-project.hs

##Demo Programs

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