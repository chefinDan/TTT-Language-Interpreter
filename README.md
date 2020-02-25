# Project: A programming language interpreter
## Overview
Introduction


What is the name of your language?
What is the language’s paradigm?
Our language is a pass-by-value imperative language.  Only functions (whether defined in advance or at runtime) are global in scope; when a function is called, it runs in a scope which initially contains only its own arguments.  Functions are therefore necessarily pure, producing the same return value for any set of inputs every time.
(Optional) Is there anything especially unique/interesting about your language that you want to highlight up front?
Design


What features does your language include? Be clear about how you satisfied the constraints of the feature menu.
Data types: Integers, Floats, Strings, Lists, and (sugar-achieved) Booleans.
Conditionals: If/Then/Else is available.
Recursion is possible; while loops are planned.
Functions: Functions are available, take arguments, and have closure over a local scope.
Strings: We have strings.  You can concatenate them via the ordinary addition operation, and there are a few other options for handling them.
List/Array type: We have lists, which can be composed of any combination of regular data types.
First-class functions: Functions are first-class values, and can be used anywhere any other value can, or assigned to a variable.  (Though of course, many operations on Functions will return errors.)
What level does each feature fall under (core, sugar, or library), and how did you determine this?
What are the safety properties of your language? If you implemented a static type system, describe it here. Otherwise, describe what kinds of errors can occur in your language and how you handle them.
Implementation


What semantic domains did you choose for your language? How did you decide on these?
Are there any unique/interesting aspects of your implementation you’d like to describe?
