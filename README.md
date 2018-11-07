# Code for The Seasoned Schemer


## Background

The book The Seasoned Schemer continues where The Little Schemer ends.
Just like The Little Schemer you don't need anything other than pen and
paper to understand the concepts of the book. Also an interactive environment
can support you to understand and experiment with the concepts shown in the
book, which is especially useful for programming beginners.

## Summary

This is code for the book The Seasoned Schemer.[1] Like the code for
The Little Schemer it is split into one scheme source file for every chapter.
This repository contains the code for the book with tests, some notes, an
additional source file with testing procedures and a script to test the complete
code. You should first read The Little Schemer before reading The Seasoned Schemer.

## Additional Information

Nearly every procedure has a signature which shows the used data types. Build-in
scheme data types or data definitions have no special indicator, compound data
types and non-built-in data structures are shown in square brackets. For example
the data definition for an atom is in square brackets because it is not a
built-in data structure in scheme. As an example here is the "atom" data
definition:

    An [atom] is
    - Number or
    - String or
    - Symbol

Here is the definition of the ```atom?``` function with the function signature as a
comment:

    ;; atom?: any -> boolean
    (define (atom? e)
       (and (not (list? e)) (not (pair? e))))

This means that the function ```atom?``` can take a scheme expression of any type as
an argument and returns value of type boolean. Another example with the function
```member?``` shows the compound data type ```[listof any]```:

    ;; member?: [atom] [listof atom] -> boolean
    (define member?
      (lambda (a lat)
        (cond
         ((null? lat) #f)
         (else (or (eq? (car lat) a)
                   (member? a (cdr lat)))))))

The function ```member?``` takes an atom and a list as arguments. The elements of
the list can be of any type. The result type is a boolean. Note that scheme has
strong dynamic types. Types are checked at run-time not at compile-time. Thus
types are checked when you run the program or evaluate a function. If you use
the wrong type, the scheme implementation returns an error. A list of data type
definitions used in The Seasoned Schemer can be found in the file NOTES. If
the definition of a function changes during the chapter, it is annotated with
1.revision, 2.revision etc.

Almost every function in the code has tests. Loading the scheme file of the
chapter evaluates the corresponding tests. If a test fails it prints a message
to output port showing the expected value and the computed value of the scheme
expression. For example the test ```(test "atom?"  (atom? 'a) #f)``` would print
these lines:

    Testing "atom?"
    Failed: (atom? 'a)
    Expected: #f
    Computed: #t

The tests have two goals. The first goal is to ensure that the code is
correct. The second goal is to show you examples how to apply the code. This is
also the reason why the tests are not in a separate file but close to the
defined function.

The recommended scheme implementation for the code is Chez Scheme, because of
its debugging features, good documentation and build-in expression editor
[2][3][4]. You can test the code by loading the chapter source file you want to
work with. If you have Chez Scheme installed, type

    scheme

into your terminal, which will start a repl. Now you can run the tests by
loading the scheme file:

    (load "chapter01.scm")

If you want to run all tests in all files just execute the scheme script
'run-tests.scm' in the code directory. This needs an installation of Chez
Scheme. Another option is to use this bash one liner in the code directory:

    for f in chapter* ; do echo '(exit)' | scheme $f -q --; done

If you already tested the chapter code restart your scheme implementation to
clear the top-level environment. This is also recommended when you have finished
a chapter and want to start with another chapter. Chez Scheme comes with a
scheme expression editior to edit scheme code. For more information about the
editor look at the 'man' file of Chez Scheme with

    man scheme

or the online documentation.[5]


References:

[1] https://mitpress.mit.edu/books/seasoned-schemer

[2] https://github.com/cisco/ChezScheme

[3] http://www.cs.indiana.edu/chezscheme/debug/

[4] https://cisco.github.io/ChezScheme/csug9.4/

[5] https://cisco.github.io/ChezScheme/csug9.5/expeditor.html#./expeditor:h0
