# scheme
 
I have seen this in a geohot video, later that day I swapped to a new laptop with fedora on it and now im developing it from there, I dont know how long it will take me to finish the book, gonna try to do it by the 21st of april. gonna log my journey.


Edit at the end: Finished on the 23rd of april at 2 AM. The deadline a bit exceeded but that's okay.

The entire thing could be refactored into more files so it would be more readable and could serve as a more academic example but as of now I have other projects to take care of so maybe it will take a bit of time

---

## Requirements

- GHC
  To install GHC just go to the GHCup site and paste the command into the terminal/powershell

--- 

## Compilation

Compile the Main.hs file with the `ghc Main.hs` command and you will get a runnable file that you can run with `./Main` and then you can input commands.

## Functionality

This scheme interpreter can set and define variables, define functions, do recursion, compare, a bit of more polish and it will support string operations.

`(set x 3)` is equal to `x=3` in python
`(set x (pow 3 3))` is equal to `x=3**3` in python
`(define (square x) (* x x))` is equal to `def square(a) return a*a` in python

The language is very simple but is meant to be more of a teaching experience than anything functional. It accomplished its task.
