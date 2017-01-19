Activity 2 --- Chop the Hydra!
==============================

Your work is to write the `chop` function that was discussed in class.

Put the code for `chop` in `src/Lib.hs`.

## Running your code

To get a Haskell REPL, type `stack repl`.

Example session:

```
$ stack repl
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Lib              ( /home/mattox/repos/cs421/template/activities/a02-hydra/src/Lib.hs, interpreted )
Ok, modules loaded: Lib.
[2 of 2] Compiling Main             ( /home/mattox/repos/cs421/template/activities/a02-hydra/app/Main.hs, interpreted )
Ok, modules loaded: Lib, Main.
Loaded GHCi configuration from /tmp/ghci8172/ghci-script
*Main Lib> chop [2,0,0]
[1,3,0]
*Main Lib> 
```

## Testing your code

To test your code, use `stack test`.

Example session:

If your code is working, you will see something like this:

```
-> % stack test
a02-hydra-0.1.0.0: configure (lib + exe + test)

   ... and lots of other lines ...

a02-hydra-0.1.0.0: test (suite: a02-hydra-test)
             
Progress: 1/2Chop Function:
  Length is preserved: [OK, passed 100 tests]
  Difference is correct: [OK, passed 100 tests]

         Properties  Total      
 Passed  2           2          
 Failed  0           0          
 Total   2           2          
             
Completed 2 action(s).
```

If it failed, you will get something like this:

```
Chop Function:
  Length is preserved: [OK, passed 100 tests]
  Difference is correct: [Failed]
*** Failed! Falsifiable (after 1 test): 
[0,1,1,1,0,2,0,4,1,0]
(used seed -2783005862933067557)

         Properties  Total      
 Passed  1           1          
 Failed  1           1          
 Total   2           2          

Completed 2 action(s).
Test suite failure for package a02-hydra-0.1.0.0
    a02-hydra-test:  exited with: ExitFailure 1
```

Notice that it shows you a test pattern that caused a failure!

## Turning in your activity

To turn this in, use `git` to commit your changes to `src/Lib.hs` and push it
to gitlab using these three commands.

```
git add src/Lib
git commit -m "Turning in my activity"
git push
```
