import Control.Parallel

-- See end of file for how I compiled this program
-- and some sample statistics about the runs.
--------------------------------------------------------

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

nfib 0 = 1
nfib 1 = 1
nfib n = 1 + nfib (n-1) + nfib (n-2)


-----------------------------------------------------

diffib1 n = par l (pseq r (l - r))
 where l = nfib n
       r = fib n

main1 = print (diffib1 38)

------------------------------------------------------
diffib2 n = let l = nfib n
                r = fib n
            in par l (l - r)

main2 = print (diffib2 38)


---------------------------------------------------
diffib3 n = let l = nfib n
                r = fib n
            in par r (l - r)

main3 = print (diffib3 38)

-------------------------------------------------
-- A more robust

diffib4 n = let l = nfib n
                r = fib n
                in par l (pseq r (l - r))

main4 = print (diffib4 38)

-----------------------------------------

main = main4

-----------------------------------------

-- May 19, 2014
-- $ which ghc
-- /cygdrive/c/HP2013.2.0.0/bin/ghc
-- here is how I compiled this program on my machine
-- notice I use full paths, so there is no confusion.
-- cd /cygdrive/d/work/sheard/Courses/AdvancedFP/web/code
-- 
-- to run version 1, where: main = main1
-- ghc --make SillyPar.hs -o SillyPar1 -rtsopts
-- ./SillyPar1.exe +RTS -s
-- main = main1
-- I got this output
{-
$ ./SillyPar1.exe +RTS -s
87403802
  30,656,758,604 bytes allocated in the heap
      23,539,276 bytes copied during GC
          59,108 bytes maximum residency (4 sample(s))
          25,992 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     58572 colls,     0 par    0.42s    0.50s     0.0000s    0.0009s
  Gen  1         4 colls,     0 par    0.00s    0.00s     0.0001s    0.0002s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   47.69s  ( 47.83s elapsed)
  GC      time    0.42s  (  0.50s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   48.11s  ( 48.33s elapsed)

  %GC     time       0.9%  (1.0% elapsed)

  Alloc rate    642,840,770 bytes per MUT second

  Productivity  99.1% of total user, 98.7% of total elapsed
-}  


-- to run version 2, where: main = main2
-- ghc --make SillyPar.hs -threaded -feager-blackholing -o SillyPar2 -rtsopts 
-- ./SillyPar2.exe +RTS -s
{-
$ ./SillyPar2.exe +RTS -s
87403802
  30,656,765,096 bytes allocated in the heap
      23,542,112 bytes copied during GC
          60,232 bytes maximum residency (4 sample(s))
          25,984 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     58572 colls,     0 par    0.41s    0.58s     0.0000s    0.0013s
  Gen  1         4 colls,     0 par    0.00s    0.00s     0.0001s    0.0002s

  TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)

  SPARKS: 1 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   52.65s  ( 52.79s elapsed)
  GC      time    0.41s  (  0.58s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   53.06s  ( 53.37s elapsed)

  Alloc rate    582,271,008 bytes per MUT second

  Productivity  99.2% of total user, 98.7% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
-}



-- On multiple cores
-- ./SillyPar2.exe +RTS -s -N2
-- main = main2
{-
$ ./SillyPar2.exe +RTS -s -N2
87403802
  30,656,764,628 bytes allocated in the heap
      23,540,072 bytes copied during GC
          60,232 bytes maximum residency (4 sample(s))
          37,192 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     58572 colls, 58572 par    6.19s    5.72s     0.0001s    0.0318s
  Gen  1         4 colls,     3 par    0.00s    0.00s     0.0001s    0.0002s

  Parallel GC work balance: 0.00% (serial 0%, perfect 100%)

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 1 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   51.75s  ( 52.20s elapsed)
  GC      time    6.19s  (  5.72s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   57.95s  ( 57.92s elapsed)

  Alloc rate    592,452,403 bytes per MUT second

  Productivity  89.3% of total user, 89.4% of total elapsed

gc_alloc_block_sync: 223494
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
-}

-- to run version 3, where: main = main3
-- ghc --make SillyPar.hs -threaded -feager-blackholing -o SillyPar3 -rtsopts
-- ./SillyPar3.exe +RTS -s -N2

{-
$ ./SillyPar3.exe +RTS -s -N2
87403802
  30,656,766,120 bytes allocated in the heap
      25,031,360 bytes copied during GC
          62,248 bytes maximum residency (4 sample(s))
          36,744 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     33785 colls, 33785 par    1.14s    0.85s     0.0000s    0.0343s
  Gen  1         4 colls,     3 par    0.00s    0.00s     0.0001s    0.0002s

  Parallel GC work balance: 58.24% (serial 0%, perfect 100%)

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 1 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   54.18s  ( 29.15s elapsed)
  GC      time    1.14s  (  0.85s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   55.32s  ( 30.00s elapsed)

  Alloc rate    565,840,690 bytes per MUT second

  Productivity  97.9% of total user, 180.6% of total elapsed

gc_alloc_block_sync: 72476
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
-}


-- where main = main4
-- ghc --make SillyPar.hs -threaded -feager-blackholing -o SillyPar4 -rtsopts
-- ./SillyPar4.exe +RTS -s -N2

{-
$ ./SillyPar4.exe +RTS -s -N2
87403802
  30,656,766,184 bytes allocated in the heap
      24,857,244 bytes copied during GC
          61,944 bytes maximum residency (4 sample(s))
          36,360 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     33605 colls, 33605 par    2.06s    1.77s     0.0001s    0.0422s
  Gen  1         4 colls,     3 par    0.02s    0.03s     0.0081s    0.0321s

  Parallel GC work balance: 58.05% (serial 0%, perfect 100%)

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 1 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   54.51s  ( 29.95s elapsed)
  GC      time    2.07s  (  1.81s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   56.58s  ( 31.76s elapsed)

  Alloc rate    562,439,817 bytes per MUT second

  Productivity  96.3% of total user, 171.6% of total elapsed

gc_alloc_block_sync: 99998
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 5
-}