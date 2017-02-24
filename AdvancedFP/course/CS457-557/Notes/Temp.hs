
-- here is how I compiled this program on my machine
-- notice I use full paths, so there is no confusion.
-- cd "D:\work\sheard\Courses\CS457-557\web\Notes"
-- 
-- to run version 1, where: main = main1
-- ghc --make SillyPar.hs -o SillyPar1 -rtsopts
-- ./SillyPar1.exe +RTS -s
-- main = main1
-- I got this output
{-
$ ./SillyPar1.exe +RTS -s
87403802
  30,656,758,556 bytes allocated in the heap
      23,535,292 bytes copied during GC
          58,760 bytes maximum residency (3 sample(s))
          25,528 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     58573 colls,     0 par    0.30s    0.47s     0.0000s    0.0016s
  Gen  1         3 colls,     0 par    0.00s    0.00s     0.0001s    0.0001s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   44.02s  ( 44.08s elapsed)
  GC      time    0.30s  (  0.47s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   44.32s  ( 44.54s elapsed)

  %GC     time       0.7%  (1.0% elapsed)

  Alloc rate    696,372,867 bytes per MUT second

  Productivity  99.3% of total user, 98.8% of total elapsed
-}  


-- to run version 2, where: main = main2
-- ghc --make SillyPar.hs -threaded -feager-blackholing -o SillyPar2 -rtsopts 
-- ./SillyPar2.exe +RTS -s
{-
$ ./SillyPar2.exe +RTS -s
87403802
  30,656,765,028 bytes allocated in the heap
      23,540,692 bytes copied during GC
          60,232 bytes maximum residency (3 sample(s))
          25,984 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     58573 colls,     0 par    0.53s    0.57s     0.0000s    0.0015s
  Gen  1         3 colls,     0 par    0.00s    0.00s     0.0001s    0.0002s

  Parallel GC work balance: nan (0 / 0, ideal 1)

                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :    0.00s    ( 49.47s)       0.00s    (  0.00s)
  Task  1 (worker) :    0.00s    ( 49.47s)       0.00s    (  0.00s)
  Task  2 (bound)  :   48.73s    ( 48.90s)       0.55s    (  0.57s)

  SPARKS: 1 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   48.75s  ( 48.90s elapsed)
  GC      time    0.53s  (  0.57s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   49.30s  ( 49.47s elapsed)

  Alloc rate    628,651,518 bytes per MUT second

  Productivity  98.9% of total user, 98.6% of total elapsed

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
  30,656,765,028 bytes allocated in the heap
      23,540,072 bytes copied during GC
          60,232 bytes maximum residency (3 sample(s))
          37,192 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     58573 colls, 58572 par    4.99s    4.63s     0.0001s    0.2944s
  Gen  1         3 colls,     3 par    0.00s    0.00s     0.0001s    0.0001s

  Parallel GC work balance: 1.00 (5826433 / 5826397, ideal 2)

                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :    0.00s    ( 55.09s)       0.00s    (  0.00s)
  Task  1 (worker) :    0.27s    ( 48.44s)       0.84s    (  6.65s)
  Task  2 (bound)  :   49.62s    ( 50.46s)       4.32s    (  4.63s)
  Task  3 (worker) :    0.00s    ( 55.09s)       0.00s    (  0.00s)

  SPARKS: 1 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 1 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   50.06s  ( 50.46s elapsed)
  GC      time    4.99s  (  4.63s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   55.05s  ( 55.09s elapsed)

  Alloc rate    612,391,601 bytes per MUT second

  Productivity  90.9% of total user, 90.9% of total elapsed

gc_alloc_block_sync: 909
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
  30,656,766,052 bytes allocated in the heap
      24,579,944 bytes copied during GC
          61,728 bytes maximum residency (3 sample(s))
          37,496 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     33184 colls, 33183 par    1.14s    0.70s     0.0000s    0.0143s
  Gen  1         3 colls,     3 par    0.00s    0.00s     0.0003s    0.0006s

  Parallel GC work balance: 1.58 (6083624 / 3850723, ideal 2)

                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :   22.79s    ( 26.49s)       0.76s    (  0.97s)
  Task  1 (worker) :    0.00s    ( 27.46s)       0.00s    (  0.00s)
  Task  2 (bound)  :   26.18s    ( 26.70s)       0.64s    (  0.77s)
  Task  3 (worker) :    0.00s    ( 27.47s)       0.00s    (  0.00s)

  SPARKS: 1 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   49.23s  ( 26.76s elapsed)
  GC      time    1.14s  (  0.70s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   50.37s  ( 27.47s elapsed)

  Alloc rate    622,675,764 bytes per MUT second

  Productivity  97.7% of total user, 179.2% of total elapsed

gc_alloc_block_sync: 12391
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
-}


-- where main = main4
-- ghc --make SillyPar.hs -threaded -feager-blackholing -o SillyPar4 -rtsopts
-- ./SillyPar4.exe +RTS -s -N2

{-
87403802
  30,656,766,116 bytes allocated in the heap
      25,247,404 bytes copied during GC
          62,264 bytes maximum residency (3 sample(s))
          38,252 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     33014 colls, 33013 par    0.70s    0.70s     0.0000s    0.0321s
  Gen  1         3 colls,     3 par    0.00s    0.00s     0.0001s    0.0002s

  Parallel GC work balance: 1.62 (6248351 / 3845915, ideal 2)

                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :    0.03s    ( 29.36s)       0.02s    (  0.03s)
  Task  1 (worker) :   28.33s    ( 28.59s)       0.45s    (  0.79s)
  Task  2 (bound)  :   26.04s    ( 28.48s)       0.37s    (  0.90s)
  Task  3 (worker) :    0.00s    ( 29.39s)       0.00s    (  0.00s)

  SPARKS: 1 (1 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   54.54s  ( 28.69s elapsed)
  GC      time    0.70s  (  0.70s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time   55.24s  ( 29.39s elapsed)

  Alloc rate    562,118,054 bytes per MUT second

  Productivity  98.7% of total user, 185.6% of total elapsed

gc_alloc_block_sync: 24567
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0
-}