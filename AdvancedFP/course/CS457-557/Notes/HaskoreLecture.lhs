Writing Music with Haskell
==========================

Iavor S. Diatchki
Galois, Inc.
















Literate Haskell
================

  * This is a literate Haskell file.

  * All text is a comment unless marked with ">"
  
  * Comments and code should be separated by text


> module HaskoreLecture where


Here we go...










Haskore
=======

  * In this lecture we shall use Haskore.

> import Haskore    -- The Haskore library
> import Data.List  -- Functions on lists

  * It is a Haskell library for describing music.

  * The goal of the lecture is to provide concrete examples of
    common FP techniques for working with algebraic data.














Pitch
=====

The following types are defined by Haskore:

  type Pitch      = (PitchClass, Octave)
  data PitchClass = Cf | C | Cs 
                  | Df | D | Ds 
                  | Ef | E | Es 
                  | Ff | F | Fs
                  | Gf | G | Gs 
                  | Af | A | As
                  | Bf | B | Bs
                    deriving (Eq,Ord,Ix,Show,Read)
   type Octave     = Int


Some example scales:

> blues, penta, major, minor :: [PitchClass]
> major = [ C, D, E, F, G, A, B ]
> minor = [ C, D, Ef, F, G, Af, Bf ]
> blues = [ C, Ef, F, Fs, G, Bf ]
> penta = [ C, D, E, G, A ]


Pitches in the same octave:

> octave         :: Octave -> [PitchClass] -> [Pitch]
> octave n ps     = [ (p,n) | p <- ps ]









Music
=====

The central datatype for describing music:

  data Music = Note Pitch Dur [NoteAttribute]   -- a note \ atomic 
             | Rest Dur                         -- a rest /    objects
             | Music :+: Music                  -- sequential composition
             | Music :=: Music                  -- parallel composition
             | Tempo  (Ratio Int) Music         -- scale the tempo
             | Trans  Int Music                 -- transposition
             | Instr  IName Music               -- instrument label
             | Player PName Music               -- player label
             | Phrase [PhraseAttribute] Music   -- phrase attributes
      deriving (Show, Eq)
 
  type Dur   = Ratio Int                        -- in whole notes
  type IName = String
  type PName = String


Examples:

Notes with the same duration:

> notes          :: Dur -> [Pitch] -> [Music]
> notes d ps      = [ Note p d [] | p <- ps ]


Joining pieces of music sequentially:

> sequential         :: [Music] -> Music
> sequential (m : ms) = m :+: sequential ms
> sequential []       = Rest 0



Joining pieces of music in parallel:

> together           :: [Music] -> Music
> together (m : ms)   = m :=: together ms
> together []         = Rest 0


Note the similarity of these definitions.







Testing
=======

We can use the function "test" to save a piece of music to a midi file
called "test.mid":

  Main> :t test
  test :: Music -> IO ()

> example1     :: Music
> example1      = sequential $ notes (1/8) $ octave 4 blues

> example2     :: Music
> example2      = example1 :+: Trans 12 example1

> tonic        :: Music
> tonic         = together $ notes (1/4) $ octave 4 [C, E, G]

> subdominant   :: Music
> subdominant   = Trans 5 tonic

> dominant     :: Music
> dominant      = Trans 2 subdominant

> example3     :: Music
> example3      = tonic :+: subdominant :+: dominant





Repeating a Phrase
==================


> repeating :: Int -> Music -> Music
> repeating n m
>   | n <= 0    = Rest 0
>   | otherwise = m :+: repeating (n-1) m

  * This definition uses "boolean guards".

  * It is similar to an if-then-else expression:

     The function uses the first branch that evaluates to True.


> example4 = repeating 4 (Tempo 4 example1)







We can also use infix notation:

> blues12   =  Tempo (1/2)
>           $  (4 `repeating` tonic)
>          :+: (2 `repeating` subdominant)
>          :+: (2 `repeating` tonic)
>          :+: dominant
>          :+: (2 `repeating` subdominant)
>          :+: (2 `repeating` tonic)
>
> example5  = Trans 12 (8 `repeating` example1)
>          :=: blues12












Analyzing Music
===============

So far we have been constructing different music values.

We can also analyze and transform music phrases.


Compute the duration of a musical phrase:

> duration :: Music -> Dur
> duration m = case m of
>                Note _ d _ -> d
>                Rest d     -> d
>                m1 :+: m2  -> duration m1 + duration m2
>                m1 :=: m2  -> duration m1 `max` duration m2
>                Tempo x m  -> duration m / x
>                Trans _  m -> duration m
>                Instr _  m -> duration m
>                Player _ m -> duration m
>                Phrase _ m -> duration m











We can play a music phrase backwards:

> backward :: Music -> Music
> backward m = case m of
>                 Note {}    -> m
>                 Rest {}    -> m
>                 m1 :+: m2  -> backward m2 :+: backward m1
>                 m1 :=: m2  -> backward m1 :=: backward m2
>                 Tempo n m  -> Tempo n (backward m)
>                 Trans x m  -> Trans x (backward m)
>                 Instr x  m -> Instr x (backward m)
>                 Player x m -> Player x (backward m)
>                 Phrase x m -> Phrase x (backward m)


The notation "Note {}" is "Note _ _ _"


> example6 :: Music
> example6 = example1 :+: backward example1

> example7 :: Music
> example7 = example1 :=: backward example1







Notes As Numbers
================

So far, we worked with notes in music notation.

If we want to generate music computationally,
it is more convenient to assign numbers to different pitch values.

> noteNum         :: PitchClass -> Int
> noteNum x        = case x of
>                      Cf -> -1; C ->  0; Cs -> 1
>                      Df ->  1; D ->  2; Ds -> 3
>                      Ef ->  3; E ->  4; Es -> 5
>                      Ff ->  4; F ->  5; Fs -> 6
>                      Gf ->  6; G ->  7; Gs -> 8
>                      Af ->  8; A ->  9; As -> 10
>                      Bf -> 10; B -> 11; Bs -> 12


Converting between notation:

> toAbsolute     :: Pitch -> Int
> toAbsolute (p,o)  = noteNum p + 12 * o


> fromAbsolute   :: Int -> Pitch
> fromAbsolute x    = let (a,b) = quotRem x 12
>                     in (notes !! b,a)
>   where notes = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]


> quickNums     :: [Int] -> Music
> quickNums ns   = sequential $ notes (1/16) $ map fromAbsolute ns

> example9       :: Music
> example9        = quickNums [ 60 .. 80 ]

> example10       :: Music
> example10        = example9 :=: quickNums [80, 79 .. 60 ]









The Tone Matrix Game
====================

Idea from:
http://lab.andre-michelle.com/tonematrix

Goal: Translating a picture into music.
  * Each column corresponds to a chord,
  * "*" marks which notes of a given scale should be played,
  * The columns are played sequentially.


> playPic :: String -> Music
> playPic pic = Instr "Marimba"      -- using a marimba
>             $ repeating 4          -- repeating 4 times
>             $ sequential           -- we want to play the chords sequentially
>             $ map (together . zipWith toNote [0..]) -- each ex-column is a chord
>             $ transpose            -- turn columns into rows
>             $ addRows              -- ensure that we have 16 lines
>             $ map addCols          -- make each line be 16 characters long
>             $ lines pic            -- split into lines

Padding with a default value:

> pad x xs      = take 16 (xs ++ repeat x)
> addCols       = pad ' '
> addRows       = pad (replicate 16 ' ')

Turning characters into notes.
"*" becomes a note, anything else is a rest.

> toNote n '*'  = Note (scale !! n) (1/32) []
>  where scale     = reverse
>                  $ take 16
>                  $ (A,3) : (B,3) : [ (n,o) | o <- [4..6], n <- penta ]
> toNote _ _    = Rest (1/32)





> playFile   :: FilePath -> IO ()
> playFile f  = do txt <- readFile f
>                  test (playPic txt)















Other Examples
==============



We can trim a piece of music to a given duration.

> trim :: Dur -> Music -> (Dur, Music)    -- returns remaining time, if any
> trim n m = case m of
>
>               Note p d a
>                 | d > n     -> (0, Note p n a)
>                 | otherwise -> (n - d, m)
>
>               Rest d
>                 | d > n     -> (0, Rest n)
>                 | otherwise -> (n - d, m)
>
>               m1 :+: m2
>                  | n1 > 0     -> let (n2,m2') = trim n1 m2
>                                  in (n2, m1' :+: m2')
>                  | otherwise  -> (0,m1')
>                 where (n1,m1') = trim n m1
>  
>               m1 :=: m2  -> let (n1,m1') = trim n m1
>                                 (n2,m2') = trim n m2
>                             in (min n1 n2, m1' :=: m2')
>
>               Tempo x m  -> let (n1,m1) = trim (x * n) m
>                              in (n1 / n, Tempo x m1)
>
>               Trans x  m -> let (n1,m1) = trim n m in (n1, Trans x m1)
>               Instr x  m -> let (n1,m1) = trim n m in (n1, Instr x m1)
>               Player x m -> let (n1,m1) = trim n m in (n1, Player x m1)
>               Phrase x m -> let (n1,m1) = trim n m in (n1, Phrase x m1)


Often, we do not care about the remaining time:

> trimmed   :: Dur -> Music -> Music
> trimmed n m = snd (trim n m)


> example6'  :: Dur
> example6' = duration (trimmed 2 example5)


Looping
=======

We can also write a combinator that will repeat the same phrase forever:

> loop :: Music -> Music
> loop m = m :+: loop m


We cannot save an infinite piece of music in a midi file.

However,  we can trim parts of different lengths.

> example7', example8  :: Music
> example7' = trimmed 2 (loop tonic)
> example8  = trimmed 1 (loop tonic)



