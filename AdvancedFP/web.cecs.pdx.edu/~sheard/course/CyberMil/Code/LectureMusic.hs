module LectureMusic where 


import Haskore
import Ratio((%))

m1 = Note (C,5)  1 []

m2 = Note (D,5) 1 []

m3 = m1 :+: m2

n1 = c 5 wn []
n2 = d 5 wn []
n3 = n1 :+: n2


cscale = c 4 qn [] :+: d 4 qn [] :+: e 4 qn [] :+: 
         f 4 qn [] :+: g 4 qn [] :+:  a 4 qn [] :+: 
         b 4 qn [] :+: c 5 qn []

chord1 = (c 4 hn [] :=: e 4 hn [])		   
		   
		   
cscale2 = line
    [c 4 qn [], d 4 qn [], e 4 qn [],
     f 4 qn [], g 4 qn [], a 4 qn [],
     b 4 qn [], c 5 qn [] ]


chords = chord [ line [Rest (3%4), cscale]
               , cscale ]

line2 list = line [ note [] | note <- list]

chord2 list = chord [ note [] | note <- list ]

cscale3 = line2 [c 4 qn, d 4 qn, e 4 qn,
                 f 4 qn, g 4 qn, a 4 qn,
                 b 4 qn, c 5 qn ]
                 
cMajor = [ n 4 hn [] | n <- [c,e,g] ]
cMinor = [ n 4 wn [] | n <- [c,ef, g] ]
                 
cMajArp = line  cMajor

cMajChd = chord cMajor
ex4 = line [ chord cMajor, chord cMinor ]

-- delay :: Dur -> Music -> Music  -- defined in library
-- delay d m = Rest d :+: m

ex5 = cscale :=: (delay dhn cscale)

ex6 = chord [line cMajor,Trans 12 (line cMajor)]


row = line2 [c 5 qn, c 5 qn, c 5 den, d 5 sn, e 5 qn
            ,e 5 den, d 5 sn, e 5 den, f 5 sn, g 5 hn
            ,triplet (c 6 qn), triplet (g 5 qn),
                               triplet (e 5 qn), triplet (c 5 qn)
            ,g 5 den, f 5 sn, e 5 den, d 5 sn, c 5 hn]



triplet n args = Tempo 3 (n args) :+: Tempo 3 (n args) :+: Tempo 3 (n args)


row1 = testNT row
row2 = testNT (Tempo 2 row)
row3 = testNT (Tempo 2 (row :=: (Rest wn :+: row)))
row4 = testNT (Tempo 2 (voice1 :=: voice2 :=: voice3))
  where voice1 = row
        voice2 = (Rest wn :+: row)
        voice3 = (Rest (wn * 2) :+: row)


row5 = testNT (Tempo 2 (voice1 :=: voice2 :=: voice3))
  where voice1 = Instr "Tenor Sax" row
        voice2 = Instr "English Horn" (Rest wn :+: row)
        voice3 = Instr "Harpsichord" (Rest (wn * 2) :+: row)
        
row6 = testNT (voice "Violin" 0 :=: voice "Flute" 1:=: voice "Tubular Bells" 2)
  where voice i part = Tempo (3%2) (Instr i (Rest (wn * part) :+: row))
        

nBeatsRest n note =
   line ((take n (repeat note)) ++ [qnr])

ex7 = 
  line [e 4 qn [], d 4 qn [], c 4 qn [], d 4 qn [],
        line [ nBeatsRest 3 (n 4 qn []) | n <- [e,d] ],
        e 4 qn [], nBeatsRest 2 (g 4 qn []) ]


main = testNT  (row)  
go = testNT