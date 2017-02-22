module DemoMusic where

-- Written by Dan Brown
-- for Functional Languages

import Music

-- The tune is a poor imitation (from distant memory) of a passage in Mike
-- Oldfield's "Tubular Bells".

line' = line . map ($ [])

m0 = line'
     [ d 5 (7%8), d 5 sn, e 5 sn, g 5 qn, fs 5 qn, e 5 qn, d 5 qn,
       e 5 sn, fs 5 (13%16), d 5 sn, e 5 sn, g 5 qn, fs 5 qn, e 5 qn, d 5 qn,
       e 5 sn, d 5 sn, e 5 dhn, a 4 (1%24), c 5 (1%24), e 5 (1%24),
       d 5 qn, c 5 qn, fs 5 qn, c 5 qn,
       c 5 dqn, d 5 sn, e 5 sn, d 5 hn, a 4 dqn, b 4 en, c 5 hn ]

m1 = Instr "Tubular Bells" $ line'
     [ d 5 (7%8), e 5 en, g 5 qn, fs 5 qn, e 5 qn, d 5 qn,
       fs 5 wn, g 5 qn, fs 5 qn, e 5 qn, d 5 qn,
       e 5 wn, d 5 qn, c 5 qn, fs 5 qn, c 5 qn,
       c 5 hn, d 5 hn, a 4 dqn, b 4 en, c 5 hn ]

m2 = Instr "Fretless Bass" $ line [b1, b1, b1, b1, b2, b2, b1, b1]

m3 = Instr "Distortion Guitar" m0 :=: Instr "Reed Organ" m0

m4 = Instr "Choir Aahs" $ line' v1 :=: line' v2

b1 = line'
     [ d 3 en, d 3 en, g 3 sn, a 3 sn, d 4 en,
       d 4 sn, c 4 sn, a 3 sn, g 3 sn, c 4 sn, d 4 sn, f 3 sn, e 3 sn]

b2 = Trans (-5) b1

v1 = [ a 5 wn, cs 6 hn, b 5 hn, a 5 wn, cs 6 hn, a 5 hn,
       c 6 wn, e 6 hn, c 6 hn, d 6 wn, a 5 wn]

v2 = [ fs 4 wn, a 4 hn, g 4 hn, fs 4 wn, a 4 hn, fs 4 hn,
       a 4 wn, c 5 hn, a 4 hn, d 5 wn, a 4 wn]

q1 = Tempo (8%5) (m1 :=: m2 :=: m3 :=: m4)

q2 = Tempo (8%5) $ m1 :=: delay (2%1) m1 :=: delay (4%1) m1

main = playWin q1
more = playWin q2
