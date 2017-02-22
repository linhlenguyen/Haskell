module MusicWorksheet where


import Haskore
import Ratio((%))

-- A useful function so we don't have to write all the []
-- See the notes for more explanation

line2 list = line [ note [] | note <- list]


-- (1) ----------------------------------------------------
-- Look at the sheet music for  Twinkle Twinle Little star
-- http://iowacreativemusic.com/common/images/stamp.php?item=256
-- Then transcribe the notes into Haskore music.
-- Test your code after transcribing each line

twinkle = line2 [ undefined ]

play1 = testNT twinkle

-- (2) ---------------------------------------------------
-- Look carefully at the sheet music. Are there notes that are repeated
-- Hint study the folling line pairs (3 & 4, 2 & 6, 1 & 5)
-- Can you redo the song by naming each line?

twinkle2 = line1 :+: line2 :+: undefined
  where line1 = undefined
        line2 = undefined
        -- add some more lines here
        
-- (3) --------------------------------------------------
-- can you turn the song into a round with 3 parts?
-- How long must each part be delayed?

round = undefined

-- (4) -------------------------------------------------
-- Experiment with different instruments. 
-- See  http://web.cecs.pdx.edu/~sheard/course/CyberMil/Code/MidiInstrumentList.html
-- for a list of instruments supported. A direct link on the Daily Record.


-- (5) -------------------------------------------------
-- Search the web for some sheet music for a song you like.
-- Can you transcribe some of it into Haskore?

