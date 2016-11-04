import Music

bar1  = c 4 qn [] :+: d 4 qn [] :+: e 4 qn [] :+: c 4 qn []

bar2  = e 4 qn [] :+: f 4 qn [] :+: g 4 hn []

bar3  = g 4 en [] :+: a 4 en [] :+: g 4 en [] :+: f 4 en []
                                :+: e 4 qn [] :+: c 4 qn []

bar4  = c 4 qn [] :+: g 3 qn [] :+: c 4 hn []

frere = line [ b | b <- [bar1, bar2, bar3, bar4], r <- [1,2] ]

makeRound    :: Int -> Dur -> Music -> Music 
makeRound n d = chord . take n . iterate (Rest d :+:) . Instr "Piano"

instrs = [ "Clarinet", "Rock Organ", "Trombone", "Vibraphone" ]

concerto d = chord . zipWith Instr instrs . iterate (Rest d :+:)

song  = Tempo 2 (makeRound 4 (2*wn) frere)
song1 = Tempo 2 (concerto (2*wn) frere)

tab m = outputMidiFile "test.mid" (makeMidi (m, defCon, defUpm))

main = writeMidi (Tempo 2 frere) "frereJacques"