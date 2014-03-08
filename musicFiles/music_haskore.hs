module GeneratedTest where
import Haskore.Basic.Duration

import Haskore.Music
import Haskore.Melody.Standard
import Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Interface.MIDI.Render as Render
import Data.List as L
import Data.Ratio
import Snippet
import Generator
import System.IO


{--
NOTE: Required modules: Haskore, Criterion
Therefore, set up:
cabal install criterion
cabal install haskore
--}
{--
main = Render.fileFromGeneralMIDIMusic "mytune.midi" song2

--outFunc :: [Generator.Pitch] -> String -> IO ()
--outFunc [] xs = render_to xs 

song = MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano $ chord
  [changeTempo (2 %+ 3) (line [c 1 (1 %+ 23) na, rest (1 %+ 23)]),
   MidiMusic.transpose 3 (line [c 1 qn na, qnr]),
   changeTempo (2 %+ 3) (c 1 (1 %+ 23) na),
   MidiMusic.transpose (- 3) (c 1 qn na), changeTempo (7 %+ 1) (rest (1 %+ 23)),
   MidiMusic.transpose 4 qnr]

song2 = MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano $ chord
    [changeTempo (2 %+ 3) (line [c 1 (1 %+ 23) na, rest (1 %+ 23)]),
   MidiMusic.transpose 3 (line [c 1 qn na, qnr]),
   changeTempo (2 %+ 3) (c 1 (1 %+ 23) na),
   MidiMusic.transpose (- 3) (c 1 qn na), changeTempo (7 %+ 1) (rest (1 %+ 23)),
   MidiMusic.transpose 4 qnr]

--}
a_min7 = [x y qn ()| (x,y) <- [(a,0),(c,1),(e,1),(g,1)]]
a_min7_chord = foldl1 (=:=) a_min7
a_min7_scale = foldl1 (+:+) a_min7

put [] = []
put ((x,y):xs) | x == A = [(a,y)] ++ put xs
           | x == As = [(as,y)] ++ put xs
           | x == Bf = [(bf,y)] ++ put xs
           | x == B = [(b,y)] ++ put xs
           | x == C = [(c,y)] ++ put xs
           | x == Cs = [(cs,y)] ++ put xs
           | x == Df = [(df,y)] ++ put xs
           | x == D = [(d,y)] ++ put xs
           | x == Ds = [(ds,y)] ++ put xs
           | x == Ef = [(ef,y)] ++ put xs
           | x == E = [(e,y)] ++ put xs
           | x == F = [(f,y)] ++ put xs
           | x == Fs = [(fs,y)] ++ put xs
           | x == Gf = [(gf,y)] ++ put xs
           | x == G = [(g,y)] ++ put xs
           | x == Gs = [(gs,y)] ++ put xs
           | x == Af = [(af,y)] ++ put xs
       --    | x == Rest = [rest] ++ put xs
    	   | otherwise = [(c,y)] ++ put xs

rhPut [] = []
rhPut ((b ::+ c):xs) = [head(rhPut[b]) + head(rhPut[c])] ++ rhPut xs
rhPut (x:xs)  | x == Bn = [bn] ++ rhPut xs
              | x == Wn = [wn] ++ rhPut xs
              | x == Hn = [hn] ++ rhPut xs
              | x == Qn = [qn] ++ rhPut xs
              | x == En = [en] ++ rhPut xs
              | x == Sn = [sn] ++ rhPut xs
              | x == Tn = [tn] ++ rhPut xs
              | x == Sfn = [sfn] ++ rhPut xs
              | x == Dwn = [dwn] ++ rhPut xs
              | x == Dhn = [dhn] ++ rhPut xs
              | x == Dqn = [dqn] ++ rhPut xs
              | x == Den = [den] ++ rhPut xs
              | x == Dsn = [dsn] ++ rhPut xs
              | x == Dtn = [dtn] ++ rhPut xs
              | x == Ddhn = [ddhn] ++ rhPut xs
              | x == Ddqn = [ddqn] ++ rhPut xs
              | x == Dden = [dden] ++ rhPut xs
    	      | otherwise = [bn] ++ rhPut xs


{--
############
CREATE SCALE (fixed length rhythms)
############
--}
--num indicates the number of notes in the scale; 
--model indicates the Pitch Model to be used; 
--startPitch indicates starting note; 
--ran is the random seed
--z is the note length (bn,wn,hn,qn,en,sn,tn,sfn)
createScale num model startPitch ran z = foldl1 (+:+) [x y z ()| (x,y) <- (put(Prelude.take num $ generate model startPitch ran))]


{--
############
CREATE CHORD (fixed length rhythms)
############
--}
--num indicates the number of notes in the chord; 
--model indicates the Pitch Model to be used; 
--startPitch indicates base note; 
--ran is the random seed
--z is the note length (bn,wn,hn,qn,en,sn,tn,sfn)
createChord num model startPitch ran z  = foldl1 (=:=) [x y z ()| (x,y) <- (put(Prelude.take num $ generate model startPitch ran))]



--length is the number of bars (in 4/4) of rhythm to be generated
--x:xs is infinite (generated) list
--out is list to be output (initially [])
{-- HOW IT WORKS:
--Takes the first element from the infinite list and appends it to the output list.
--Checks if this new list (newOut) is longer than the defined 'length'.
--If longer, we return the output list (without the most recently appended element, since this makes it too long)
--Otherwise, we update our infinite list to be the tail of the previous infinite list, and we update our output list to be newOut.
--}
createRhythBars :: Rational -> [Rhythm] -> [Rhythm] -> [Rhythm]
createRhythBars _ [] _ = []
createRhythBars length (x:xs) out   | (currentLength) > length = out
                                  | otherwise = createRhythBars length xs newOut
    where newOut = out ++ [x]
          currentLength = foldl1 (+) $ map Haskore.Basic.Duration.toRatio (rhPut(newOut))


{--
creating melodies with varying rhythm to a fixed length (bars)
--}

--function taking a Pitch Stream and Rhythm Stream and creating a Haskore.Melody

compRhythPitch _ []  = []
compRhythPitch [] _ = []
compRhythPitch ((x1,x2):xs) (y:ys)  =  [x1 x2 y ()] ++ compRhythPitch xs ys

--  | x1 == C =  [x1 x2 y (1%2)] ++ compRhythPitch xs ys        
--                                    |otherwise            

--len indicates the length of the melody in bars (4/4); 
--noteModel indicates the Pitch model to be used;
--rhythMod indicates the rhythm model to be used 
--startPitch indicates starting note; 
--ran is the random seed
createScale1 len noteModel rhythModel startPitch startRhythm ran = foldl1 (+:+) $ compRhythPitch notes rhyth
    where notes = (put(generate noteModel startPitch ran))
          rhyth = rhPut(createRhythBars len (rhGenerate rhythModel startRhythm ran) [])

--num indicates the number of notes in the scale; 
--noteModel indicates the Pitch model to be used;
--rhythMod indicates the rhythm model to be used 
--startPitch indicates starting note; 
--ran is the random seed
--createChord1 num noteModel rhythMod startPitch ran = foldl1 (=:=)  

{--EXAMPLES--}
bluScE12 = createScale 12 cBlues7 (Ef,1) 120 en
bluSc24Mel = createScale 24 cBlues7Melody (C,1) 120 en


--Function taking list of pitches, list of rhythms and returning a midi file
--NOTE: Both pitches and rhythm trianed on 2-note markov chain
makeMelodyDoub :: [Pitch] -> [Rhythm] -> Int -> Rational -> FilePath -> IO ()
makeMelodyDoub pitches rhythms ran bars name = render_to name mel
    where mel = foldl1 (+:+) $ compRhythPitch notes rhyth
          notes = (put(generatePPair (trainPitchPairs pitches) (head(pitches),head(tail(pitches))) ran))
          rhyth = rhPut(createRhythBars bars (generateRPair (trainRhythPairs rhythms) (head(rhythms),head(tail(rhythms))) ran) [])


--Function taking list of pitches, list of rhythms and returning a midi file
--NOTE: Both pitches and rhythm trianed on 3-note markov chain
makeMelodyTrip :: [Pitch] -> [Rhythm] -> Int -> Rational -> FilePath -> IO ()
makeMelodyTrip pitches rhythms ran bars name = render_to name mel
    where mel = foldl1 (+:+) $ compRhythPitch notes rhyth
          notes = (put(generatePTh  (trainPitchTrips pitches) (head(pitches),head(tail(pitches)),head(tail(tail(pitches)))) ran))
          rhyth = rhPut(createRhythBars bars (generateRTh (trainRhythTrips rhythms) (head(rhythms),head(tail(rhythms)),head(tail(tail(rhythms)))) ran) [])

{-- 
bn   = 2       -- brevis
wn   = 1       -- whole note
hn   = 1%+ 2    -- half note
qn   = 1%+ 4    -- quarter note
en   = 1%+ 8    -- eight note
sn   = 1%+16    -- sixteenth note
tn   = 1%+32    -- thirty-second note
sfn  = 1%+64    -- sixty-fourth note

dotted, doubleDotted :: T -> T
dotted       = ((3%+2) *)
doubleDotted = ((7%+4) *)

dwn  = dotted wn    -- dotted whole note
dhn  = dotted hn    -- dotted half note
dqn  = dotted qn    -- dotted quarter note
den  = dotted en    -- dotted eighth note
dsn  = dotted sn    -- dotted sixteenth note
dtn  = dotted tn    -- dotted thirty-second note

ddhn = doubleDotted hn  -- double-dotted half note
ddqn = doubleDotted qn  -- double-dotted quarter note
dden = doubleDotted en  -- double-dotted eighth note


cf = note' Cf;  c = note' C;  cs = note' Cs
df = note' Df;  d = note' D;  ds = note' Ds
ef = note' Ef;  e = note' E;  es = note' Es
ff = note' Ff;  f = note' F;  fs = note' Fs
gf = note' Gf;  g = note' G;  gs = note' Gs
af = note' Af;  a = note' A;  as = note' As
bf = note' Bf;  b = note' B;  bs = note' Bs





//////////////////////////////////////////////
//------------------------------------------//
//------------------------------------------//
//--INSTRUCTIONS ON USING music_haskore.hs--//
//------------------------------------------//
//------------------------------------------//
//////////////////////////////////////////////

NOTE: A full list of Pitch and Rhythm definitions can be found above (to be put at the end of doc eventually)


open terminal and type the following into the command prompt:
cd math_music/
ghci music_haskore.hs

---------------------
DEFINING A NOTE MODEL 
---------------------

let noteModel = trainPitches [setOfPitches] (e.g. [A,B,A,C,A,D,A])

-----------------------
DEFINING A RHYTHM MODEL 
-----------------------

let rhythModel = trainRhythm [setOfDurations] (e.g. [En,Qn,Qn,Qn,Qn,En,En,En,En,Qn,Qn,Qn,Qn,Qn,Qn])

---------------------------------------------------
DEFINING A MELODY (rhythm and note models required) 
---------------------------------------------------

---------------------------------------------------
---------------------------------------------------
EXAMPLE 1:
let myScale = createScale 20 noteModel B 12 en 

-- this is a fixed note length melody (does not use rhythm model)
-- the number indicates the number of notes in the melody
-- 'noteModel' indicates that 'noteModel' is to be used as the Pitch model
-- B is the start note of the melody
-- 12 is the random number used to generate the melody
-- en is the duration of each note in the melody (Eighth note)
---------------------------------------------------
---------------------------------------------------

---------------------------------------------------
---------------------------------------------------
EXAMPLE 2:
let myScale2 = createScale1 2 noteModel rhythModel A Qn 905

-- the number indicates the length (in 4/4 bars) of the melody.
-- 'noteModel' indicates that 'noteModel' is to be used as the Pitch model
-- 'rhythModel' indicates that 'rhythModel' is to be used as the Rhythm model
-- A is the start note of the melody
-- Qn is the duration of the first note in the melody
-- 905 is the random number used to generate the melody
---------------------------------------------------
---------------------------------------------------


--------------------------------------------
CREATING A MIDI FILE FROM THE DEFINED MELODY
--------------------------------------------

render_to "myScale.midi" myScale

--produces a midi file called "myScale.midi" in your current directory, defined by the melody 'myScale'


render_to "myScale2.midi" myScale2

--produces a midi file called "myScale2.midi" in your current directory, defined by the melody 'myScale2'

--}


{- HELPER FUNCTIONS -}
-- endOnCloseBracket ensures that the string ends on a closed bracket, thus ensuring the Rhythm or melody isn't 'open'

endOnChar :: String -> Char -> String
endOnChar [] _ = []
endOnChar xs y | last xs == y = xs
               | otherwise = endOnChar (init xs) y

semicolForComma :: String -> String
semicolForComma [] = []
semicolForComma (x:xs)   | x == ';' = [','] ++ (semicolForComma xs)
                         | x == '\n' = [','] ++ (semicolForComma xs)
                         | otherwise = [x] ++ (semicolForComma xs)

fileToNotes :: FilePath -> IO ()
fileToNotes source = do
  contents <- readFile source
  putStrLn (semicolForComma $ endOnChar contents ')')

fileToRhyth :: FilePath -> IO()
fileToRhyth source = do
  contents <- readFile source
  putStrLn (semicolForComma $ endOnChar contents 'n')


melRhythToMIDI :: FilePath -> FilePath -> FilePath -> IO ()
melRhythToMIDI note rhyth dest = do
  rhythm <- readFile rhyth
  pitch <- readFile note
  render_to dest (foldl1 (+:+) $ (compRhythPitch (put ( read ("["++(semicolForComma $ endOnChar pitch ')')++"]")::[Pitch])) (rhPut ( read ("["++(semicolForComma $ endOnChar rhythm 'n')++"]")::[Rhythm]))))
