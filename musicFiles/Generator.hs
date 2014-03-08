module Generator where

import Data.Ratio
import Data.List
import Random
import Maybe

{--
Module Contents:
Datatypes  ................................. #1.1
General Use functions ...................... #1.2

One Note functions ......................... #2.0
 - Melody .................................. #2.1
 - Rhythm .................................. #2.2
 - Pitch Stream Generation ................. #2.3
 - Rhythm Stream Generation ................ #2.4
 - Examples ................................ #2.5

Two Note functions ......................... #3.0
 - Melody .................................. #3.1
 - Rhythm .................................. #3.2
 - Pitch Stream Generation ................. #3.3
 - Rhythm Stream Generation ................ #3.4
 - Examples ................................ #3.5

Three Note functions ....................... #4.0
 - Melody .................................. #4.1
 - Rhythm .................................. #4.2
 - Pitch Stream Generation ................. #4.3
 - Rhythm Stream Generation ................ #4.4
 - Examples ................................ #4.5


Module information:
This module is used to train Pitch and Rhythm Markov Models as well as generating pitch sequences and rhythm sequences based on these models.

-----------------------
-- ABSTRACT DATATYPE --
-----------------------

Interval - (a,b) - A tuple of Rational numbers less than 1, such that a is always less than b. Represents a probability interval.

----------------------------------------
-- SIMPLE DATATYPES (Single note states) --
----------------------------------------

-- PITCH --

Pitch - Defines the pitch of a note. Used to train Pitch Models. Represented using a capital letter or capital letter followed by a lowercase f or s (to indicate flat or sharp, respectively). e.g. Af - A flat; B - B; Cs - C sharp, etc.
PrbPair - Defines a trainsition tuple: A Pitch and it's associated probability interval.
PitchModel - Defines a Markov Model for pitches. A tuple: A State (Pitch) and associated transition tuples.

--------------------------------------
--------------------------------------

-- RHYTHM --

Rhythm - Defines the rhythm duration of a note. Used to train Rhythm Models. Represented by a capital letter followed by one or more lowercase letters (see music_haskore.hs for full list). e.g. Qn - Quarter note
RhPrbPair - Defines a trainsition tuple: A Rhythm and it's associated probability interval.
RhModel - Defines a Markov Model for rhythm. A tuple: A State (Rhythm duration) and associated transition tuples.

NOTE: Rhythms can be added to create more complex rhythms using the ::+ infix operator.
For example: (En ::+ En) is equivalent to Qn (Two eighth notes summed equals a quarter note).


--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------

---------------
-- FUNCTIONS --
---------------

-- PITCH --

trainPitches :: [Pitch] -> PitchModel
Takes a set of Pitches and returns a PitchModel (Markov Model) based on the probabilities of note-to-note transitions.

e.g.
Prelude> trainPitches [A,B,A,B,C,D,E,G,A]
[(A,[(B,(0 % 1,1 % 1))]),(B,[(A,(0 % 1,1 % 2)),(C,(1 % 2,1 % 1))]),(C,[(D,(0 % 1,1 % 1))]),(D,[(E,(0 % 1,1 % 1))]),(E,[(G,(0 % 1,1 % 1))]),(G,[(A,(0 % 1,1 % 1))])]

--This shows there's a probability interval of (0,1) of transitioning from A to B (i.e. 100%), and a probability interval of (0,1/2) of transitioning from B to A, and (1/2,1) of transitioning from B to C (i.e. 50% each), etc.

--------------------------------------
--------------------------------------

generate :: PitchModel -> Pitch -> Int -> [Pitch]
Takes a PitchModel, a start Pitch and an integer between 0 and 1000 (used as random seed); Generates an infinite Pitch stream.

e.g.
Prelude> let myPitchModel = trainPitches [A,B,A,B,C,D,E,G,A]
Prelude> generate myPitchModel A 121
[A,B,C,D,E,G,A,B,A,B,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,A,B,C,D,E,G,A,B,A,B,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,A,B,C,D,E,G,A,B,A,B,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,A,B,C,D,E,G,A,B,A,B,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,A,B,C,D,E,G,A,B,A,B,A,B,A,B,C,D,E,G,A,B,C,D,E,G,A,B,A,B,C,D,E,G,A,B,A,B,..............

--This stream continues infinitely. Since C,D,E and G only occur once, they have associated transitions that force them to always occur in that pattern followed by A. This is why we get ((A,B)*C,D,E,G,(A,B)*)*.

--------------------------------------
--------------------------------------

melodyToStream :: [Pitch] -> Pitch -> Int -> Int -> [Pitch]
Takes a list of pitches, a starting pitch, an integer between 0 and 1000 (random seed) and an integer (to determine the length of the output) and generates a stream of pitches, of specified length, from a model trained on the pitch list.

--e.g.
Prelude> melodyToStream [C,D,E,F,G,A,B,C,D,Ef,F,G,Af,Bf,C] C 121 100
[C,D,Ef,F,G,A,B,C,D,Ef,F,G,A,B,C,D,E,F,G,Af,Bf,C,D,Ef,F,G,A,B,C,D,Ef,F,G,Af,Bf,C,D,Ef,F,G,Af,Bf,C,D,E,F,G,Af,Bf,C,D,Ef,F,G,Af,Bf,C,D,E,F,G,A,B,C,D,E,F,G,A,B,C,D,E,F,G,Af,Bf,C,D,E,F,G,Af,Bf,C,D,Ef,F,G,A,B,C,D,Ef,F,G,Af,Bf,C,D]

--This stream is of length 100, starts on note C and is generated using a model based on the given pitch list ([C,D,E,F,G,A,B,C,D,Ef,F,G,Af,Bf,C]) and the integer 121.

--------------------------------------
--------------------------------------
######################################
--------------------------------------
--------------------------------------

-- RHYTHM --

trainRhythm :: [Rhythm] -> RhModel
Takes a set of Rhythms and returns a RhModel based on the probabilities of note-to-note transitions.

e.g.
Prelude> trainRhythm [Qn,Qn,Sn,Sn,Qn,En,En,En,En,Qn]
[(Qn,[(Qn,(0 % 1,1 % 3)),(Sn,(1 % 3,2 % 3)),(En,(2 % 3,1 % 1))]),(En,[(En,(0 % 1,3 % 4)),(Qn,(3 % 4,1 % 1))]),(Sn,[(Sn,(0 % 1,1 % 2)),(Qn,(1 % 2,1 % 1))])]

--This shows there's a probability interval of (0,1/3) of transitioning from Qn to Qn, a probability interval of (1/3,2/3) of transitioning from Qn to Sn, and a probability interval of (2/3,1) of transitioning from Qn to En (i.e. 33.3333...% each), etc.

--------------------------------------
--------------------------------------

rhGenerate :: RhModel -> Rhythm -> Int -> [Rhythm] 
Takes a Model, a starting Rhythm and an integer between 0 and 1000 (used as random seed); Generates an infinite Rhythm stream.

e.g.
Prelude> let myRhythModel = trainRhythm [Qn,Qn,Sn,Sn,Qn,En,En,En,En,Qn]
Prelude> rhGenerate myRhythModel Qn 873
[Qn,Qn,Sn,Qn,Sn,Qn,Qn,Sn,Sn,Sn,Sn,Qn,En,En,Qn,Sn,Qn,Qn,En,Qn,En,En,En,En,En,En,Qn,Sn,Sn,Qn,Qn,En,En,En,En,En,En,En,En,En,En,En,Qn,Qn,Qn,Sn,Qn,En,Qn,En,Qn,Qn,Sn,Qn,Sn,Qn,Qn,Sn,Sn,Sn,Sn,Qn,En,En,Qn,Sn,Qn,Qn,En,Qn,En,En,En,En,En,En,Qn,Sn,Sn,Qn,Qn,En,En,En,En,En,En,En,En,En,En,En,Qn,Qn,Qn,Sn,Qn,En,Qn,En,Qn,Qn,Sn,Qn,Sn,Qn,Qn,Sn,Sn,Sn,Sn,Qn,En,En,Qn,Sn,Qn,Qn,En,Qn,En,En,En,En,En,En,Qn,Sn,Sn,Qn,Qn,En,En,En,En,En,En,En,En,En,En,En,Qn,Qn,Qn,Sn,Qn,En,Qn,En,Qn,Qn,Sn,Qn,Sn,Qn,Qn,Sn,Sn,Sn,Sn,Qn,En,En,Qn,Sn,Qn,Qn,En,Qn,.......

--This stream continues infinitely.

--------------------------------------
--------------------------------------

rhythmToStream :: [Rhythm] -> Rhythm -> Int -> Int -> [Rhythm]
Takes a list of Rhythms, a starting Rhythm, an integer between 0 and 1000 (random seed) and an integer (to determine the length of the output) and generates a stream of Rhythms, of specified length, from a model trained on the pitch list.

--e.g. 
Prelude> rhythmToStream [Hn,Qn,Qn,Bn,Hn,Bn,Bn,Qn] Hn 197 100
[Hn,Bn,Qn,Bn,Hn,Bn,Qn,Bn,Bn,Bn,Bn,Qn,Bn,Hn,Qn,Qn,Bn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Bn,Hn,Qn,Bn,Hn,Bn,Bn,Hn,Bn,Bn,Qn,Bn,Bn,Qn,Qn,Bn,Bn,Qn,Bn,Hn,Bn,Qn,Bn,Bn,Bn,Bn,Qn,Bn,Hn,Qn,Qn,Bn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Bn,Hn,Qn,Bn,Hn,Bn,Bn,Hn,Bn,Bn,Qn,Bn,Bn,Qn,Qn]

--This stream is of length 100, begins with a Hn and is generated using a model based on the given rhythm list ([Hn,Qn,Qn,Bn,Hn,Bn,Bn,Qn]) and the integer 197.

--}

-- DATATYPES -- #1.1

type Interval = (Rational, Rational)


data Note = A|As|B|Bf|C|Cs|Df|D|Ds|Ef|E|F|Fs|Gf|G|Gs|Af|Z|Rest  deriving (Read,Ord,Show,Eq)
type Pitch = (Note,Int) 
type PrbPair = (Pitch,Interval)
type PitchModel = [(Pitch, [(Pitch,Interval)])]


data Rhythm = Bn|Wn|Hn|Qn|En|Sn|Tn|Sfn|Dwn|Dhn|Dqn|Den|Dsn|Dtn|Ddhn|Ddqn|Dden| Rhythm ::+ Rhythm deriving (Read,Ord,Show,Eq)
type RhPrbPair = (Rhythm,Interval)
type RhModel = [(Rhythm, [(Rhythm,Interval)])]




type PitchPair = (Pitch,Pitch)
type TwoPrbPair = (PitchPair ,Interval)
type TwoPitchModel =  [(PitchPair, [(PitchPair,Interval)])]

type RhythPair = (Rhythm, Rhythm)
type TwoRhPrbPair = (RhythPair, Interval)
type TwoRhModel = [(RhythPair, [(RhythPair, Interval)])]




type PitchTrip = (Pitch,Pitch,Pitch)
type ThPrbPair = (PitchTrip ,Interval)
type ThPitchModel =  [(PitchTrip, [(PitchTrip,Interval)])]

type RhythTrip = (Rhythm,Rhythm, Rhythm)
type ThRhPrbPair = (RhythTrip, Interval)
type ThRhModel = [(RhythTrip, [(RhythTrip, Interval)])]

{-- General use functions --}

flatten :: [[a]] -> [a]
flatten = foldl (++) []

unit :: Interval
unit = (0,1)

within :: Rational -> Interval -> Bool
within x (l,r) = (l<= x) && (x <r)

intWithin :: Interval -> Interval -> Bool
intWithin (x,y) (l,r) 	| (x>=l) && (y<=r) = True
			| otherwise = False

getPitch :: [PrbPair] -> Rational -> Pitch
getPitch [] _ = (Z,0)
getPitch mod rat |rat `within` snd(head(mod)) = (head(noteSet))
		|otherwise = getPitch (tail(mod)) rat
	where
	noteSet = [x|(x,_) <- mod] 

getRhythm :: [RhPrbPair] -> Rational -> Rhythm
getRhythm [] _ = Bn
getRhythm mod rat |rat `within` snd(head(mod)) = (head(rhythmSet))
		|otherwise = getRhythm (tail(mod)) rat
	where
	rhythmSet = [x|(x,_) <- mod]

getPitchPair :: [TwoPrbPair] -> Rational -> PitchPair
getPitchPair [] _ = ((Z,0),(Z,0))
getPitchPair mod rat |rat `within` snd(head(mod)) = (head(noteSet))
		|otherwise = getPitchPair (tail(mod)) rat
	where
	noteSet = [x|(x,_) <- mod] 

getRhythPair :: [TwoRhPrbPair] -> Rational -> RhythPair
getRhythPair [] _ = (Bn,Bn)
getRhythPair mod rat |rat `within` snd(head(mod)) = (head(noteSet))
		|otherwise = getRhythPair (tail(mod)) rat
	where
	noteSet = [x|(x,_) <- mod] 

getPitchTrip :: [ThPrbPair] -> Rational -> PitchTrip
getPitchTrip [] _ = ((Z,0),(Z,0),(Z,0))
getPitchTrip mod rat |rat `within` snd(head(mod)) = (head(noteSet))
		|otherwise = getPitchTrip (tail(mod)) rat
	where
	noteSet = [x|(x,_) <- mod] 

getRhythTrip :: [ThRhPrbPair] -> Rational -> RhythTrip
getRhythTrip [] _ = (Bn,Bn,Bn)
getRhythTrip mod rat |rat `within` snd(head(mod)) = (head(noteSet))
		|otherwise = getRhythTrip (tail(mod)) rat
	where
	noteSet = [x|(x,_) <- mod] 

difference :: Interval -> Rational
difference (x,y) = (y - x)

pick :: Interval -> Rational
pick (l,r) = (l+r)/2

pack :: Rational -> Rational -> Rational
pack b x = (b + x)/2

narrow :: Interval -> Interval -> Interval
narrow (l,r) (p,q) =  (l + (r-l)*p, l + (r-l)*q)

widen :: Interval -> Interval -> Interval
widen (a,b) (l,r) =  ((a-l)/(r-l),(b-l)/(r-l))

begins :: (Eq a) => [a] -> [a] -> Bool
begins xs ys 
	| stripPrefix xs ys == Nothing 	= False
	| otherwise			= True


---------------------------
-- SINGLE NOTE FUNCTIONS -- #2.0
---------------------------

--------------------------------------------------------------------
---------------Melody training (pitch, not rhythm)------------------ #2.1
--------------------------------------------------------------------

trainPitches :: [Pitch] -> PitchModel
trainPitches xs = step4 (step3 (step2 (step1 xs)))


--Step1 takes a note and it's following note and 'pairs' them, for each note in the melody (excluding the final note)

step1 :: [Pitch] -> [(Pitch,[(Pitch,Integer)])]
step1 (x:[]) = []
step1 (x:y:[]) = [(x,[(y,1)])]
step1 (x:y:xs) = [(x,[(y,1)])] ++ step1 (y:xs)


--Step2 orders and groups them into subarrays such that all note pairs "A,_" come first in their own aray, "As,_" come second in their own array, etc.

step2 :: [(Pitch,[(Pitch,Integer)])] -> [[(Pitch,[(Pitch,Integer)])]]
step2 inp = filter (not . null) [[((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==A], [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==As], [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Bf] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==B] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==C], [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Cs], [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Df] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==D], [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Ds] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Ef] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==E] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==F] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Fs] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Gf], [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==G] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Gs] , [((x1,x2),y)| ((x1,x2),y) <- inp, x1 ==Af]]

--Step3 assigns probabilities to the note pairs thusly:
--If the note pair 'A,B' occurs twice and 'A,C' occurs once, and these are the ONLY note pairs starting with A, 'A,B' is assigned 2/3 and 'A,C' is assigned 1/3.
--It takes the output of step2 (an ordered list of lists of note pairs) and ouputs a list of tuples:
--First element being a Pitch and the second element being all possible following notes and their probabilities
--e.g. If the initial note array was [A,B,C,D,A,B,A,B,A,C,A], step3 returns:
--[(A,[(B,3 % 4),(C,1 % 4)]),(B,[(C,1 % 3),(A,2 % 3)]),(C,[(D,1 % 2),(A,1 % 2)]),(D,[(A,1 % 1)])]
--it can be seen that B follows A 3/4 times and C follows A 1/4 times, C follows B 1/3 times and A follows B 2/3 times, etc.

step3 :: [[(Pitch,[(Pitch,Integer)])]] -> [(Pitch,[(Pitch, Rational)])]
step3 [] = []
step3 (x:xs) = [(fst(head(head(x:xs))), overRoutine temp2 [])] ++ step3 xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]

--overRoutine takes a list of (Pitch,Rational) pairs and a list of Pitches and returns the Pitches in the first list that are NOT included in the second list, along with the sum of their associated rationals
--e.g. overRoutine [(A,1%3),(B,1%4),(C,1%6),(A,1%8),(F,1%8)] [B,C] returns [(A,11 % 24),(F,1 % 8)]

overRoutine :: [(Pitch, Rational)] -> [Pitch] -> [(Pitch, Rational)]
overRoutine [] _ = []
overRoutine (x:xs) ys 	| (fst(x) `elem` ys) = overRoutine xs newYs
			| otherwise = [subroutine (x:xs)] ++ overRoutine xs newYs
	where newYs = ys ++ [fst(x)]

--subroutine takes a list of (Pitch,Rational) pairs and returns the first Pitch from the list and the sum of it's associated rationals
--e.g. [(A,1%2),(B,1%3),(A,1%6)] returns (A,2%3) since the sum of the rationals associated with A (being the first note in the list) is 2%3

subroutine :: [(Pitch, Rational)] -> (Pitch, Rational)
subroutine ((x1,y1):[]) = (x1,y1)
subroutine ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  subroutine ((x1,y1+y2):xs)
					| otherwise = subroutine ((x1,y1):xs)

{--
NOTE ON STEP 3: Step3 uses overRoutine, starting with an empty list as it's second argument, to build up a list of associated probabilities for note pairs.
--}

--step4 takes the output of step3 (A list of (pitch, [(Pitch,Rational)]) tuples) and converts the Rationals to intervals.

step4 :: [(Pitch, [(Pitch,Rational)])] -> [(Pitch, [(Pitch,Interval)])]
step4 [] = []
step4 (x:xs) = [(fst(x), step4_helper (snd(x))) ] ++ step4 xs

--step4 helper takes a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs that are equivalent.
--NOTE: If the sum of the rationals doesn't add to 1, it outputs a 'Z' Pitch at the end which will make the model unuseable in any further functions.
--Prelude> step4_helper [(A,1%3),(B,1%2),(C,1%6)]
--[(A,(0 % 1,1 % 3)),(B,(1 % 3,5 % 6)),(C,(5 % 6,1 % 1))]
--AND
--Prelude> step4_helper [(A,1%3),(B,1%2)]
--[(A,(0 % 1,1 % 3)),(B,(1 % 3,5 % 6)),(Z,(5 % 6,1 % 1))]

step4_helper :: [(Pitch, Rational)] -> [(Pitch,Interval)] 
step4_helper [] = []
step4_helper xs  
	| finalRat == 1%1 = step4_helper_helper 0 xs
	| finalRat < 1%1 = step4_helper_helper 0 xs ++ [((Z,0),(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (step4_helper_helper 0 xs))))

--step4_helper_helper takes a Rational and a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs, such that:
--Prelude> step4_helper_helper (0%1) [(A,1%2)]
--[(A,(0 % 1,1 % 2))]
--AND
--Prelude> step4_helper_helper (1%6) [(A,1%3),(B,1%2)]
--[(A,(1 % 6,1 % 2)),(B,(1 % 2,1 % 1))]

step4_helper_helper :: Rational -> [(Pitch, Rational)] -> [(Pitch,Interval)] 
step4_helper_helper _ [] = []
step4_helper_helper rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ step4_helper_helper (rat+y) xs



--------------------------------------------------------------------
-------------------------Rhythm training---------------------------- #2.2
--------------------------------------------------------------------
--NOTE: Process is the same as for melody, see above for function descriptions

trainRhythm :: [Rhythm] -> RhModel
trainRhythm xs = rhStep4 (rhStep3 (rhStep2 (rhStep1 xs)))

rhStep1 :: [Rhythm] -> [(Rhythm,[(Rhythm,Integer)])]
rhStep1 (x:[]) = []
rhStep1 (x:y:[]) = [(x,[(y,1)])]
rhStep1 (x:y:xs) = [(x,[(y,1)])] ++ rhStep1 (y:xs)

rhStep2 :: [(Rhythm,[(Rhythm,Integer)])] -> [[(Rhythm,[(Rhythm,Integer)])]]
rhStep2 inp = filter (not . null) [[(x,y)| (x,y) <- inp, x ==Bn], [(x,y)| (x,y) <- inp, x ==Wn], [(x,y)| (x,y) <- inp, x ==Hn] , [(x,y)| (x,y) <- inp, x ==Qn] , [(x,y)| (x,y) <- inp, x ==En], [(x,y)| (x,y) <- inp, x ==Sn], [(x,y)| (x,y) <- inp, x ==Tn] , [(x,y)| (x,y) <- inp, x ==Sfn], [(x,y)| (x,y) <- inp, x ==Dwn] , [(x,y)| (x,y) <- inp, x ==Dhn] , [(x,y)| (x,y) <- inp, x ==Dqn] , [(x,y)| (x,y) <- inp, x ==Den] , [(x,y)| (x,y) <- inp, x ==Dsn] , [(x,y)| (x,y) <- inp, x ==Dtn] , [(x,y)| (x,y) <- inp, x ==Ddhn] , [(x,y)| (x,y) <- inp, x ==Ddqn] , [(x,y)| (x,y) <- inp, x ==Dden]]



rhStep3 :: [[(Rhythm,[(Rhythm,Integer)])]] -> [(Rhythm,[(Rhythm, Rational)])]
rhStep3 [] = []
rhStep3 (x:xs) = [(fst(head(head(x:xs))), rhOverRoutine temp2 [])] ++ rhStep3 xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]

rhOverRoutine :: [(Rhythm, Rational)] -> [Rhythm] -> [(Rhythm, Rational)]
rhOverRoutine [] _ = []
rhOverRoutine (x:xs) ys 	| (fst(x) `elem` ys) = rhOverRoutine xs newYs
			| otherwise = [rhSubroutine (x:xs)] ++ rhOverRoutine xs newYs
	where newYs = ys ++ [fst(x)]

rhSubroutine :: [(Rhythm, Rational)] -> (Rhythm, Rational)
rhSubroutine ((x1,y1):[]) = (x1,y1)
rhSubroutine ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  rhSubroutine ((x1,y1+y2):xs)
					| otherwise = rhSubroutine ((x1,y1):xs)

rhStep4 :: [(Rhythm, [(Rhythm,Rational)])] -> [(Rhythm, [(Rhythm,Interval)])]
rhStep4 [] = []
rhStep4 (x:xs) = [(fst(x), rhStep4_helper (snd(x))) ] ++ rhStep4 xs

rhStep4_helper :: [(Rhythm, Rational)] -> [(Rhythm,Interval)] 
rhStep4_helper [] = []
rhStep4_helper xs  
	| finalRat == 1%1 = rhStep4_helper_helper 0 xs
	| finalRat < 1%1 = rhStep4_helper_helper 0 xs ++ [(Bn,(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (rhStep4_helper_helper 0 xs))))

rhStep4_helper_helper :: Rational -> [(Rhythm, Rational)] -> [(Rhythm,Interval)] 
rhStep4_helper_helper _ [] = []
rhStep4_helper_helper rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ rhStep4_helper_helper (rat+y) xs


--------------------------------------- #2.3
{------ PITCH STREAM GENERATION ------} 
---------------------------------------

generate :: PitchModel -> Pitch -> Int -> [Pitch] --Takes a PitchModel and a start Pitch and generates infinite note stream
generate xs note int =  [note] ++ generate xs noteNew newInt
	where
	(newInt, stdgen) = randomR (0,999) (mkStdGen int)
	noteNew = getPitch (snd(head(noteTrans))) ((toInteger(newInt))%1000)
	noteTrans = [(x,y)|(x,y) <- xs, x == note]

melodyToStream :: [Pitch] -> Pitch -> Int -> Int -> [Pitch]
melodyToStream xs note int amm = Prelude.take amm (generate (trainPitches xs) note int)

--e.g. let hunnedC = melodyToStream [C,D,E,F,G,A,B,C,D,Ef,F,G,Af,Bf,C] C 121 100
-- hunnedC
-- [C,D,Ef,F,G,A,B,C,D,Ef,F,G,A,B,C,D,E,F,G,Af,Bf,C,D,Ef,F,G,A,B,C,D,Ef,F,G,Af,Bf,C,D,Ef,F,G,Af,Bf,C,D,E,F,G,Af,Bf,C,D,Ef,F,G,Af,Bf,C,D,E,F,G,A,B,C,D,E,F,G,A,B,C,D,E,F,G,Af,Bf,C,D,E,F,G,Af,Bf,C,D,Ef,F,G,A,B,C,D,Ef,F,G,Af,Bf,C,D]

--------------------------------- #2.4
{------ RHYTHM GENERATION ------}
---------------------------------

rhGenerate :: RhModel -> Rhythm -> Int -> [Rhythm] --Takes a Model, a starting Rhythm and a random number and generates an infinite note stream
rhGenerate xs rhyth int =  [rhyth] ++ rhGenerate xs rhythmNew newInt
	where
	(newInt, stdgen) = randomR (0,999) (mkStdGen int)
	rhythmNew = getRhythm (snd(head(rhythmTrans))) ((toInteger(newInt))%1000)
	rhythmTrans = [(x,y)|(x,y) <- xs, x == rhyth]

rhythmToStream :: [Rhythm] -> Rhythm -> Int -> Int -> [Rhythm]
rhythmToStream xs rhyth int amm = Prelude.take amm (rhGenerate (trainRhythm xs) rhyth int)

--e.g. let hunnedQn = rhythmToStream [Hn,Qn,Qn,Bn,Hn,Bn,Bn,Qn] Hn 197 100
-- hunnedQn
-- [Hn,Bn,Qn,Bn,Hn,Bn,Qn,Bn,Bn,Bn,Bn,Qn,Bn,Hn,Qn,Qn,Bn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Bn,Hn,Qn,Bn,Hn,Bn,Bn,Hn,Bn,Bn,Qn,Bn,Bn,Qn,Qn,Bn,Bn,Qn,Bn,Hn,Bn,Qn,Bn,Bn,Bn,Bn,Qn,Bn,Hn,Qn,Qn,Bn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Hn,Qn,Qn,Qn,Bn,Bn,Hn,Qn,Bn,Hn,Bn,Bn,Hn,Bn,Bn,Qn,Bn,Bn,Qn,Qn]


-------------------------------
{------ MODEL UPDATING -------}
-------------------------------

--updateModel:: PitchModel -> [Pitch] -> PitchModel



---------------------- #2.5
{-- EXAMPLE MODELS --}
----------------------

cMaj = trainPitches [(A,0),(B,0),(C,1),(D,1),(E,1),(F,1),(G,1),(A,1)]

cJazzMin = trainPitches [(C,1),(D,1),(Ef,1),(F,1),(G,1),(A,1),(B,1),(C,2)]

cBlues7 = trainPitches [(C,1),(D,1),(Ef,1),(F,1),(Fs,1),(G,1),(Gf,1),(C,2)]

cBlues7Melody =  trainPitches [(C,1),(D,1),(Ef,1),(F,1),(Fs,1),(G,1),(Gf,1),(C,2),(Ef,2),(G,2),(F,2),(D,2),(Fs,2),(D,3),(C,1)]



--------------------------
--- TWO NOTE FUNCTIONS --- #3.0
--------------------------

pitchToPairs :: [Pitch] -> [PitchPair]
pitchToPairs (x:y:[]) = [(x,y)]
pitchToPairs (x:y:xs) = [(x,y)] ++ (pitchToPairs (y:xs))

--------------------------------------------------------------------
---------------Melody training (pitch, not rhythm)------------------ #3.1
--------------------------------------------------------------------

trainPitchPairs :: [Pitch] -> TwoPitchModel
trainPitchPairs xs = step4PPair (step3PPair (step2PPair (step1PPair (pitchToPairs xs)) []))


--Step1 takes a note and it's following note and 'pairs' them, for each note in the melody (excluding the final note)

step1PPair :: [PitchPair] -> [(PitchPair,[(PitchPair,Integer)])]
step1PPair (x:[]) = []
step1PPair (x:y:[]) = [(x,[(y,1)])]
step1PPair (x:y:xs) = [(x,[(y,1)])] ++ step1PPair (y:xs)


--Step2 orders and groups them into subarrays such that all note pairs "A,_" come first in their own aray, "As,_" come second in their own array, etc.
--Note: list2 starts off as empty list ALWAYS, and contains all pitch pairs that have already been seen

step2PPair :: [(PitchPair,[(PitchPair,Integer)])] -> [PitchPair] -> [[(PitchPair,[(PitchPair,Integer)])]]
step2PPair [] _ = []
step2PPair ((pitchpair,ys):xs) list2 = filter (not . null) [[(x,y)| (x,y) <- ((pitchpair,ys):xs), x ==pitchpair, x `notElem` list2]] ++ step2PPair xs newList2
    where newList2 = list2 ++ [pitchpair]



--Step3 assigns probabilities to the note pairs thusly:
--If the note pair 'A,B' occurs twice and 'A,C' occurs once, and these are the ONLY note pairs starting with A, 'A,B' is assigned 2/3 and 'A,C' is assigned 1/3.
--It takes the output of step2 (an ordered list of lists of note pairs) and ouputs a list of tuples:
--First element being a Pitch and the second element being all possible following notes and their probabilities
--e.g. If the initial note array was [A,B,C,D,A,B,A,B,A,C,A], step3 returns:
--[(A,[(B,3 % 4),(C,1 % 4)]),(B,[(C,1 % 3),(A,2 % 3)]),(C,[(D,1 % 2),(A,1 % 2)]),(D,[(A,1 % 1)])]
--it can be seen that B follows A 3/4 times and C follows A 1/4 times, C follows B 1/3 times and A follows B 2/3 times, etc.


step3PPair :: [[(PitchPair,[(PitchPair,Integer)])]] -> [(PitchPair,[(PitchPair, Rational)])]
step3PPair [] = []
step3PPair (x:xs) = [(fst(head(head(x:xs))), overRoutinePPair temp2 [])] ++ step3PPair xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]



--overRoutinePpair takes a list of (PitchPair,Rational) pairs and a list of PitchPairs and returns the PitchPairs in the first list that are NOT included in the second list, along with the sum of their associated rationals
--e.g. overRoutinePPair [(A,1%3),(B,1%4),(C,1%6),(A,1%8),(F,1%8)] [B,C] returns [(A,11 % 24),(F,1 % 8)]

overRoutinePPair :: [(PitchPair, Rational)] -> [PitchPair] -> [(PitchPair, Rational)]
overRoutinePPair [] _ = []
overRoutinePPair (x:xs) ys 	| (fst(x) `elem` ys) = overRoutinePPair xs newYs
			| otherwise = [subroutinePPair (x:xs)] ++ overRoutinePPair xs newYs
	where newYs = ys ++ [fst(x)]



--subroutine takes a list of (PitchPair,Rational) pairs and returns the first PitchPair from the list and the sum of it's associated rationals
--e.g. [(A,1%2),(B,1%3),(A,1%6)] returns (A,2%3) since the sum of the rationals associated with A (being the first note in the list) is 2%3

subroutinePPair :: [(PitchPair, Rational)] -> (PitchPair, Rational)
subroutinePPair ((x1,y1):[]) = (x1,y1)
subroutinePPair ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  subroutinePPair ((x1,y1+y2):xs)
					| otherwise = subroutinePPair ((x1,y1):xs)

{--
NOTE ON STEP 3: Step3PPair uses overRoutinePPair, starting with an empty list as it's second argument, to build up a list of associated probabilities for note pairs.

--}

--step4 takes the output of step3 (A list of (pitch, [(Pitch,Rational)]) tuples) and converts the Rationals to intervals.

step4PPair :: [(PitchPair, [(PitchPair,Rational)])] -> [(PitchPair, [(PitchPair,Interval)])]
step4PPair [] = []
step4PPair (x:xs) = [(fst(x), step4_helperPPair (snd(x))) ] ++ step4PPair xs

--step4 helper takes a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs that are equivalent.
--NOTE: If the sum of the rationals doesn't add to 1, it outputs a 'Z' Pitch at the end which will make the model unuseable in any further functions.
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3),((A,C),1%6)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((A,C),(5 % 6,1 % 1))]
--AND
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((Z,Z),(5 % 6,1 % 1))]

step4_helperPPair :: [(PitchPair, Rational)] -> [(PitchPair,Interval)] 
step4_helperPPair [] = []
step4_helperPPair xs  
	| finalRat == 1%1 = step4_helper_helperPPair 0 xs
	| finalRat < 1%1 = step4_helper_helperPPair 0 xs ++ [(((Z,0),(Z,0)),(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (step4_helper_helperPPair 0 xs))))

--step4_helper_helper takes a Rational and a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs, such that:
--Prelude> step4_helper_helperPPair (1%6) [((A,A),1%2)]
--[((A,A),(1 % 6,2 % 3))]
--AND
--Prelude> step4_helper_helperPPair 0 [((A,A),1%2),((A,B),1%2)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,1 % 1))]


step4_helper_helperPPair :: Rational -> [(PitchPair, Rational)] -> [(PitchPair,Interval)] 
step4_helper_helperPPair _ [] = []
step4_helper_helperPPair rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ step4_helper_helperPPair (rat+y) xs


----------------------
------- RHYTHM ------- #3.2
----------------------

rhythToPairs :: [Rhythm] -> [RhythPair]
rhythToPairs (x:y:[]) = [(x,y)]
rhythToPairs (x:y:xs) = [(x,y)] ++ (rhythToPairs (y:xs))



trainRhythPairs :: [Rhythm] -> TwoRhModel
trainRhythPairs xs = step4RPair (step3RPair (step2RPair (step1RPair (rhythToPairs xs)) []))


--Step1 takes a note and it's following note and 'pairs' them, for each note in the melody (excluding the final note)

step1RPair :: [RhythPair] -> [(RhythPair,[(RhythPair,Integer)])]
step1RPair (x:[]) = []
step1RPair (x:y:[]) = [(x,[(y,1)])]
step1RPair (x:y:xs) = [(x,[(y,1)])] ++ step1RPair (y:xs)


--Step2 orders and groups them into subarrays such that all rhythm pairs "Qn,_" come first in their own aray, "En,_" come second in their own array, etc.
--Note: list2 starts off as empty list ALWAYS, and contains all rhythm pairs that have already been seen

step2RPair :: [(RhythPair,[(RhythPair,Integer)])] -> [RhythPair] -> [[(RhythPair,[(RhythPair,Integer)])]]
step2RPair [] _ = []
step2RPair ((rhythpair,ys):xs) list2 = filter (not . null) [[(x,y)| (x,y) <- ((rhythpair,ys):xs), x ==rhythpair, x `notElem` list2]] ++ step2RPair xs newList2
    where newList2 = list2 ++ [rhythpair]



--Step3 assigns probabilities to the note pairs thusly:
--If the note pair 'A,B' occurs twice and 'A,C' occurs once, and these are the ONLY note pairs starting with A, 'A,B' is assigned 2/3 and 'A,C' is assigned 1/3.
--It takes the output of step2 (an ordered list of lists of note pairs) and ouputs a list of tuples:
--First element being a Pitch and the second element being all possible following notes and their probabilities
--e.g. If the initial note array was [A,B,C,D,A,B,A,B,A,C,A], step3 returns:
--[(A,[(B,3 % 4),(C,1 % 4)]),(B,[(C,1 % 3),(A,2 % 3)]),(C,[(D,1 % 2),(A,1 % 2)]),(D,[(A,1 % 1)])]
--it can be seen that B follows A 3/4 times and C follows A 1/4 times, C follows B 1/3 times and A follows B 2/3 times, etc.


step3RPair :: [[(RhythPair,[(RhythPair,Integer)])]] -> [(RhythPair,[(RhythPair, Rational)])]
step3RPair [] = []
step3RPair (x:xs) = [(fst(head(head(x:xs))), overRoutineRPair temp2 [])] ++ step3RPair xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]



--overRoutinePpair takes a list of (PitchPair,Rational) pairs and a list of PitchPairs and returns the PitchPairs in the first list that are NOT included in the second list, along with the sum of their associated rationals
--e.g. overRoutinePPair [(A,1%3),(B,1%4),(C,1%6),(A,1%8),(F,1%8)] [B,C] returns [(A,11 % 24),(F,1 % 8)]

overRoutineRPair :: [(RhythPair, Rational)] -> [RhythPair] -> [(RhythPair, Rational)]
overRoutineRPair [] _ = []
overRoutineRPair (x:xs) ys 	| (fst(x) `elem` ys) = overRoutineRPair xs newYs
			| otherwise = [subroutineRPair (x:xs)] ++ overRoutineRPair xs newYs
	where newYs = ys ++ [fst(x)]



--subroutine takes a list of (RhythPair,Rational) pairs and returns the first RhythPair from the list and the sum of it's associated rationals
--e.g. [((Qn,Qn),1%2),((En,Qn),1%3),((Qn,Qn),1%6)] returns ((Qn,Qn),2%3) since the sum of the rationals associated with Qn (being the first note in the list) is 2%3

subroutineRPair :: [(RhythPair, Rational)] -> (RhythPair, Rational)
subroutineRPair ((x1,y1):[]) = (x1,y1)
subroutineRPair ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  subroutineRPair ((x1,y1+y2):xs)
					| otherwise = subroutineRPair ((x1,y1):xs)


--NOTE ON STEP 3: Step3PPair uses overRoutinePPair, starting with an empty list as it's second argument, to build up a list of associated probabilities for note pairs.



--step4 takes the output of step3 (A list of (Rhythm, [(Rhythm,Rational)]) tuples) and converts the Rationals to intervals.

step4RPair :: [(RhythPair, [(RhythPair,Rational)])] -> [(RhythPair, [(RhythPair,Interval)])]
step4RPair [] = []
step4RPair (x:xs) = [(fst(x), step4_helperRPair (snd(x))) ] ++ step4RPair xs

--step4 helper takes a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs that are equivalent.
--NOTE: If the sum of the rationals doesn't add to 1, it outputs a 'Z' Pitch at the end which will make the model unuseable in any further functions.
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3),((A,C),1%6)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((A,C),(5 % 6,1 % 1))]
--AND
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((Z,Z),(5 % 6,1 % 1))]

step4_helperRPair :: [(RhythPair, Rational)] -> [(RhythPair, Interval)] 
step4_helperRPair [] = []
step4_helperRPair xs  
	| finalRat == 1%1 = step4_helper_helperRPair 0 xs
	| finalRat < 1%1 = step4_helper_helperRPair 0 xs ++ [((Bn,Bn),(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (step4_helper_helperRPair 0 xs))))

--step4_helper_helper takes a Rational and a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs, such that:
--Prelude> step4_helper_helperPPair (1%6) [((A,A),1%2)]
--[((A,A),(1 % 6,2 % 3))]
--AND
--Prelude> step4_helper_helperPPair 0 [((A,A),1%2),((A,B),1%2)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,1 % 1))]


step4_helper_helperRPair :: Rational -> [(RhythPair, Rational)] -> [(RhythPair,Interval)] 
step4_helper_helperRPair _ [] = []
step4_helper_helperRPair rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ step4_helper_helperRPair (rat+y) xs


------------------------------- #3.3
{------ NOTE GENERATION ------}
-------------------------------

generatePPair :: TwoPitchModel -> PitchPair -> Int -> [Pitch] --Takes a PitchModel and a start Pitch and generates infinite note stream
generatePPair xs notePair int =  [snd(notePair)] ++ generatePPair xs notePairNew newInt
	where
	(newInt, stdgen) = randomR (0,999) (mkStdGen int)
	notePairNew = getPitchPair (snd(head(noteTrans))) ((toInteger(newInt))%1000)
	noteTrans = [((x,y),z)|((x,y),z) <- xs, (x,y) == notePair]

melodyToStreamPPair :: [Pitch] -> PitchPair -> Int -> Int -> [Pitch]
melodyToStreamPPair xs notePair int amm = Prelude.take amm (generatePPair (trainPitchPairs xs) notePair int)

--e.g. melodyToStreamPPair [C,F,A,D,G,D,A,C,F,G,C,F,D,F,A,D,F,G,D,A,C,A,C,G,A,G,D,C,F,G] (A,C) 876 100
-- [C,F,D,F,G,C,F,D,F,G,C,F,G,D,C,F,D,F,G,D,A,C,G,A,G,D,A,C,G,A,G,D,A,C,F,D,F,A,D,G,D,A,C,F,G,D,A,C,A,C,F,D,F,A,D,G,D,C,F,G,C,F,G,D,C,F,D,F,G,D,A,C,G,A,G,D,A,C,G,A,G,D,A,C,F,D,F,A,D,G,D,A,C,F,G,D,A,C,A,C]

------------------------------- #3.4
{------ RHYTHM GENERATION ------}
-------------------------------

generateRPair :: TwoRhModel -> RhythPair -> Int -> [Rhythm] --Takes a PitchModel and a start Pitch and generates infinite note stream
generateRPair xs notePair int =  [snd(notePair)] ++ generateRPair xs notePairNew newInt
	where
	(newInt, stdgen) = randomR (0,999) (mkStdGen int)
	notePairNew = getRhythPair (snd(head(noteTrans))) ((toInteger(newInt))%1000)
	noteTrans = [((x,y),z)|((x,y),z) <- xs, (x,y) == notePair]

melodyToStreamRPair :: [Rhythm] -> RhythPair -> Int -> Int -> [Rhythm]
melodyToStreamRPair xs notePair int amm = Prelude.take amm (generateRPair (trainRhythPairs xs) notePair int)

--e.g. melodyToStreamRPair [Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn] (Qn,En) 293 100
-- [En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn]

--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

--------------------------
--- THREE NOTE FUNCTIONS --- #4.0
--------------------------

pitchToTrips :: [Pitch] -> [PitchTrip]
pitchToTrips (x:y:z:[]) = [(x,y,z)]
pitchToTrips (x:y:z:xs) = [(x,y,z)] ++ (pitchToTrips (y:z:xs))

--------------------------------------------------------------------
---------------Melody training (pitch, not rhythm)------------------ #4.1
--------------------------------------------------------------------

trainPitchTrips :: [Pitch] -> ThPitchModel
trainPitchTrips xs = step4PTh (step3PTh (step2PTh (step1PTh (pitchToTrips xs)) []))

--Step1 takes a note and it's following note and 'pairs' them, for each note in the melody (excluding the final note)

step1PTh :: [PitchTrip] -> [(PitchTrip,[(PitchTrip,Integer)])]
step1PTh (x:[]) = []
step1PTh (x:y:[]) = [(x,[(y,1)])]
step1PTh (x:y:xs) = [(x,[(y,1)])] ++ step1PTh (y:xs)


--Step2 orders and groups them into subarrays such that all note pairs "A,_" come first in their own aray, "As,_" come second in their own array, etc.
--Note: list2 starts off as empty list ALWAYS, and contains all pitch pairs that have already been seen

step2PTh :: [(PitchTrip,[(PitchTrip,Integer)])] -> [PitchTrip] -> [[(PitchTrip,[(PitchTrip,Integer)])]]
step2PTh [] _ = []
step2PTh ((pitchtrip,ys):xs) list2 = filter (not . null) [[(x,y)| (x,y) <- ((pitchtrip,ys):xs), x ==pitchtrip, x `notElem` list2]] ++ step2PTh xs newList2
    where newList2 = list2 ++ [pitchtrip]



--Step3 assigns probabilities to the note pairs thusly:
--If the note pair 'A,B' occurs twice and 'A,C' occurs once, and these are the ONLY note pairs starting with A, 'A,B' is assigned 2/3 and 'A,C' is assigned 1/3.
--It takes the output of step2 (an ordered list of lists of note pairs) and ouputs a list of tuples:
--First element being a Pitch and the second element being all possible following notes and their probabilities
--e.g. If the initial note array was [A,B,C,D,A,B,A,B,A,C,A], step3 returns:
--[(A,[(B,3 % 4),(C,1 % 4)]),(B,[(C,1 % 3),(A,2 % 3)]),(C,[(D,1 % 2),(A,1 % 2)]),(D,[(A,1 % 1)])]
--it can be seen that B follows A 3/4 times and C follows A 1/4 times, C follows B 1/3 times and A follows B 2/3 times, etc.


step3PTh :: [[(PitchTrip,[(PitchTrip,Integer)])]] -> [(PitchTrip,[(PitchTrip, Rational)])]
step3PTh [] = []
step3PTh (x:xs) = [(fst(head(head(x:xs))), overRoutinePTh temp2 [])] ++ step3PTh xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]



--overRoutinePpair takes a list of (PitchPair,Rational) pairs and a list of PitchPairs and returns the PitchPairs in the first list that are NOT included in the second list, along with the sum of their associated rationals
--e.g overRoutinePTh [(((A,0),(B,0),(C,0)),1%2),(((A,0),(B,0),(C,2)),1%3),(((A,0),(B,0),(C,0)),1%6)] []
-- [(((A,0),(B,0),(C,0)),2 % 3),(((A,0),(B,0),(C,2)),1 % 3)]


overRoutinePTh :: [(PitchTrip, Rational)] -> [PitchTrip] -> [(PitchTrip, Rational)]
overRoutinePTh [] _ = []
overRoutinePTh (x:xs) ys 	| (fst(x) `elem` ys) = overRoutinePTh xs newYs
			| otherwise = [subroutinePTh (x:xs)] ++ overRoutinePTh xs newYs
	where newYs = ys ++ [fst(x)]



--subroutine takes a list of (PitchPair,Rational) pairs and returns the first PitchPair from the list and the sum of it's associated rationals
--e.g. subroutinePTh [(((A,0),(B,0),(C,0)),1%2),(((A,0),(B,0),(C,2)),1%3),(((A,0),(B,0),(C,0)),1%6)]
-- (((A,0),(B,0),(C,0)),2 % 3)

subroutinePTh :: [(PitchTrip, Rational)] -> (PitchTrip, Rational)
subroutinePTh ((x1,y1):[]) = (x1,y1)
subroutinePTh ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  subroutinePTh ((x1,y1+y2):xs)
					| otherwise = subroutinePTh ((x1,y1):xs)

{--
NOTE ON STEP 3: Step3PPair uses overRoutinePPair, starting with an empty list as it's second argument, to build up a list of associated probabilities for note pairs.

--}

--step4 takes the output of step3 (A list of (pitch, [(Pitch,Rational)]) tuples) and converts the Rationals to intervals.

step4PTh :: [(PitchTrip, [(PitchTrip,Rational)])] -> [(PitchTrip, [(PitchTrip,Interval)])]
step4PTh [] = []
step4PTh (x:xs) = [(fst(x), step4_helperPTh (snd(x))) ] ++ step4PTh xs

--step4 helper takes a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs that are equivalent.
--NOTE: If the sum of the rationals doesn't add to 1, it outputs a 'Z' Pitch at the end which will make the model unuseable in any further functions.
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3),((A,C),1%6)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((A,C),(5 % 6,1 % 1))]
--AND
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((Z,Z),(5 % 6,1 % 1))]



step4_helperPTh :: [(PitchTrip, Rational)] -> [(PitchTrip,Interval)] 
step4_helperPTh [] = []
step4_helperPTh xs  
	| finalRat == 1%1 = step4_helper_helperPTh 0 xs
	| finalRat < 1%1 = step4_helper_helperPTh 0 xs ++ [(((Z,0),(Z,0),(Z,0)),(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (step4_helper_helperPTh 0 xs))))

--step4_helper_helper takes a Rational and a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs, such that:
--Prelude> step4_helper_helperPPair (1%6) [((A,A),1%2)]
--[((A,A),(1 % 6,2 % 3))]
--AND
--Prelude> step4_helper_helperPTh (1%2) [(((A,0),(B,0),(C,0)),1%2),(((A,0),(B,0),(C,2)),1%3),(((A,0),(B,0),(C,0)),1%6)] 
--[(((A,0),(B,0),(C,0)),(1 % 2,1 % 1))]



step4_helper_helperPTh :: Rational -> [(PitchTrip, Rational)] -> [(PitchTrip,Interval)] 
step4_helper_helperPTh _ [] = []
step4_helper_helperPTh rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ step4_helper_helperPTh (rat+y) xs

----------------------
------- RHYTHM ------- #4.2
----------------------

rhythToTrips :: [Rhythm] -> [RhythTrip]
rhythToTrips (x:y:z:[]) = [(x,y,z)]
rhythToTrips (x:y:z:xs) = [(x,y,z)] ++ (rhythToTrips (y:z:xs))




trainRhythTrips :: [Rhythm] -> ThRhModel
trainRhythTrips xs = step4RTh (step3RTh (step2RTh (step1RTh (rhythToTrips xs)) []))


--Step1 takes a note and it's following note and 'pairs' them, for each note in the melody (excluding the final note)

step1RTh :: [RhythTrip] -> [(RhythTrip,[(RhythTrip,Integer)])]
step1RTh (x:[]) = []
step1RTh (x:y:[]) = [(x,[(y,1)])]
step1RTh (x:y:xs) = [(x,[(y,1)])] ++ step1RTh (y:xs)


--Step2 orders and groups them into subarrays such that all rhythm pairs "Qn,_" come first in their own aray, "En,_" come second in their own array, etc.
--Note: list2 starts off as empty list ALWAYS, and contains all rhythm pairs that have already been seen

step2RTh :: [(RhythTrip,[(RhythTrip,Integer)])] -> [RhythTrip] -> [[(RhythTrip,[(RhythTrip,Integer)])]]
step2RTh [] _ = []
step2RTh ((rhythtrip,ys):xs) list2 = filter (not . null) [[(x,y)| (x,y) <- ((rhythtrip,ys):xs), x ==rhythtrip, x `notElem` list2]] ++ step2RTh xs newList2
    where newList2 = list2 ++ [rhythtrip]



--Step3 assigns probabilities to the note pairs thusly:
--If the note pair 'A,B' occurs twice and 'A,C' occurs once, and these are the ONLY note pairs starting with A, 'A,B' is assigned 2/3 and 'A,C' is assigned 1/3.
--It takes the output of step2 (an ordered list of lists of note pairs) and ouputs a list of tuples:
--First element being a Pitch and the second element being all possible following notes and their probabilities
--e.g. If the initial note array was [A,B,C,D,A,B,A,B,A,C,A], step3 returns:
--[(A,[(B,3 % 4),(C,1 % 4)]),(B,[(C,1 % 3),(A,2 % 3)]),(C,[(D,1 % 2),(A,1 % 2)]),(D,[(A,1 % 1)])]
--it can be seen that B follows A 3/4 times and C follows A 1/4 times, C follows B 1/3 times and A follows B 2/3 times, etc.


step3RTh :: [[(RhythTrip,[(RhythTrip,Integer)])]] -> [(RhythTrip,[(RhythTrip, Rational)])]
step3RTh [] = []
step3RTh (x:xs) = [(fst(head(head(x:xs))), overRoutineRTh temp2 [])] ++ step3RTh xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]



--overRoutinePpair takes a list of (PitchPair,Rational) pairs and a list of PitchPairs and returns the PitchPairs in the first list that are NOT included in the second list, along with the sum of their associated rationals
--e.g. overRoutinePPair [(A,1%3),(B,1%4),(C,1%6),(A,1%8),(F,1%8)] [B,C] returns [(A,11 % 24),(F,1 % 8)]

overRoutineRTh :: [(RhythTrip, Rational)] -> [RhythTrip] -> [(RhythTrip, Rational)]
overRoutineRTh [] _ = []
overRoutineRTh (x:xs) ys 	| (fst(x) `elem` ys) = overRoutineRTh xs newYs
			| otherwise = [subroutineRTh (x:xs)] ++ overRoutineRTh xs newYs
	where newYs = ys ++ [fst(x)]



--subroutine takes a list of (RhythPair,Rational) pairs and returns the first RhythPair from the list and the sum of it's associated rationals
--e.g. [((Qn,Qn),1%2),((En,Qn),1%3),((Qn,Qn),1%6)] returns ((Qn,Qn),2%3) since the sum of the rationals associated with Qn (being the first note in the list) is 2%3

subroutineRTh :: [(RhythTrip, Rational)] -> (RhythTrip, Rational)
subroutineRTh ((x1,y1):[]) = (x1,y1)
subroutineRTh ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  subroutineRTh ((x1,y1+y2):xs)
					| otherwise = subroutineRTh ((x1,y1):xs)


--NOTE ON STEP 3: Step3PPair uses overRoutinePPair, starting with an empty list as it's second argument, to build up a list of associated probabilities for note pairs.



--step4 takes the output of step3 (A list of (Rhythm, [(Rhythm,Rational)]) tuples) and converts the Rationals to intervals.

step4RTh :: [(RhythTrip, [(RhythTrip,Rational)])] -> [(RhythTrip, [(RhythTrip,Interval)])]
step4RTh [] = []
step4RTh (x:xs) = [(fst(x), step4_helperRTh (snd(x))) ] ++ step4RTh xs

--step4 helper takes a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs that are equivalent.
--NOTE: If the sum of the rationals doesn't add to 1, it outputs a 'Z' Pitch at the end which will make the model unuseable in any further functions.
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3),((A,C),1%6)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((A,C),(5 % 6,1 % 1))]
--AND
--Prelude> step4_helperPPair [((A,A),1%2),((A,B),1%3)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,5 % 6)),((Z,Z),(5 % 6,1 % 1))]

step4_helperRTh :: [(RhythTrip, Rational)] -> [(RhythTrip, Interval)] 
step4_helperRTh [] = []
step4_helperRTh xs  
	| finalRat == 1%1 = step4_helper_helperRTh 0 xs
	| finalRat < 1%1 = step4_helper_helperRTh 0 xs ++ [((Bn,Bn,Bn),(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (step4_helper_helperRTh 0 xs))))

--step4_helper_helper takes a Rational and a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs, such that:
--Prelude> step4_helper_helperPPair (1%6) [((A,A),1%2)]
--[((A,A),(1 % 6,2 % 3))]
--AND
--Prelude> step4_helper_helperPPair 0 [((A,A),1%2),((A,B),1%2)]
--[((A,A),(0 % 1,1 % 2)),((A,B),(1 % 2,1 % 1))]


step4_helper_helperRTh :: Rational -> [(RhythTrip, Rational)] -> [(RhythTrip,Interval)] 
step4_helper_helperRTh _ [] = []
step4_helper_helperRTh rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ step4_helper_helperRTh (rat+y) xs


------------------------------- #4.3
{------ NOTE GENERATION ------}
-------------------------------

generatePTh :: ThPitchModel -> PitchTrip -> Int -> [Pitch] --Takes a PitchModel and a start Pitch and generates infinite note stream
generatePTh xs noteTrip int =  [note] ++ generatePTh xs noteTripNew newInt
	where
        (_,_,note) = noteTrip
	(newInt, stdgen) = randomR (0,999) (mkStdGen int)
	noteTripNew = getPitchTrip (snd(head(noteTrans))) ((toInteger(newInt))%1000)
	noteTrans = [((x1,x2,x3),z)|((x1,x2,x3),z) <- xs, (x1,x2,x3) == noteTrip]



melodyToStreamPTh :: [Pitch] -> PitchTrip -> Int -> Int -> [Pitch]
melodyToStreamPTh xs noteTrip int amm = Prelude.take amm (generatePTh (trainPitchTrips xs) noteTrip int)

--e.g. melodyToStreamPPair [C,F,A,D,G,D,A,C,F,G,C,F,D,F,A,D,F,G,D,A,C,A,C,G,A,G,D,C,F,G] (A,C) 876 100
-- [C,F,D,F,G,C,F,D,F,G,C,F,G,D,C,F,D,F,G,D,A,C,G,A,G,D,A,C,G,A,G,D,A,C,F,D,F,A,D,G,D,A,C,F,G,D,A,C,A,C,F,D,F,A,D,G,D,C,F,G,C,F,G,D,C,F,D,F,G,D,A,C,G,A,G,D,A,C,G,A,G,D,A,C,F,D,F,A,D,G,D,A,C,F,G,D,A,C,A,C]

------------------------------- #4.4
{------ RHYTHM GENERATION ------}
-------------------------------

generateRTh :: ThRhModel -> RhythTrip -> Int -> [Rhythm] --Takes a PitchModel and a start Pitch and generates infinite note stream
generateRTh xs noteTrip int =  [note] ++ generateRTh xs noteTripNew newInt
	where
        (_,_,note) = noteTrip
	(newInt, stdgen) = randomR (0,999) (mkStdGen int)
	noteTripNew = getRhythTrip (snd(head(noteTrans))) ((toInteger(newInt))%1000)
	noteTrans = [((x1,x2,x3),z)|((x1,x2,x3),z) <- xs, (x1,x2,x3) == noteTrip]

melodyToStreamRTh :: [Rhythm] -> RhythTrip -> Int -> Int -> [Rhythm]
melodyToStreamRTh xs noteTrip int amm = Prelude.take amm (generateRTh (trainRhythTrips xs) noteTrip int)

--e.g. melodyToStreamRPair [Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn] (Qn,En) 293 100
-- [En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,En,En,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn,En,En,Qn,Qn,Qn,Qn,Qn,Qn,En,En,Qn,En,En,Qn,Qn]

