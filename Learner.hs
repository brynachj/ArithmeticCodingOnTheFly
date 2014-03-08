module Learner where

import Ratio
import List
import Random
import Maybe
import Data.Int
import Data.List

type Interval = (Rational, Rational)
type Bit = Rational
type Transition = (String,Interval)
type Model = [(String, [Transition])]


myBinMod = frequencyLearn "01"


--FREQUENCY CHARACTER MODERL TRAINING (Models based purely on character frequency)

frequencyLearn :: String -> Model -- Trains a character frequency model from an input.
frequencyLearn str = step4 [("\167",freqStr str (length str))]

frequencyLearnFromFile :: FilePath -> FilePath -> IO()
frequencyLearnFromFile source dest = do
	contents <- readFile source
	writeFile dest ([head(contents)] ++ "\n" ++  (show (frequencyLearn contents)))

freqStr :: String -> Int -> [(String,Rational)]
freqStr [] _ = []
freqStr (s:str) strLen = [([s],(toInteger(length(filter (==s) (s:str)))%(toInteger strLen)))] ++ freqStr (filter (/=s) str) strLen


{-- 
train :: FilePath -> [(String,Rational)] -> IO()
train dest given = do
	writeFile dest (show (step4 [('\167', given)]))
--}

--TRAINING A 1-CHARACTER CONTEXT MODEL FROM AN INPUT

trainStringFromFile :: FilePath -> FilePath -> IO()
trainStringFromFile source dest = do
	contents <- readFile source
	writeFile dest ([head(contents)] ++ "\n" ++ (show (trainString [contents])))

trainStringToFile :: String -> FilePath -> IO() -- Trains a 1-character context model from an input.
trainStringToFile string dest = do
	writeFile dest ([head(string)] ++ "\n" ++ (show (trainString [string])))

trainString :: [String] -> Model
trainString inp  | (elemIndex "\n" (map fst xs) == Nothing) = xs ++ [("\n",[(head(inp),(0%1,1%1))])]
                 | otherwise = xs
                 where xs = trainStringHelp inp

trainStringHelp :: [String] -> Model
trainStringHelp xs = step4 (step3 (step2 (step1 xs) []))


--HELPER FUNCTIONS

flatten :: [[a]] -> [a]
flatten = foldl (++) []


--Step1 takes a char and it's following char and 'pairs' them, for char note in the input (excluding the final note)

step1 :: [String] -> [(String,[(String,Integer)])]
step1 (x:[]) = []
step1 (x:y:[]) = [(x,[(y,1)])]
step1 (x:y:xs) = [(x,[(y,1)])] ++ step1 (y:xs)


--Step2 orders and groups them into subarrays such that all char pairs "'char1',_" come first in their own aray, "'char2',_" come second in their own array, etc.

step2 :: [(String,[(String,Integer)])] -> [String] -> [[(String,[(String,Integer)])]]
step2 [] _ = []
step2 ((y1,y2):ys) list2 = filter (not . null) [[(x1,x2)| (x1,x2) <- ((y1,y2):ys), x1 == y1, x1 `notElem` list2]] ++ step2 ys newList2
    where newList2 = list2 ++ [y1]

--Step3 assigns probabilities to the note pairs thusly:
--If the note pair 'A,B' occurs twice and 'A,C' occurs once, and these are the ONLY note pairs starting with A, 'A,B' is assigned 2/3 and 'A,C' is assigned 1/3.
--It takes the output of step2 (an ordered list of lists of note pairs) and ouputs a list of tuples:
--First element being a Pitch and the second element being all possible following notes and their probabilities
--e.g. If the initial note array was [A,B,C,D,A,B,A,B,A,C,A], step3 returns:
--[(A,[(B,3 % 4),(C,1 % 4)]),(B,[(C,1 % 3),(A,2 % 3)]),(C,[(D,1 % 2),(A,1 % 2)]),(D,[(A,1 % 1)])]
--it can be seen that B follows A 3/4 times and C follows A 1/4 times, C follows B 1/3 times and A follows B 2/3 times, etc.

step3 :: [[(String,[(String,Integer)])]] -> [(String,[(String, Rational)])]
step3 [] = []
step3 (x:xs) = [(fst(head(head(x:xs))), overRoutine temp2 [])] ++ step3 xs
	where
		temp1 = flatten [y | (_,y) <- x]
		temp2 = [(x,(y%(toInteger(length(temp1)))))|(x,y) <- temp1]

--overRoutine takes a list of (Pitch,Rational) pairs and a list of Pitches and returns the Pitches in the first list that are NOT included in the second list, along with the sum of their associated rationals
--e.g. overRoutine [(A,1%3),(B,1%4),(C,1%6),(A,1%8),(F,1%8)] [B,C] returns [(A,11 % 24),(F,1 % 8)]

overRoutine :: [(String, Rational)] -> [String] -> [(String, Rational)]
overRoutine [] _ = []
overRoutine (x:xs) ys 	| (fst(x) `elem` ys) = overRoutine xs newYs
			| otherwise = [subroutine (x:xs)] ++ overRoutine xs newYs
	where newYs = ys ++ [fst(x)]

--subroutine takes a list of (Pitch,Rational) pairs and returns the first Pitch from the list and the sum of it's associated rationals
--e.g. [(A,1%2),(B,1%3),(A,1%6)] returns (A,2%3) since the sum of the rationals associated with A (being the first note in the list) is 2%3

subroutine :: [(String, Rational)] -> (String, Rational)
subroutine ((x1,y1):[]) = (x1,y1)
subroutine ((x1,y1):(x2,y2):xs) 	| (x1 == x2) =  subroutine ((x1,y1+y2):xs)
					| otherwise = subroutine ((x1,y1):xs)

{--
NOTE ON STEP 3: Step3 uses overRoutine, starting with an empty list as it's second argument, to build up a list of associated probabilities for note pairs.
--}

--step4 takes the output of step3 (A list of (pitch, [(Pitch,Rational)]) tuples) and converts the Rationals to intervals.

step4 :: [(String, [(String,Rational)])] -> [(String, [(String,Interval)])]
step4 [] = []
step4 (x:xs) = [(fst(x), step4_helper (snd(x))) ] ++ step4 xs

--step4 helper takes a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs that are equivalent.
--NOTE: If the sum of the rationals doesn't add to 1, it outputs a 'Z' Pitch at the end which will make the model unuseable in any further functions.
--Prelude> step4_helper [(A,1%3),(B,1%2),(C,1%6)]
--[(A,(0 % 1,1 % 3)),(B,(1 % 3,5 % 6)),(C,(5 % 6,1 % 1))]
--AND
--Prelude> step4_helper [(A,1%3),(B,1%2)]
--[(A,(0 % 1,1 % 3)),(B,(1 % 3,5 % 6)),(Z,(5 % 6,1 % 1))]

step4_helper :: [(String, Rational)] -> [(String,Interval)] 
step4_helper [] = []
step4_helper xs  
	| finalRat == 1%1 = step4_helper_helper 0 xs
	| finalRat < 1%1 = step4_helper_helper 0 xs ++ [("?",(finalRat,1%1))]
	| otherwise = []
	where
	finalRat = (snd(snd(last (step4_helper_helper 0 xs))))

--step4_helper_helper takes a Rational and a set of (Pitch,Rational) pairs and returns the set of (Pitch,Interval) pairs, such that:
--Prelude> step4_helper_helper (0%1) [(A,1%2)]
--[(A,(0 % 1,1 % 2))]
--AND
--Prelude> step4_helper_helper (1%6) [(A,1%3),(B,1%2)]
--[(A,(1 % 6,1 % 2)),(B,(1 % 2,1 % 1))]

step4_helper_helper :: Rational -> [(String, Rational)] -> [(String,Interval)] 
step4_helper_helper _ [] = []
step4_helper_helper rat ((x,y):xs) 
	| (rat + y) > 1 = []
	| otherwise = [(x,(rat,rat+y))] ++ step4_helper_helper (rat+y) xs


{-- Dyadic Conversion --}

ratRound :: Rational -> Rational -> Rational
ratRound num x | num == 0 = 0
               | otherwise = (round (num / x)) % denominator(x)

intervalRound :: Interval -> Rational -> Interval
intervalRound (x,y) num = (ratRound x num, ratRound y num)

toDyad :: Model -> Model
toDyad [] = []
toDyad (x:xs) = [(fst(x), [(z1,intervalRound z2 (1%(2^16)))| (z1,z2) <-snd(x)])] ++ toDyad xs

toDyadGranular :: Model -> Int -> Model
toDyadGranular [] _ = []
toDyadGranular (x:xs) num = [(fst(x), [(z1,intervalRound z2 (1%(2^num)))| (z1,z2) <-snd(x)])] ++ toDyadGranular xs num
