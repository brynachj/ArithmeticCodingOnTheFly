module Arith_enc_states_scaling where

import Data.Ratio
import Data.List
import Random
import Maybe
import Learner
import System.IO

narrow :: Interval -> Interval -> Interval
narrow (l,r) (p,q) =  (l + (r-l)*p, l + (r-l)*q)

within :: Rational -> Interval -> Bool
within x (l,r) = (l<= x) && (x <r)

intWithin :: Interval -> Interval -> Bool
intWithin (x,y) (l,r) 	| (x>=l) && (y<=r) = True
			| otherwise = False


getFromSt1 :: [Int] -> [Int] -> Model -> Maybe (String, (Ratio Integer, Ratio Integer))
getFromSt1 [] _ _ = Nothing
getFromSt1 (x:xs) ys mod   | mayb == Nothing = getFromSt1 xs (ys++[x]) mod
                           | otherwise = mayb
                             where mayb = getElem (snd(mod !! 0)) (bitsToInt (map toRational ys))

factor2 :: Interval -> Interval -> Integer -> (Interval,Interval,Integer)
factor2 (a,b) (c,d) n | (b<=1/2) && (d<=1/2) = factor2 (2*a,2*b) (2*c,2*d) (2*n)
                      | (a>1/2) && (c>1/2) = factor2 ((2*a)-1,(2*b)-1) ((2*c)-1,(2*d)-1) (2*n)
                      | otherwise = ((a,b),(c,d),n)

--toUnit takes an interval i1 and an interval i2 WITHIN i1 and returns i2 scaled such that i1 = (0,1)

toUnit :: Interval -> Interval -> Interval
toUnit (x1,y1) (x2,y2) = ((1/(y1-x1))*(x2 - x1), (1/(y1-x1))*(y2- x1))

-- currModOver takes: 
-- a rational - denoting the rational from which to begin the state boundary
-- a state - denoting the state 

currModOver :: Rational -> [(String, Interval)] -> Interval -> [(String, Interval)]
currModOver _ [] _ = []
currModOver num ((x,(l,r)):xs) (a,b) = [(x,(num, num1))] ++ currModOver num1 xs (a,b)
	where w = b-a
	      y = r-l
	      num1 = (w*y) + num

pack :: Rational -> Rational -> Rational
pack b x = (b + x)/2

{-- GENERATE --}
generate :: Model -> String -> Int -> [String]
generate xs char int = generate_help xs char int (mkStdGen int)

--e.g. generate model_basic '1' 650
--returns: [2,3,2,3,2,1,2,1,3,2,2,2,2,1,1,2,1,1,2,3,1,1,1,2,3,3,2,3,1,1,2,1,3,1,1.... infinite

generate_help :: Model -> String -> Int -> StdGen -> [String]
generate_help xs char int std = [a] ++ generate_help xs a newInt newStd
	where
        (newInt, newStd) = randomR (1,1999) std       
	a = decodeSym xs char ((toInteger(int))%2000)

genToFile :: Model -> Int -> Int -> FilePath -> IO ()
genToFile model len ran dest = do  
    writeFile dest $ concat (take len $ generate model (fst(head(model))) ran)

genFromModelFile :: FilePath -> Int -> Int -> FilePath -> IO ()
genFromModelFile mod len ran dest = do
    model <- readFile mod
    writeFile dest ([head(model) ] ++  concat (take len $ generate (read (tail(tail(model)))::Model) [(head(model))] ran))


{-- ENCODE --}
encode :: [String] -> [Int]
encode str = encodeHelper (trainString str) (0,1) str

encodeWModel :: [String] -> Model -> [Int]
encodeWModel str mod = encodeHelper mod (0,1) str

encodeFromFile :: FilePath -> FilePath -> FilePath -> IO ()
encodeFromFile source dest model = do  
    contents <- readFile source  
    writeFile dest  (showing(encode [contents]) )
    writeFile model ([head(contents)] ++ "\n" ++ (show (trainString [contents])))

encodeFromFileWModel :: FilePath -> FilePath -> FilePath -> IO ()
encodeFromFileWModel source mod dest = do  
    contents <- readFile source  
    model <- readFile mod
    writeFile dest (showing(encodeWModel [contents] (read (tail(tail(model)))::Model)) )

encodeTest :: Model -> Int -> Int -> String -> [Int]
encodeTest mod ran num char = encodeWModel (take num (generate mod char ran)) mod

{-- DECODE  --}

decode:: [Int] -> Model -> [String]
decode input mod = (decodeHelper (snd (mod !! 0)) mod (0,1) input 1)
    where Just (a,b) = getFromSt1 input [] mod

decodeFromFile :: FilePath -> FilePath -> IO()
decodeFromFile source dest = do
  contents <- readFile source
  writeFile dest (concat (decode (stringToBin (tail(tail(contents)))) (read((lines contents) !! 0)::Model)))

decodeFromFileWModel :: FilePath -> FilePath -> FilePath -> IO ()
decodeFromFileWModel source mod dest = do  
    contents <- readFile source  
    model <- readFile mod
    writeFile dest ([head(model) ] ++ (concat ((decode (stringToBin (contents))) (read (tail(tail(model)))::Model))))

decodeTest :: Model -> Int -> Int -> [Char]
decodeTest mod ran takeInt = take takeInt $ decode (map fromIntegral (generate myBinMod "0" ran)) mod


{- TRANSLATE -}

translateFromFiles :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
translateFromFiles source mod1 mod2 dest = do
  str <- readFile source
  model1 <- readFile mod1
  model2 <- readFile mod2
  writeFile dest ([head(model2) ] ++ (concat (decode (encodeWModel [str] (read  (tail(tail(model1)))::Model)) (read (tail(tail(model2)))::Model))))
        

{- Dyadic Conversion -}

toDyadFromFile :: FilePath -> FilePath -> IO ()
toDyadFromFile source dest = do
	mod <- readFile source
	writeFile dest ([head(mod) ] ++ ['\n'] ++ (show(toDyad (read  (tail(tail(mod)))::Model))))

toDyadGranFromFile :: FilePath -> FilePath -> Int -> IO ()
toDyadGranFromFile source dest num = do
	mod <- readFile source
	writeFile dest  ([head(mod) ] ++ ['\n'] ++ (show(toDyadGranular (read (tail(tail(mod)))::Model) num)))

encodeHelper :: Model -> Interval -> [String] -> [Int]
encodeHelper _ _ [] = []
encodeHelper model (p,q) (x:[]) = a  ++ encodeHelper model newInterval stream
	where 
	(a, newInterval,stream)	 |(q <= 1%2) = ([0],(p*2,q*2), [x])
					 |(p >= 1%2) = ([1],((p*2)-1,(q*2)-1), [x])
   				         |otherwise  = ([],(0,1),[])
encodeHelper model (p,q) (x:xs) = a  ++ encodeHelper model newInterval stream
	where 
	(a, newInterval,stream, mod) 	|(q <= 1%2) = ([0],(p*2,q*2),(x:xs),model)
					|(p >= 1%2) = ([1],((p*2)-1,(q*2)-1),(x:xs),model)
   				        |otherwise     = ([],updatedInterval,xs,model)
	updatedInterval = (p,q) `narrow` (encodeSym model x (head(xs)))
			



{-- Decode --}

decodeHelper :: [Transition] -> Model -> Interval -> [Int] -> Integer -> [String]
decodeHelper _ _ _ [] _ = []
decodeHelper state model (l,r) (x:xs) n = a ++ decodeHelper newSt model newInt stream newN
	where
		(a,newSt,newInt,stream,newN) 	| maybeSym == Nothing = ([],state,updInt,xs,updN)
						| otherwise = ([sym],updSt,factLR,x:xs,freshN)
		updInt  | (x ==0)   = (l,l+(1%newN))
			| otherwise = (r-(1%newN),r)
		maybeSym | length model == 1 = getSingElem state (l,r)
			 | otherwise = getElem state (l,r)
		Just (sym,(int1,int2)) = maybeSym
		Just charIndex = elemIndex sym (map fst model)
		updSt   | length model == 1 = currModOver factInt1 (snd(head model)) (factInt1,factInt2)
                        | otherwise = currModOver factInt1 (snd (model !! (charIndex))) (factInt1,factInt2)
		updN = n*2
                (factLR, (factInt1, factInt2), factN) = factor2 (l,r) (int1,int2) 1
                freshN = div n factN

biggestDenom :: Interval -> Integer
biggestDenom (x,y)	| denominator(x) > denominator(y) = denominator(x*2)
			| otherwise = denominator(y*2)


{--Encoding to Rationals --}

stringToBin :: String -> [Int]
stringToBin [] = []
stringToBin (x:xs) = [read[x]::Int] ++ stringToBin xs

encodeSym :: Model -> String -> String -> Interval
encodeSym mod sym1 sym2 | length mod == 1 = sigProb
                        | otherwise = n
    where signifier = fst (head mod)
          (sigString,sigProb) = head[(s1,s2)| (s1,s2) <- snd(head mod), s1 == sym2]
          (a,b) = head[(x,y) | (x,y) <- mod, x == sym1]
          [(m,n)] = [(z,q) | (z,q) <- b, z == sym2]

decodeSym :: Model -> String -> Rational -> String
decodeSym [] _ _ = "-"
decodeSym ((x,y):xs) chr rat |length((x,y):xs) == 1 = result
                             |chr == x = result
		             |otherwise = decodeSym xs chr rat
	where
	[result] = [z1|(z1,z2) <- y, rat `within` z2 ]

getSymInt :: Model -> String -> Interval -> Maybe (String,Interval)
getSymInt [] _ _ = Nothing
getSymInt ((x,y):xs) char int | char == x = pair
			      | otherwise = getSymInt xs char int
    where pair  | length [(x1,x2)|(x1,x2) <- y, int `intWithin` x2 ] == 0 = Nothing
                | otherwise = Just $ head([(x1,x2)|(x1,x2) <- y, int `intWithin` x2 ])


getSingElem :: [(String,Interval)] -> Interval -> Maybe (String,Interval)
getSingElem [] _ = Nothing
getSingElem ((y,z):ys) int | y == "\167" = Just (y,z)
                         | int `intWithin` z =  Just (y,z)
			 | otherwise = getElem ys int

getElem :: [(String,Interval)] -> Interval -> Maybe (String,Interval)
getElem [] _ = Nothing
getElem ((y,z):ys) int  | int `intWithin` z =  Just (y,z)
			| otherwise = getElem ys int


{-- Binary operations --}

{--FROMBITS
Takes a binary string and outputs the midpoint of the interval represented by that binary string
Uses 'pack' to return the binary fraction obtained by adding a final 1 to the end of the given list
--}

fromBits :: [Bit] -> Rational
fromBits = foldr pack (0.5)

bitsToInt :: [Bit] -> Interval
bitsToInt xs = ((foldr pack (1%2) xs) - ((1%2)^((length xs) + 1)),(foldr pack (1%2) xs) + ((1%2)^((length xs) + 1)))

--e.g1. fromBits [0] = 1%4 -- since binary 0.0... is between 0 and 1/2
--e.g2. fromBits [0,1,0] = 5%16 -- since binary 0.010... is between 1/4 and 3/8

toBits :: Interval -> [Bit]
toBits = unfoldr nextBit


nextBit :: Interval -> Maybe(Bit,Interval)
nextBit (l,r)
	| r<=(1/2) = Just(0,(2*l,2*r))
	| (1/2)<=l = Just(1,(2*l-1,2*r-1))
	|otherwise = Nothing

showing :: [Int] -> String
showing [] = []
showing (x:xs) = (show x) ++ (showing xs)

{--TOBITS:
If r<1/2 then the binary expansion of any fraction x s.t. l < x< r begins with 0;
If l<1/2 it begins with a 1.
In the remaining case where l<1/2<r, [] is returned.
--}

