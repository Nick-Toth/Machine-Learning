{- ************************************************
\\ File:  TrainingData.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 12, 2017
\\
// Overview: Training data generator for a basic
\\ perceptron implementation (Perceptron.hs).
//
\\ ************************************************ -}


module TrainingData( cnv_b,
                     genDataBundle,
                     printData) where

import System.Random


-- Type aliases for readability.
type Packet = (Float, Float)
type Bundle_A = [([Float], Float)]
type Bundle_B = [(Packet, Float)]


{- An infinite list of pseudo-randoms (0 to 1) from a seed. -}
sampleData :: Int -> [Float]
sampleData seed = map (flatten . fst) $ scanl (\(r, gen) _ -> random gen) ( random (mkStdGen seed) ) $ repeat ()


{- A list of tuples from sampleData of length size. -}
genTData :: Int -> Int -> [Packet]
genTData size seed = do
    let sd = sampleData seed
    zip (take size sd) (take size ( drop size sd ))


-- Rounds a float from (0, 1).
flatten :: Float -> Float
flatten flt | flt >= 0.5 = 1.0
            | otherwise = 0.0


{- A function of a tuple of Floats (each from 1 to 0), where
   { (0, 0) = 0 } , { (0, 1) = 0 } , { (1, 0) = 1 } , { (1, 1) = 1 } -}
target :: Packet -> Float
target gen_data | fst gen_data == 1 = 1
                | otherwise         = 0


{- A list of size tripples where the first two values are pseudo randoms and
   the third is a function of the first two, namely target.  -}
genDataBundle :: Int -> Int -> Bundle_B
genDataBundle size seed = do
    let input  = genTData size seed
        output = map target input
    zip input output


-- Display a data set
printData :: Bundle_B -> IO ()
printData training_data = do
    let a = map (show) training_data
    putStrLn "\n  Printing data:\n"
    mapM_ (putStrLn . ("    " ++)) a
    putStrLn "\n"


{- Convert a list of tuples of lists (with 2 elements) and single
   values into a list of tuples of tuples and single values. -}
cnv_a :: Bundle_A -> Bundle_B
cnv_a lst = cnv lst (\a b c -> ((a,b), c) ) (!! 0) (!! 1)

{- Convert a list of tuples of tuples and single values into
   a list of tuples of lists (with 2 elements) and single values. -}
cnv_b :: Bundle_B -> Bundle_A
cnv_b lst = cnv lst (\a b c -> (a:b:[], c)) fst snd

{- Convert between a list of tuples of tuples and single values, and
   a list of tuples of lists (with 2 elements) and single values. -}
cnv :: [(a1, t1)] -> (t2 -> t3 -> t1 -> a2) -> (a1 -> t2) -> (a1 -> t3) -> [a2]
cnv lst f g h = do
    let ulst = unzip lst
    [ f a b c | a <- map g (fst ulst),
                    b <- map h (fst ulst),
                    c <- snd ulst ]