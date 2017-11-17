{- ************************************************
\\ File:  TrainingData.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 16, 2017
\\
// Overview: Tools for generating / formatting
\\ data for testing machine learning algorithms.
//
\\ ************************************************ -}

module TrainingData( cnv_a, cnv_b,  -- Convert between Bundle_A and Bundle_B
                     printBinData,  -- Display data set.
                     genDataBundle, -- Generate a set of binary training data.
                     scanMNIST,     -- Extract a random set of mnist data.
                     bitVec         -- String to [Float]. e.g., "012" => [0.0,1.0,2.0]
                   ) where

import System.Random
import Data.Functor
import Data.Int

-- Extract mnist data from .gz files.
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as B_Str


{-*************** Binary Data Tools ***************-}


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


-- Rounds a float in [0, 1].
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


-- Display a data set.
printBinData training_data = do
    let a = map show training_data
    putStrLn "\n  Printing data:\n"
    mapM_ (putStrLn . ("    " ++)) a
    putStr "\n"


{- Convert a list of tuples of lists (with 2 elements) and single
   values into a list of tuples of tuples and single values. -}
cnv_a :: Bundle_A -> Bundle_B
cnv_a lst = cnv lst (\a b c -> ((a,b), c) ) (!! 0) (!! 1)

{- Convert a list of tuples of tuples and single values into
   a list of tuples of lists (with 2 elements) and single values. -}
cnv_b :: Bundle_B -> Bundle_A
cnv_b lst = cnv lst (\a b c -> (a:b:[], c)) fst snd

{- Convert between Bundle_A and Bundle_B. -}
cnv :: [(a1, t1)] -> (t2 -> t3 -> t1 -> a2) -> (a1 -> t2) -> (a1 -> t3) -> [a2]
cnv lst f g h = do
    let ulst = unzip lst
    [ f a b c | a <- map g (fst ulst),
                b <- map h (fst ulst),
                c <- snd ulst ]



{-*************** MNIST data ***************-}


-- Alias for readability
type BStr = B_Str.ByteString
type StrPkt = (String, String) 

-- MNIST file names
(mnist_imgs, mnist_labs) = ("mnist-images.gz", "mnist-labels.gz") :: StrPkt

-- General data dimension specs.
(dim_sq, dims, dim_rng) = ( 28, 784, [0..27]) :: (Int64, Int64, [Int64])

-- Specifications for rendering bitmaps.
renderSpec :: Integral a => a -> Char
renderSpec n = specs !! (fromIntegral n * length specs `div` 256)
  where specs = "01" -- Characters to be used in bitmap generation.


-- Creates a bitmap for some image using renderSpec.
bitmap :: BStr -> Int64 -> [String]
bitmap img idx = [ (renderSpec.B_Str.index img.(idx * dims + 16 + r * dim_sq +)) <$> dim_rng | r <- dim_rng ]


-- Create a single data packet - (bitmap, label)
packet :: BStr -> BStr -> Int64 -> StrPkt
packet img lab idx = (unlines $ bitmap img idx, show (B_Str.index lab (idx + 8)))


-- Generate a random list of mnist data packets.
scanMNIST :: (Num t, Eq t) => t -> IO [StrPkt]
scanMNIST size = do
  -- Extract images and labels.
  img <- decompress <$> B_Str.readFile mnist_imgs
  lab <- decompress <$> B_Str.readFile mnist_labs
  -- Initialize list building
  scanMNIST' (img, lab) size []

-- Recursive delegate of scanMNIST for building a list of data packets.
scanMNIST' :: (Eq t, Num t) => (BStr, BStr) -> t -> [StrPkt] -> IO [StrPkt]
scanMNIST' _ 0 data_set = return data_set
scanMNIST' (img, lab) size data_set = do
  -- Generate random index for data sample.
  rand_idx <- (`mod` 60000) <$> randomIO
  -- Create a new data packet from random index.
  let pkt = packet img lab rand_idx
  -- Add data packet to list, continue building.
  scanMNIST' (img, lab) (size-1) (data_set ++ [pkt])


{- Generally useful formatting functions -}

-- Convert a string of binary digits into a list of bits (Float).
bitVec :: String -> [Float]
bitVec bitmap = map (\wrd -> read [wrd] :: Float) bitmap

-- Convert a list of strings of binary digits into a list of lists bits (Float).
bitVec_fromMtx :: [String] -> [[Float]]
bitVec_fromMtx bitmap = map (\x -> map ( \y -> (read y :: Float) ) (words x)) ( bitmap)
