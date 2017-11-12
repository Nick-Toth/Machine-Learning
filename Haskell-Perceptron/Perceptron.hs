{- ************************************************
\\ File:  Perceptron.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 12, 2017
\\
// Overview: Example of a perceptron in haskell.
\\
// ************************************************ -}


module Perceptron ( genWeights,
                    train,
                    l_rate,
                    threshold) where

import System.Random (getStdRandom, randomR)
import Control.Monad (replicateM)


-- Aliases for readability.
type Target = Float; type Weight = Float; type Output = Float; type Input = Float
type Weights = [Weight]; type Inputs = [Input]; type Outputs = [Output]
type DataPacket = (Inputs, Target); type DataSet = [DataPacket]


-- Default network parameters.
l_rate = 0.1 :: Float
threshold = 0.2 :: Float


-- Create a list of randomized floats in the range (-0.5, 0.5).
genWeights :: Int -> IO [Float]
genWeights weight_count = replicateM weight_count (getStdRandom $ randomR (-0.5, 0.5))


{- Calculate the output for an iteration.
   sgn(y, threshold) = 0 if y ≤ threshold
                       1 if y > threshold -}
sgn :: Inputs -> Weights -> Float -> Output
sgn inputs weights threshold'
    | y >= 0 = 1
    | otherwise = 0
      where y = (foldl (+) 0 (zipWith (*) inputs weights)) - threshold'


-- Update weight corresponding to a single data packet.
updateWeight :: Float -> (Input, Weight) -> Output
updateWeight err_diff (input, weight) = weight + ( l_rate * input * err_diff)


-- Update the perceptron's weight list.
updateWeights :: Inputs -> Weights -> Target -> Float -> Outputs
updateWeights inputs weights target output = map (updateWeight (target - output)) (zip inputs weights)


{- Delegate of iter.
   Train the perceptron on a single data packet. -}
iter :: Inputs -> Weights -> Target -> Outputs
iter inputs weights target = do
    -- Train the network on the current data packet.
    -- Return updated weights.
    updateWeights inputs weights target out
    -- Calculate the output for the current packet.
    where out = sgn inputs weights threshold


{- Delegate of train.
   Run each packet of data through the perceptron. -}
epoch :: DataSet -> Weights -> DataPacket
epoch inputs weights = (updated_weights, err)
    where
        -- Iter function alias for updating weights with foldl.
        iter_f weights (inputs, target) = iter inputs weights target
        -- Train the network on each data packet.
        updated_weights = foldl iter_f weights inputs
        -- Calculate the epoch error average.
        weight_len = fromIntegral $ length weights
        zipped_weights = map abs (zipWith (-) updated_weights weights)
        err = foldl (+) 0 zipped_weights / weight_len


{- Simulate the perceptron learning algorithm on
   some data set :: ([inputs], target output). -}
train :: DataSet -> Weights -> Float -> DataPacket
train inputs weights epochs
      -- If the epoch error (delta) is zero, return the results.
    | delta == 0 = (updated_weights, epochs)
      -- If the previous epoch error was not zero, train another epoch.
    | otherwise = train inputs updated_weights (epochs + 1)
        -- Train the perceptron (1 epoch).
        where (updated_weights, delta) = epoch inputs weights
