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
                    threshold,
                    sgn ) where

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


-- Calculate the perceptron's initial output.
--  ∑ Weights•Inputs - bias
sgn :: Inputs -> Weights -> Output
sgn inputs weights = sgn' $ (foldl (+) 0 (zipWith (*) inputs weights)) - threshold

-- Calculate the perceptron's binary output.
sgn' :: Output -> Output
sgn' y | y >= 0    = 1
       | otherwise = 0


-- Update the perceptron's weight list.
updateWeights :: Inputs -> Weights -> Target -> Float -> Outputs
updateWeights inputs weights target output = map (updateWeight (target - output)) (zip inputs weights)

-- Update weight corresponding to a single data packet.
updateWeight :: Float -> (Input, Weight) -> Output
updateWeight err_diff (input, weight) = weight + ( l_rate * input * err_diff)


{- Delegate of iter.
   Train the perceptron on a single data packet. -}
iter :: Inputs -> Weights -> Target -> Outputs
iter inputs weights target =
    -- Train the network on the current
    -- data packet. Return updated weights.
    updateWeights inputs weights target out
    -- Calculate the output for the current packet.
    where out = sgn inputs weights


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


-- Initialize the first train' call.
train :: DataSet -> Weights -> DataPacket
train inputs weights = train' inputs weights 1

-- Simulate the perceptron learning algorithm.
train' :: DataSet -> Weights -> Float -> DataPacket
train' inputs weights epochs
      -- If the epoch error is zero, return the results.
    | err == 0.0 = (updated_weights, epochs)
      -- If the previous epoch error was not zero, train another epoch.
    | otherwise = train' inputs updated_weights (epochs + 1)
        -- Train the perceptron (1 epoch).
        where (updated_weights, err) = epoch inputs weights
