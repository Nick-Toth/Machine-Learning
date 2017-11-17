{- ************************************************
\\ File:  Perceptron.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 12, 2017
\\
// Overview: A typical perceptron implementation.
\\
// ************************************************ -}


module Perceptron ( genWeights,
                    train,
                    train_cl,
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
l_rate = 0.002 :: Float
threshold = 0.002 :: Float


{- Target transformation for training multiple perceptrons in parallel.
   For example, in classifying the minst dataset, this function maps
   each target to 1. This is done to maintain binary inputs/outputs. -}
targTrans :: Target -> Target -> Target
targTrans target target' | target == target' = 1
                         | otherwise = 0


-- Create a list of randomized floats in the range (-0.5, 0.5).
genWeights :: Int -> IO [Float]
genWeights weight_count = replicateM weight_count (getStdRandom $ randomR (-0.5, 0.5))


-- Calculate the perceptron's initial output.
--  ∑ Weights•Inputs - bias
sgn :: Inputs -> Weights -> Output
sgn inputs weights = sgn' $ (foldl (+) 0 (zipWith (*) inputs weights)) - threshold

-- Calculate the perceptron's binary output.
sgn' :: Output -> Output
sgn' y | y >= 0 = 1
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
epoch :: DataSet -> Weights -> Target -> DataPacket
epoch inputs weights target' = (u_weights, err)
    where
      -- Iter function alias for updating weights with foldl.
      iter_f weights (inputs, target) = iter inputs weights target
      -- Train the network on each data packet.
      u_weights = foldl iter_f weights inputs
      -- Calculate the epoch error average.
      weight_len = fromIntegral $ length weights
      zipped_weights = map abs (zipWith (-) u_weights weights)
      err = foldl (+) 0 zipped_weights / weight_len


{- Initialize the first train' call (for binary perceptron).
   @inputs: A list of tuples containing training data and labels.
   @weights: A list of starting weights.
-}
train :: DataSet -> Weights -> DataPacket
train inputs weights = train' inputs weights 1 1


{- Initialize the first train' call (for parallel perceptrons).
   @target': If a given perceptron is being trained to classify
             the hand written digit 8, then target' should be 8. -}
train_cl :: DataSet -> Weights -> Target -> DataPacket
train_cl inputs weights target' = train' inputs weights 1 target'


-- Simulate the perceptron learning algorithm.
train' :: DataSet -> Weights -> Float -> Target -> DataPacket
train' inputs weights epochs target'
      -- If the epoch error is zero, return the results.
    | err == 0.0 = (u_weights, epochs)
      -- If the previous epoch error was not zero, train another epoch.
    | otherwise = train' inputs u_weights (epochs + 1) target'
        -- Train the perceptron (1 epoch).
        where (u_weights, err) = epoch [ (fst x, targTrans (snd x) target') | x <- inputs] weights target'
