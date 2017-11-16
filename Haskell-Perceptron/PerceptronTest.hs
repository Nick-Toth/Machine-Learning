{- ************************************************
\\ File:  PerceptronTest.hs
// Name:  Nick G. Toth
\\ Email: ntoth@pdx.edu
// Date:  November 12, 2017
\\
// Overview: Test execution of the perceptron
\\ learning algorithm. For data source, see
// TrainingData.hs. For perceptron implementation
\\ see Perceptron.hs.
//
\\ ************************************************ -}


import Perceptron
import TrainingData


-- The number of packets in each bundle/epoch.
packets = 10 :: Int

-- For the random data generator.
seed = 0 :: Int


main :: IO ()
main = do

  -- Simulate the perceptron learning algorithm
  output <- test_gen

  -- Display the results of training the network.
  printMeta output

  {- Test final weights -}
  let complete_dataset = [[0,0], [0,1], [1,0], [1,1]]
      -- Extract final weights from output
      fin_wts = fst output
      -- Perceptron output function. ∑ Weights•Inputs - bias
      test [a, b] = (foldl (+) 0 (zipWith (*) [a, b] fin_wts)) - threshold
      -- Execute output function on testing set.
      -- If all goes well, tst_out = [0,0,1,1].
      tst_out = map ((\y -> if y >= 0 then 1 else 0) . test) complete_dataset

  putStrLn $ "\n\n  Post-Training test:\n"
          ++ "\n    Complete input set: " ++ show complete_dataset
          ++ "\n    Correct outputs:    " ++ "[0,0,1,1]"
          ++ "\n    Test Outputs:       " ++ show tst_out
          ++ "\n\n Goodbye!\n"


{- Simulate perceptron on random binary data
   created with the generator in TrainingData.hs.

   Binary function :=
      (0, 0) => 0
      (0, 1) => 0
      (1, 0) => 1
      (1, 1) => 1
-}
test_gen :: IO ([Float], Float)
test_gen = do

  -- Initialize a list of randomized weights.
  weights <- genWeights 2

      -- Generate random training data.
  let raw_data = genDataBundle packets seed
      -- Convert data format from [((Float, Float), Float)] to [([Float], Float)]
      bundle = cnv_b raw_data
      -- Train the network
      output = train bundle weights

  -- Display the test data.
  printData raw_data

  return output


-- Convert a string of binary digits into a list of bits (Float).
bitVec :: String -> [Float]
bitVec bitmap = map (\wrd -> read [wrd] :: Float) bitmap

-- Convert a list of strings of binary digits into a list of lists bits (Float).
bitVec_fromMtx :: [String] -> [[Float]]
bitVec_fromMtx bitmap = map (\x -> map ( \y -> (read y :: Float) ) (words x)) ( bitmap)


-- Print metadata for perceptron training algorithm results.
printMeta :: ([Float], Float) -> IO ()
printMeta output = putStrLn $
     "\n  Perceptron trained with a dataset of size " ++ show packets ++ "\n"
  ++ "\n    Epochs:        " ++ show epochs
  ++ "\n    Final Weights: " ++ show weights
  ++ "\n    Learning rate: " ++ show l_rate
  ++ "\n    Threshold:     " ++ show threshold
    where
      -- Extract data from output.
      weights = fst output
      epochs = snd output
