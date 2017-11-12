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
  -- Initialize a list of randomized weights.
  weights <- genWeights packets

      -- Generate random training data.
  let raw_data = genDataBundle packets seed
      -- Convert data format from [((Float, Float), Float)] to [([Float], Float)]
      cnv_data = cnv_b raw_data

  -- Display the test data.
  printData raw_data

  -- Train the network
  let output = train cnv_data weights 1

  -- Display the results of training the network.
  putStrLn $ "\n  Perceptron trained with a dataset of size " ++ show packets
          ++ " in " ++ show (snd output) ++ " epochs."
          ++ "\n  Learning rate: " ++ show l_rate
          ++ "\n  Threshold: " ++ show threshold
          ++ "\n\n  Goodbye!\n"