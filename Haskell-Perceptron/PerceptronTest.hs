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
import TrainingData -- Training data generation


-- The number of packets in each training data bundle.
packets = 150 :: Int
-- For the random data generator.
seed = 0 :: Int


main :: IO ()
main = do
  --test_gen -- Simulate perceptron with binary input data.
  test_mnist -- Simulate perceptron with mnist data.
  putStrLn "\n\n  Goodbye!\n"

--(foldl (+) 0 (zipWith (*) [0.0,1.0] [3.2759907e-3,2.4698852e-3])) - threshold
{- Simulate perceptron on random binary data
   created with the generator in TrainingData.hs.

   Binary function :=
      (0, 0) => 0
      (0, 1) => 0
      (1, 0) => 1
      (1, 1) => 1
-}
test_gen :: IO ()
test_gen = do
  -- Initialize a list of randomized weights.
  weights <- genWeights 2

  let packs = min packets 10
      -- Generate random training data.
      raw_data = genDataBundle packs seed
      -- Convert data format from [((Float, Float), Float)] to [([Float], Float)]
      -- Use explicit bundle for guaranteed correct output.
      bundle = cnv_b raw_data -- bundle = [([0,0], 0), ([0,1],0), ([1,0],1), ([1,1],1)]
      -- Train the network
      output = train bundle weights
      -- Extract data from output.
      fnl_wts = fst output
      epochs = snd output
      -- Perceptron output function. ∑ Weights•Inputs - bias
      pcep_test tst_inp = (foldl (+) 0 (zipWith (*) tst_inp fnl_wts)) - threshold
      test_set = [[0,0], [0,1], [1,0], [1,1]]
      -- Execute output function on testing set.
      tst_out = map ((\y -> if y >= 0 then 1 else 0) . pcep_test) test_set

  -- Display the test data.
  printBinData raw_data

  -- Print general metadata about the perceptron's training
  putStrLn $ "\n  Perceptron trained with:\n"
          ++ "\n    Training-data packets: " ++ show packs
          ++ "\n    Epochs:                " ++ show epochs
          ++ "\n    Final Weights:         " ++ show fnl_wts
          -- ++ "\n    Final Weights: " ++ show weights
          ++ "\n    Learning rate:         " ++ show l_rate
          ++ "\n    Threshold:             " ++ show threshold
          -- Separate metadata and test data.
          ++ "\n\n    " ++ take 35 (cycle "-")
          ++ "\n\n  Post-Training test:\n"
          ++ "\n    Complete input set: " ++ show test_set
          ++ "\n    Correct outputs:    " ++ "[0,0,1,1]"
          ++ "\n    Perceptron outputs: " ++ show tst_out


{- Simulate perceptron algorithm for classifying
   hand written digits from the mnist data set.
   
   Creates a list of 10 perceptrons (each corresponding to
   a hand written digit), and trains them to classify the
   mnist data set.
-}
test_mnist :: IO ()
test_mnist = do

  -- Initialize a list of randomized weights.
  weights <- genWeights 784

  -- mnist training data.
  trn_data <- scanMNIST packets
  tst_data <- scanMNIST 1

      -- Remove newlines, and convert training data strings to floats.
  let bitmap_strs = map (\img -> bitVec (filter (/= '\n') img)) (map fst trn_data)
      -- Convert labels from string to float.
      labels = map (\lab -> read lab :: Float) (map snd trn_data)
      -- Bundle training data and labels.
      bundle = zip bitmap_strs labels
      -- Train each perceptron. The digit specified by x corresponds
      -- to the digit that the perceptron will learn to classify.
      output = map (\x -> train_cl bundle weights x) [0..9]
      -- De-list testing data.
      (inp_str, lab_str) = tst_data !! 0
      -- Convert input data to list of Floats.
      tst_inp = bitVec (filter (/= '\n') inp_str)
      tst_lab = read lab_str :: Float
      -- Extract final weights for the perceptron
      -- corresponding to the selected test data.
      fnl_wts = fst (output !! round tst_lab)
      -- Calculate the perceptron's output.
      pcep_test = binCond (>= 0) (foldl (+) 0 (zipWith (*) tst_inp fnl_wts) - threshold)

  -- Print general metadata about training
  putStrLn $ "\n  Perceptrons (10) trained with: \n"
          ++ "\n    Training-data packets: " ++ show packets
          ++ "\n    Epochs:                " ++ show (map snd output)
          ++ "\n    Learning rate:         " ++ show l_rate
          ++ "\n    Threshold:             " ++ show threshold
          ++ "\n\n  " ++ take 35 (cycle "-")
          ++ "\n\n  Post-Training test:\n"
          ++ "\n    Test data label: " ++ show tst_lab
          ++ "\n    Correct outputs: " ++ show (outputVec tst_lab)
          ++ "\n    Test outputs:    " ++ show (outputVec pcep_test)


-- A conditional function which returns 1 for true, 0 for false.
binCond :: Num p => (t -> Bool) -> t -> p
binCond cnd x = if cnd x then 1 else 0

-- Creates a list of 10 bits. The bit at index valid is 1, others are all 0.
outputVec :: (Eq a, Num b, Num a, Enum a) => a -> [b]
outputVec valid = map (binCond (== valid)) [0..9]
