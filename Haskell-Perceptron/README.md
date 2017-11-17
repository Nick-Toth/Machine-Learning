# Haskell-Perceptron
This is a simple perceptron implementation in haskell

## Overview

main (PerceptronTest.hs) currently trains a perceptron
to learn the binary function f a b = a
Training data is generated using TrainingData.hs


## Example Output (MNIST data)

    Perceptrons (10) trained with: 

      Training-data packets: 100
      Epochs:                [4.0,9.0,4.0,8.0,6.0,11.0,7.0,7.0,9.0,10.0]
      Learning rate:         2.0e-2
      Threshold:             2.0e-2

    -----------------------------------

    Post-Training test:

      Test data label: 1.0
      Correct outputs: [0,1,0,0,0,0,0,0,0,0]
      Test outputs:    [0,1,0,0,0,0,0,0,0,0]

    Goodbye!


## Example Output (Binary data)

    Printing data:

      ((1.0,1.0),1.0)
      ((0.0,1.0),0.0)
      ((0.0,1.0),0.0)
      ((0.0,0.0),0.0)
      ((0.0,1.0),0.0)
      ((1.0,1.0),1.0)
      ((0.0,1.0),0.0)
      ((0.0,1.0),0.0)
      ((1.0,0.0),1.0)
      ((1.0,0.0),1.0)


    Perceptron trained with:

      Training-data packets: 10
      Epochs:                3.0
      Final Weights:         [3.6854597e-3,3.9280136e-4]
      Learning rate:         2.0e-3
      Threshold:             2.0e-3
  
    -----------------------------------

    Post-Training test:

      Complete input set: [[0.0,0.0],[0.0,1.0],[1.0,0.0],[1.0,1.0]]
      Correct outputs:    [0,0,1,1]
      Perceptron outputs: [0,0,1,1]

    Goodbye!
