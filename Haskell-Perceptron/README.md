# Haskell-Perceptron
This is a simple perceptron implementation in haskell

## Overview

main (PerceptronTest.hs) currently trains a perceptron
to learn the binary function f a b = a
Training data is generated using TrainingData.hs

## Example Output


    Printing data:

    ((1.0,0.0),1.0)
    ((1.0,0.0),1.0)
    ((0.0,0.0),0.0)
    ((1.0,1.0),1.0)
    ((1.0,0.0),1.0)
    ((1.0,0.0),1.0)
    ((1.0,0.0),1.0)
    ((0.0,1.0),0.0)
    ((1.0,0.0),1.0)
    ((1.0,1.0),1.0)


    Perceptron trained with a dataset of size 10

    Epochs:        2.0
    Final Weights: [0.20775846,0.14747033]
    Learning rate: 0.1
    Threshold:     0.2


    Post-Training test:

    Complete input set: [[0.0,0.0],[0.0,1.0],[1.0,0.0],[1.0,1.0]]
    Correct outputs:    [0,0,1,1]
    Test Outputs:       [0,0,1,1]

    Goodbye!
