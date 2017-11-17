/* ***********************************************************
\\ File:    Perceptron.h
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Feb. 15th, 2017
\\
// Overview: Declaration of the perceptron algorithm. See
\\ Perceptron.cpp (especially function comments) for more 
// information. See Data.h/cpp for information regarding the
\\ training data used to train this perceptron.
//
\\ ***********************************************************/

#include "Data.h"

#ifndef PERCEPTRON_H
#define PERCEPTRON_H

class Perceptron
{
  public: 

    // Constructor for perceptron without autotrain.
    Perceptron( short num_inputs,
                short packets ) : num_inputs(num_inputs),
                                  packets(packets)
    { initPerceptron(); }

    // Constructor for perceptron where training starts immediately. 
    Perceptron(Data & training_set) : num_inputs(training_set.getNumInputs()),
                                      packets(training_set.getNumPackets())
    { initPerceptron(); this->train(training_set); }

    // Deallocate synaptic weights.
    ~Perceptron(void) { delete[] weights; weights = nullptr; }

    // Train the perceptron on some dataset from the Data class.
    void train(Data &training_set);
    // Get the perceptron's output for a given data packet.
    short getOutput(const short data[]) const;

  private:

    // Called by the constructors of Perceptron for initializing class fields.
    void initPerceptron(void);
    // Update the synaptic weights during training.
    void updateWeights(const short data[], double recent_err);
    // Get the sgn of the perceptron's output during training.
    short sgn(double y) const;
    // Initialize the synaptic weights to random numbers between 
    // zero and one tenth with 6 point precision.
    void initWeights(void);
    // Get a random number between zero and one tenth with 6 point
    // precision for initializing the synaptic weights.
    double getRandom(void) const { return static_cast<double>(rand() % 100000) / 1000000; }


    double epoch_err, // The total error for a given epoch.
           eta,       // The perceptron's learning rate.
           * weights; // The perceptron's synaptic weights.

    short num_inputs, // The number of inputs in each data packet.
          packets,    // The number of packets in the dataset.
          theta;      // Perceptron SGN Threshold

};

#endif // PERCEPTRON_H