/* ***********************************************************
\\ File:    Data.h
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Feb. 15th, 2017
\\
// Overview: Declaration of a binary training data generator
\\ for testing the perceptron defined in Perceptron.cpp.
//
\\ ***********************************************************/

#include <iostream>
#include <cstdlib>
#include <cmath>

#ifndef DATA_GEN_H
#define DATA_GEN_H

#ifndef nullptr
#define nullptr NULL
#endif // nullptr

class Data
{
  public: 

    // Constructor for generating dataset with custom
    // packet / input specifications. Currently, this
    // class exclusively generates 2D data.
    Data( short num_inputs,
          short packets ) : num_inputs(num_inputs),
                            packets(packets)
    { createTrainingData(); }

    // Constructor for a default 10 packet, 2 input dataset.
    Data(void);
    // Deallocate dataset.
    ~Data(void);

    // Create a set of training data for the Perceptron.
    void createTrainingData(void);
    // Print out the dataset.
    void printData(void) const;
    // Generate a random bit.
    short randBit(void) const { return ( rand() % 2 ); }
    // Return the number of inputs in each data packet.
    short getNumInputs(void) const { return num_inputs; }
    // Return the number of packets in the dataset.
    short getNumPackets(void) const { return packets; }

    // Retrieve data from the dataset.
    const short * get(short index) const { return testing_ds[index]; }

  private:

    // The training dataset.
    short ** testing_ds;

    short num_inputs, // The number of inputs in each data packet.
          packets; // The number of packets in the dataset.
};

#endif // DATA_GEN_H
