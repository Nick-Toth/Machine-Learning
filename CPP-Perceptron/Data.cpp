/* ***********************************************************
\\ File:    Data.cpp
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Feb. 15th, 2017
\\
// Overview: Implementation of a binary training data
\\ generator for testing the perceptron defined in 
// Perceptron.cpp. See function comments for implementation
\\ details.
//
\\ ***********************************************************/

#include "Data.h"


/* ******************************************
\\ Default Constructor for generating a 
// dataset with ten packets, each with two 
\\ inputs and one target output.
// 
\\ ******************************************/
Data :: Data()
{
    // Init perceptron parameters.
    this->num_inputs = 2;
    this->packets = 10;

    createTrainingData(); // Create a training dataset.
}


/* ******************************************
\\ Deallocate the dataset.
//
\\ ******************************************/
Data :: ~Data(void)
{
  // For each data packet in the dataset..
  for(short index = 0; index < this -> packets; ++index)
  {
    // Deallocate that data packet.
    delete[] testing_ds[index];
    testing_ds[index] = NULL;
  }

  // Deallocate the dataset.
  delete[] testing_ds; testing_ds = NULL;
}


/* ******************************************
\\ Generates a set of 2D linearly separable 
// training data for the perceptron. Each 
\\ data packet in the set consists of two 
// inputs and the target output. Each of 
\\ the two inputs, in this example, are set
// to the output of a random bit generator.
\\ The target output is set to 0 if the 
// first input is a 0, or a 1 if the first 
\\ input is a 1. 
//
\\       * Possible Data Packets *
//          -------------------
\\          |  I₁ |  I₂ |  O  |
//          -------------------
\\          |  0  |  0  |  0  | => Type 1
//          -------------------
\\          |  0  |  1  |  0  | => Type 2
//          -------------------
\\          |  1  |  0  |  1  | => Type 3
//          -------------------
\\          |  1  |  1  |  1  | => Type 4
//          -------------------
\\
// ******************************************/
void Data :: createTrainingData(void)
{
          // Outer, inner loop index counters.
    short index = 0, jndex = 0,
          // The first input for each data packet.
          fst = 0; 

    // Allocate memory for the test dataset.
    this -> testing_ds = new short *[this -> packets];

    // For every packet in the dataset..
    for(; index < this -> packets; ++index)
        // Allocate memory for the packet to store
        // the inputs and the target output.
        this -> testing_ds[index] = new short[this -> num_inputs + 1];

    // For every packet in the dataset..
    for(index = 0; index < this -> packets; ++index)
    {
        // For every input in the current packet..
        for(jndex = 0; jndex < this -> num_inputs; ++jndex)
            // Set the input to a random bit.
            this -> testing_ds[index][jndex] = this -> randBit();

        // Store the value of the first input
        fst = testing_ds[index][0];

        // If the first input is 0..
        if(fst == 0)
            // Set the target output to 0.
            this -> testing_ds[index][num_inputs] = 0;
        // If the first input is 1..
        else
            // Set the target output to 1.
            this -> testing_ds[index][num_inputs] = 1;
    }

    //printData(); // Print out the newly generated data.
}


/* ******************************************
\\ Prints out the contents of the generated 
// dataset.
\\
// ******************************************/
void Data :: printData(void) const
{
    short index = 0, // dataset index counter.
          jndex = 0; // Data packet index counter.

    // For every packet of data in the dataset..
    for(; index < this -> packets; ++index)
    {
        // On a new line, print out a label for the index
        // of the dataset containing the packet.
        std :: cout << "\ndata[" << index << "] =>";

        // For every datum in the packet..
        for(jndex = 0; jndex < this -> num_inputs + 1; ++jndex)
            // Print out a space, followed by the datum.
            std :: cout << ' ' << (this -> testing_ds[index][jndex]);
    }
    std::cout << std::endl;
}