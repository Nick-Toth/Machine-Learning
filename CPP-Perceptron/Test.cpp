/* ***********************************************************
\\ File:    Test.cpp
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Feb. 15th, 2017
\\
// Overview: Simple test file for the perceptron defined in
\\ Perceptron.h/cpp, as well as the training data generator
// defined in Data.h/cpp.
\\
// The description of the perceptron algorithm is documented
\\ well in this project, however, it's rather plain. For a
// more interesting perceptron implementation, which trains
\\ using the mnist data set, check out my haskell perceptron
// at https://github.com/NickGThomas/Machine-Learning/tree/
\\ master/Haskell-Perceptron
//
\\ ***********************************************************/

#include "Perceptron.cpp"
#include "Data.cpp"

using namespace std;

int main()
{
  // Create the default dataset of 10 packets
  // of 2 random binary inputs & 1 target output.
  Data *ds1 = new Data(7, 10);

  // Create a perceptron and train on the 
  // dataset created above.
  Perceptron *p1 = new Perceptron(*ds1);

  // Deallocate the data_set and the perceptron.
  delete p1; p1 = nullptr;
  delete ds1; ds1 = nullptr;

  // Fin.
  return 0;
}
