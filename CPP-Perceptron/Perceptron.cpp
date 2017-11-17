/* ***********************************************************
\\ File:    Perceptron.cpp
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Feb. 15th, 2017
\\
// Overview: Implementation of the perceptron algorithm.
\\ Training data for this class is generated using the Data
// class (Data.h/cpp).
\\
// ***********************************************************/

#include "Perceptron.h"


/* *******************************************
\\ Initialize the perceptron. This is here
// to keep the constructors nice n' short.
\\
// *******************************************/
void Perceptron :: initPerceptron(void)
{
  // Print an alert that a new perceptron has been created.
  std::cout << "\nNew Perceptron Created!" << std::endl;

  // Initialize class fields.
  this->eta = 0.15; // Learning rate.
  this->theta = 0;  // Threshold.
  this->epoch_err = 0.0; // The total error for each epoch.

  // Stocastic initialization of synaptic weights
  // from 0 to 1 with 6 point precision.
  initWeights();
}


/* *******************************************
\\ Train the network on the given set of 
// training data.
\\
// *******************************************/
void Perceptron :: train(Data & training_set)
{
  // Condition for terminating the training loop. 
  bool epoch_complete = false;

  double recent_err = 0.0; // The error for a given training
                           // sample.

  short epochs = 0, // The total number of epochs for which
                    // the network has trained. 
        index = 0, // Index counter for traversing the 
                   // set of training data packets. 
        y = 0; // Network output.

  // The data packet from the training set
  // currently being trained on. Updated
  // at the beginning of the inner-loop.
  const short * data = nullptr;

  // While the training condition has not been met..
  while(!epoch_complete)
  {
    epoch_err = 0.0; // Reset error for each epoch.

    // For every data packet in the training set..
    for(index = 0; index < this->packets;++index)
    {
      // Store the next data packet.
      data = training_set.get(index);

      // Get the perceptron's output for the current 
      // training sample.
      y = getOutput(data);

      // If the network's output is not correct..
      if(y != data[this->num_inputs])
      {
        // Calculate and store the error for this 
        // incorrect output. **See the Recent Error
        // Chart in the description of updateWeights().
        recent_err = (data[this->num_inputs] - y);
    
        // Update the synaptic weights with the
        // recent error data.
        this->updateWeights(data, recent_err);
    
        // Add the recent error data to the total
        // error for this epoch.
        this->epoch_err += std::abs(recent_err);
      }
    }
    ++epochs; // Increment the epoch counter. 

    // If the perceptron has trained for 100 epochs or no errors 
    // were made in the last epoch, discontinue. 
    epoch_complete = (epochs == 100 || epoch_err == 0.0)? true : false;
  }

  // The perceptron training has succeeded.
  if(epoch_err == 0.0)
    // Print out the total number of epochs.
    std::cout << "\nPerceptron trained with a dataset of size " << this->packets
              << " in " << epochs << " epochs." << std::endl;
  // The perceptron training has failed.
  else 
    // Print out the perceptron's specifications.
    std::cout << "\nPerceptron training failed." << std::endl
              << epochs << " epochs completed" << std::endl
              << "Final epoch error: " << epoch_err << std::endl
              << "Size of dataset: " << this->packets << std::endl
              << "Learning rate: " << this->eta << std::endl;
}


/* *******************************************
\\ ∆ Weight = eta * (y - ŷ) * input 
// corresponding to the current weight 
\\ i.e. [input]. where:
//   eta = the perceptron's learning rate.
\\   y = Correct Output
//   ŷ = Perceptron Output
\\   
// Note that recent error (y - ŷ) is
\\ calculated in train().
//
\\         * Recent Error Chart *
//          ---------------------
\\          |  y  |  ŷ  | y - ŷ |
//          ---------------------
\\          |  0  |  0  |   0   |
//          ---------------------
\\          |  0  |  1  |   -1  |
//          ---------------------
\\          |  1  |  0  |   1   |
//          ---------------------
\\          |  1  |  1  |   0   |
//          ---------------------
\\
// *******************************************/
void Perceptron :: updateWeights( const short data[],
                                  double recent_err )
{
  // For every input/weight pair..
  for(short index = 0; index < this->num_inputs; ++index)
    // Calculate the ∆ Weight and add it to the weight at [index].
    this->weights[index] += eta * recent_err * data[index];
}


/* *******************************************
\\ After calculating the sum of the products 
// of each input and corresponding weight,
\\ sgn checks to see if that sum is greater 
// or less than the threshold (theta). Next,
\\ the output of the sgn is compared with 
// the target output for the data packet 
\\ currently being trained. i.e. This function
// returns the actual output of the perceptron
\\ for a given set if inputs.
// 
\\ The sgn aka sign function is defined as:
//
\\           { 0 | Y ≤ Threshold
// sgn (Y) = {
\\           { 1 | Y > Threshold
//
\\ *******************************************/
short Perceptron :: sgn(double y) const
{ return ( y <= this->theta )? 0 : 1; }


/* *******************************************
\\ Calculates and returns the perceptron's
// output(y) for each data entry. 
\\
// y = sgn ( ∑ⱼ WⱼXⱼ)
\\ 
// *******************************************/
short Perceptron :: getOutput(const short data[]) const
{
  double y = 0; // The perceptron's output.

  short index = 0; // Index counter traversing the inputs
                   // and synaptic weights. 

  // For every input and weight pair..
  for(; index < this->num_inputs; ++index)
    // Add the product of the corresponding weight and 
    // input for this iteration to the output.
    y += this->weights[index] * data[index];

  // Calculate and return the sgn of the output to train().
  return sgn(y);
}


/* *******************************************
\\ Create the perceptron's synaptic weight 
// vector, and initialize each weight to a 
\\ random number between zero and one tenth
// with 6 point precision.
\\
// *******************************************/
void Perceptron :: initWeights(void)
{
  short index = 0; // Index counter for initializing
                   // the perceptron's synaptic weights.

  // Allocate memory for synaptic weights. The number of 
  // weights is proportional to the number of inputs in
  // the dataset.
  this->weights = new double[num_inputs];

  // From 0 to the number of inputs in the dataset..
  for(; index < num_inputs; ++index)
    // Initialize the corresponding perceptron weight
    // to a random number between zero and one tenth
    // with 6 point precision..
    this->weights[index] = getRandom();
}