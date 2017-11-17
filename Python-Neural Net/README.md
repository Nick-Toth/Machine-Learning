# Pynet <img align="right" src="https://s11.postimg.org/wxur5xoj7/pynet.png">

Pynet is a basic feed forward neural network that I wrote as part of a technical writing assignment for school.
___

### Training data should be formatted as follows:

#####The head of the file should be an array where
* Index 0 denotes the number of input neurons.
* Indices 1..len-2 denote the number of hidden layers and the number of nodes in each layer.
* Index len-1 denotes the number of output neurons.

#####Everything below the head should take the form: a.. m.. where 
<p> </p>
* a..  is a series of input values. The length of the series should be equal to the value in the header array at index 0, minus the number of outputs.
* m..  is a series of target outputs.  The length of the series should be equal to the value in the header array at index len-1.

###  Here is an example of a valid dataset:
####  The header creates a network with..
* An input layer with 3 neurons.
* 2 hidden layers with 2 neurons each.
* An output layer with one neuron.

####  The training data under the header expresses a pattern where the output is the least common (between 0 and 1) of the inputs.

<div>
  <div> &nbsp&nbsp[ 3 , 2 , 2 , 1 ]</div> 
  <div> &nbsp&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp1 </div>  
  <div> &nbsp&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp1&nbsp&nbsp&nbsp1 </div>  
  <div> &nbsp&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp1&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp1 </div>  
  <div> &nbsp&nbsp&nbsp&nbsp0&nbsp&nbsp&nbsp1&nbsp&nbsp&nbsp1&nbsp&nbsp&nbsp0 </div>  
  <div> &nbsp&nbsp&nbsp&nbsp...&nbsp </div>  
</div>

#### To give you an idea of how the results are output, here are the results from the last 5 cycles of a 100 cycle iteration of the dataset described above.


Cycle # 96

	Network Input: [0, 0, 0]

	Targets: [1]

	Network Output: [0.9668580925834825]

	Last average error: 0.0331419074165



Cycle # 97

	Network Input: [0, 1, 0]

	Targets: [1]

	Network Output: [0.8022620315005046]

	Last average error: 0.197737968499



Cycle # 98

	Network Input: [1, 1, 0]

	Targets: [0]

	Network Output: [0.055542475281255006]

	Last average error: 0.0555424752813



Cycle # 99

	Network Input: [1, 0, 1]

	Targets: [0]

	Network Output: [0]

	Last average error: 0.00890305956605



Cycle # 100

	Network Input: [1, 1, 1]

	Targets: [0]

	Network Output: [0]

	Last average error: 0.023186649356
