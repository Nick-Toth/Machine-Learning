""" ***********************************************************
\\ File:    Network.py
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Oct, 2016 / Nov 17th, 2017
\\
// Overview: A simple feed-forward neural network
\\ implementation. See Neuron.py for Neuron and Connection
// class implementations. See Data.py for training data
\\ information. See Driver.py for a network test.
//
\\ ******************************************************** """ 


from Neuron import Neuron
from math import sqrt


class Network:

    lstErrAvg = 0.0 # Updated network error average
    adjerror = 100.0 # Factor in error adjustment.

    # top (topology) is an array of values, where index 0 points to the input layer,
    # index 1..n-1 points to the hidden layer(s) and index n points to the output layer.
    # The values are the number of neurons to be created in the corresponding layer.
    def __init__( self, top ):

        # Contains all of the network's layers
        self.layers = [ ]

        # Calculate total number of layers
        layercnt = len(top)
        # If there is more than one hidden layer, add the hidden layers!

        # Create input, hidden and output layers.
        # Layer class initializes the neurons.
        for l in range(layercnt):
            print "\n\tCreating layer!"

            # Number of neurons based on top + 1 bias neuron on each layer
            neuroncnt = top[l] + 1

            # The number of "real" neurons on the next layer (the outputs of each neuron)
            nextneuroncnt = 0 if l == layercnt - 1 else top[ l + 1 ]

            self.layers.append( Layer( neuroncnt, nextneuroncnt ))


    # Inputs are fed directly into feedForward from the network driver.
    # The input layer is fed into each neuron in the following layer.
    def feedForward( self , inputs ):

        # Attach input to corresponding input-neurons
        for n in range( len(inputs) ):
            self.layers[0].neurons[n].outputVal = inputs[n]

        # Forward propagation
        for l in range( 1 , len(self.layers) ):
            prevLayer = self.layers[ l - 1 ]

            for n in self.layers[l].neurons[ : -1 ]:

                # For each neuron (except those in the input layer),
                # call that neurons feed forward, with an argument
                # of the entire previous layer.
                n.feedForward(prevLayer)


    # After the inputs traverse the network, backprop is called to
    # update all of the Connections (weight, deltaweight) in the network
    def backProp( self, targets ):

        outputLayer = self.layers[ len(self.layers) - 1 ]
        avgerr = 0.0

        # Update network's error average
        for n in range( len( outputLayer.neurons) - 1 ):
            delta = targets[n] - outputLayer.neurons[n].outputVal
            avgerr += delta * delta

        # Calculate the square root of the error average, divided by the
        # number of non-bias neurons in the output layer
        avgerr /= len(outputLayer.neurons) - 1
        avgerr = sqrt(avgerr)

        # Set updated network error average
        self.lstErrAvg = ( self.lstErrAvg *
                              self.adjerror + avgerr) / \
                            ( self.adjerror + 1.0 )

        # For every output neuron, calculate and store the gradient
        # given the corresponding target output
        for n in range( len(outputLayer.neurons) - 1 ):
            outputLayer.neurons[n].setOGrads( targets[n] )

        # Calculate and set gradients in the hidden layer
        for l in range( len(self.layers) - 2, 0, -1 ):
            hiddenLayer = self.layers[l]
            nextLayer = self.layers[ l + 1 ]

            # For every neuron in the current hidden layer,
            # calculate and store the gradients given the
            # neurons in the next (right/output) layer.
            for n in hiddenLayer.neurons:
                n.setHGrads(nextLayer)

        # For each neuron (except those in the input layer)
        for l in range( len(self.layers) - 1, 0, -1 ):
            layer = self.layers[l]
            lastLayer = self.layers[ l - 1 ]

            # Update Connection weights.
            for neuron in layer.neurons[ : -1 ]:
                neuron.updInpWts(lastLayer)


    def printAvgErr(self, erravg): print erravg


    # Retrieve the network's output.
    def getResults( self ):
        results = [ ]
        for neuron in self.layers[ -1 ].neurons[ : -1 ]:
            # Prevents negative outputs
            if( neuron.outputVal < 0 ): results.append(0)
            else: results.append(neuron.outputVal)
        return results


# Each Layer instance contains a different layer of neurons
# All of the network's Layer instances are stored in Network,
# in an array called layers.
class Layer:
    def __init__( self, neuroncnt, outputcnt ):

        self.neurons = [ ]

        for n in range( neuroncnt ):
            self.neurons.append( Neuron(outputcnt, n) )

        # force the bias nodes's output to 1.0. it's the last neuron created above
        self.neurons[-1].outputVal = 1