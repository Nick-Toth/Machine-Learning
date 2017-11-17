""" ***********************************************************
\\ File:    Neuron.py
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Oct, 2016 / Nov 17th, 2017
\\
// Overview: Neuron and Connection class definitions for a
\\ simple feed-forward neural network. See Network.py for
// more info.
\\
// ******************************************************** """ 


from math import sqrt, tanh
import random


class Neuron:

    # .2 works well for the generic dataset
    # Probably keep well under 1.0 unless you know what you're doing
    l_rate = 0.2

    # Adds a fraction of the previous weight update to the current weight update.
    # Greater momentum may require lower learning speed.
    momentum = 0.5

    def __init__( self, numOutputs, index ):

        # Stores connections (weights) between neurons.
        self.outputWeights = [ ]

        # Initializing connection weights
        for w in range(numOutputs):
            self.outputWeights.append( Connection( ) )

        self.outputVal = 0.0   #  <-\
        self.gradient = 0.0    #   <-- Initializing class fields.
        self.index = index     #  <-/

        print "\n\tNeuron Created!"

    # Neuron.feedForward is passed in the previous layer of neurons from Network.feedForward.
    def feedForward( self, prevLayer ):
        sumPrev = 0.0

        for prevNeuron in prevLayer.neurons:
            # The weights and outputs of the neurons in the previous layer are multiplied and summed.
            sumPrev += prevNeuron.outputVal * prevNeuron.outputWeights[self.index].weight

        # The neurons output is set to the transfer functions output, given the sum.
        self.outputVal = Neuron.tf(sumPrev)


    #  Called by backprop to calculate and set the output neuron gradients
    def setOGrads( self, target ):
        delta = target - self.outputVal
        self.gradient = delta * Neuron.tf_dx(self.outputVal)


    # Called by backprop to calculate and set the hidden neuron gradients
    def setHGrads( self, nextLayer ):
        dow = self.calcDow(nextLayer)
        self.gradient = dow * Neuron.tf_dx(self.outputVal)

    # Used to calculate gradients
    def calcDow( self, nextLayer ):
        sum = 0.0
        for n in range( len( nextLayer.neurons ) - 1 ):
            sum += self.outputWeights[n].weight * nextLayer.neurons[n].gradient
        return sum

    # Called by backprop to update the output weights (weight and delta)
    # of the neurons in the last layer.
    def updInpWts( self , lastLayer ):

        for n in range( len( lastLayer.neurons )):

            neuron = lastLayer.neurons[n]
            lastDelta = neuron.outputWeights[self.index].delta

            newDelta = self.l_rate * neuron.outputVal * self.gradient + self.momentum * lastDelta

            neuron.outputWeights[self.index].delta = newDelta
            neuron.outputWeights[self.index].weight += newDelta


    # Transfer function.
    @staticmethod
    def tf(n): return tanh(n)
    # tf derivative.
    @staticmethod
    def tf_dx(x): return 1 - tanh(x) ** 2


# Each neuron contains an array of Connections,
# which store the synaptic weights.
class Connection:
    def __init__(self):
        self.weight = Connection.rndWt( )
        self.delta = 0.0

    # Random initial-weight generator.
    @staticmethod
    def rndWt( ): return random.uniform( 0, 1 )