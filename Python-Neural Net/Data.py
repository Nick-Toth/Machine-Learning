""" ***********************************************************
\\ File:    Data.py
// Name:    Nick G. Toth
\\ Email:   ntoth@pdx.edu
// Created: Oct, 2016 / Nov 17th, 2017
\\
// Overview: Training data generator for the feed-forward
\\ network defined in Network.py and Neuron.py.
//
\\ ******************************************************** """ 


import random

# Generates a random dataset for a neural net with
# Generic - 2 Input , 4 Hidden , 1 Output
# Saves data to a file filename
def genData(filename):
    file = open( filename, "w+" )
    file.truncate( )
    file.write( "[2,4,1]\n" )

    for n in range(100):
        file.write( makeData( ) )
    file.close( )


# Create 2 new ints, either 0 or 1. Create a 3rd int at 0.
# If the first is a 1, then the third is set to 1.
# An array of the three ints is formatted and returned to genData
def makeData( ):
    a = randInt( )
    b = randInt( )
    c = 0
    if a == 1: c = 1
    return _formatData( a, b, c )


# Format data from makeData, into a string of the format "a b c"
def _formatData( a, b, c ):
    return  str(a) + " " + str(b) + " " + str(c) + "\n"


# Generate and round a random number from 0 to 1. Convert to int.
def randInt( ):
    return int( round( random.uniform( 0, 1 ) ) )


# Parse the specs of the dataset.
# Topline of dataset file should read [a,b,c] where
#
#   a Is an int representing the number neurons in the input layer
#
#   b.. Is a series of one or more ints representing the number of hidden layers and the
#       number of neurons in each layer.
#
#   c Is an int representing the number neurons in the output layer.
#     It is the last element in the array!
#
#   e.g.
#      [2,4,1]   -> 2 Input neurons, 1 layer / 4 hidden neurons, 1 output neuron
#      [2,3,6,1] -> 3 Input neurons, 2 hidden layers - 3 and 6 neurons respectively, 1 output neurons
#
# ect..
def parseFstLn(line):

    fstindx,lstindx = 0,0

    for ch in line:
        if ch == '[': fstindx = line.index(ch)

    rev = list( reversed(line) )

    for ch in rev:
        if ch == ']':
            # Return the specs from the file, from the index of '['
            # to the index of the length of the reversed string up to
            # ']', subtracted from the length of the original line.
            return eval( line[ fstindx:( len(line) - rev.index(ch))])