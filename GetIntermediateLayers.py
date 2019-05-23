# Callin Switzer
# 23 May 2017
# Get all node values from a trained network, given a set of weights

import numpy as np
import pickle
import os
import pandas as pd


# this function returns values of all layers
def returnLayerValues(weights, inputData):
    '''
    Returns all layer values from fitted network (including input and output)
    
    The fitted network uses "tanh" activation for hidden layers and
        "linear" activation for the final layer
        
    Params:
        weights (list): list of weights from a fitted model
        inputData (array or Data frame): input data that will be run through the network
    
    Returns:
        layer values (list): values of all of the units in the network.
            - the 0'th item in the list is the input data
            - the final item in the list is the final prediction
    '''
    
    LayerValues = [inputData.astype("float64")]
    for layerNum in np.arange(0, len(wts), 2):
        
        # calculate dot product and add bias
        nextLayer = np.dot(LayerValues[-1], wts[layerNum]) + wts[layerNum+1]
        
        if layerNum != (len(wts)-2):
            # apply activation function, except for final layer
            nextLayer = np.tanh(nextLayer)
        
        # append to list
        LayerValues.append(nextLayer)
        
    return(LayerValues)


# load example data
pathToTrainingX= r"D:\Dropbox\AcademiaDropbox\UW\NNetVisualization\ExampleData\X_train_small.csv"
x_train = pd.read_csv(pathToTrainingX)

# in case you want to compare predicted to actual, you can load the Y dataset
pathToTrainingY= r"D:\Dropbox\AcademiaDropbox\UW\NNetVisualization\ExampleData\Y_train_small.csv"
y_train = pd.read_csv(pathToTrainingY)