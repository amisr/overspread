#! /usr/bin/env python

"""
xxxxx
"""
import scipy
import tables
import os


# loads the lag ambiguity function from a file
def load_amb_func(path,full=0):

    outDict={}
    
    dat=read_whole_h5file(path)
    outDict=dat['/']
    
    return outDict
    
# copyAmbDict
def copyAmbDict(file,path):
    """
    Copies ambiguity function from data files to something the fitter expects and needs
    """
    
    node = file.getNode(path)
    
    outAmb={}
    try:
        outAmb['Delay']=node.Delay.read()
        outAmb['Range']=node.Range.read()
        outAmb['Lags']=node.Lags.read()
        outAmb['Wlag']=node.Wlag.read()
        outAmb['Wrange']=node.Wrange.read()
    except:
        print 'Dont understand format of Ambiguity in data file.'
    
    return outAmb