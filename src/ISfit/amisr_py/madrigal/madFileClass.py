#!/usr/bin/env python

"""

"""

import 

class madFileClass:

    def __init__(self,hdf5File,kinst,kindat,cedarObj,madrigalFile,lowerRange=None,upperRange=None):        
        
        """
        Inputs:
            hdf5File - full path to hdf5 file with ISR data in SRI format
            kinst - instrument code (integer)
            kindat - data file kindat (integer)
            cedarObj - existing madrigal.cedar.MadrigalCedarFile to append data to.
                       If None, new madrigal.cedar.MadrigalCedarFile
                       created using madrigalFile.
            madrigalFile - name of Madrigal file to create or append to.
            lowerRange - lower range cutoff.  If None (the default), no lower range cutoff.
            upperRange - upper range cutoff.  If None (the default), no upper range cutoff.
        """
        
        # copy to class vars
        self.hdf5File=hdf5File
        self.kinst=kinst
        self.kindat=kindat
        self.cedarObj=cedarObj
        self.madrigalFile=madrigalFile
        self.lowerRange=lowerRange
        self.upperRange=upperRange
        return