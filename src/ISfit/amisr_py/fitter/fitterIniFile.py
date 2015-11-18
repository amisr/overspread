#!/usr/bin/env python

"""

"""

import ConfigParser, os
import scipy
#from amisr_py.io import *

class fitterIniFile:

    AllowedIonMasses = (1,4,12,14,16,29,28,30,32,44)

    def __init__(self,conffile):

        self.FITOPTS={}
        self.DEFOPTS={}
        self.OPTS={}    
        
        # parse the ini file
        self.ini_parse(conffile)
    
        return

    # This function parses the configuration files
    def ini_parse(self,inifile):
        
        # setup ConfigParser object
        config=ConfigParser.RawConfigParser(allow_no_value=True)
        print inifile.split(',')
        config.read(inifile.split(','))                    



    
        """
        # do some error checking
        if self.FITOPTS['DO_FITS']:
            if len(self.FITOPTS['summationRule']) != self.FITOPTS['Ngroup']:
                raise ValueError, 'Summation rule must have Ngroup entries'            
            if self.FITOPTS['GroupHt'].size != self.FITOPTS['Ngroup']:
                raise ValueError, 'GroupHt must have length Ngroup'
            if self.FITOPTS['mi'].size != self.FITOPTS['NION']:
                raise ValueError, 'mi must have length NION'
            if (self.FITOPTS['Ifit'].shape[0] != self.FITOPTS['Ngroup']) or (self.FITOPTS['Ifit'].shape[1] != self.FITOPTS['NION']+1) or (self.FITOPTS['Ifit'].shape[2] != 4):
                raise ValueError, 'Ifit must have size (Ngroup) x (NION+1) x 4'
            if (self.FITOPTS['GroupHt'][-1]<self.FITOPTS['htmax']):
                raise ValueError, 'GroupHt must go up to htmax!'
            for imi in self.FITOPTS['mi']:
                if not imi in self.AllowedIonMasses:
                    raise ValueError, 'Ion masses must be in : %s' % str(self.AllowedIonMasses)

        """
                                                
        return                
