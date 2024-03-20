#!/usr/bin/env python

"""
Take the output of a partitioned experiment and sew it back together

R. H. Varney (10/2016)

"""

import sys, os.path, glob, datetime, time, copy, re, shutil
import optparse, ConfigParser
import tables, ctypes
import scipy, scipy.interpolate
import matplotlib
matplotlib.use('Agg')
import pylab

import io_utils, plot_utils, model_utils, flipchem, proc_utils, geomag, process_data
from ISfitter import *
from constants import *

from run_fitter import Run_Fitter

MAXFEV_C=20

##############################

class BadComposition(Exception):
    def __init__( self ):
        Exception.__init__(self, 'Composition not converging')

class reassembleOutput(Run_Fitter):
    # Initializes the class
    # Inherit everything from the Run_Fitter class. We will replace the run method though.
    def __init__(self,options):
        Run_Fitter.__init__(self,options)

    # run
    def run(self):

        # Partition Options
        self.OPTS['npart']=int(self.options.npart)

        if (self.FITOPTS['fitcal'] == 1):
            print("Appending -fitcal to file names.")

            #Input file names
            self.OPTS['infile']=[self.OPTS['outfile'][:-3]+'_'+'part_%d'%n+'-fitcal.h5' for n in range(self.OPTS['npart'])]

            #Now change the output file name so it has the -fitcal string appended to it
            temp = self.OPTS['outfile']
            temp2 = temp[:-3]+'-fitcal'+temp[-3:]
            print("Changing output filename to " + temp2)
            self.OPTS['outfile'] = temp2

            #Finally, change the output directory name for the plots directory
            temp = self.OPTS['plotsdir']+'-fitcal'
            print("Changing plotting dirname to " + temp)
            self.OPTS['plotsdir'] = temp
        else:
            #Input file names
            self.OPTS['infile']=[self.OPTS['outfile'][:-3]+'_'+'part_%d'%n+'.h5' for n in range(self.OPTS['npart'])]

        


        #copy the zeroth part file to the (locked) main outfile
        print 'copying',self.OPTS['infile'][0],' to ',self.OPTS['outfile']
        shutil.copyfile(self.OPTS['infile'][0],self.OPTS['outfile'])

        # Now append the other files to end of the zeroth
        for n in range(1,self.OPTS['npart']):
            # open file in append mode
            outh5file=tables.open_file(self.OPTS['outfile'], mode = "a")

            partfile=io_utils.read_whole_h5file(self.OPTS['infile'][n])

            print 'appending ',partfile['/ProcessingParams']['TxPower'].shape
                    
            io_utils.createDynamicArray2(outh5file,self.h5Paths['Params'][0]+'/TxPower',partfile['/ProcessingParams']['TxPower'])
            if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                io_utils.createDynamicArray2(outh5file,self.h5Paths['Params'][0]+'/AeuTx',partfile['/ProcessingParams']['AeuTx'])
                io_utils.createDynamicArray2(outh5file,self.h5Paths['Params'][0]+'/AeuRx',partfile['/ProcessingParams']['AeuRx'])
                io_utils.createDynamicArray2(outh5file,self.h5Paths['Params'][0]+'/AeuTotal',partfile['/ProcessingParams']['AeuTotal'])
            # Time           
            io_utils.createDynamicArray2(outh5file,self.h5Paths['Time'][0],partfile['/Time'],partfile['/Time'].keys())
            # MSIS model
            if self.FITOPTS['DO_FITS']:
                io_utils.createDynamicArray2(outh5file,self.h5Paths['MSIS'][0],partfile['/MSIS'],partfile['/MSIS'].keys())
            # Raw Power
            io_utils.createDynamicArray2(outh5file,self.h5Paths['RawPower'][0],partfile['/NeFromPower'],['Ne_Mod','Ne_NoTr','SNR','dNeFrac']) 
            # Fitted Parameters
            if self.FITOPTS['DO_FITS']:
                io_utils.createDynamicArray2(outh5file,self.h5Paths['FitInfo'][0],partfile['/FittedParams/FitInfo'],partfile['/FittedParams/FitInfo'].keys())
                io_utils.createDynamicArray2(outh5file,self.h5Paths['Fitted'][0],partfile['/FittedParams'],['Errors','Fits','Ne','dNe','Noise'])
            
            # close file     
            outh5file.close()  
                        
        start = time.time()
        # make some final color plots
        if (self.OPTS['plotson']>0):
            if len(self.OPTS['pcolClims'])>0 and len(self.OPTS['pcolYlims'])>0:
                plot_utils.replot_pcolor_all(self.OPTS['outfile'],saveplots=1,opath=self.OPTS['plotsdir'],clims=self.OPTS['pcolClims'],ylim=self.OPTS['pcolYlims'])
            elif len(self.OPTS['pcolYlims'])>0:
                plot_utils.replot_pcolor_all(self.OPTS['outfile'],saveplots=1,opath=self.OPTS['plotsdir'],ylim=self.OPTS['pcolYlims'])
            elif len(self.OPTS['pcolClims'])>0:
                plot_utils.replot_pcolor_all(self.OPTS['outfile'],saveplots=1,opath=self.OPTS['plotsdir'],clims=self.OPTS['pcolClims'])
            else:
                plot_utils.replot_pcolor_all(self.OPTS['outfile'],saveplots=1,opath=self.OPTS['plotsdir'])
        print(time.time() - start)

        return 0

#######

if __name__ == '__main__':
    
    # parse input options
    usage = 'reassemblOutput [OPTIONS]'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-c','--conf',dest='conffile',
                        default='configuration.ini',
                        metavar="FILE",
                        help='Configuration file or list of configuration files (default configuration.ini)')
    parser.add_option('-n','--npartition',dest='npart',default='4',help='Number of partitions to reassemble')
    (options,args) = parser.parse_args()
    
    # go go go
    RF=reassembleOutput(options)
    RF.run()
    
    sys.exit(0)
