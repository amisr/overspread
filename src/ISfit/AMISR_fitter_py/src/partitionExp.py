#!/usr/bin/env python

"""
This is a skeleton of run_fitter.py used to determine how to partition an experiment

R. H. Varney (10/2016)

TODO: Speed this up by making it read only the needed portions of the data files (/Time) instead of everything

"""

import sys, os.path, glob, datetime, time, copy
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

class PartitionExp(Run_Fitter):

    # Initializes the class
    # Inherit everything from the Run_Fitter class. We will replace the run method though.
    def __init__(self,options):
        Run_Fitter.__init__(self,options)
        
    # run
    def run(self):
    # main routine that runs the fitting loop. 
    # call after instantiating a run_fitter instance
        print "***************************************"
        print self.FITOPTS['fitcal']
        if (self.FITOPTS['fitcal'] == 1):
            print("Appending -fitcal to file names.")

            #Now change the output file name so it has the -fitcal string appended to it
            temp = self.OPTS['outfile']
            temp2 = temp[:-3]+'-fitcal'+temp[-3:]
            print("Changing output filename to " + temp2)
            self.OPTS['outfile'] = temp2

            #Finally, change the output directory name for the plots directory
            temp = self.OPTS['plotsdir']+'-fitcal'
            print("Changing plotting dirname to " + temp)
            self.OPTS['plotsdir'] = temp

        #Initialize figure handles
        self.ContinueFromLocked = 1

        # close all figures
        pyplot.close('all')

        # check out the FILELIST
        try:
            if (type(self.OPTS['FILELIST'])!=tuple):
                self.OPTS['FILELIST']=tuple([self.OPTS['FILELIST']])
            NFREQ=len(self.OPTS['FILELIST'])
        except:
            print 'Problem understanding filelist'
            return

        # check out the raw file paths
        try:
            if (type(self.OPTS['ipath'])!=tuple):
                self.OPTS['ipath']=tuple([self.OPTS['ipath']])
        except:
            print 'Problem understanding FILE_PATH'
            return

        # read the file that contains the list of files to process
        files=[]
        input_files = []
        for ii in range(NFREQ): # for each of the frequencies
            print self.OPTS['FILELIST'][ii]
            f=open(self.OPTS['FILELIST'][ii]) # open
            files.append(f.readlines()) # read list
            f.close() # close
            input_files.append(list())
            #print files
            for ir in range(len(files[ii])):
                files[ii][ir]=files[ii][ir].rstrip('\n')
                files[ii][ir]=files[ii][ir].rstrip('\r')
            #print files
            for ir in range(files[ii].count('')):
                files[ii].remove('')
                files[ii].remove('\n')
                files[ii].remove('\r')
            #print files
            files2=copy.copy(files[ii])
            for ir in range(len(files2)):
                if files2[ir].rfind('*') != -1:
                    for path in self.OPTS['ipath']:
                        tfiles=glob.glob(os.path.join(path,files2[ir]))
                        print tfiles
                        files[ii].extend(tfiles)
                    files[ii].remove(files2[ir])
                    

            files[0] = sorted(files[0],key=os.path.basename)
            files[ii] = sorted(files[ii],key=os.path.basename)
            if len(files[ii])!=len(files[0]): # abort! they need to be the same number of files
                if len(files[ii]) > len(files[0]):
                    iterfiles = files[ii]
                    cmpfiles = files[0]
                else:
                    iterfiles = files[0]
                    cmpfiles = files[ii]
                    
                #print([os.path.basename(s) for s in cmpfiles])
                for iterfile in iterfiles:
                    g = [s for s in cmpfiles if os.path.basename(iterfile)[0:9] in s]
                    if not g:
                        print("*****************************************************************")
                        print('Could find multifreq file associated with: %s' % (iterfile))
                        print("*****************************************************************")
                raise IOError, 'For multiple frequency/external cal, need the same number of files for each freq...'                                                                                                                                                                                                                                                                                                                                                            
            files[ii]=sorted(files[ii],key=os.path.basename)
            #files[ii].sort() # sort the file sequence
        nfiles=len(files[0]) # number of files to process
                
        
        if nfiles==0: # abort!
            print 'Nothing to do...'
            return
    

        #Don't make the plot directory or output file
        NrecsToSkip=0

        # initialize some vars
        done=0 # flag to say when we are done
        IIrec=0 # record counter
        Irec=0 # record counter within a file
        frec=0 # file counter
        newexp=0 # experiment switch
        read_new=1 # flag that says to read a new file
        Iplot=self.OPTS['nplots'] # plot counter
        Ibeams=None
            
        # read the first file
        Ibad=[]
        outputAll=[]
        for ii in range(NFREQ):
            outputAll.append(self.read_a_datafile(files[ii],frec)) # read the first data files
            if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                Ibad.append(scipy.where((outputAll[ii]['/Antenna']['Mode'][:,0] != outputAll[ii]['/Antenna']['Mode'][:,1]) | (outputAll[ii]['/Antenna']['Event'][:,0] != outputAll[ii]['/Antenna']['Event'][:,1]))[0])                            
        output=outputAll[0] 
        curexpname=self.get_expname(os.path.join(self.OPTS['ipath'],files[0][frec]))
        RecInt = scipy.median(output['/Time']['UnixTime'][:,1] - output['/Time']['UnixTime'][:,0])
                                                                                                        
        print 'Experiment: ' + curexpname
        
        ### start: main loop
        while not done:
                                                
            # find which records we should read
            #try:
            if 1==1:
                RecStartTime = output['/Time']['UnixTime'][Irec,0]
                RecEndTime = RecStartTime+self.FITOPTS['Recs2Integrate']+RecInt/2.0
                if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    AntennaMode = output['/Antenna']['Mode'][Irec,0]
                    AntennaEvent = output['/Antenna']['Event'][Irec,0]
                Irecs=[]
                for ii in range(NFREQ):
                    uTime = scipy.mean(outputAll[ii]['/Time']['UnixTime'],axis=1)
                    trecs = scipy.where((uTime>=RecStartTime) & (uTime<=RecEndTime))[0]
                    if len(trecs)==0:
                        trecs=[min([Irec,uTime.shape[0]-1])]
                    Irecs.append(trecs)
                    
                    print trecs
                    print uTime.shape

                    if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                        I=scipy.where((outputAll[ii]['/Antenna']['Mode'][Irecs[ii],0] != AntennaMode) | (outputAll[ii]['/Antenna']['Mode'][Irecs[ii],1] != AntennaMode) |
                            (outputAll[ii]['/Antenna']['Event'][Irecs[ii],0] != AntennaEvent) | (outputAll[ii]['/Antenna']['Event'][Irecs[ii],1] != AntennaEvent))[0]
                        if len(I)>0:
                            Irecs[ii]=Irecs[ii][:I[0]]

            """
            except:
                Irecs=[]
                for ii in range(NFREQ):
                    Irecs.append([-1])
            """
      
            breakout=0
            while (Irec>=output['/Time']['UnixTime'].shape[0] or Irecs[0][-1]==(output['/Time']['UnixTime'].shape[0]-1)) and done==0 and breakout==0: # it has read the last record, so try to read another file
                frec+=1
                if frec>=nfiles:
                    done=1
                else:
                    # read another file
                    expname=self.get_expname(files[0][frec])                  
                    if expname==curexpname:
                        Irec=0
                        for ifreq in range(NFREQ):   
                            try:
                                trec=Irecs[ifreq][0]
                            except:
                                trec=-1
                            outputAll[ifreq]=self.read_a_datafile(files[ifreq],frec,outputAll[ifreq],trec,outputAll[ifreq]['/Time']['UnixTime'].shape[0])
                            if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                                Ibad[ifreq]=scipy.where((outputAll[ifreq]['/Antenna']['Mode'][:,0] != outputAll[ifreq]['/Antenna']['Mode'][:,1]) | (outputAll[ifreq]['/Antenna']['Event'][:,0] != outputAll[ifreq]['/Antenna']['Event'][:,1]))[0]                            
                        output=outputAll[0]
                    elif Irec<output['/Time']['UnixTime'].shape[0]: # case where next file is a new experiment, but we want to process the last group of recs
                            frec-=-1
                            breakout=1
                    else: # we've transitioned to a new experiment
                        print 'New experiment: ' + expname
                        curexpname=expname
                        newexp=1
                        self.BMCODES=None
                        outputAll=[]
                        for ii in range(NFREQ):
                            outputAll.append(self.read_a_datafile(files[ii],frec)) # read the first data files
                        output=outputAll[0] 
                        Irec=0   
                    
                    
                    
                    # get records
                    RecStartTime = output['/Time']['UnixTime'][Irec,0]
                    RecEndTime = RecStartTime+self.FITOPTS['Recs2Integrate']+RecInt/2.0
                    if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                        AntennaMode = output['/Antenna']['Mode'][Irec,0]
                        AntennaEvent = output['/Antenna']['Event'][Irec,0]
                    Irecs=[]
                    for ii in range(NFREQ):
                        uTime = scipy.mean(outputAll[ii]['/Time']['UnixTime'],axis=1)
                        trecs = scipy.where((uTime>=RecStartTime) & (uTime<=RecEndTime))[0]
                        if len(trecs)==0:
                            trecs=[Irec]                      
                        Irecs.append(trecs)
                        if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                            I=scipy.where((output['/Antenna']['Mode'][Irecs[ii],0] != AntennaMode) | (outputAll[ii]['/Antenna']['Mode'][Irecs[ii],1] != AntennaMode) |
                                (outputAll[ii]['/Antenna']['Event'][Irecs[ii],0] != AntennaEvent) | (outputAll[ii]['/Antenna']['Event'][Irecs[ii],1] != AntennaEvent))[0]
                            if len(I)>0:
                                Irecs[ii]=Irecs[ii][:I[0]]
            if done==1 and Irecs[0][-1]==-1:
                break;
            
            # print the record numbers
            print '\nFile ' + str(frec) + ' of ' + str(nfiles)
            print 'Integration Number: ' + str(IIrec) + ', Recs Being Integrated: ' + str(Irecs[0][0]) + ':' + str(Irecs[0][-1])
            fstr='File %d of %d, Rec %d, ' % (frec,nfiles,IIrec)
                        
            # skip records
            if NrecsToSkip>0:
                Irec=Irecs[0][-1]+1
                try:
                    while Ibad[0].__contains__(Irec):
                        Irec=Irec+1
                except: ''
                IIrec=IIrec+1
                Iplot=Iplot+1
                NrecsToSkip-=1
                continue

            #cut the guts of the fitter

            # increment counters
            Irec=Irecs[0][-1]+1
            try:
                while Ibad[0].__contains__(Irec):
                    Irec=Irec+1
            except: ''
            IIrec=IIrec+1
            Iplot=Iplot+1
                    
        ### end: main loop
                        
        return IIrec-1
#######

if __name__ == '__main__':
    
    # parse input options
    usage = 'partitionExp [OPTIONS]'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-c','--conf',dest='conffile',
                        default='configuration.ini',
                        metavar="FILE",
                        help='Configuration file or list of configuration files (default configuration.ini)')
    parser.add_option('-n','--npartition',dest='npart',default='4',help='Number of partitions to split experiment into')
    (options,args) = parser.parse_args()
    
    RF=PartitionExp(options)
    IIrec=RF.run()

    print 'Full Experiment consists of %d records'%IIrec

    np=int(options.npart)
    whole = IIrec/np #integer division
    remainder = IIrec % np
    wholep1=whole+1

    iii=0
    for n in range(np):
        beg=iii
        if n<=remainder:
            end=beg+wholep1
        else:
            end=beg+whole
        
        print 'Segment %d spans records %d to %d'%(n,beg,end)

        ftxt=open(os.path.join(RF.OPTS['outputpath'],'iirec_part_%d.ini'%n),'w')
        ftxt.write('[PARTITION]\n')
        ftxt.write('partitioned=True\n')
        ftxt.write('string=part_%d\n'%n)
        ftxt.write('beg=%d\n'%beg)
        ftxt.write('end=%d\n'%end)
        ftxt.close()

        iii=end
    
    sys.exit(0)
