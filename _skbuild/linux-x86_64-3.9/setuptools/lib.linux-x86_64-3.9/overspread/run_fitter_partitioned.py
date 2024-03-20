#!/usr/bin/env python

"""
Variant of run_fitter that supports a partitioned experiment

R. H. Varney (10/2016)

"""

import sys, os.path, glob, datetime, time, copy, re
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

#For fitcal files (files that are calibrated and fitted at the same time)
#we need to add a Calibration record
from add_calibration_record import *

MAXFEV_C=20

##############################

class BadComposition(Exception):
    def __init__( self ):
        Exception.__init__(self, 'Composition not converging')

class Run_Fitter_Partitioned(Run_Fitter):

    # Initializes the class
    # Inherit everything from the Run_Fitter class. We will replace the run method though.
    def __init__(self,options):
        Run_Fitter.__init__(self,options)


        # Some additional ini config stuff now

        # Partition Options
        partitioned=io_utils.ini_tool(self.config,'PARTITION','partitioned',required=0,defaultParm=False)
        self.OPTS['partitioned']=partitioned in ['True','true','1','T','t','yes']
        self.OPTS['partitionString']=io_utils.ini_tool(self.config,'PARTITION','string',required=0,defaultParm='')
        self.OPTS['partitionBeg']=int(io_utils.ini_tool(self.config,'PARTITION','beg',required=0,defaultParm=0))
        self.OPTS['partitionEnd']=int(io_utils.ini_tool(self.config,'PARTITION','end',required=0,defaultParm=0))

        print partitioned,self.OPTS['partitioned'],self.OPTS['partitionString'],self.OPTS['partitionBeg'],self.OPTS['partitionEnd']

        if self.OPTS['partitioned']:
            self.IIrec_first=self.OPTS['partitionBeg']
        else:
            self.IIrec_first=0

        #Doctor the output file name
        if self.OPTS['partitioned']:
            self.OPTS['outfile']=self.OPTS['outfile'][:-3]+'_'+self.OPTS['partitionString']+'.h5'

        print "OUTPUT FILE NAME:"
        print self.OPTS['outfile']

        
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

            if len(files[ii])!=len(files[0]): # abort! they need to be the same number of files
                raise IOError, 'For multiple frequency/external cal, need the same number of files for each freq...'
            files[ii]=sorted(files[ii],key=os.path.basename)
            #files[ii].sort() # sort the file sequence
        nfiles=len(files[0]) # number of files to process

        if nfiles==0: # abort!
            print 'Nothing to do...'
            return
    

        # make plot directory
        if self.OPTS['saveplots']==1:  
            if not os.path.exists(self.OPTS['plotsdir']):
                try:
                    os.mkdir(self.OPTS['plotsdir'])
                except:
                    print 'Cant make plots dir'
        
        # create the output file
        self.OPTS['outfileLocked'] = self.OPTS['outfile']+'.lock'
        if os.path.exists(self.OPTS['outfileLocked']) and self.ContinueFromLocked:
            try:
                output=io_utils.read_whole_h5file(self.OPTS['outfileLocked'])
                print output.keys()
                NrecsToSkip=output['/Time']['UnixTime'].shape[0]
                del output
                print "Continuing using " + self.OPTS['outfileLocked'] + " from record " + str(NrecsToSkip)
            except:
                raise IOError, 'Unable to continue from locked file: ' + self.OPTS['outfileLocked']
        else:
            #try:
            NrecsToSkip=0
            outh5file=tables.open_file(self.OPTS['outfileLocked'], mode = "w", title = "Fitted Output File")
            io_utils.createh5groups(outh5file,[self.h5Paths['MSIS'],self.h5Paths['Geomag'],self.h5Paths['RawPower'],self.h5Paths['Params'],self.h5Paths['Site'],self.h5Paths['Time']])
            if self.FITOPTS['DO_FITS']:
                io_utils.createh5groups(outh5file,[self.h5Paths['Fitted'],self.h5Paths['FitInfo']])
                if self.OPTS['saveACFs']:
                    io_utils.createh5groups(outh5file,[self.h5Paths['ACFs']])
            if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                io_utils.createh5groups(outh5file,[self.h5Paths['Antenna']])
            outh5file.close()
            #except:
            #    raise IOError, 'Cannot create output file: ' + self.OPTS['outfileLocked']
        

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
        curexpname=self.get_expname(files[0][frec])
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

            #Check if this IIrec is in the partition
            if self.OPTS['partitioned']:
                if IIrec<self.OPTS['partitionBeg'] or IIrec>=self.OPTS['partitionEnd']:
                    Irec=Irecs[0][-1]+1
                    IIrec=IIrec+1
                    Iplot=Iplot+1
                    continue

            # 
            if self.FITOPTS['MOTION_TYPE']==0:
                output['/Setup']['BeamcodeMap']=scipy.asarray(output['/Setup']['BeamcodeMap'])

            try:
                output['/Rx']['CalTemp']
            except:
                print self.DEFOPTS['CAL_TEMP_DEF']
                output['/Rx']['CalTemp']=self.DEFOPTS['CAL_TEMP_DEF']
            
            # process a record
#            try:  
            #if 1==1:
            if NFREQ==1:
                [S,Noise,Cal]=eval('process_data.'+self.OPTS['proc_funcname']+"(output,Irecs[0],self.FITOPTS,self.AMB,doamb=(not self.AMB['Loaded']),extCal=self.FITOPTS['useExternalCal'],h5DataPath=self.OPTS['h5DataPath'],BeamCodes=self.BMCODES)")
            else:
                [S,Noise,Cal]=eval('process_data.'+self.OPTS['proc_funcname']+"(outputAll,Irecs,self.FITOPTS,self.AMB,doamb=(not self.AMB['Loaded']),extCal=self.FITOPTS['useExternalCal'],h5DataPath=self.OPTS['h5DataPath'],BeamCodes=self.BMCODES)")
#            except: 
#                raise RuntimeError, 'Error calling %s' % self.OPTS['proc_funcname']
            try:
                (Nbeams,self.Nlags,self.Nranges)=S['Acf']['Data'].shape
            except:
                (Nbeams,self.Nranges)=S['Power']['Data'].shape
            self.S=S
            
            # Ambiguity function
            if (not self.AMB['Loaded']): # if it hasn't already been loaded, we need to try to get it from data files
                if (not S.has_key('Acf')):
                    if (not S['Power'].has_key('Ambiguity')):
                        raise RuntimeError, 'No valid ambiguity function in data files or specified external file.'
                    else:
                        self.AMB=S['Power']['Ambiguity']
                        self.AMB['Loaded']=1   
                elif (not S['Acf'].has_key('Ambiguity')): # this is the case where we needed to get it from the data file but unable to
                    raise RuntimeError, 'No valid ambiguity function in data files or specified external file.'
                else:
                    self.AMB=S['Acf']['Ambiguity']
                    self.AMB['Loaded']=1            

            # Transmitter
            Tx={}
            try: 
                Tx['Frequency']=scipy.median(scipy.mean(output['/Tx']['Frequency'][Irecs[0],:],axis=1)) 
                if Tx['Frequency']<1.0e6:
                    raise ValueError, "Tx Frequency Not Set, Using Default"
            except: Tx['Frequency']=self.DEFOPTS['TX_FREQ_DEF']
            try: Tx['Power']=scipy.median(scipy.mean(output['/Tx']['Power'][Irecs[0],:],axis=1))
            except: Tx['Power']=self.DEFOPTS['TX_POWER_DEF']
            try: Tx['Aeu']=scipy.median(scipy.mean(output['/Tx']['AeuTx'][Irecs[0],:],axis=1)) 
            except: Tx['Aeu']=-1
            try: S['Acf']['Psc']=S['Acf']['Psc']*Tx['Power']; 
            except: '' # Power scaling factor
            if Tx['Power']==0.0:
                Tx['Power']=self.DEFOPTS['TX_POWER_DEF']
            self.k_radar0=4.0*pi*Tx['Frequency']/v_lightspeed
            self.FITOPTS['p_om0']=self.k_radar0*scipy.sqrt(2.0*v_Boltzmann*self.FITOPTS['p_T0']/(self.FITOPTS['p_M0']*v_amu))
            
            
            # Receive
            Rx={}
            try: Rx['Frequency']=scipy.median(scipy.mean(output['/Rx']['Frequency'][Irecs[0],:],axis=1)) 
            except: Rx['Frequency']=self.DEFOPTS['TX_FREQ_DEF']
            try: Rx['Aeu']=scipy.median(scipy.mean(output['/Rx']['AeuRx'][Irecs[0],:],axis=1)) 
            except: Rx['Aeu']=-1
            try: Rx['AeuTotal']=scipy.median(scipy.mean(output['/Site']['AeuTotal'][Irecs[0],:],axis=1))
            except: Rx['AeuTotal']=-1;    
                
            # Time
            self.Time['UnixTime']=scipy.array([output['/Time']['UnixTime'][Irecs[0][0],0],output['/Time']['UnixTime'][Irecs[0][-1],1]])
            tmp1=datetime.datetime.utcfromtimestamp(self.Time['UnixTime'][0]); r1=datetime.date(tmp1.year,tmp1.month,tmp1.day)
            tmp2=datetime.datetime.utcfromtimestamp(self.Time['UnixTime'][1]); r2=datetime.date(tmp2.year,tmp2.month,tmp2.day)
            self.Time['Year']=scipy.array([tmp1.year,tmp2.year])
            self.Time['Month']=scipy.array([tmp1.month,tmp2.month])
            self.Time['Day']=scipy.array([tmp1.day,tmp2.day])
            self.Time['dtime']=scipy.array([(float(tmp1.hour)+float(tmp1.minute)/60.0+float(tmp1.second)/3600.0),(float(tmp2.hour)+float(tmp2.minute)/60.0+float(tmp2.second)/3600.0)])
            self.Time['doy']=scipy.array([int(r1.strftime('%j')),int(r2.strftime('%j'))])
            
            # get some standard info the first time through
            if self.BMCODES==None or newexp:
                                                                        
                self.BMCODES=S['BMCODES'] # beamcodes
                self.BMCODES[:,2]=scipy.absolute(self.BMCODES[:,2])
            
                # read site information
                self.Site['Latitude']=float(output['/Site']['Latitude']) # site latitude
                self.Site['Longitude']=float(output['/Site']['Longitude']) # site longitude
                if self.Site['Longitude']>0: # some files have the sign of the longitude flipped
                    self.Site['Longitude']*=-1
                self.Site['Altitude']=float(output['/Site']['Altitude']) # site altitude 
                try: self.Site['Code']=int(output['/Site']['Code']); # site code
                except: ''; 
                try: self.Site['Name']=output['/Site']['Name']; # site name
                except: '';              
                
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    if IIrec==self.IIrec_first or newexp or Ibeams==None: # first time through
                        if newexp:
                            newexp=0
                            try:
                                del self.Gmag
                            except:
                                ""
                        Im1=scipy.where(self.BMCODES[:,0]!=-1.0)[0]
                        if len(self.FITOPTS['Beams2do'])>0:
                            Ibeams=[]
                            for ii in range(len(self.FITOPTS['Beams2do'])):
                                try:
                                    Ibeams.append(int(scipy.where(self.BMCODES[:,0]==self.FITOPTS['Beams2do'][ii])[0]))
                                except:
                                    raise RuntimeError, 'No beamcode: %d!!' % (self.FITOPTS['Beams2do'][ii])
                        elif len(Im1)!=Nbeams:
                            Ibeams=Im1
                        else:
                            Ibeams=range(Nbeams)
                        self.BMCODES=self.BMCODES[Ibeams,:]
                        self.Nbeams=len(Ibeams)   
                    

                    # run the geomagnetic model
                    #print self.ct_geolib,self.Time['Year'][0],self.BMCODES,self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0
                    
                    self.gmag=geomag.geomag(self.ct_geolib,self.Time['Year'][0],self.BMCODES,self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0) # run the geomag model             

                    if self.OPTS['plotson']>0: # plot the geomag
                        gfig=plot_utils.geoplot(self.BMCODES[:,1],self.BMCODES[:,2],self.gmag['Range'],self.gmag['Altitude'],self.gmag['MagneticLatitude'],self.gmag['MagneticLongitude'],self.gmag['Dip'],self.gmag['Declination'])
                        if self.OPTS['saveplots']==1: # save the geomag
                            if os.path.exists(self.OPTS['plotsdir']):
                                oname= "%d-%d-%d geoplot.png" % (self.Time['Month'][0],self.Time['Day'][0],self.Time['Year'][0])
                                gfig.savefig(os.path.join(self.OPTS['plotsdir'],oname))
                    if self.FITOPTS['DO_FITS']==0:
                        Gmag=self.gmag

                else: # Az, El
                    self.Nbeams=1                    
                    self.gmag=geomag.geomag(self.ct_geolib,self.Time['Year'][0],self.BMCODES,self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0,rng=scipy.array([0.0])) # run the geomag model             

                # get geomag info of site to 2 decimal places
                self.Site['MagneticLatitude']=float(scipy.asarray(self.gmag['MagneticLatitude'][0,0]).round(decimals=2))
                self.Site['MagneticLongitude']=float(scipy.asarray(self.gmag['MagneticLongitude'][0,0]).round(decimals=2))
                self.Site['MagneticLocalTimeMidnight']=float(scipy.asarray(self.gmag['MLTMidnightUT'][0,0]).round(decimals=3))
                            
            # calculate MLT at the site
            if self.OPTS['ComputeMLT']>=1: # just calculates MLT at the site
                self.Time['MagneticLocalTimeSite']=self.Time['dtime']-self.Site['MagneticLocalTimeMidnight']
                if self.Time['MagneticLocalTimeSite'][0]<0.0:
                    self.Time['MagneticLocalTimeSite']+=24.0
                                                                                                                                                                                                                                                        
            # get altitude using geodetic conversion
            if S.has_key('Power'):
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    S['Power']['Altitude']=proc_utils.range2height(self.ct_geolib,scipy.squeeze(S['Power']['Range'])/1000.0,S['BMCODES'][:,1],S['BMCODES'][:,2],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)        
                elif self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    S['Power']['Altitude']=proc_utils.range2height(self.ct_geolib,scipy.squeeze(S['Power']['Range'])/1000.0,[S['AvgAzimuth']],[S['AvgElevation']],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)        
            if S.has_key('Acf'):
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    S['Acf']['Altitude']=proc_utils.range2height(self.ct_geolib,scipy.squeeze(S['Acf']['Range'])/1000.0,S['BMCODES'][:,1],S['BMCODES'][:,2],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)        
                elif self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    S['Acf']['Altitude']=proc_utils.range2height(self.ct_geolib,scipy.squeeze(S['Acf']['Range'])/1000.0,[S['AvgAzimuth']],[S['AvgElevation']],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)        
            
            # Trim data based on beam
            if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                #print Cal
                #print Noise
                S=process_data.trim_Ibeams(S,Ibeams,Nbeams)
                Noise=process_data.trim_Ibeams(Noise,Ibeams,Nbeams)
                
                Cal=process_data.trim_Ibeams(Cal,Ibeams,Nbeams)
                

            # model for computing Ne from power
            Mod={}
            Mod['Te']=scipy.ones(S['Power']['Altitude'].shape)*1000
            Mod['Ti']=scipy.ones(S['Power']['Altitude'].shape)*1000
            Mod['Ne']=scipy.ones(S['Power']['Altitude'].shape)*1.0e11
            
            # compute density profile using apriori model for temperatures
            S['Power']['SNR']=scipy.real(S['Power']['SNR'])
            if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                (S['Power']['Ne_Mod'],S['Power']['Ne_NoTr'],tPsc)=proc_utils.ne_prof(S['Power']['Data'],S['Power']['Range'],S['Power']['Altitude'],Mod,Tx['Power'],S['Power']['Pulsewidth'],Tx['Frequency'],S['BMCODES'][:,3])
            elif self.FITOPTS['MOTION_TYPE']==1: # Az,El
                (S['Power']['Ne_Mod'],S['Power']['Ne_NoTr'],tPsc)=proc_utils.ne_prof(S['Power']['Data'],S['Power']['Range'],S['Power']['Altitude'],Mod,Tx['Power'],S['Power']['Pulsewidth'],Tx['Frequency'],S['Ksys'])
            S['Power']['dNeFrac']=1.0/scipy.sqrt(S['Power']['Kint']*S['Power']['PulsesIntegrated'])*(1.0+scipy.absolute(1.0/S['Power']['SNR'])+S['Power']['iSCR'])
            S['Power']['dNeFrac'][scipy.where(S['Power']['SNR']<0.0)]=scipy.nan
            if self.FITOPTS['uselag1']: # if we are using the 1st lag to estimate the density, then we need to scale it a bit
                S['Power']['Ne_Mod']=S['Power']['Ne_Mod']/scipy.sum(scipy.absolute(self.AMB['Wlag'][S['Acf']['Lag1Index'],:]))
                S['Power']['Ne_NoTr']=S['Power']['Ne_NoTr']/scipy.sum(scipy.absolute(self.AMB['Wlag'][S['Acf']['Lag1Index'],:]))            
            else:
                S['Power']['Ne_Mod']=S['Power']['Ne_Mod']/scipy.sum(scipy.absolute(self.AMB['Wlag'][0,:]))
                S['Power']['Ne_NoTr']=S['Power']['Ne_NoTr']/scipy.sum(scipy.absolute(self.AMB['Wlag'][0,:]))


            # Parameters needed for calculating the perturbation noise_acf
            sample_time = output['/Rx']['SampleTime']
            temp = output['/Setup']['RxFilterfile']
            filter_coefficients = scipy.array([float(x) for x in temp.split('\n')[:-1]])

            # A function to compute the perturbation noise acf based on the assumption that such noise
            # is white and broadband (at least compared to the width of the filter)
            def compute_noise_acf(num_lags,sample_time,impulse_response):

                t_num_taps = impulse_response.size
                t_times = scipy.arange(t_num_taps)*1e-6
                t_acf = scipy.convolve(impulse_response,impulse_response)[t_num_taps-1:]
                t_acf = t_acf / t_acf[0]

                t_lag_times = scipy.arange(num_lags)*sample_time
                interp_func = scipy.interpolate.interp1d(t_times,t_acf,bounds_error=0, fill_value=0)
                noise_acf = interp_func(t_lag_times)

                return noise_acf

            # Compute the perturbation noise acf
            num_lags = self.Nlags
            perturbation_noise_acf = compute_noise_acf(num_lags,sample_time,filter_coefficients)


            ### start: DO_FITS              
            if self.FITOPTS['DO_FITS']: # do the fits
                if self.FITOPTS['FullProfile']:
                    (trng,tht,tne,tfits,terrs,tmod_ACF,tmeas_ACF,terrs_ACF,tfitinfo)=self.call_fitter_FP(S,Noise,sstr=fstr)
                else:
                    (trng,tht,tne,tnoise,tfits,terrs,tmod_ACF,tmeas_ACF,terrs_ACF,tfitinfo,modelOut,Gmag)=self.call_fitter(S,Noise,perturbation_noise_acf,sstr=fstr)
                self.FITS['Range']=trng
                self.FITS['Altitude']=tht
                self.FITS['Ne']=tne[:,:,0]
                self.FITS['Noise'] = tnoise
                self.FITS['dNe']=tne[:,:,1]
                self.FITS['Fits']=tfits
                self.FITS['Errors']=terrs
                self.FITS['FitInfo']=tfitinfo
                if self.OPTS['saveACFs']:         
                    tshape=list(tmeas_ACF.shape); #tshape.append(2)                    
                    #ttmeas=scipy.zeros(tshape,dtype='Float64'); ttmeas[:,:,:,0]=tmeas_ACF.real; ttmeas[:,:,:,1]=tmeas_ACF.imag
                    #ttmod=scipy.zeros(tshape,dtype='Float64'); ttmod[:,:,:,0]=tmod_ACF.real; ttmeas[:,:,:,1]=tmod_ACF.imag
                    self.FITS['ACFs']={}                    
                    self.FITS['ACFs']['ModelACF']=tmod_ACF #ttmod
                    self.FITS['ACFs']['MeasACF']=tmeas_ACF #ttmeas
                    self.FITS['ACFs']['ErrsACF']=terrs_ACF
                    #print tmeas_ACF[0,:,5]
                    #print tmeas_ACF.imag[0,:,5]
                    #print ttmeas[0,:,5]
                    #print self.FITS['ACFs']['MeasACF'][0,:,5]
                    #xxxx
                            
                # make the plots if we are supposed to
                if (self.OPTS['plotson']>0) and (Iplot>=self.OPTS['nplots']):
                    figg1 = None
                    figg2 = None
                    figg3 = None
                    figg4 = None
                    figg5 = None
                    ax1 = None
                    ax2 = None
                    ax3 = None
                    ax4 = None
                    ax5 = None

                    Iplot=0
                    try:
                        IbPl=range(len(Ibeams))
                    except:
                        IbPl=[]

                    title= "%d-%d-%d %.3f-%.3f UT" % (self.Time['Month'][0],self.Time['Day'][0],self.Time['Year'][0],self.Time['dtime'][0],self.Time['dtime'][1])

                    print "Making profile plots..."
                    try:
                        (figg1,ax1,_)=plot_utils.test_plot(self,IIrec,[],[],self.OPTS['xlims'],Ibeams=IbPl,dofrac=self.OPTS['plotfrac'])
                    except Exception as e:
                        print("Plotting failed: "+str(e))
                        figg1 = None

                    if (self.OPTS['saveplots']==1) and (figg1 is not None):
                        if os.path.exists(self.OPTS['plotsdir']):
                            oname=title + '.png'
                            figg1.savefig(os.path.join(self.OPTS['plotsdir'],oname))
                        else:
                            print "Can't output plots, path doesn't exist"
                            self.OPTS['saveplots']=0

                    if self.OPTS['plotson']>1:
                        print "Plotting ACFs..."
                        try:
                            (figg2,ax2)=plot_utils.acf_plot(tmeas_ACF,terrs_ACF,tmod_ACF,tht/1000.0,self.BMCODES,title,Ibeams=IbPl)
                        except Exception as e:
                            print("Plotting failed: "+str(e))
                            figg2 = None

                        if (self.OPTS['saveplots']==1) and (os.path.exists(self.OPTS['plotsdir'])) and (figg2 is not None):
                            oname='acf ' + title + '.png'
                            figg2.savefig(os.path.join(self.OPTS['plotsdir'],oname))

                        if self.OPTS['dumpSpectra']>0:
                            print "Plotting Spectra..."
                            try:
                                (figg6,ax6)=plot_utils.spc_plot(tmeas_ACF,terrs_ACF,tmod_ACF,tht/1000.0,self.BMCODES,title,Ibeams=IbPl)
                            except Exception as e:
                                print("Plotting failed: "+str(e))
                                figg6 = None

                            if (self.OPTS['saveplots']==1) and (os.path.exists(self.OPTS['plotsdir'])) and (figg6 is not None):
                                oname='spc ' + title + '.png'
                                figg6.savefig(os.path.join(self.OPTS['plotsdir'],oname))


                    if self.OPTS['dumpSpectra']>1:
                        try:
                            (figg3,ax3,figg4,ax4,figg5,ax5)=plot_utils.spc_pcolor_plot(tmeas_ACF,tmod_ACF,1.0/self.S['Acf']['Lags'][0,-1]/2.0,tht/1000.0,
                                self.BMCODES,title=title,ylim=[self.FITOPTS['htmin']/1000.0,self.FITOPTS['htmax']/1000.0],clim=[-19.0,-16.0])
                        except Exception as e:
                            print("Plotting failed: "+str(e))
                            figg3, figg4, figg5 = None, None, None

                        if (self.OPTS['saveplots']==1) and (os.path.exists(os.path.join(self.OPTS['plotsdir'],'Spectra'))) and (None not in [figg3, figg4, figg5]):
                            figg3.savefig(os.path.join(self.OPTS['plotsdir'],'Spectra','Spectra ' + title + ' Measured.png'))
                            figg4.savefig(os.path.join(self.OPTS['plotsdir'],'Spectra','Spectra ' + title + ' Modeled.png'))
                            figg5.savefig(os.path.join(self.OPTS['plotsdir'],'Spectra','Spectra ' + title + ' Residual.png'))

                    pyplot.close('all')
                    del figg1
                    del figg2
                    del figg3
                    del figg4
                    del figg5
                    del ax1
                    del ax2
                    del ax3
                    del ax4
                    del ax5
            
            # Output data to file
            # open file
            with tables.open_file(self.OPTS['outfileLocked'], mode = "a") as outh5file:
                # Processing params
                if IIrec==self.IIrec_first:
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/ProcessingTimeStamp',scipy.array(time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())))
                    # Tx/Rx frequency and stuff
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/TxFrequency',scipy.array(Tx['Frequency']))
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/RxFrequency',scipy.array(Rx['Frequency']))
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/BaudLength',scipy.array(S['Power']['TxBaud']))
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/PulseLength',scipy.array(S['Power']['Pulsewidth']))
                    # Site
                    io_utils.createStaticArray(outh5file,self.h5Paths['Site'][0],self.Site,self.Site.keys())
                    # Beamcodes
                    if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                        io_utils.createStaticArray(outh5file,'/BeamCodes',self.BMCODES)
                io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/TxPower',scipy.array(Tx['Power']))
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/AeuTx',scipy.array(Tx['Aeu']))
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/AeuRx',scipy.array(Rx['Aeu']))
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/AeuTotal',scipy.array(Rx['AeuTotal']))
                # Antenna motion
                if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Antenna'][0],S,['Azimuth','Elevation','AvgAzimuth','AvgElevation'])
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Antenna'][0]+'/Mode',scipy.array(output['/Antenna']['Mode'][Irec,0]))
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Antenna'][0]+'/Event',scipy.array(output['/Antenna']['Event'][Irec,0]))
                # Time
                io_utils.createDynamicArray(outh5file,self.h5Paths['Time'][0],self.Time,self.Time.keys())
                # MSIS model
                if self.FITOPTS['DO_FITS']:
                    io_utils.createDynamicArray(outh5file,self.h5Paths['MSIS'][0],modelOut,modelOut.keys())
                # Geomag
                if self.FITOPTS['MOTION_TYPE']==1 or self.OPTS['dynamicAlts']==1: # Az,El or case when we want to include entire array
                    if self.FITOPTS['DO_FITS']:
                        io_utils.createDynamicArray(outh5file,self.h5Paths['Geomag'][0],Gmag,Gmag.keys())
                else: # Beamcodes
                    if IIrec==self.IIrec_first:
                        io_utils.createStaticArray(outh5file,self.h5Paths['Geomag'][0],Gmag,Gmag.keys())
                    self.Gmag=Gmag
                # Raw Power
                if self.FITOPTS['MOTION_TYPE']==1 or self.OPTS['dynamicAlts']==1: # Az,El or case when we want to include entire array
                    io_utils.createDynamicArray(outh5file,self.h5Paths['RawPower'][0],S['Power'],['Ne_NoTr','Ne_Mod','SNR','dNeFrac','Altitude','Range'])
                else: # Beamcodes
                    io_utils.createDynamicArray(outh5file,self.h5Paths['RawPower'][0],S['Power'],['Ne_NoTr','Ne_Mod','SNR','dNeFrac'])
                    if IIrec==self.IIrec_first:
                        io_utils.createStaticArray(outh5file,self.h5Paths['RawPower'][0],S['Power'],['Altitude','Range'])
                # Fitted Parameters
                if self.FITOPTS['DO_FITS']:
                    io_utils.createDynamicArray(outh5file,self.h5Paths['FitInfo'][0],self.FITS['FitInfo'],self.FITS['FitInfo'].keys())
                    if self.FITOPTS['MOTION_TYPE']==1 or self.OPTS['dynamicAlts']==1: # Az,El or case when we want to include entire array
                        io_utils.createDynamicArray(outh5file,self.h5Paths['Fitted'][0],self.FITS,['Errors','Fits','Ne','dNe','Range','Altitude','Noise'])
                    else: # beamcodes
                        io_utils.createDynamicArray(outh5file,self.h5Paths['Fitted'][0],self.FITS,['Errors','Fits','Ne','dNe','Noise'])
                        if IIrec==self.IIrec_first:
                            io_utils.createStaticArray(outh5file,self.h5Paths['Fitted'][0],self.FITS,['Altitude','Range'])
                    if IIrec==self.IIrec_first:
                        io_utils.createStaticArray(outh5file,self.h5Paths['Fitted'][0]+'/IonMass',self.FITOPTS['mi'])
                    if self.OPTS['saveACFs']:
                        io_utils.createDynamicArray(outh5file,self.h5Paths['ACFs'][0],self.FITS['ACFs'],self.FITS['ACFs'].keys())
                # set attributes
                if IIrec==self.IIrec_first:
                    io_utils.setAtrributes(outh5file,self.h5Attribs)

            # increment counters
            Irec=Irecs[0][-1]+1
            try:
                while Ibad[0].__contains__(Irec):
                    Irec=Irec+1
            except: ''
            IIrec=IIrec+1
            Iplot=Iplot+1
                    
        ### end: main loop

        if (self.FITOPTS['fitcal'] == 1):
            print("Adding calibration information.")

            fname = self.OPTS['outfileLocked']
            calFname = self.FITOPTS['beamMapScaleFile']
            calMethodIndex = 0  #Plasma Line! Need to change this so there is a test done to determine which method was used!!!
            add_calibration_info(fname,calFname,calMethodIndex)

        # rename output file
        try:
            if os.path.exists(self.OPTS['outfile']):
                os.remove(self.OPTS['outfile'])
            os.rename(self.OPTS['outfileLocked'],self.OPTS['outfile'])
        except:
            raise IOError, 'Error renaming output file: ' + self.OPTS['outfileLocked'] + 'to ' + self.OPTS['outfile']


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

        return 0

#######

if __name__ == '__main__':
    
    # parse input options
    usage = 'run_fitter_partitioned [OPTIONS]'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-c','--conf',dest='conffile',
                        default='configuration.ini',
                        metavar="FILE",
                        help='Configuration file or list of configuration files (default configuration.ini)')
    (options,args) = parser.parse_args()
    
    # go go go
    RF=Run_Fitter_Partitioned(options)
    RF.run()
    
    sys.exit(0)
