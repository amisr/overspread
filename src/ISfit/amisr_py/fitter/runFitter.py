#!/usr/bin/env python

"""

"""

from amisr_py.constants.constants import *
from amisr_py.io import *
from amisr_py.utils import *
from amisr_py.models import *
from amisr_py.ismodel import *
from amisr_py.lib import geolib

from fitterIniFile import *
from fitterOutputFile import *

import ISfitfun
import processData

import optparse, os, copy, glob
import ctypes, tables
import scipy

import matplotlib
matplotlib.use('Agg')
import pylab

# Fitter
class Fitter:

    # init
    def __init__(self,options):
        """ initialization function """
        
        # initialize vars
        self.options=options
        self.OPTS={}
        self.FITOPTS={}
        self.DEFOPTS={}
        self.AMB={}; self.AMB['Loaded']=0
        self.FITS={}
        self.Time={}
        self.Site={}
        self.Params={}
        self.Antenna={}
        self.BMCODES=None
        self.ContinueFromLocked=0 # whether to allow fitter to continue from a locked file
        
        # get configuration options
        self.iniParams = fitterIniFile(options['conffile']) 
        self.OPTS = self.iniParams.OPTS
        self.FITOPTS = self.iniParams.FITOPTS
        self.DEFOPTS = self.iniParams.DEFOPTS
        self.k_radar0=4.0*pi*self.DEFOPTS['TX_FREQ_DEF']/v_lightspeed
        self.FITOPTS['p_om0']=self.k_radar0*scipy.sqrt(2.0*v_Boltzmann*self.FITOPTS['p_T0']/(self.FITOPTS['p_M0']*v_amu))                    
                                                                  
        # load libraries
        try:
            self.ct_msis=ctypes.CDLL(self.iniParams.LIB_MSIS) # MSIS library
            self.ct_geolib=ctypes.CDLL(self.iniParams.LIB_GEOLIB) # GEOLIB library
            self.ct_flipchem=ctypes.CDLL(self.iniParams.LIB_FLIPCHEM) # FLIP chemistry library
        except:
            raise IOError, 'Problem loading libraries'

        # set some environment variables
        os.putenv('IGRF_PATH',self.iniParams.IGRF_PATH)
            
        # load the lag ambiguity function
        try:
            if type(self.OPTS['AMB_PATH'])!=tuple:
                if os.path.exists(self.OPTS['AMB_PATH']):
                    self.AMB=io_utils.load_amb_func(self.OPTS['AMB_PATH'],full=self.FITOPTS['FullProfile'])
                    self.AMB['Loaded']=1
                    print 'Read ambiguity function from external file - ' + self.OPTS['AMB_PATH']
            else:
                if len(self.OPTS['AMB_PATH'])!=2:
                   print 'Ambiguity Function as tuple must be length 2'
                else:
                    tmp=io_utils.load_amb_func(self.OPTS['AMB_PATH'][0],full=self.FITOPTS['FullProfile'])
                    self.AMB=io_utils.load_amb_func(self.OPTS['AMB_PATH'][1],full=self.FITOPTS['FullProfile'])
                    self.AMB['Wlag'][0,:]=scipy.interpolate.interp1d(tmp['Delay'], tmp['Wlag'],bounds_error=0,fill_value=0.0)(self.AMB['Delay']) # linear interpolation
                    self.AMB['Wrange'][0,:]=scipy.interpolate.interp1d(tmp['Range'], tmp['Wrange'],bounds_error=0,fill_value=0.0)(self.AMB['Range']) # linear interpolation
                    self.AMB['WlagSum'][0]=tmp['WlagSum'][0]
                    self.AMB['WrangeSum'][0]=tmp['WrangeSum'][0]
                    self.AMB['Loaded']=1                    
                    print 'Read ambiguity function from external file - ' + self.OPTS['AMB_PATH'][0]
                    print 'Read ambiguity function from external file - ' + self.OPTS['AMB_PATH'][1]
        except:
            ''
            #raise IOError,'Problem reading ambiguity function from file %s even though file exists' % self.OPTS['AMB_PATH']
                
        # set some other variables
        if self.FITOPTS['DO_FITS']:
            self.FITOPTS['NFIT']=scipy.zeros(self.FITOPTS['Ngroup'])
            for i in range(self.FITOPTS['Ngroup']):
                self.FITOPTS['NFIT'][i]=scipy.where(self.FITOPTS['Ifit'][i,:,:]==1)[0].shape[0]

        # get list of files
        self.getFileList()
        if self.nfiles.__contains__(0): # abort!
            ### TODO - ADD EXCEPTION HERE        
            print 'Nothing to do...'
            xxxxx
        
        # processing class
        self.procInst = processData.processLongPulse_mot1(self.FITOPTS,self.NFREQ)        
        
        # spectral class
        if self.FITOPTS['DO_FITS']:
            input={'Nion':self.FITOPTS['NION'],'mi':self.FITOPTS['mi'],'f0':self.DEFOPTS['TX_FREQ_DEF']}
            self.cspect = spec.ISspec(input,emode=[1,0,1],imode=[0,0,1])
        
        # make plot directory
        if self.OPTS['saveplots']==1:  
            if not os.path.exists(self.OPTS['plotsdir']):
                try:
                    os.mkdir(self.OPTS['plotsdir'])
                except:
                    ### TODO - ADD EXCEPTION HERE        
                    print 'Cant make plots dir'
                    xxxxx

        # close all figures
        pylab.close('all')
        
        # create the output file
        self.OPTS['outfileLocked'] = self.OPTS['outfile']+'.lock'
        self.outh5file = fitterOutputFile()
        # locked file continuation
        if os.path.exists(self.OPTS['outfileLocked']) and self.ContinueFromLocked:
            try:
                xxxx
                output=io_utils.read_whole_h5file(self.OPTS['outfileLocked'])
                print output.keys()
                self.NrecsToSkip=output['/Time']['UnixTime'].shape[0]
                del output
                print "Continuing using " + self.OPTS['outfileLocked'] + " from record " + str(NrecsToSkip)
            except:
                raise IOError, 'Unable to continue from locked file: ' + self.OPTS['outfileLocked']
        # new file
        else:
            try:
                self.NrecsToSkip=0
                self.outh5file.createFile(self.OPTS['outfileLocked'])
                self.outh5file.createh5groups()
                """
                self.outh5file.createh5groups(outh5file,[self.outh5file.h5Paths['MSIS'],self.outh5file.h5Paths['Geomag'],self.outh5file.h5Paths['RawPower'],self.outh5file.h5Paths['Params'],self.outh5file.h5Paths['Site'],self.outh5file.h5Paths['Time']])
                if self.FITOPTS['DO_FITS']:
                    self.outh5file.createh5groups(outh5file,[self.h5Paths['Fitted'],self.h5Paths['FitInfo']])
                    if self.OPTS['saveACFs']:
                        self.outh5file.createh5groups(outh5file,[self.h5Paths['ACFs']])
                if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    self.outh5file.createh5groups(outh5file,[self.h5Paths['Antenna']])
                """
                self.outh5file.closeFile()  
            except:
                raise IOError, 'Cannot create output file: ' + self.OPTS['outfileLocked']
                                    
        return
    
    # getFileList
    def getFileList(self):
        """ sets the list of data files """

        # check out the FILELIST
        try:
            if (type(self.OPTS['FILELIST'])!=tuple):
                self.OPTS['FILELIST']=tuple([self.OPTS['FILELIST']])
            self.NFREQ=len(self.OPTS['FILELIST'])
        except:
            ### TODO - ADD EXCEPTION HERE        
            print 'Problem understanding filelist'
            xxxxxxx
    
        # read the file that contains the list of files to process
        files=[]
        nfiles=[]
        for ifreq in range(self.NFREQ): # for each of the frequencies   
            f=open(self.OPTS['FILELIST'][ifreq]) # open
            files.append(f.readlines()) # read list
            f.close() # close
            files[ifreq]=[files[ifreq][ir].rstrip('\n') for ir in range(len(files[ifreq]))]
            [files[ifreq].remove('') for ir in range(files[ifreq].count(''))]
            files2=copy.copy(files[ifreq])
            [files[ifreq].extend(glob.glob(os.path.join(self.OPTS['ipath'],files2[ir]))) for ir in range(len(files2)) if files2[ir].rfind('*') != -1]
            [files[ifreq].remove(files2[ir]) for ir in range(len(files2))]
            files[ifreq].sort() # sort the file sequence
            nfiles.append(len(files[ifreq]))
        self.files = files
        self.nfiles = nfiles
    
        return
        
    # get_expname
    def get_expname(self,h5file):
        """ returns the name of the experiment """
        
        try:
            expname = h5file.root.Setup.Experimentfile.read()
            if type(expname)==scipy.ndarray:
                expname=expname[0]
            expname=expname.splitlines()[1].split('=')[1]
        except:
            expname=''
            
        return expname        
        
    # openFile
    def openFile(self,fname):
        """ Opens a data file, returns file pointer """
        
        try:
            fp = tables.openFile(fname,'r')
            print 'Opening file %s' % fname
        except:        
            raise IOError, 'Unable to open file %s' % fname
    
        return fp
        
    # call_fitter
    def call_fitter(self,S,sstr=''):
        """ 
        
        """
        
        ### array dimensions
        (Nbeams,Nranges,Nlags)=S['Acf'].shape
        
        ### fitter variables
        #??
        
        ### Output variables
        Ihtbm=scipy.zeros(Nbeams)
        HT=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan # Altitude
        RNG=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan # Range
        ne_out=scipy.zeros((Nbeams,Nranges,2),dtype='Float64')*scipy.nan # Fitted densities
        FITS_out=scipy.zeros((Nbeams,Nranges,self.FITOPTS['NION']+1,4),dtype='Float64')*scipy.nan # Fitted parameters
        ERRS_out=scipy.zeros((Nbeams,Nranges,self.FITOPTS['NION']+1,4),dtype='Float64')*scipy.nan # Errors from fits
        mod_ACF=scipy.zeros((Nbeams,Nlags,Nranges),dtype='Complex64')*scipy.nan # model ACFs
        meas_ACF=scipy.zeros((Nbeams,Nlags,Nranges),dtype='Complex64')*scipy.nan # measured ACFs
        errs_ACF=scipy.zeros((Nbeams,Nlags,Nranges),dtype='Float64')*scipy.nan # errors on the ACFs
        fitinfo={} # dict containing information on fit
        fitinfo['fitcode']=scipy.zeros((Nbeams,Nranges),dtype='Int16') # a fit code
        fitinfo['dof']=scipy.zeros((Nbeams,Nranges),dtype='Int16') # degrees of freedom
        fitinfo['chi2']=scipy.zeros((Nbeams,Nranges),dtype='Float32') # reduced chi2
        fitinfo['nfev']=scipy.zeros((Nbeams,Nranges),dtype='Int16') # number of function evals
        models={} # dict containing model params
        models['nHe']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nO']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nN2']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nO2']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nAr']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nMass']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nH']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nN']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nOanom']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['Texo']=scipy.zeros((Nbeams,Nranges),dtype='Float32')*scipy.nan
        models['Tn']=scipy.zeros((Nbeams,Nranges),dtype='Float32')*scipy.nan
        models['SolarZen']=scipy.zeros((Nbeams,Nranges),dtype='Float32')*scipy.nan
        models['LocalSolarTime']=scipy.zeros((Nbeams,Nranges),dtype='Float32')*scipy.nan
        models['SolarDec']=scipy.zeros((Nbeams,Nranges),dtype='Float32')*scipy.nan
        models['nNO']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['nN2D']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan
        models['qOp']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan 
        models['nuen']=scipy.zeros((Nbeams,Nranges),dtype='Float64')*scipy.nan 
        models['nuin']=scipy.zeros((Nbeams,Nranges,self.FITOPTS['NION']),dtype='Float64')*scipy.nan 

        ### geomagnetic parameters
        try:
            gmag=self.Gmag
        except:
            gmag=geomag.blankGmag(Nbeams,Nranges)
            
        ### geophys indices
        decTime=self.Time['dtime'][0] #(self.Time['dtime'][0]+self.Time['dtime'][1]+24.0*(self.Time['doy'][1]-self.Time['doy'][0]))/2.0
        (f107, f107a, ap)=msis_ext.read_geophys(int(self.Time['Year'][0]),int(self.Time['doy'][0]),decTime,self.iniParams.GEOPHYS_PATH)    
        models['f107']=f107; models['f107a']=f107a; models['AP']=ap            

        ### lags to fit
        if len(self.FITOPTS['Lags2fit'])==0:
            Iy=range(Nlags)
        else:
            xxxx
            # need to write
        if not self.FITOPTS['fit0lag'] and Iy.__contains__(0):
            Iy=Iy[1:]

        ### Fitting loop
        # loop over beams
        for ibm in range(Nbeams):               
            if 1==1:
            
                AzAng = S['BMCODES'][ibm,1]
                ElAng = S['BMCODES'][ibm,2]
                
                print '\nBeam %d (Az:%2.1f,El:%2.1f)' % (ibm,AzAng,ElAng)        

                # run the geomag model  
                tgmag=geomag.geomagTime(self.ct_geolib,self.Time['Year'][0],scipy.array([AzAng]),scipy.array([ElAng]),\
                    self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0,rng=S['Range'][ibm,:]/1e3)            
                for key in gmag.iterkeys():
                    if gmag[key].shape == (Nbeams,Nranges):
                        gmag[key][ibm,:]=tgmag[key]
                    else:
                        gmag[key][ibm,:]=tgmag[key]  
                                
                # loop over ranges
                groupIndex=-1; nfit=0
                for irng in range(Nranges):
                    
                    irng=20
                    
                    ### Initial setup
                    
                    # temporary variables
                    alt = S['Altitude'][ibm,irng]
                    rng = S['Range'][ibm,irng]
                    acf = S['Acf'][ibm,irng,:]
                    acfVar = S['AcfVar'][ibm,irng,:]
                    psc = S['Psc'][ibm,irng]
                    ne = S['Ne'][ibm,irng]
                    
                    if groupIndex<0 or alt>=self.FITOPTS['GroupHt'][groupIndex]:
                        groupIndex+=1
                        Ifit = self.FITOPTS['Ifit'][groupIndex]
                        nfit = int(self.FITOPTS['NFIT'][groupIndex]+1) 
                        isfitter = ISfitfun.fitfun(self.cspect,Ifit,nfit,self.AMB['Delay'],scipy.transpose(self.AMB['Wlag'][Iy,:]),self.FITOPTS['mi'])
                        
                    # mesg
                    print '%s Alt: %2.1f, Rng: %2.1f km' % (sstr,alt/1e3,rng/1e3)
                        
                    ### Call some models
                    
                    # MSIS
                    (HEdens,Odens,N2dens,O2dens,ARdens,MassDens,Hdens,Ndens,AnomOdens,Texo,tn,nui,nue,qOp) = msis_ext.call_MSIS(self.ct_msis,self.Time['doy'][0],
                        decTime,gmag['Latitude'][ibm,irng],gmag['Longitude'][ibm,irng],self.Time['Year'][0],alt/1000.0,ap,f107a,f107,self.FITOPTS['z50'],mass=self.FITOPTS['mi'])
                    models['nHe'][ibm,irng]=HEdens*1.0e6
                    models['nO'][ibm,irng]=Odens*1.0e6
                    models['nN2'][ibm,irng]=N2dens*1.0e6
                    models['nO2'][ibm,irng]=O2dens*1.0e6
                    models['nAr'][ibm,irng]=ARdens*1.0e6
                    models['nMass'][ibm,irng]=MassDens*1.0e6/1.0e3
                    models['nH'][ibm,irng]=Hdens*1.0e6
                    models['nN'][ibm,irng]=Ndens*1.0e6
                    models['nOanom'][ibm,irng]=AnomOdens*1.0e6
                    models['Texo'][ibm,irng]=Texo
                    models['Tn'][ibm,irng]=tn
                    models['qOp'][ibm,irng]=qOp                    
                    models['nuen'][ibm,irng]=nue                       
                    models['nuin'][ibm,irng,:]=nui    
                    nu = scipy.append(nui,nue)
    
                    # initial flip ion chemistry, with te=ti=tn and Ne = initial guess
                    LTHRS,SZAD,DEC,OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT=flipchem.call_flip(self.ct_flipchem,int(self.Time['Year'][0]),int(self.Time['doy'][0]),decTime,alt/1000.0,
                        gmag['Latitude'][ibm,irng],gmag['Longitude'][ibm,irng],ap,f107,f107a,tn,tn,tn,Odens,O2dens,N2dens,HEdens,0.5*Ndens,ne*1.0e-6)
                    models['SolarZen'][ibm,irng]=SZAD
                    models['LocalSolarTime'][ibm,irng]=LTHRS
                    models['SolarDec'][ibm,irng]=DEC
                    models['nNO'][ibm,irng]=NNO*1.0e6
                    models['nN2D'][ibm,irng]=N2D*1.0e6

                    ### Set up parameter arrays
                    
                    # Initialize
                    ti = scipy.ones(self.FITOPTS['NION']+1,dtype='float64')*tn*1.1; ti[-1]*=1.1
                    mi = self.FITOPTS['mi']
                    psi = scipy.zeros(self.FITOPTS['NION']+1,dtype='float64')
                    vi = scipy.zeros(self.FITOPTS['NION']+1,dtype='float64')
                    
                    # set collision frequency to model if necessary
                    psi = [nu[iion] if Ifit[iion,2]==-2 else 0.0 for iion in range(self.FITOPTS['NION']+1)]

                    terr=scipy.zeros((4,self.FITOPTS['NION']+1),dtype='Float64')*scipy.nan
                    if 1==1:
                        nloops=0
                        while 1:
                            nloops+=1

                            ### set ion density
                            ni = scipy.ones(self.FITOPTS['NION']+1,dtype='float64')
                            ni[scipy.where((Ifit[:-1,0]==0) | (Ifit[:-1,0]==1))]=0.0  
                            
                            # ions from model
                            I=scipy.where(Ifit[:,0]==-2)[0]
                            if I.size != 0:
                                for a in range(I.size):
                                    if self.FITOPTS['molecularModel']==0:
                                        if mi[I[a]]>=28 and mi[I[a]]<=32: # its a molecular 
                                            ni[I[a]]=1.0-models['qOp'][ibm,irng]
                                    elif self.FITOPTS['molecularModel']==1:
                                        if mi[I[a]]==16.0: # O+
                                            ni[I[a]]=OXPLUS
                                        elif mi[I[a]]==32.0: # O2+
                                            ni[I[a]]=O2PLUS
                                        elif mi[I[a]]==30.0: # NO+
                                            ni[I[a]]=NOPLUS
                                        elif mi[I[a]]==28.0: # N2+ 
                                            ni[I[a]]=N2PLUS
                                        elif mi[I[a]]==14.0: # N+ 
                                            ni[I[a]]=NPLUS
                                        else:   
                                            ni[I[a]]=0.0
                            I=scipy.where(Ifit[:,0]==-1)[0]
                            if I.size==1:   
                                ni[I]=1.0-(scipy.sum(ni[:-1])-1.0)
                            elif I.size>1:  
                                raise ValueError, "Can't have more than one ion -1"                              
                            
                            isfitter.setArrays(ne,ni,ti,psi,vi)
                            
                            ### Initial guess
                            if 1==1:                                
                                p0=[(ne,self.FITOPTS['p_N0'])]
                                # composition
                                p0.extend([(ni[iion],1.0) for iion in range(self.FITOPTS['NION']+1) if Ifit[iion,0]==1])
                                # temperature
                                p0.extend([(ti[iion],self.FITOPTS['p_T0']) for iion in range(self.FITOPTS['NION']+1) if Ifit[iion,1]==1])
                                # collision frequency
                                p0.extend([(nu[iion],self.FITOPTS['p_om0']) for iion in range(self.FITOPTS['NION']+1) if Ifit[iion,2]==1])
                                # velocity
                                p0.extend([(vi[iion],100.0) for iion in range(self.FITOPTS['NION']+1) if Ifit[iion,3]==1])

                                params0 = scipy.array([el[0] for el in p0])
                                scaler = scipy.array([el[1] for el in p0])
                                                                
                                isfitter.updateArrays(params0)
                                isfitter.cspect.adjustParams({'B':gmag['Babs'][ibm,irng]})
                                                                    
                                MAXFEV_C=20 # need to be able to set this somewhere

                                (x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(ISfitfun.fitf,params0,\
                                    (isfitter,acf[Iy],acfVar[Iy],psc,\
                                    0.75*tn,self.FITOPTS['LagrangeParams']),
                                    full_output=1,epsfcn=1.0e-5,ftol=1.0e-5, xtol=1.0e-5, gtol=0.0, \
                                    maxfev=MAXFEV_C*params0.shape[0],factor=100.0,diag=1.0/scaler)

                                print isfitter.Parms
                                
                                print scipy.sqrt(scipy.diag(cov_x))
                                
                                # record termination parameter of fitter
                                fitinfo['fitcode'][ibm,irng]=ier
                                if cov_x==None:
                                    try:
                                        fitinfo['fitcode'][ibm,irng]=-fitcode[ibm,irng]
                                    except:
                                        fitinfo['fitcode'][ibm,irng]=-45
                                else:
                                    cov_x=scipy.sqrt(scipy.diag(cov_x))
                                    terr[IfitMR]=cov_x[1:]                                
                                infodict['fvec']=infodict['fvec'][:-len(self.FITOPTS['LagrangeParams'])]
                                fitinfo['nfev'][ibm,irng]=infodict['nfev']
                                fitinfo['dof'][ibm,irng]=infodict['fvec'].shape[0]-params0.shape[0]-1
                                fitinfo['chi2'][ibm,irng]=scipy.real(scipy.sum(scipy.power(scipy.real(infodict['fvec']),2.0))/fitinfo['dof'][ibm,irng])
                  

                                        
                    xxx
        
        return 

    # increment
    def increment(self,Irecs,outputAll,Irec,IIrec,Iplot):
        """ Increments counters """
        
        # close any files, increment record counters
        for ifreq in range(self.NFREQ):
            if Irecs[ifreq][-1][1]<(len(outputAll[ifreq])-1):
                Irec[ifreq]=0
            else:
                Irec[ifreq] = Irecs[ifreq][-1][0]+1
            while len(outputAll[ifreq])>1:
                outputAll[ifreq][0].close()        
                outputAll[ifreq].pop(0)   
        IIrec=IIrec+1
        Iplot=Iplot+1       
        
        return outputAll,Irec,IIrec,Iplot    
                
    # run
    def run(self):
        """
            main routine that runs the fitting loop. 
            call after instantiating a run_fitter instance
        """
    
        # initialize some local vars
        done = [0 for ifreq in range(self.NFREQ)] # flag to say when we are done
        IIrec=0 # record counter
        newexp=0 # experiment switch
        Iplot=self.OPTS['nplots'] # plot counter
        Ibeams=None
        Irec = [0 for ifreq in range(self.NFREQ)] # record counter within file
        frec = [0 for ifreq in range(self.NFREQ)] # file counter
            
        # deal with the first file
        outputAll = [[self.openFile(self.files[ifreq][0])] for ifreq in range(self.NFREQ)]

        """
        # bad records
        if self.FITOPTS['MOTION_TYPE']==1: # Az,El
            Ibad.append(scipy.where((outputAll[ii]['/Antenna']['Mode'][:,0] != outputAll[ii]['/Antenna']['Mode'][:,1]) | (outputAll[ii]['/Antenna']['Event'][:,0] != outputAll[ii]['/Antenna']['Event'][:,1]))[0])                            
        """
        
        # median record integration; assumed constant
        RecInt = max([scipy.median(outputAll[ifreq][0].root.Time.UnixTime[:,1] - outputAll[ifreq][0].root.Time.UnixTime[:,0]) for ifreq in range(self.NFREQ)])
        
        curexpname= [self.get_expname(outputAll[ifreq][0]) for ifreq in range(self.NFREQ)]

        print 'Experiment: %s' % curexpname
    
        ### start: main loop
        while not all(done):

            ### find which records we should read
            
            # start and end time of record
            RecStartTime = [outputAll[ifreq][0].root.Time.UnixTime[Irec[ifreq],0] for ifreq in range(self.NFREQ)]
            ifreq0 = RecStartTime.index(min(RecStartTime)) # index of minimum value
            RecStartTime = min(RecStartTime)
            RecEndTime = RecStartTime+self.FITOPTS['Recs2Integrate']+RecInt/2.0              
            # Antenna modes and events
            if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                AntennaMode = outputAll[ifreq0][0].root.Antenna.Mode[Irec[ifreq0],0]
                AntennaEvent = outputAll[ifreq0][0].root.Antenna.Event[Irec[ifreq0],0]
            # record indices
            Irecs=[]
            for ifreq in range(self.NFREQ):
                ifcnt = 0
                uTime = scipy.mean(outputAll[ifreq][ifcnt].root.Time.UnixTime,axis=1)
                trecs = [(irec,ifcnt) for irec in range(uTime.shape[0]) if uTime[irec]>=RecStartTime and uTime[irec]<=RecEndTime \
                    and outputAll[ifreq][ifcnt].root.Antenna.Mode[irec,0]==AntennaMode and outputAll[ifreq][ifcnt].root.Antenna.Mode[irec,1]==AntennaMode \
                    and outputAll[ifreq][ifcnt].root.Antenna.Event[irec,0]==AntennaEvent and outputAll[ifreq][ifcnt].root.Antenna.Event[irec,1]==AntennaEvent]
                # includes last record in file - need to add file
                ifcnt = 0
                while trecs[-1][0]==(uTime.shape[0]-1) and trecs[-1][1]==ifcnt:
                    ifcnt+=1
                    frec[ifreq]+=1
                    if frec[ifreq]<self.nfiles[ifreq]:
                        outputAll[ifreq].append(self.openFile(self.files[ifreq][frec[ifreq]]))
                        uTime = scipy.mean(outputAll[ifreq][ifcnt].root.Time.UnixTime,axis=1)
                        trecs.extend([(irec,ifcnt) for irec in range(uTime.shape[0]) if uTime[irec]>=RecStartTime and uTime[irec]<=RecEndTime \
                            and outputAll[ifreq][ifcnt].root.Antenna.Mode[irec,0]==AntennaMode and outputAll[ifreq][ifcnt].root.Antenna.Mode[irec,1]==AntennaMode \
                            and outputAll[ifreq][ifcnt].root.Antenna.Event[irec,0]==AntennaEvent and outputAll[ifreq][ifcnt].root.Antenna.Event[irec,1]==AntennaEvent])                    
                    else: # last file, done
                        done[ifreq]=1 
                #
                Irecs.append(trecs)

            ### print the record numbers
            print '\nFile %s of %s ' % ([f+1 for f in frec], self.nfiles)
            print 'Integration Number: %d, %s Recs Being Integrated' % (IIrec,[len(Irecs[ifreq]) for ifreq in range(self.NFREQ)])
            fstr='File %s of %s, Rec %d, ' % ([f+1 for f in frec], self.nfiles,IIrec)

            ### skip records
            if self.NrecsToSkip >0:
                outputAll,Irec,IIrec,Iplot = self.increment(Irecs,outputAll,Irec,IIrec,Iplot)
                self.NrecsToSkip-=1
                continue
            
            ### get initial record info
            self.procInst.reinit(outputAll,Irecs)
            self.Time, Tx, Rx = self.procInst.getRecInfo()
            
            ### process a record
            S = self.procInst.process(doamb=(not self.AMB['Loaded']))
            
            ### ambiguity function
            if not self.AMB['Loaded']: # if it hasn't already been loaded, it should have been read from the data files.
                if not S.has_key('Acf'):
                    if not S['Power'].has_key('Ambiguity'):
                        raise RuntimeError, 'No valid ambiguity function in data files or specified external file.'
                    else:
                        self.AMB=S['Power']['Ambiguity']
                        self.AMB['Loaded']=1   
                elif not S['Acf'].has_key('Ambiguity'): # this is the case where we needed to get it from the data file but unable to
                    raise RuntimeError, 'No valid ambiguity function in data files or specified external file.'
                else:
                    self.AMB=S['Acf']['Ambiguity']
                    self.AMB['Loaded']=1   
                                
            ### get some standard info the first time through
            if self.BMCODES is None or newexp:
                                                                        
                self.BMCODES=S['BMCODES'] # beamcodes
                self.BMCODES[:,2]=abs(self.BMCODES[:,2])
            
                # read site information
                self.Site['Latitude']=outputAll[0][0].root.Site.Latitude.read() # site latitude
                self.Site['Longitude']=outputAll[0][0].root.Site.Longitude.read() # site longitude
                if self.Site['Longitude']>0: # some files have the sign of the longitude flipped
                    self.Site['Longitude']*=-1
                self.Site['Altitude']=outputAll[0][0].root.Site.Altitude.read() # site altitude 
                try: self.Site['Code']=int(outputAll[0][0].root.Site.Code.read()); # site code
                except: ''; 
                try: self.Site['Name']=outputAll[0][0].root.Site.Name.read() # site name
                except: '';              
            
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    if IIrec==0 or newexp or Ibeams==None: # first time through
                    
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
            
            ### get altitude using geodetic conversion
            if S.has_key('Power'):
                S['Power']['Altitude']=util_fcns.range2height(self.ct_geolib,scipy.squeeze(S['Power']['Range'])/1000.0,S['BMCODES'][:,1],S['BMCODES'][:,2],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)        
            if S.has_key('Acf'):
                S['Acf']['Altitude']=util_fcns.range2height(self.ct_geolib,scipy.squeeze(S['Acf']['Range'])/1000.0,S['BMCODES'][:,1],S['BMCODES'][:,2],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)        
            
            ### apply summation rule to data
            summed = self.procInst.applySummationRule()

            ### compute density profile using apriori model for temperatures
            # model for computing Ne from power
            Mod={}
            Mod['Te']=scipy.ones(S['Power']['Altitude'].shape)*1000
            Mod['Ti']=scipy.ones(S['Power']['Altitude'].shape)*1000
            Mod['Ne']=scipy.ones(S['Power']['Altitude'].shape)*1.0e11            
            # compute density
            (S['Power']['Ne_Mod'],S['Power']['Ne_NoTr'],tPsc)=util_fcns.ne_prof(S['Power']['Data'],S['Power']['Range'],S['Power']['Altitude'],Mod,Tx['Power'],S['Pulsewidth'],Tx['Frequency'],S['BMCODES'][:,3])
            # error on density measurement
            S['Power']['dNeFrac']=1.0/scipy.sqrt(scipy.repeat(S['Power']['Kint']*S['Power']['PulsesIntegrated'][:,scipy.newaxis],S['Power']['SNR'].shape[1],axis=1))*(1.0+scipy.absolute(1.0/S['Power']['SNR'])+S['Power']['iSCR'])
            S['Power']['dNeFrac'][scipy.where(S['Power']['SNR']<0.0)]=scipy.nan
            if self.FITOPTS['uselag1']: # if we are using the 1st lag to estimate the density, then we need to scale it a bit
                S['Power']['Ne_Mod']=S['Power']['Ne_Mod']/scipy.sum(scipy.absolute(self.AMB['Wlag'][S['Acf']['Lag1Index'],:]))
                S['Power']['Ne_NoTr']=S['Power']['Ne_NoTr']/scipy.sum(scipy.absolute(self.AMB['Wlag'][S['Acf']['Lag1Index'],:]))            
            else:
                S['Power']['Ne_Mod']=S['Power']['Ne_Mod']/scipy.sum(scipy.absolute(self.AMB['Wlag'][0,:]))
                S['Power']['Ne_NoTr']=S['Power']['Ne_NoTr']/scipy.sum(scipy.absolute(self.AMB['Wlag'][0,:]))

            ### do the fits              
            if self.FITOPTS['DO_FITS']: 
                # running fitting function
                self.call_fitter(summed,fstr)
                # do fits
                
                # make plots
            xxx
            
            ### Output data to file

            

            return
            print Irecs
            xxx
                   
            ### increment counters            
            outputAll,Irec,IIrec,Iplot = self.increment(Irecs,outputAll,Irec,IIrec,Iplot)
                            
            
            """
            try:
                while Ibad[0].__contains__(Irec):
                    Irec=Irec+1
            except: ''
            """
     
        
        return
   
#######

if __name__ == '__main__':
    # parse input options
    usage = 'run_fitter [OPTIONS]'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-c','--conf',dest='conffile',
                        default='configuration.ini',
                        metavar="FILE",
                        help='Configuration file or list of configuration files (default configuration.ini)')
    (options,args) = parser.parse_args()
        
    print options
        
    # go go go
    RF=Fitter(options)
    RF.run()
    
    sys.exit(0)