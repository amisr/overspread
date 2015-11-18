#!/usr/bin/env python

"""

"""

from amisr_py.constants.constants import *
from amisr_py.io import *
from amisr_py.plotting import plotVvels
import vvels

import os
import scipy, scipy.stats
import time
import ConfigParser

class condFile(ioclass.outputFileClass):

    def __init__(self):
        self.title = 'Vector Velocities Resolved in Altitude'
        
        self.h5Paths = {
            'Params'    :   ('/ProcessingParams','Experiment Parameters'),\
            'Site'      :   ('/Site','Site Parameters'),\
            'Time'      :   ('/Time','Time Information'),\
            'IonoParams' : ('/IonoParams','Ionospheric Parameters')
        }        
                        
        self.h5Attribs = {
            '/Time/dtime' : [('TITLE','Decimal Time'),('Size','Nrecords x 2 (Start and end of integration)'),('Unit','UT Hours')],\
            '/Time/UnixTime' : [('TITLE','Unix Time'),('Size','Nrecords x 2 (Start and end of integration)'),('Unit','Seconds')],\
            '/Time/MagneticLocalTime' : [('TITLE','Magnetic Local Time'),('Size','Nrecords x 2 (Start and end of integration)'),('Unit','UT Hours')],\
            '/Site/Altitude' : [('TITLE','Altitude'),('Description','Altitude of site'),('Unit','Meters')],\
            '/Site/Code' : [('TITLE','Site Code')],\
            '/Site/Latitude' : [('TITLE','Latitude'),('Description','Latitude of site'),('Unit','Degrees North')],\
            '/Site/Longitude' : [('TITLE','Longitude'),('Description','Longitude of site'),('Unit','Degrees East')],\
            '/Site/MagneticLatitude' : [('TITLE','Magnetic Latitude'),('Description','Magnetic Latitude of site'),('Unit','Degrees North')],\
            '/Site/MagneticLongitude' : [('TITLE','Magnetic Longitude'),('Description','Magnetic Longitude of site'),('Unit','Degrees East')],\
            '/Site/MagneticLocalTimeMidnight' : [('TITLE','Magnetic Local Time Midnight'),('Unit','UT Hours')],\
            '/Site/Name' : [('TITLE','Name'),('Description','Site Name')],\
            '/ProcessingParams/ProcessingTimeStamp' : [('TITLE','Processing Time Stamp')],\
            '/ProcessingParams/BaudLength' : [('TITLE','Baud Length'),('Unit','Seconds')],\
            '/ProcessingParams/PulseLength' : [('TITLE','Pulse Length'),('Unit','Seconds')],\
            '/ProcessingParams/RxFrequency' : [('TITLE','Rx Frequency'),('Description','Receive frequency'),('Unit','Hertz')],\
            '/ProcessingParams/TxFrequency' : [('TITLE','Tx Frequency'),('Description','Transmit frequency'),('Unit','Hertz')],\
            '/ProcessingParams/SourceFile': [('TITLE','Source File'),('Description','Source File of Measurements')],\
            '/IonoParams/BeamCodes' : [('TITLE','BeamCodes'),('Description','Beamcode array'),('Size','Nbeams x 4 (Beamcode, Azimuth (degrees), Elevation (degrees), System constant (m^5/s)')],\
            '/IonoParams/Altitude': [('TITLE','Altitude'),('Description','Altitude'),('Unit','Meters')],\
            '/IonoParams/Range': [('TITLE','Range'),('Description','Range'),('Unit','Meters')],\
            '/IonoParams/Ne': [('TITLE','Electron Density'),('Description','Electron density'),('Unit','electrons/m^{-3}')],\
            '/IonoParams/errNe': [('TITLE','Error on Electron Density'),('Description','Error on Electron Density'),('Unit','electrons/m^{-3}')],\
            '/IonoParams/Ball': [('TITLE','Magnetic Field Strength'),('Description','Magnetic Field Strength'),('Unit','Tesla')],\
            '/IonoParams/Sh': [('TITLE','Hall Conductivity'),('Description','Hall Conductivity'),('Unit','mho/m')],\
            '/IonoParams/errSh': [('TITLE','Error on Hall Conductivity'),('Description','Error on Hall Conductivity'),('Unit','mho/m')],\
            '/IonoParams/Sp': [('TITLE','Pedersen Conductivity'),('Description','Pedersen Conductivity'),('Unit','mho/m')],\
            '/IonoParams/errSp': [('TITLE','Error on Pedersen Conductivity'),('Description','Error on  Pedersen Conductivity'),('Unit','mho/m')],\
            '/IonoParams/Nuin': [('TITLE','Ion-Neutral Collision Frequency'),('Description','Mass-Weighted Ion-Neutral Collision Frequency'),('Unit','1/Seconds')],\
            '/IonoParams/Wci': [('TITLE','Ion Cyclotron Frequency'),('Description','Mass-Weighted Ion Cyclotron Frequency'),('Unit','Radians/Second')],\
            '/IonoParams/Dec': [('TITLE','Declination Angle'),('Description','Declination Angle'),('Unit','Degrees')],\
            '/IonoParams/Dip': [('TITLE','Dip Angle'),('Description','Dip Angle'),('Unit','Degrees')],
        }

        return
        
class cond:
    
    def __init__(self,inifiles,sec):
    
        self.DefaultIni = '/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/config_cond-default.ini'

        # parse ini file
        self.ini_parse(inifiles,sec)
        
        # input file
        self.h5file=io_utils.h5file(self.filename) # h5file instance

        # output directory
        if not os.path.exists(os.path.dirname(self.outputFname)):
            os.makedirs(os.path.dirname(self.outputFname))

        return
        
    def ini_parse(self,inifile,sec):        

        # setup ConfigParser object
        config=ConfigParser.ConfigParser()
        
        # read default ini file
        fn = config.read(self.DefaultIni)
        if len(fn)!=1 or fn[0]!=self.DefaultIni:
            raise IOError, 'Unable to read default ini file %s', self.DefaultIni

        # read supplied ini file
        config.read(inifile.split(','))

        # make sure necessary section exists
        if not config.has_section(sec):
            raise IOError, 'Configuration files must contain section %s', sec

        self.config=config

        # set processing parameters
          
        # set plotting parameters
        self.makeplot=eval(io_utils.ini_tool(config,sec,'makeplot',required=0,defaultParm=config.get('DefaultParameters','makeplot')))
        self.clim=eval(io_utils.ini_tool(config,sec,'clim',required=0,defaultParm=config.get('DefaultParameters','clim')))
        self.sc=eval(io_utils.ini_tool(config,sec,'sc',required=0,defaultParm=config.get('DefaultParameters','sc')))
        
        # get filenames
        self.saveout=1
        self.filename=io_utils.ini_tool(config,sec,'inputFilename',required=1)
        self.outputFname=io_utils.ini_tool(config,sec,'outputFilename',required=1)

        return
        
    def setOutDicts(self,dat):

        self.Time = {
            'UnixTime'  :   None,\
            'dtime' :   None,\
            'MagneticLocalTime' : None
        }
        
        self.Site = dat['/Site']

        self.IonoParams={
            'Altitude' : None,\
            'Ne'    :   None,\
            'errNe' :   None,\
            'Ball'  :   None,\
            'Sh'    :   None,\
            'errSh' :   None,\
            'Sp'    :   None,\
            'errSp' :   None,\
            'Nuin'  :   None,\
            'Wci'   :   None,\
            'Dec'   :   None,\
            'Dip'   :   None            
        }
        
        self.Params = {
            'SourceFile'    :   os.path.basename(self.filename), \
            'PulseLength'   :   None ,\
            'BaudLength'   :   None ,\
            'RxFrequency'   :   None ,\
            'TxFrequency'   :   None ,\
            'ProcessingTimeStamp'    : time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()),\
            'TitleString'   :   self.titleString
        }

        try: self.Params['PulseLength']=dat['/ProcessingParams']['PulseLength']
        except: self.Params['PulseLength']=0.0
        try: self.Params['BaudLength']=dat['/ProcessingParams']['BaudLength']
        except: self.Params['BaudLength']=0.0
        try: self.Params['RxFrequency']=dat['/ProcessingParams']['RxFrequency']
        except: self.Params['RxFrequency']=0.0
        try: self.Params['TxFrequency']=dat['/ProcessingParams']['TxFrequency']
        except: self.Params['TxFrequency']=0.0
    
        return
        
    def createOutputFile(self):
        
        # create output file
        ofile = condFile()
        ofile.createFile(self.outputFname+'.h5')

        # create groups
        ofile.createh5groups()
        
        # create arrays
        ofile.createStaticArray(ofile.h5Paths['Params'][0],self.Params,keys2do=self.Params.keys())
        ofile.createStaticArray(ofile.h5Paths['Time'][0],self.Time,keys2do=self.Time.keys())
        ofile.createStaticArray(ofile.h5Paths['Site'][0],self.Site,keys2do=self.Site.keys())
        ofile.createStaticArray(ofile.h5Paths['IonoParams'][0],self.IonoParams,keys2do=self.IonoParams.keys())
        
        # set attributes in output file
        ofile.setAtrributes()
        
        # close file
        ofile.closeFile()
        
        return
           
    def createPlots(self):
    
        sp = self.IonoParams['Sp']; dsp= self.IonoParams['errSp']
        sh = self.IonoParams['Sh']; dsh= self.IonoParams['errSh']
        bmcodes = self.IonoParams['Beamcodes']
        
        alt=self.IonoParams['Altitude']
        timeout = self.Time['UnixTime']
        
        for ibm in range(bmcodes.shape[0]):
            tbm = bmcodes[ibm,0] 
            talt = alt[ibm,:]
            tsp = scipy.squeeze(sp[:,ibm,:])
            tdsp = scipy.squeeze(dsp[:,ibm,:])
            tsh = scipy.squeeze(sh[:,ibm,:])
            tdsh = scipy.squeeze(dsh[:,ibm,:])
            
            Ialt = scipy.where(scipy.isfinite(talt))    
            #scipy.place(tsp,tsp==scipy.nan,[0])
            #scipy.place(tdsp,tdsp==scipy.nan,[0])
            #scipy.place(tsh,tsh==scipy.nan,[0])
            #scipy.place(tdsh,tdsh==scipy.nan,[0])
            tintSp = scipy.trapz(tsp[:,Ialt],talt[Ialt])
            tdintSp = 0*tintSp #scipy.trapz(tdsp[:,Ialt],talt[Ialt])
            tintSh = scipy.trapz(tsh[:,Ialt],talt[Ialt])
            tdintSh = 0*tintSh  #scipy.trapz(tdsh[:,Ialt],talt[Ialt])
                            
            condplot = plotVvels.condPlot()
            condplot.makePlot(timeout,talt/1e3,tsp,tdsp,tsh,tdsh,tintSp,tdintSp,tintSh,tdintSh,\
                title='Conductivity - %d' % tbm,units='mho/m',cax=[1e-10,1e-4],caxInt=[0.01,10.0],label='Altitude (km)',sc=1)
            if self.saveout:
                condplot.figg.savefig('%s-%d.png' % (self.outputFname,tbm))        

        return
                          
    def docond(self):

        # read data
        dat1 = self.h5file.readWholeh5file()

        # beamcodes
        BeamCodes=dat1['/']['BeamCodes']

        # geomag
        kpn=(dat1['/Geomag']['kpn'])
        kpe=(dat1['/Geomag']['kpe'])
        kpar=(dat1['/Geomag']['kpar'])
        plat=(dat1['/Geomag']['MagneticLatitude'])
        plong=(dat1['/Geomag']['MagneticLongitude'])
        RangeGmag=(dat1['/Geomag']['Range'])
        Babs=dat1['/Geomag']['Babs']
        Bmed = scipy.stats.nanmedian(Babs)
        for i in range(Bmed.ndim):
            Bmed=scipy.stats.nanmedian(Bmed)
        dec1=dat1['/Geomag']['Declination']
        dip1=dat1['/Geomag']['Dip']

        # fitted params
        ht=dat1['/FittedParams']['Altitude']
        Range=dat1['/FittedParams']['Range']
        ne1=dat1['/FittedParams']['Ne']
        dne1=dat1['/FittedParams']['dNe']
        nu1=dat1['/FittedParams']['Fits'][:,:,:,:,2]
        frac1=dat1['/FittedParams']['Fits'][:,:,:,:,0]
        mass=dat1['/FittedParams']['IonMass']
                
        # conductivity
        sp1 = scipy.zeros(ne1.shape); dsp1 = scipy.zeros(ne1.shape)
        sh1 = scipy.zeros(ne1.shape); dsh1 = scipy.zeros(ne1.shape)
        wci1 = scipy.zeros(ne1.shape)
        nuin1 = scipy.sum(frac1[:,:,:,:-1]*nu1[:,:,:,:-1],axis=3)     
        for iion in range(len(mass)+1):
            if iion==len(mass):
                tm = v_electronmass
                tn = ne1; tdn = dne1
                wci = -1.0*v_elemcharge*Babs/tm
            else:
                tm = mass[iion]*v_amu
                tn = ne1*frac1[:,:,:,iion]; tdn = dne1*frac1[:,:,:,iion]
                wci = v_elemcharge*Babs/tm
                wci1 += wci*frac1[:,:,:,iion]
            tnu = nu1[:,:,:,iion]               
            # pedersen
            tsp = scipy.power(tnu,2.0)/(scipy.power(tnu,2.0) + scipy.power(wci,2.0))
            tdsp = scipy.absolute(tsp) * v_elemcharge**2.0 * tdn / tm / tnu   
            tsp = tsp * v_elemcharge**2.0 * tn / tm / tnu            
            sp1 += tsp; dsp1 += tdsp            
            # hall
            tsh = -1.0*tnu*wci/(scipy.power(tnu,2.0) + scipy.power(wci,2.0))
            tdsh = scipy.absolute(tsh) * v_elemcharge**2.0 * tdn / tm / tnu
            tsh = tsh * v_elemcharge**2.0 * tn / tm / tnu
            sh1 += tsh; dsh1 += tdsh
        
         # title str
        yr=dat1['/Time']['Year'][0,0]; mon=dat1['/Time']['Month'][0,0]; day=dat1['/Time']['Day'][0,0]
        self.titleString=' %d-%d-%d' % (mon, day, yr)
        
        ### Set up output dicts
        self.setOutDicts(dat1)
        # Time
        self.Time['UnixTime'] = dat1['/Time']['UnixTime']
        self.Time['dtime'] = dat1['/Time']['dtime']
        self.Time['MagneticLocalTime'] = dat1['/Time']['MagneticLocalTimeSite']
        # IonoParams
        self.IonoParams['Beamcodes'] = BeamCodes
        self.IonoParams['Altitude'] = ht
        self.IonoParams['Range'] = Range
        self.IonoParams['Ball']=Babs
        self.IonoParams['Ne']=ne1; self.IonoParams['errNe']=dne1
        self.IonoParams['Sp']=sp1; self.IonoParams['errSp']=dsp1
        self.IonoParams['Sh']=sh1; self.IonoParams['errSh']=dsh1
        self.IonoParams['Nuin']=nuin1
        self.IonoParams['Wci']=wci1
        self.IonoParams['Dec']=dec1
        self.IonoParams['Dip']=dip1

        ### Output file
        if self.saveout:
            self.createOutputFile()
            
        ### Plot output
        if self.makeplot:
            self.createPlots()
        
        return