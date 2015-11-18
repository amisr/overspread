#!/usr/bin/env python

"""

"""

from amisr_py.constants.constants import *
from amisr_py.utils import *
from amisr_py.io import *
from amisr_py.plotting import plotVvels
import vvels

import os
import scipy, scipy.stats
import time
import ConfigParser

class cwindsFile(ioclass.outputFileClass):

    def __init__(self):
        self.title = 'Neutral Winds and Electrodynamics'
        
        self.h5Paths = {
            'Params'    :   ('/ProcessingParams','Experiment Parameters'),\
            'Site'      :   ('/Site','Site Parameters'),\
            'Time'      :   ('/Time','Time Information'),\
            'Electrodynamics' : ('/Electrodynamics','Electrodynamics')
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
            '/ProcessingParams/SourceFileVi': [('TITLE','Source File'),('Description','Source File of Vi Measurements')],\
            '/ProcessingParams/SourceFileVe': [('TITLE','Source File'),('Description','Source File of Ve Measurements')],\
            '/Electrodynamics/Altitude': [('TITLE','Altitude'),('Description','Altitude'),('Unit','Meters')],\
            '/Electrodynamics/Jperp' : [('TITLE','Perpendicular Current'),('Description','Perpendicular Current'),('Unit','A/m^2')],\
            '/Electrodynamics/errJperp' : [('TITLE','Error on Perpendicular Current'),('Description','Error on Perpendicular Current'),('Unit','A/m^2')],\
            '/Electrodynamics/Ugmag' : [('TITLE','Wind, Geomagnetic Coords'),('Description','Wind, Geomagnetic Coords'),('Unit','m/s')],\
            '/Electrodynamics/errUgmag' : [('TITLE','Error on Wind'),('Description','Error on Wind'),('Unit','m/s')],\
            '/Electrodynamics/Ugeo' : [('TITLE','Wind, Geographic Coords'),('Description','Wind, Geographic Coords'),('Unit','m/s')],\
            '/Electrodynamics/errUgeo' :[('TITLE','Error on Wind'),('Description','Error on Wind'),('Unit','m/s')],\
            '/Electrodynamics/HallConductivity' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/PedersenConductivity' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errHallConductivity' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errPedersenConductivity' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/HallConductance' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/PedersenConductance' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/CollisionFrequency' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/Kappa' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/ElectromagneticEnergyTransferRate' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errElectromagneticEnergyTransferRate' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/ElectromagneticEnergyTransferRateInt' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errElectromagneticEnergyTransferRateInt' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/JouleHeatingRate' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errJouleHeatingRate' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/JouleHeatingRateInt' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errJouleHeatingRateInt' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/PassiveDissipationRate' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errPassiveDissipationRate' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/PassiveDissipationRateInt' : [('TITLE',''),('Description',''),('Unit','')],\
            '/Electrodynamics/errPassiveDissipationRateInt' : [('TITLE',''),('Description',''),('Unit','')],\
        }

        return
        
class cwinds:
    
    def __init__(self,inifiles,sec='cwinds'):
    
        self.DefaultIni = '/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/config_cwinds-default.ini'

        # parse ini file
        self.ini_parse(inifiles,sec)
                
        # input file
        self.h5fileVi=io_utils.h5file(self.inputFilenameVi) # h5file instance
        self.h5fileVe=io_utils.h5file(self.inputFilenameVe) # h5file instance

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
        self.maxAlt=eval(io_utils.ini_tool(config,sec,'MaxAlt',required=0,defaultParm=config.get('DefaultParameters','MaxAlt')))

        # set plotting parameters
        self.makeplot=eval(io_utils.ini_tool(config,sec,'makeplot',required=0,defaultParm=config.get('DefaultParameters','makeplot')))
        self.climJ=eval(io_utils.ini_tool(config,sec,'climCurrent',required=0,defaultParm=config.get('DefaultParameters','climCurrent')))
        self.climU=eval(io_utils.ini_tool(config,sec,'climWind',required=0,defaultParm=config.get('DefaultParameters','climWind')))
        self.climEmInt=eval(io_utils.ini_tool(config,sec,'climEnergyInt',required=0,defaultParm=config.get('DefaultParameters','climEnergyInt')))
        self.climSig=eval(io_utils.ini_tool(config,sec,'climConductivity',required=0,defaultParm=config.get('DefaultParameters','climConductivity')))
        self.climSigInt=eval(io_utils.ini_tool(config,sec,'climConductivityInt',required=0,defaultParm=config.get('DefaultParameters','climConductivityInt')))
        
        # get filenames
        self.saveout=1
        self.inputFilenameVi=io_utils.ini_tool(config,sec,'inputFilenameVi',required=1)
        self.inputFilenameVe=io_utils.ini_tool(config,sec,'inputFilenameVe',required=1)
        self.outputFname=io_utils.ini_tool(config,sec,'outputFilename',required=1)

        return
        
    def setOutDicts(self,dat):

        self.Site = dat['/Site']
        self.Time = dat['/Time']
        
        self.Electrodynamics = {
            'Altitude' : None, \
            'Jperp' : None, 'errJperp' : None, \
            'Ugmag' : None, 'errUgmag' : None, \
            'Ugeo' : None, 'errUgeo' : None, \
            'HallConductivity' : None, 'PedersenConductivity' : None, \
            'errHallConductivity' : None, 'errPedersenConductivity' : None, \
            'HallConductance' : None, 'PedersenConductance' : None, \
            'CollisionFrequency' : None, 'Kappa' : None, \
            'ElectromagneticEnergyTransferRate' : None, 'errElectromagneticEnergyTransferRate' : None, \
            'ElectromagneticEnergyTransferRateInt' : None, 'errElectromagneticEnergyTransferRateInt' : None, \
            'JouleHeatingRate' : None, 'errJouleHeatingRate' : None, \
            'JouleHeatingRateInt' : None, 'errJouleHeatingRateInt' : None, \
            'PassiveDissipationRate' : None, 'errPassiveDissipationRate' : None, \
            'PassiveDissipationRateInt' : None, 'errPassiveDissipationRateInt' : None
        }

        self.Params = {
            'SourceFileVi'    :   os.path.basename(self.inputFilenameVi), \
            'SourceFileVe'    :   os.path.basename(self.inputFilenameVe), \
            'ProcessingTimeStamp'    : time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())        
        }

        return
        
    def createOutputFile(self):
        
        # create output file
        ofile = cwindsFile()
        ofile.createFile(self.outputFname+'.h5')

        # create groups
        ofile.createh5groups()
        
        # create arrays
        ofile.createStaticArray(ofile.h5Paths['Params'][0],self.Params,keys2do=self.Params.keys())
        ofile.createStaticArray(ofile.h5Paths['Time'][0],self.Time,keys2do=self.Time.keys())
        ofile.createStaticArray(ofile.h5Paths['Site'][0],self.Site,keys2do=self.Site.keys())
        ofile.createStaticArray(ofile.h5Paths['Electrodynamics'][0],self.Electrodynamics,keys2do=self.Electrodynamics.keys())
        
        # set attributes in output file
        ofile.setAtrributes()
        
        # close file
        ofile.closeFile()
        
        return
           
    def createPlots(self):
    
        J = self.Electrodynamics['Jperp']; dJ=self.Electrodynamics['errJperp']
        U = self.Electrodynamics['Ugeo']; dU=self.Electrodynamics['errUgeo']
        Ugmag = self.Electrodynamics['Ugmag']; dUgmag=self.Electrodynamics['errUgmag']
        Sh = self.Electrodynamics['HallConductivity']; Sp = self.Electrodynamics['PedersenConductivity']
        dSh = self.Electrodynamics['errHallConductivity']; dSp = self.Electrodynamics['errPedersenConductivity']
        IntSh = self.Electrodynamics['HallConductance']; IntSp = self.Electrodynamics['PedersenConductance']
        dIntSh = self.Electrodynamics['errHallConductance']; dIntSp = self.Electrodynamics['errPedersenConductance']
        
        JdotE=self.Electrodynamics['ElectromagneticEnergyTransferRate']; dJdotE=self.Electrodynamics['errElectromagneticEnergyTransferRate']
        JdotEp=self.Electrodynamics['JouleHeatingRate']; dJdotEp=self.Electrodynamics['errJouleHeatingRate']
        SpE2=self.Electrodynamics['PassiveDissipationRate']; dSpE2=self.Electrodynamics['errPassiveDissipationRate']
        IntJdotE=self.Electrodynamics['ElectromagneticEnergyTransferRateInt']; dIntJdotE=self.Electrodynamics['errElectromagneticEnergyTransferRateInt']
        IntJdotEp=self.Electrodynamics['JouleHeatingRateInt']; dIntJdotEp=self.Electrodynamics['errJouleHeatingRateInt']
        IntSpE2=self.Electrodynamics['PassiveDissipationRateInt']; dIntSpE2=self.Electrodynamics['errPassiveDissipationRateInt'] 
                
        talt=scipy.stats.stats.nanmedian(self.Electrodynamics['Altitude'],axis=0)
        timeout = self.Time['UnixTime']
        MLTtime1 = self.Time['MagneticLocalTime']

        # current
        vecPlot = plotVvels.vvelsPlot()
        vecPlot.makePlot(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,J[:,:,1]*1e6,J[:,:,0]*1e6,dJ[:,:,1]*1e6,dJ[:,:,0]*1e6,\
            title='Current' + self.titleString,p=[1e6,1e6,1e6,1e6],cax=self.climJ,ncols=2,label='Altitude (km)',\
            doQuiv=1, units=r"$\mu A/m^2$", parm='J')
        if self.saveout:
            vecPlot.figg.savefig(self.outputFname+'-Jperp.png')        

        # wind
        vecPlot = plotVvels.vvelsPlot()
        vecPlot.makePlot(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,U[:,:,1],U[:,:,0],dU[:,:,1],dU[:,:,0],\
            title='Neutral Wind' + self.titleString,p=[200.0,0.5,500,50],cax=self.climU,ncols=3,label='Altitude (km)',\
            doQuiv=1, units="m/s", parm='U',vz=U[:,:,2],dvz=dU[:,:,2],vzsc=10.0,geo=1)
        if self.saveout:
            vecPlot.figg.savefig(self.outputFname+'-Ugeo.png')     
        vecPlot = plotVvels.vvelsPlot()
        vecPlot.makePlot(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,Ugmag[:,:,1],Ugmag[:,:,0],dUgmag[:,:,1],dUgmag[:,:,0],\
            title='Neutral Wind' + self.titleString,p=[200.0,0.5,500,50],cax=self.climU,ncols=3,label='Altitude (km)',\
            doQuiv=1, units="m/s", parm='U',vz=Ugmag[:,:,2],dvz=dUgmag[:,:,2],vzsc=10.0,geo=0)
        if self.saveout:
            vecPlot.figg.savefig(self.outputFname+'-Ugmag.png')
                   
        # conductivity
        condPlot = plotVvels.condPlot()   
        condPlot.makePlot(timeout,talt/1e3,Sp,dSp,Sh,dSh,IntSp,dIntSp,IntSh,dIntSh,cax=self.climSig,sc=1e3,caxInt=self.climSigInt)
        if self.saveout:
            condPlot.figg.savefig(self.outputFname+'-Conductivity.png') 
                    
        # energy transfer rates
        tratePlot = plotVvels.tratePlot()   
        tratePlot.makePlot(timeout,talt/1e3,JdotE,dJdotE,JdotEp,dJdotEp,SpE2,dSpE2,IntJdotE,dIntJdotE,IntJdotEp,dIntJdotEp,IntSpE2,dIntSpE2,\
            caxInt=self.climEmInt)
        if self.saveout:
            tratePlot.figg.savefig(self.outputFname+'-EnergyTransferRates.png')         
                    
        return
                          
    def docwinds(self):

        # read data
        dat1 = self.h5fileVi.readWholeh5file()
        dat2 = self.h5fileVe.readWholeh5file()
        self.titleString=dat1['/ProcessingParams']['TitleString']

        # Vi params
        Vi = dat1['/VectorVels']['Vest']; dVi = dat1['/VectorVels']['errVest']
        AltVi = dat1['/VectorVels']['Altitude']
        Ne = dat1['/IonoParams']['Ne']
        dNe = dat1['/IonoParams']['errNe']
        timeVi = dat1['/Time']['UnixTime']
        Sh = dat1['/IonoParams']['Sh']
        Sp = dat1['/IonoParams']['Sp']
        dSh = dat1['/IonoParams']['errSh']
        dSp = dat1['/IonoParams']['errSp']
        Nu = dat1['/IonoParams']['Nuin']
        Wci = dat1['/IonoParams']['Wci']
        kappa = Wci/Nu
        Ball = dat1['/IonoParams']['Ball']
        Dip = dat1['/IonoParams']['Dip']
        Dec = dat1['/IonoParams']['Dec']
        
        # Ve params
        timeVe = dat2['/Time']['UnixTime']; mtimeVe = scipy.mean(timeVe,axis=1)
        Eavg = dat2['/VectorVels']['Eavg']; dEavg = dat2['/VectorVels']['errEavg']
        Bavg = scipy.mean(dat2['/IonoParams']['Bavg'])
        Ve = scipy.zeros(Eavg.shape); dVe = scipy.zeros(Eavg.shape)
        Ve[:,:,0] = Eavg[:,:,1]/Bavg; dVe[:,:,0] = dEavg[:,:,1]/Bavg;
        Ve[:,:,1] = -Eavg[:,:,0]/Bavg; dVe[:,:,1] = -dEavg[:,:,0]/Bavg;
              
        Jperp=scipy.zeros(Vi.shape)*scipy.nan; dJperp=scipy.zeros(Vi.shape)*scipy.nan; Jperp=Jperp[:,:,:-1]; dJperp=dJperp[:,:,:-1]
        Ugmag=scipy.zeros(Vi.shape)*scipy.nan; dUgmag=scipy.zeros(Vi.shape)*scipy.nan;
        Ugeo=scipy.zeros(Vi.shape)*scipy.nan; dUgeo=scipy.zeros(Vi.shape)*scipy.nan;
        Nmax=0
        for irec in range(timeVi.shape[0]):
            
            # get Vi
            Ialt = scipy.where((AltVi[irec,:]<=self.maxAlt*1e3))[0]
            if len(Ialt)>Nmax:
                Nmax=len(Ialt)
            tvi = Vi[irec,Ialt,:]; tdvi = dVi[irec,Ialt,:]
            
            # get other params
            tne = scipy.repeat(Ne[irec,Ialt][:,scipy.newaxis],2,axis=1); tdne = scipy.repeat(dNe[irec,Ialt][:,scipy.newaxis],2,axis=1)
            tkap = kappa[irec,Ialt]; tb = Ball[irec,Ialt]
            tdip = Dip[irec,Ialt]; tdec = Dec[irec,Ialt]
            
            # get Ve, E
            Itm=scipy.where((mtimeVe>=timeVi[irec,0]) & (mtimeVe<=timeVi[irec,1]))[0]
            if len(Itm)==0:
                xxx
            elif len(Itm)==1:
                tve = Ve[Itm]; tdve = dVe[Itm]
                te = Eavg[Itm]; tde = dEavg[Itm]
            else:
                tve = scipy.nansum(Ve[Itm]/scipy.power(dVe[Itm],2.0),axis=0)/scipy.nansum(1.0/scipy.power(dVe[Itm],2.0),axis=0)
                tdve = scipy.sqrt(1.0/scipy.nansum(1.0/scipy.power(dVe[Itm],2.0),axis=0))
                te = scipy.nansum(Eavg[Itm]/scipy.power(dEavg[Itm],2.0),axis=0)/scipy.nansum(1.0/scipy.power(dEavg[Itm],2.0),axis=0)
                tde = scipy.sqrt(1.0/scipy.nansum(1.0/scipy.power(dEavg[Itm],2.0),axis=0))
            tve = tve.real; tdve = tdve.real
            te = te.real; tde = tde.real
            tve = scipy.repeat(tve,len(Ialt),axis=0); tdve = scipy.repeat(tdve,len(Ialt),axis=0); 
            te = scipy.repeat(te,len(Ialt),axis=0); tde = scipy.repeat(tde,len(Ialt),axis=0); 

            # current
            Jperp[irec,:len(Ialt),:] = v_elemcharge * tne * (tvi[:,:-1] - tve)
            dJperp[irec,:len(Ialt),:] = v_elemcharge*scipy.sqrt( tdne**2.0*(tvi[:,:-1]-tve)**2.0 + tdve**2.0*tne**2.0 + tdvi[:,:-1]**2.0*tne**2.0 )
            
            # wind
            Ugmag[irec,:len(Ialt),0] = tvi[:,0] - tkap*(tvi[:,1] + te[:,0]/tb)
            Ugmag[irec,:len(Ialt),1] = tvi[:,1] - tkap*(-tvi[:,0] + te[:,1]/tb)
            Ugmag[irec,:len(Ialt),2] = tvi[:,2]
            dUgmag[irec,:len(Ialt),0] = scipy.sqrt(tdvi[:,0]**2.0 + tdvi[:,1]**2.0*tkap**2.0 + tde[:,0]**2.0*(tkap/tb)**2.0)
            dUgmag[irec,:len(Ialt),1] = scipy.sqrt(tdvi[:,1]**2.0 + tdvi[:,0]**2.0*tkap**2.0 + tde[:,1]**2.0*(tkap/tb)**2.0)
            dUgmag[irec,:len(Ialt),2] = scipy.sqrt(tdvi[:,2]**2.0)

            for ialt in range(len(Ialt)):
                Ugeo[irec,ialt,:] = scipy.squeeze(util_fcns.gmag2geo(scipy.array([[Ugmag[irec,ialt,1]],[Ugmag[irec,ialt,0]],[Ugmag[irec,ialt,2]]]),tdec[ialt]*pi/180.0,tdip[ialt]*pi/180.0))

        AltVi = AltVi[:,:Nmax]
        Jperp = Jperp[:,:Nmax,:]; dJperp = dJperp[:,:Nmax,:]
        Ugmag = Ugmag[:,:Nmax,:]; dUgmag = dUgmag[:,:Nmax,:]
        Ugeo = Ugeo[:,:Nmax,:]; dUgeo = dUgeo[:,:Nmax,:]
        Sh = Sh[:,:Nmax]; Sp=Sp[:,:Nmax]
        dSh = dSh[:,:Nmax]; dSp=dSp[:,:Nmax]

        # conductances
        talt = scipy.stats.nanmean(AltVi,axis=0)
        tSh = Sh.copy(); tdSh = dSh.copy(); tSp = Sp.copy(); tdSp = dSp.copy()
        tSh[scipy.where(dSh/Sh>1.0)]=scipy.nan; #tdSh[scipy.where(dSh/Sh>1.0)]=scipy.nan
        IntSh = scipy.trapz(scipy.nan_to_num(tSh),x=AltVi,axis=1);
        dIntSh = scipy.sqrt(scipy.trapz(scipy.nan_to_num(tdSh*tdSh),x=None,dx=scipy.asarray(scipy.diff(AltVi,axis=1)**2.0),axis=1))
        tSp[scipy.where(dSp/Sp>1.0)]=scipy.nan; #tdSp[scipy.where(dSp/Sp>1.0)]=scipy.nan
        IntSp = scipy.trapz(scipy.nan_to_num(tSp),x=AltVi,axis=1);
        dIntSp = scipy.sqrt(scipy.trapz(scipy.nan_to_num(tdSp*tdSp),x=None,dx=scipy.asarray(scipy.diff(AltVi,axis=1)**2.0),axis=1))

        # e-m transfer rate
        JdotE = Jperp[:,:,0]*Eavg[:,:,0] + Jperp[:,:,1]*Eavg[:,:,1]
        dJdotE = scipy.sqrt( dJperp[:,:,0]**2.0*Eavg[:,:,0]**2.0 + Jperp[:,:,0]**2.0*dEavg[:,:,0]**2.0 \
         + dJperp[:,:,1]**2.0*Eavg[:,:,1]**2.0 + Jperp[:,:,1]**2.0*dEavg[:,:,1]**2.0)
        IntJdotE = scipy.trapz(scipy.nan_to_num(JdotE),x=AltVi,axis=1)
        dIntJdotE = scipy.sqrt(scipy.trapz(scipy.nan_to_num(dJdotE*dJdotE),x=None,dx=scipy.asarray(scipy.diff(AltVi,axis=1)**2.0),axis=1))

        # joule heating rate
        JdotEp = scipy.sum(Jperp**2.0,axis=2)/(Sp + Sh**2.0/Sp)        
        dJdotEp = scipy.sqrt(scipy.sum((2.0*Jperp*dJperp)**2.0,axis=2)/(Sp + Sh**2.0/Sp)**2.0 \
            + JdotEp**2.0/(Sp + Sh**2.0/Sp)**2.0*(dSp**2.0*(1.0-Sh**2.0/Sp**2.0)**2.0 + dSh**2.0*(2.0*Sh/Sp)**2.0))      
        Ibad = scipy.where(scipy.absolute(dJdotEp/JdotEp)>0.5); JdotEp[Ibad]=scipy.nan
        IntJdotEp = scipy.trapz(scipy.nan_to_num(JdotEp),x=AltVi,axis=1)
        dIntJdotEp = scipy.sqrt(scipy.trapz(scipy.nan_to_num(dJdotEp*dJdotEp),x=None,dx=scipy.asarray(scipy.diff(AltVi,axis=1)**2.0),axis=1))
        
        # passive energy diss rate
        SpE2 = Sp*scipy.sum(Eavg*Eavg,axis=2)
        dSpE2 = scipy.sqrt(4.0*Sp**2.0*scipy.sum((dEavg*Eavg)**2.0,axis=2) + dSp**2.0*scipy.sum(Eavg**2.0,axis=2)**2.0)
        IntSpE2 = scipy.trapz(scipy.nan_to_num(SpE2),x=AltVi,axis=1)
        dIntSpE2 = scipy.sqrt(scipy.trapz(scipy.nan_to_num(dSpE2*dSpE2),x=None,dx=scipy.asarray(scipy.diff(AltVi,axis=1)**2.0),axis=1))
                        
        ### Set up output dicts
        self.setOutDicts(dat1)
        # Electrodynamics        
        self.Electrodynamics['Altitude']=AltVi;        
        self.Electrodynamics['Jperp']=Jperp; self.Electrodynamics['errJperp']=dJperp
        self.Electrodynamics['Ugmag']=Ugmag; self.Electrodynamics['errUgmag']=dUgmag
        self.Electrodynamics['Ugeo']=Ugeo; self.Electrodynamics['errUgeo']=dUgmag
        self.Electrodynamics['HallConductivity']=Sh; self.Electrodynamics['PedersenConductivity']=Sp
        self.Electrodynamics['errHallConductivity']=dSh; self.Electrodynamics['errPedersenConductivity']=dSp
        self.Electrodynamics['HallConductance']=IntSh; self.Electrodynamics['PedersenConductance']=IntSp
        self.Electrodynamics['errHallConductance']=dIntSh; self.Electrodynamics['errPedersenConductance']=dIntSp
        self.Electrodynamics['CollisionFrequency']=Nu; self.Electrodynamics['Kappa']=kappa;
        self.Electrodynamics['ElectromagneticEnergyTransferRate']=JdotE; self.Electrodynamics['errElectromagneticEnergyTransferRate']=dJdotE; 
        self.Electrodynamics['ElectromagneticEnergyTransferRateInt']=IntJdotE; self.Electrodynamics['errElectromagneticEnergyTransferRateInt']=dIntJdotE;
        self.Electrodynamics['JouleHeatingRate']=JdotEp; self.Electrodynamics['errJouleHeatingRate']=dJdotEp; 
        self.Electrodynamics['JouleHeatingRateInt']=IntJdotEp; self.Electrodynamics['errJouleHeatingRateInt']=dIntJdotEp;
        self.Electrodynamics['PassiveDissipationRate']=SpE2; self.Electrodynamics['errPassiveDissipationRate']=dSpE2; 
        self.Electrodynamics['PassiveDissipationRateInt']=IntSpE2; self.Electrodynamics['errPassiveDissipationRateInt']=dIntSpE2;
        
        ### Output file        
        if self.saveout:
            self.createOutputFile()        
        
        ### Plot output
        self.createPlots()
                
        return
        
        ### Set up output dicts
        self.setOutDicts(dat1)
        # Time
        self.Time['UnixTime'] = timeout
        self.Time['dtime'] = dtimeout
        self.Time['MagneticLocalTime'] = MLTtime1
        # Vector Vels
        self.VectorVels['Altitude'] = Alt1;
        self.VectorVels['Nmeas'] = Nall1.astype('int32')
        self.VectorVels['Eavg'] = Eavg; self.VectorVels['errEavg'] = dEavg
        self.VectorVels['Vest'] = vvels1; self.VectorVels['errVest'] = dvvels1
        self.VectorVels['Vmag'] = Vmag; self.VectorVels['errVmag'] = dVmag
        self.VectorVels['Vdir'] = Vmag; self.VectorVels['errVdir'] = Vdir

        ### Output file
        if self.saveout:
            self.createOutputFile()
            
        ### Plot output
            self.createPlots()
        
        return
        