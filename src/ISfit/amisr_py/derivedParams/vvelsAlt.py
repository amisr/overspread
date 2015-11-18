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

class vvelsAltFile(ioclass.outputFileClass):

    def __init__(self):
        self.title = 'Vector Velocities Resolved in Altitude'
        
        self.h5Paths = {
            'Params'    :   ('/ProcessingParams','Experiment Parameters'),\
            'Site'      :   ('/Site','Site Parameters'),\
            'Time'      :   ('/Time','Time Information'),\
            'VectorVels' : ('/VectorVels','Vector Velocities'),\
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
            '/VectorVels/Altitude' : [('TITLE','Altitude'),('Description','Altitude of measurement'),('Unit','Meters')],\
            '/VectorVels/Eavg' : [('TITLE','Electric Field'),('Description','Averaged Electric Field'),('Unit','V/m')],\
            '/VectorVels/Bavg' : [('TITLE','Magnetic Field'),('Description','Averaged Magnetic Field Used for E-field Calculation'),('Unit','T')],\
            '/VectorVels/Nmeas' : [('TITLE','Number of Measurements'),('Description','Number of Measurements in Estimate')],\
            '/VectorVels/Vdir' : [('TITLE','Ion Velocity Direction'),('Description','Ion Velocity Direction'),('Unit','Degrees North of East')],\
            '/VectorVels/Vest' : [('TITLE','Ion Velocity Vector'),('Description','Ion Velocity Vector'),('Size','Nrecords x Naltitudes x 3 (Perp-North, Perp-East, Anti-Parallel)'),('Unit','m/s')],\
            '/VectorVels/Vmag' : [('TITLE','Ion Velocity Magnitude'),('Description','Ion Velocity Magnitude'),('Unit','m/s')],\
            '/VectorVels/errVdir' : [('TITLE','Error on Ion Velocity Direction'),('Description','Error on Ion Velocity Direction'),('Unit','Degrees')],\
            '/VectorVels/errVest' : [('TITLE','Error on Ion Velocity Vector'),('Description','Error on Ion Velocity Vector'),('Size','Nrecords x Naltitudes x 3 (Perp-North, Perp-East, Anti-Parallel)'),('Unit','m/s')],\
            '/VectorVels/errVmag' : [('TITLE','Error on Ion Velocity Magnitude'),('Description','Error on Ion Velocity Magnitude'),('Unit','m/s')],\
            '/VectorVels/errEavg' : [('TITLE','Error on Electric Field'),('Description','Error on Averaged Electric Field'),('Unit','V/m')],\
            '/ProcessingParams/ProcessingTimeStamp' : [('TITLE','Processing Time Stamp')],\
            '/ProcessingParams/BaudLength' : [('TITLE','Baud Length'),('Unit','Seconds')],\
            '/ProcessingParams/PulseLength' : [('TITLE','Pulse Length'),('Unit','Seconds')],\
            '/ProcessingParams/RxFrequency' : [('TITLE','Rx Frequency'),('Description','Receive frequency'),('Unit','Hertz')],\
            '/ProcessingParams/TxFrequency' : [('TITLE','Tx Frequency'),('Description','Transmit frequency'),('Unit','Hertz')],\
            '/ProcessingParams/MinAlt': [('TITLE','Min Altitude'),('Description','Minimum Altitude for E-field Calculation'),('Unit','Meters')],\
            '/ProcessingParams/MaxAlt': [('TITLE','Max Altitude'),('Description','Maximum Altitude for E-field Calculation'),('Unit','Meters')],\
            '/ProcessingParams/Covar': [('TITLE','Covariance Matrix'),('Description','A-priori Covariance Matrix (perp-North, perp-East, anti-parallel)'),('Unit','(m/s)^2')],\
            '/ProcessingParams/ErrorElim': [('TITLE','Error Elimination'),('Description','Error Elimination Criteria')],\
            '/ProcessingParams/SourceFile': [('TITLE','Source File'),('Description','Source File of Measurements')],\
            '/IonoParams/Altitude': [('TITLE','Altitude'),('Description','Altitude'),('Unit','Meters')],\
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
        
class vvelsAlt:
    
    def __init__(self,inifiles,sec):
    
        self.DefaultIni = '/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/config_vvelsAlt-default.ini'

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
        self.minAlt=eval(io_utils.ini_tool(config,sec,'minalt',required=0,defaultParm=config.get('DefaultParameters','minalt')))
        self.maxAlt=eval(io_utils.ini_tool(config,sec,'maxalt',required=0,defaultParm=config.get('DefaultParameters','maxalt')))
        self.ppp=eval(io_utils.ini_tool(config,sec,'ppp',required=0,defaultParm=config.get('DefaultParameters','ppp')))
        self.covar=eval(io_utils.ini_tool(config,sec,'covar',required=0,defaultParm=config.get('DefaultParameters','covar')))
        self.zoomWhole=eval(io_utils.ini_tool(config,sec,'zoomWhole',required=0,defaultParm=config.get('DefaultParameters','zoomWhole')))
        self.chirp=eval(io_utils.ini_tool(config,sec,'chirp',required=0,defaultParm=config.get('DefaultParameters','chirp')))
        self.neMin=eval(io_utils.ini_tool(config,sec,'neMin',required=0,defaultParm=config.get('DefaultParameters','neMin')))
          
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
        
        self.VectorVels = {
            'Altitude' : None,\
            'Eavg' : None,\
            'Nmeas' : None,\
            'Vdir' : None,\
            'Vest' : None,\
            'Vmag' : None,\
            'errVmag' : None,\
            'errVest' : None,\
            'errVdir' : None,\
            'errEavg' : None
        }

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
            'MinAlt'    :   self.minAlt*1e3, \
            'MaxAlt'    :   self.maxAlt*1e3, \
            'Covar'     :   self.covar, \
            'ErrorElim' :   self.ppp, \
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
        ofile = vvelsAltFile()
        ofile.createFile(self.outputFname+'.h5')

        # create groups
        ofile.createh5groups()
        
        # create arrays
        ofile.createStaticArray(ofile.h5Paths['Params'][0],self.Params,keys2do=self.Params.keys())
        ofile.createStaticArray(ofile.h5Paths['Time'][0],self.Time,keys2do=self.Time.keys())
        ofile.createStaticArray(ofile.h5Paths['VectorVels'][0],self.VectorVels,keys2do=self.VectorVels.keys())
        ofile.createStaticArray(ofile.h5Paths['Site'][0],self.Site,keys2do=self.Site.keys())
        ofile.createStaticArray(ofile.h5Paths['IonoParams'][0],self.IonoParams,keys2do=self.IonoParams.keys())
        
        # set attributes in output file
        ofile.setAtrributes()
        
        # close file
        ofile.closeFile()
        
        return
           
    def createPlots(self):
    
        vvels1 = self.VectorVels['Vest']; dvvels1 = self.VectorVels['errVest']
        Vmag = self.VectorVels['Vmag']; dVmag = self.VectorVels['errVmag']
        Vdir = self.VectorVels['Vdir']; dVdir = self.VectorVels['errVdir']
        Eavg = self.VectorVels['Eavg']; dEavg = self.VectorVels['errEavg']
        
        talt=scipy.stats.stats.nanmedian(self.VectorVels['Altitude'],axis=0)
        timeout = self.Time['UnixTime']
        MLTtime1 = self.Time['MagneticLocalTime']

        # velocity vector
        vecPlot = plotVvels.vvelsPlot()
        vecPlot.makePlot(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],\
            title='Vector Velocities' + self.titleString,p=self.ppp,sc=self.sc,cax=self.clim,ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2],vzsc=10.0,label='Altitude (km)')
        if self.saveout:
            vecPlot.figg.savefig(self.outputFname+'-vvec.png')        
        
        # velocity magnitude
        magPlot = plotVvels.vvelsMagPlot()
        magPlot.makePlot(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,Vmag,Vdir,dVmag,dVdir,
                title='Vector Velocities' + self.titleString,cax1=[0.0,self.clim[1]],cax2=[-180.0,180.0],label='Altitude (km)')
        if self.saveout:
            magPlot.figg.savefig(self.outputFname+'-vmag.png')

        # electric field
        efieldPlot = plotVvels.efieldPlot()
        efieldPlot.makePlot(timeout,Eavg[:,:,1],Eavg[:,:,0],dEavg[:,:,1],dEavg[:,:,0],\
            title='Electric Fields' + self.titleString)
        if self.saveout:
            efieldPlot.figg.savefig(self.outputFname+'-efield.png')

        return
                          
    def dovelsAlt(self):

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
        ht=scipy.squeeze(dat1['/FittedParams']['Altitude'])
        Range=scipy.squeeze(dat1['/FittedParams']['Range'])
        vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+self.chirp
        dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
        ne1=dat1['/FittedParams']['Ne']
        dne1=dat1['/FittedParams']['dNe']
        nu1=dat1['/FittedParams']['Fits'][:,:,:,:,2]
        frac1=dat1['/FittedParams']['Fits'][:,:,:,:,0]
        mass=dat1['/FittedParams']['IonMass']
        
        # low densities
        I=scipy.where((ne1<self.neMin))
        ne1[I]=scipy.nan
        vlos1[I]=scipy.nan; dvlos1[I]=scipy.nan
        
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
        
        # time
        time1=dat1['/Time']['UnixTime']
        doy1=dat1['/Time']['doy']
        dtime1=dat1['/Time']['dtime']+(doy1-doy1[0,0])*24.0
        MLT=dat1['/Time']['MagneticLocalTimeSite']
        yr=dat1['/Time']['Year']
        mon=dat1['/Time']['Month']
        day=dat1['/Time']['Day']
    
        # just do a portion of data
        if len(self.zoomWhole)!=0:
            I=scipy.where((dtime1[:,0]>=self.zoomWhole[0]) & (dtime1[:,1]<=self.zoomWhole[1]))[0]	
            vlos1=vlos1[I]
            dvlos1=dvlos1[I]
            time1=time1[I]
            doy1=doy1[I]		
            dtime1=dtime1[I]		
            MLT=MLT[I]
            yr=yr[I]
            mon=mon[I]
            day=day[I]
    
        # title str
        yr=yr[0,0]; mon=mon[0,0]; day=day[0,0]
        self.titleString=' %d-%d-%d' % (mon, day, yr)
    
        (Nrecs,Nrngs)=ht.shape
    
        timeout=[]
        dtimeout=[]
        MLTtime1=[]
        
        done=0 # flag to say when we are done
        Irec=0 # record counter
        IIrec=0 # record counter
        while not done:
            
            # get scan 1 records
            I=scipy.where((Event[Irec:] != Event[Irec]))[0]
            if len(I)>0:
                IrecsScan=range(Irec,Irec+I[0])
            Irecs=IrecsScan        

            # get scan 2 records
            Irec=Irecs[-1]+1
            I=scipy.where((Event[Irec:] != Event[Irec]))[0]
            if len(I)>0:
                IrecsScan=range(Irec,Irec+I[0])
            else:
                IrecsScan=range(Irec,time1.shape[0])
                done=1
            Irecs.extend(IrecsScan)	
        
            # get scan 3 records
            Irect=Irecs[-1]+1
            I=scipy.where((Event[Irect:] != Event[Irect]))[0]
            if len(I)>0:
                IrecsScan=range(Irect,Irect+I[0])
            else:
                IrecsScan=range(Irect,time1.shape[0])
                done=1			
            Irecs.extend(IrecsScan)	
            
            els = AvgElevation[Irecs]; Iel = scipy.argmin(els)
            
            # resolve
            Vest=scipy.zeros((Nrngs,3))*scipy.nan; dVest=scipy.zeros((Nrngs,3))*scipy.nan
            Nmeas=scipy.zeros((Nrngs))*scipy.nan; Alts=scipy.zeros((Nrngs))*scipy.nan
            Bavg=scipy.zeros((Nrngs))*scipy.nan
            ne = scipy.zeros((Nrngs))*scipy.nan; dne = scipy.zeros((Nrngs))*scipy.nan
            sp = scipy.zeros((Nrngs))*scipy.nan; dsp = scipy.zeros((Nrngs))*scipy.nan;
            sh = scipy.zeros((Nrngs))*scipy.nan; dsh = scipy.zeros((Nrngs))*scipy.nan; 
            nu = scipy.zeros((Nrngs))*scipy.nan; wci = scipy.zeros((Nrngs))*scipy.nan;
            dec = scipy.zeros((Nrngs))*scipy.nan; dip = scipy.zeros((Nrngs))*scipy.nan;
            for irng in range(Nrngs):
                tht = ht[Irecs[Iel],irng]
                if scipy.isfinite(tht):
                    htin=[]; vlosin=[]; dvlosin=[]; kin=[]; bin=[]; nein=[]; dnein=[]; 
                    spin=[]; shin=[]; dspin=[]; dshin=[]
                    nuin=[]; wciin=[]; decin=[]; dipin=[]
                    for ism in range(len(Irecs)):
                        Ihts = scipy.where(scipy.isfinite(ht[Irecs[ism],:]))[0]
                        Iht=scipy.argmin(scipy.absolute(ht[Irecs[ism],:][Ihts]-tht))
                        Iht=Ihts[Iht]
                        htin.append(ht[Irecs[ism],Iht])
                        vlosin.append(vlos1[Irecs[ism],0,Iht])
                        dvlosin.append(dvlos1[Irecs[ism],0,Iht])					
                        kin.append([kpn[Irecs[ism],0,Iht],kpe[Irecs[ism],0,Iht],kpar[Irecs[ism],0,Iht]])
                        bin.append(Babs[Irecs[ism],0,Iht])
                        nein.append(ne1[Irecs[ism],0,Iht]); dnein.append(dne1[Irecs[ism],0,Iht])
                        spin.append(sp1[Irecs[ism],0,Iht]); dspin.append(dsp1[Irecs[ism],0,Iht]);
                        shin.append(sh1[Irecs[ism],0,Iht]); dshin.append(dsh1[Irecs[ism],0,Iht]);
                        nuin.append(nuin1[Irecs[ism],0,Iht]); wciin.append(wci1[Irecs[ism],0,Iht]);
                        decin.append(dec1[Irecs[ism],0,Iht]); dipin.append(dip1[Irecs[ism],0,Iht]);
                    htin=scipy.array(htin); vlosin=scipy.array(vlosin); dvlosin=scipy.array(dvlosin); kin=scipy.array(kin); bin=scipy.array(bin)
                    (tAlt_out,tVest,tdVest,xx,tNmeas)=vvels.compute_velvec2([[0.0*1000,2000.0*1000]],vlosin,dvlosin,kin,htin,[],htin,\
                        htmin=0.0*1000,htmax=2000.0*1000,covar=self.covar,p=self.ppp)		
                    Vest[irng,:]=tVest; dVest[irng,:]=tdVest
                    Nmeas[irng]=tNmeas; Alts[irng]=scipy.mean(htin)
                    ne[irng],s = scipy.average(scipy.array(nein),weights=1.0/scipy.array(dnein)**2.0,returned=True); dne[irng] = scipy.sqrt(1.0/s)
                    sp[irng],s = scipy.average(scipy.array(spin),weights=1.0/scipy.array(dspin)**2.0,returned=True); dsp[irng] = scipy.sqrt(1.0/s)
                    sh[irng],s = scipy.average(scipy.array(shin),weights=1.0/scipy.array(dshin)**2.0,returned=True); dsh[irng] = scipy.sqrt(1.0/s)
                    nu[irng] = scipy.mean(nuin); wci[irng] = scipy.mean(wciin)
                    dec[irng] = scipy.mean(decin); dip[irng] = scipy.mean(dipin); Bavg[irng]=scipy.mean(bin)
                                        
            # save time
            timeout.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
            dtimeout.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])
            MLTtime1.append([MLT[Irecs[0],0],MLT[Irecs[-1],1]])
            if IIrec>0:
                if MLTtime1[IIrec][0]<MLTtime1[IIrec-1][0]:
                    MLTtime1[IIrec][0]=MLTtime1[IIrec][0]+24.0
                    MLTtime1[IIrec][1]=MLTtime1[IIrec][1]+24.0

            # store data
            if IIrec==0:
                vvels1=Vest[scipy.newaxis,:,:]
                dvvels1=dVest[scipy.newaxis,:,:]
                Nall1=Nmeas[scipy.newaxis,:]
                Alt1=Alts[scipy.newaxis,:]
                Ball1=Bavg[scipy.newaxis,:]
                NeAll1=ne[scipy.newaxis,:]; dNeAll1=dne[scipy.newaxis,:]
                SpAll1=sp[scipy.newaxis,:]; dSpAll1=dsp[scipy.newaxis,:]                
                ShAll1=sh[scipy.newaxis,:]; dShAll1=dsh[scipy.newaxis,:]                
                NuAll1=nu[scipy.newaxis,:]; WciAll1=wci[scipy.newaxis,:]  
                DecAll1=dec[scipy.newaxis,:]; DipAll1=dec[scipy.newaxis,:]                      
            else:
                vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
                dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)
                Nall1=scipy.concatenate((Nall1,Nmeas[scipy.newaxis,:]),axis=0)
                Alt1=scipy.concatenate((Alt1,Alts[scipy.newaxis,:]),axis=0)
                Ball1=scipy.concatenate((Ball1,Bavg[scipy.newaxis,:]),axis=0)
                NeAll1=scipy.concatenate((NeAll1,ne[scipy.newaxis,:]),axis=0); dNeAll1=scipy.concatenate((dNeAll1,dne[scipy.newaxis,:]),axis=0)
                SpAll1=scipy.concatenate((SpAll1,sp[scipy.newaxis,:]),axis=0); dSpAll1=scipy.concatenate((dSpAll1,dsp[scipy.newaxis,:]),axis=0)
                ShAll1=scipy.concatenate((ShAll1,sh[scipy.newaxis,:]),axis=0); dShAll1=scipy.concatenate((dShAll1,dsh[scipy.newaxis,:]),axis=0)
                NuAll1=scipy.concatenate((NuAll1,nu[scipy.newaxis,:]),axis=0); WciAll1=scipy.concatenate((WciAll1,wci[scipy.newaxis,:]),axis=0);
                DecAll1=scipy.concatenate((DecAll1,dec[scipy.newaxis,:]),axis=0); DipAll1=scipy.concatenate((DipAll1,dip[scipy.newaxis,:]),axis=0);
            if scipy.mod(IIrec,100)==0 or done==1:
                print 'Record %d of %d' % (IIrec,Nrecs)
            IIrec=IIrec+1		

        # Electric field
        Eavg=scipy.zeros((Ball1.shape[0],1,2))*scipy.nan
        dEavg=scipy.zeros((Ball1.shape[0],1,2))*scipy.nan
        Bavg=scipy.zeros((Ball1.shape[0]))*scipy.nan        
        for itime in range(Eavg.shape[0]):
            Ialt=scipy.where((Alt1[itime,:]>=self.minAlt*1000.0) & (Alt1[itime,:]<=self.maxAlt*1000.0))[0]
            Bavg[itime] = scipy.stats.nanmean(Ball1[itime,Ialt])            
            Eavg[itime,0,0]=-1.0*scipy.nansum(Ball1[itime,Ialt]*vvels1[itime,Ialt,1]/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,1],2.0))/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,1],2.0))
            Eavg[itime,0,1]=1.0*scipy.nansum(Ball1[itime,Ialt]*vvels1[itime,Ialt,0]/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,0],2.0))/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,0],2.0))
            dEavg[itime,0,0]=scipy.sqrt(1.0/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,1],2.0)))
            dEavg[itime,0,1]=scipy.sqrt(1.0/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,0],2.0)))

        MLTtime1=scipy.array(MLTtime1)
        timeout=scipy.array(timeout)
        dtimeout=scipy.array(dtimeout)
        
        Vmag = scipy.sqrt( scipy.power(vvels1[:,:,0],2.0) + scipy.power(vvels1[:,:,1],2.0) ).real
        dVmag = scipy.sqrt( scipy.power(dvvels1[:,:,0],2.0)*scipy.power(vvels1[:,:,0]/Vmag,2.0) + scipy.power(dvvels1[:,:,1],2.0)*scipy.power(vvels1[:,:,1]/Vmag,2.0) ).real
        Vdir = 180.0/pi*scipy.arctan2(vvels1[:,:,1],vvels1[:,:,0]).real
        dVdir=180.0/pi*((1.0/scipy.absolute(vvels1[:,:,0]))*(1.0/(1.0+scipy.power(vvels1[:,:,1]/vvels1[:,:,0],2.0)))*scipy.sqrt(scipy.power(dvvels1[:,:,1],2.0)+scipy.power(vvels1[:,:,1]/vvels1[:,:,0]*dvvels1[:,:,0],2.0))).real
        
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
        # Additional
        self.IonoParams['Altitude'] = Alt1;
        self.IonoParams['Bavg']=Bavg
        self.IonoParams['Ne']=NeAll1; self.IonoParams['errNe']=dNeAll1
        self.IonoParams['Ball']=Ball1
        self.IonoParams['Sp']=SpAll1; self.IonoParams['errSp']=dSpAll1
        self.IonoParams['Sh']=ShAll1; self.IonoParams['errSh']=dShAll1
        self.IonoParams['Nuin']=NuAll1
        self.IonoParams['Wci']=WciAll1
        self.IonoParams['Dec']=DecAll1
        self.IonoParams['Dip']=DipAll1

        ### Output file
        if self.saveout:
            self.createOutputFile()
            
        ### Plot output
            self.createPlots()
        
        return
        
    def dovelsAltORIG(self):

        # read data
        dat1 = self.h5file.readWholeh5file()

        # antenna
        AvgAzimuth=dat1['/Antenna']['AvgAzimuth']
        AvgElevation=dat1['/Antenna']['AvgElevation']
        Azimuth=dat1['/Antenna']['Azimuth']
        Elevation=dat1['/Antenna']['Elevation']
        Event=dat1['/Antenna']['Event']
        Mode=dat1['/Antenna']['Mode']

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
        ht=scipy.squeeze(dat1['/FittedParams']['Altitude'])
        Range=scipy.squeeze(dat1['/FittedParams']['Range'])
        vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+self.chirp
        dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
        ne1=dat1['/FittedParams']['Ne']
        dne1=dat1['/FittedParams']['dNe']
        nu1=dat1['/FittedParams']['Fits'][:,:,:,:,2]
        frac1=dat1['/FittedParams']['Fits'][:,:,:,:,0]
        mass=dat1['/FittedParams']['IonMass']
        
        # low densities
        I=scipy.where((ne1<self.neMin))
        ne1[I]=scipy.nan
        vlos1[I]=scipy.nan; dvlos1[I]=scipy.nan
        
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
                                    
        # time
        time1=dat1['/Time']['UnixTime']
        doy1=dat1['/Time']['doy']
        dtime1=dat1['/Time']['dtime']+(doy1-doy1[0,0])*24.0
        MLT=dat1['/Time']['MagneticLocalTimeSite']
        yr=dat1['/Time']['Year']
        mon=dat1['/Time']['Month']
        day=dat1['/Time']['Day']
    
        # just do a portion of data
        if len(self.zoomWhole)!=0:
            I=scipy.where((dtime1[:,0]>=self.zoomWhole[0]) & (dtime1[:,1]<=self.zoomWhole[1]))[0]	
            vlos1=vlos1[I]
            dvlos1=dvlos1[I]
            time1=time1[I]
            doy1=doy1[I]		
            dtime1=dtime1[I]		
            MLT=MLT[I]
            yr=yr[I]
            mon=mon[I]
            day=day[I]
    
        # title str
        yr=yr[0,0]; mon=mon[0,0]; day=day[0,0]
        self.titleString=' %d-%d-%d' % (mon, day, yr)
    
        (Nrecs,Nrngs)=ht.shape
    
        timeout=[]
        dtimeout=[]
        MLTtime1=[]
        
        done=0 # flag to say when we are done
        Irec=0 # record counter
        IIrec=0 # record counter
        while not done:
            
            # get scan 1 records
            I=scipy.where((Event[Irec:] != Event[Irec]))[0]
            if len(I)>0:
                IrecsScan=range(Irec,Irec+I[0])
            Irecs=IrecsScan        

            # get scan 2 records
            Irec=Irecs[-1]+1
            I=scipy.where((Event[Irec:] != Event[Irec]))[0]
            if len(I)>0:
                IrecsScan=range(Irec,Irec+I[0])
            else:
                IrecsScan=range(Irec,time1.shape[0])
                done=1
            Irecs.extend(IrecsScan)	
        
            # get scan 3 records
            Irect=Irecs[-1]+1
            I=scipy.where((Event[Irect:] != Event[Irect]))[0]
            if len(I)>0:
                IrecsScan=range(Irect,Irect+I[0])
            else:
                IrecsScan=range(Irect,time1.shape[0])
                done=1			
            Irecs.extend(IrecsScan)	
            
            els = AvgElevation[Irecs]; Iel = scipy.argmin(els)
            
            # resolve
            Vest=scipy.zeros((Nrngs,3))*scipy.nan; dVest=scipy.zeros((Nrngs,3))*scipy.nan
            Nmeas=scipy.zeros((Nrngs))*scipy.nan; Alts=scipy.zeros((Nrngs))*scipy.nan
            Bavg=scipy.zeros((Nrngs))*scipy.nan
            ne = scipy.zeros((Nrngs))*scipy.nan; dne = scipy.zeros((Nrngs))*scipy.nan
            sp = scipy.zeros((Nrngs))*scipy.nan; dsp = scipy.zeros((Nrngs))*scipy.nan;
            sh = scipy.zeros((Nrngs))*scipy.nan; dsh = scipy.zeros((Nrngs))*scipy.nan; 
            nu = scipy.zeros((Nrngs))*scipy.nan; wci = scipy.zeros((Nrngs))*scipy.nan;
            dec = scipy.zeros((Nrngs))*scipy.nan; dip = scipy.zeros((Nrngs))*scipy.nan;
            for irng in range(Nrngs):
                tht = ht[Irecs[Iel],irng]
                if scipy.isfinite(tht):
                    htin=[]; vlosin=[]; dvlosin=[]; kin=[]; bin=[]; nein=[]; dnein=[]; 
                    spin=[]; shin=[]; dspin=[]; dshin=[]
                    nuin=[]; wciin=[]; decin=[]; dipin=[]
                    for ism in range(len(Irecs)):
                        Ihts = scipy.where(scipy.isfinite(ht[Irecs[ism],:]))[0]
                        Iht=scipy.argmin(scipy.absolute(ht[Irecs[ism],:][Ihts]-tht))
                        Iht=Ihts[Iht]
                        htin.append(ht[Irecs[ism],Iht])
                        vlosin.append(vlos1[Irecs[ism],0,Iht])
                        dvlosin.append(dvlos1[Irecs[ism],0,Iht])					
                        kin.append([kpn[Irecs[ism],0,Iht],kpe[Irecs[ism],0,Iht],kpar[Irecs[ism],0,Iht]])
                        bin.append(Babs[Irecs[ism],0,Iht])
                        nein.append(ne1[Irecs[ism],0,Iht]); dnein.append(dne1[Irecs[ism],0,Iht])
                        spin.append(sp1[Irecs[ism],0,Iht]); dspin.append(dsp1[Irecs[ism],0,Iht]);
                        shin.append(sh1[Irecs[ism],0,Iht]); dshin.append(dsh1[Irecs[ism],0,Iht]);
                        nuin.append(nuin1[Irecs[ism],0,Iht]); wciin.append(wci1[Irecs[ism],0,Iht]);
                        decin.append(dec1[Irecs[ism],0,Iht]); dipin.append(dip1[Irecs[ism],0,Iht]);
                    htin=scipy.array(htin); vlosin=scipy.array(vlosin); dvlosin=scipy.array(dvlosin); kin=scipy.array(kin); bin=scipy.array(bin)
                    (tAlt_out,tVest,tdVest,xx,tNmeas)=vvels.compute_velvec2([[0.0*1000,2000.0*1000]],vlosin,dvlosin,kin,htin,[],htin,\
                        htmin=0.0*1000,htmax=2000.0*1000,covar=self.covar,p=self.ppp)		
                    Vest[irng,:]=tVest; dVest[irng,:]=tdVest
                    Nmeas[irng]=tNmeas; Alts[irng]=scipy.mean(htin)
                    ne[irng],s = scipy.average(scipy.array(nein),weights=1.0/scipy.array(dnein)**2.0,returned=True); dne[irng] = scipy.sqrt(1.0/s)
                    sp[irng],s = scipy.average(scipy.array(spin),weights=1.0/scipy.array(dspin)**2.0,returned=True); dsp[irng] = scipy.sqrt(1.0/s)
                    sh[irng],s = scipy.average(scipy.array(shin),weights=1.0/scipy.array(dshin)**2.0,returned=True); dsh[irng] = scipy.sqrt(1.0/s)
                    nu[irng] = scipy.mean(nuin); wci[irng] = scipy.mean(wciin)
                    dec[irng] = scipy.mean(decin); dip[irng] = scipy.mean(dipin); Bavg[irng]=scipy.mean(bin)
                                        
            # save time
            timeout.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
            dtimeout.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])
            MLTtime1.append([MLT[Irecs[0],0],MLT[Irecs[-1],1]])
            if IIrec>0:
                if MLTtime1[IIrec][0]<MLTtime1[IIrec-1][0]:
                    MLTtime1[IIrec][0]=MLTtime1[IIrec][0]+24.0
                    MLTtime1[IIrec][1]=MLTtime1[IIrec][1]+24.0

            # store data
            if IIrec==0:
                vvels1=Vest[scipy.newaxis,:,:]
                dvvels1=dVest[scipy.newaxis,:,:]
                Nall1=Nmeas[scipy.newaxis,:]
                Alt1=Alts[scipy.newaxis,:]
                Ball1=Bavg[scipy.newaxis,:]
                NeAll1=ne[scipy.newaxis,:]; dNeAll1=dne[scipy.newaxis,:]
                SpAll1=sp[scipy.newaxis,:]; dSpAll1=dsp[scipy.newaxis,:]                
                ShAll1=sh[scipy.newaxis,:]; dShAll1=dsh[scipy.newaxis,:]                
                NuAll1=nu[scipy.newaxis,:]; WciAll1=wci[scipy.newaxis,:]  
                DecAll1=dec[scipy.newaxis,:]; DipAll1=dec[scipy.newaxis,:]                      
            else:
                vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
                dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)
                Nall1=scipy.concatenate((Nall1,Nmeas[scipy.newaxis,:]),axis=0)
                Alt1=scipy.concatenate((Alt1,Alts[scipy.newaxis,:]),axis=0)
                Ball1=scipy.concatenate((Ball1,Bavg[scipy.newaxis,:]),axis=0)
                NeAll1=scipy.concatenate((NeAll1,ne[scipy.newaxis,:]),axis=0); dNeAll1=scipy.concatenate((dNeAll1,dne[scipy.newaxis,:]),axis=0)
                SpAll1=scipy.concatenate((SpAll1,sp[scipy.newaxis,:]),axis=0); dSpAll1=scipy.concatenate((dSpAll1,dsp[scipy.newaxis,:]),axis=0)
                ShAll1=scipy.concatenate((ShAll1,sh[scipy.newaxis,:]),axis=0); dShAll1=scipy.concatenate((dShAll1,dsh[scipy.newaxis,:]),axis=0)
                NuAll1=scipy.concatenate((NuAll1,nu[scipy.newaxis,:]),axis=0); WciAll1=scipy.concatenate((WciAll1,wci[scipy.newaxis,:]),axis=0);
                DecAll1=scipy.concatenate((DecAll1,dec[scipy.newaxis,:]),axis=0); DipAll1=scipy.concatenate((DipAll1,dip[scipy.newaxis,:]),axis=0);
            if scipy.mod(IIrec,100)==0 or done==1:
                print 'Record %d of %d' % (IIrec,Nrecs)
            IIrec=IIrec+1		

        # Electric field
        Eavg=scipy.zeros((Ball1.shape[0],1,2))*scipy.nan
        dEavg=scipy.zeros((Ball1.shape[0],1,2))*scipy.nan
        Bavg=scipy.zeros((Ball1.shape[0]))*scipy.nan        
        for itime in range(Eavg.shape[0]):
            Ialt=scipy.where((Alt1[itime,:]>=self.minAlt*1000.0) & (Alt1[itime,:]<=self.maxAlt*1000.0))[0]
            Bavg[itime] = scipy.stats.nanmean(Ball1[itime,Ialt])            
            Eavg[itime,0,0]=-1.0*scipy.nansum(Ball1[itime,Ialt]*vvels1[itime,Ialt,1]/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,1],2.0))/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,1],2.0))
            Eavg[itime,0,1]=1.0*scipy.nansum(Ball1[itime,Ialt]*vvels1[itime,Ialt,0]/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,0],2.0))/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,0],2.0))
            dEavg[itime,0,0]=scipy.sqrt(1.0/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,1],2.0)))
            dEavg[itime,0,1]=scipy.sqrt(1.0/scipy.nansum(1.0/scipy.power(Ball1[itime,Ialt]*dvvels1[itime,Ialt,0],2.0)))

        MLTtime1=scipy.array(MLTtime1)
        timeout=scipy.array(timeout)
        dtimeout=scipy.array(dtimeout)
        
        Vmag = scipy.sqrt( scipy.power(vvels1[:,:,0],2.0) + scipy.power(vvels1[:,:,1],2.0) ).real
        dVmag = scipy.sqrt( scipy.power(dvvels1[:,:,0],2.0)*scipy.power(vvels1[:,:,0]/Vmag,2.0) + scipy.power(dvvels1[:,:,1],2.0)*scipy.power(vvels1[:,:,1]/Vmag,2.0) ).real
        Vdir = 180.0/pi*scipy.arctan2(vvels1[:,:,1],vvels1[:,:,0]).real
        dVdir=180.0/pi*((1.0/scipy.absolute(vvels1[:,:,0]))*(1.0/(1.0+scipy.power(vvels1[:,:,1]/vvels1[:,:,0],2.0)))*scipy.sqrt(scipy.power(dvvels1[:,:,1],2.0)+scipy.power(vvels1[:,:,1]/vvels1[:,:,0]*dvvels1[:,:,0],2.0))).real
        
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
        # Additional
        self.IonoParams['Altitude'] = Alt1;
        self.IonoParams['Bavg']=Bavg
        self.IonoParams['Ne']=NeAll1; self.IonoParams['errNe']=dNeAll1
        self.IonoParams['Ball']=Ball1
        self.IonoParams['Sp']=SpAll1; self.IonoParams['errSp']=dSpAll1
        self.IonoParams['Sh']=ShAll1; self.IonoParams['errSh']=dShAll1
        self.IonoParams['Nuin']=NuAll1
        self.IonoParams['Wci']=WciAll1
        self.IonoParams['Dec']=DecAll1
        self.IonoParams['Dip']=DipAll1

        ### Output file
        if self.saveout:
            self.createOutputFile()
            
        ### Plot output
            self.createPlots()
        
        return

        
    