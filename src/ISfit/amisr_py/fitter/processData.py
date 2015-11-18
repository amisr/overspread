0#!/usr/bin/env python

"""

"""

from amisr_py.utils.util_fcns import *
from amisr_py.constants.constants import *
from amisr_py.ambiguity import amb_utils

import summationRule

import scipy
import datetime

# processDataClass
class processDataClass:
    """ Process Data Class """
    
    sumRuleFuncs={'lpmodern':summationRule.lpmodern}
    
    # init
    def __init__(self,opts,nfreq):
        """ initialization function """

        self.acfopts = opts
        self.nfreq = nfreq
        self.fconts = []
        self.Irecs = []
        
        return
        
    # reinit
    def reinit(self,fconts,Irecs):
        """ re-initializes class variables """
    
        self.fconts=fconts
        self.Irecs=Irecs
    
        self.Time={}
        self.Tx={}
        self.Rx={}
        self.S={}
    
        return    
        
    # getRecInfo
    def getRecInfo(self):
        """
        Returns dicts with standard Tx, Rx, and Time parameters for an integration
        and common for all experiments
        """
        
        Tx={}
        Rx={}
        Time={}

        for ifreq in range(self.nfreq):
            
            tfconts = self.fconts[ifreq]
            tirecs = self.Irecs[ifreq]
            
            nfiles = tirecs[-1][-1]-tirecs[0][-1]+1
            
            for ifile in range(nfiles):
            
                irecs = [tirecs[ir][0] for ir in range(len(tirecs)) if tirecs[ir][1]==ifile]
            
                tfile = tfconts[ifile]    
                
                try:
                    txaeus = tfile.root.Tx.AeuTx[irecs,:]
                    rxaeus = tfile.root.Rx.AeuRx[irecs,:]
                except:
                    txaeus = None
                    rxaeus = None
                    
                if ifile==0 and ifreq==0:
                    Tx['Frequency'] = tfile.root.Tx.Frequency[irecs,:]
                    Tx['Power'] = tfile.root.Tx.Power[irecs,:]
                    Rx['Frequency'] = tfile.root.Rx.Frequency[irecs,:]
                    if txaeus is not None:
                        Tx['Aeu'] = txaeus
                    if rxaeus is not None:
                        Rx['Aeu'] = rxaeus       
                    Time['UnixTime'] = tfile.root.Time.UnixTime[irecs,:]
                else:
                    Tx['Frequency']=scipy.concatenate((Tx['Frequency'],tfile.root.Tx.Frequency[irecs,:]))
                    Tx['Power']=scipy.concatenate((Tx['Power'],tfile.root.Tx.Power[irecs,:]))
                    Rx['Frequency']=scipy.concatenate((Rx['Frequency'],tfile.root.Rx.Frequency[irecs,:]))
                    if txaeus is not None:
                        Tx['Aeu'] = scipy.concatenate((Tx['Aeu'],txaeus))
                    if rxaeus is not None:
                        Rx['Aeu'] = scipy.concatenate((Rx['Aeu'],rxaeus))                                      
                    Time['UnixTime'] = scipy.concatenate((Time['UnixTime'],tfile.root.Time.UnixTime[irecs,:]))
                

        Tx['Frequency'] = scipy.median(scipy.mean(Tx['Frequency'],axis=1))
        Tx['Power'] = scipy.median(scipy.mean(Tx['Power'],axis=1))
        Rx['Frequency'] = scipy.median(scipy.mean(Rx['Frequency'],axis=1))
        try: Rx['Aeu'] = scipy.median(scipy.mean(Rx['Aeu'],axis=1))
        except: ''
        try: Tx['Aeu'] = scipy.median(scipy.mean(Tx['Aeu'],axis=1))
        except: ''

        Time['UnixTime'] = scipy.array([Time['UnixTime'].min(),Time['UnixTime'].max()])
        tmp1=datetime.datetime.utcfromtimestamp(Time['UnixTime'][0]); r1=datetime.date(tmp1.year,tmp1.month,tmp1.day)
        tmp2=datetime.datetime.utcfromtimestamp(Time['UnixTime'][1]); r2=datetime.date(tmp2.year,tmp2.month,tmp2.day)
        Time['Year']=scipy.array([tmp1.year,tmp2.year])
        Time['Month']=scipy.array([tmp1.month,tmp2.month])
        Time['Day']=scipy.array([tmp1.day,tmp2.day])
        Time['dtime']=scipy.array([(float(tmp1.hour)+float(tmp1.minute)/60.0+float(tmp1.second)/3600.0),(float(tmp2.hour)+float(tmp2.minute)/60.0+float(tmp2.second)/3600.0)])
        Time['doy']=scipy.array([int(r1.strftime('%j')),int(r2.strftime('%j'))])
        
        self.Time=Time
        self.Tx=Tx
        self.Rx=Rx
        
        return Time, Tx, Rx

    # applySummationRule
    def applySummationRule(self):     
        
        S=self.S

        Nlags = S['Acf']['Lags'].shape[0]
        Nbeams = S['BMCODES'].shape[0]

        summed = {'Range':[],'Altitude':[],'Acf':[],'AcfVar':[], \
            'SNR':[],'Nint':[],'Psc':[],'Ne':[],'BMCODES':S['BMCODES']}
        
        if self.acfopts['BinByRange']==1:
            Nrs=self.acfopts['Nrngs']
            Ialt=scipy.where((S['Acf']['Range']>=self.acfopts['rngmin']))[0]
            htind=Ialt[0]
                    
            groupIndex=-1
            nsum = self.acfopts['summationRule']
                        
            rng=[]; alt=[]; acf=[]; snr=[]; k=[]; acfVar=[]; psc=[]; ne=[]
            for ibm in range(Nbeams):
                for irng in range(Nrs):

                    # index for central range gate
                    htind = htind+nsum[0]
                    htindmax = htind+max(nsum)
                    
                    # check bounds
                    if htindmax>=S['Acf']['Range'].shape[0]:
                        break
                    if groupIndex<0 or S['Acf']['Altitude'][ibm,htind]>=self.acfopts['GroupHt'][groupIndex]:
                        groupIndex+=1
                        sumRule = self.sumRuleFuncs[self.acfopts['summationFunction']](self.acfopts['summationRule'][groupIndex],Nlags)
                        nsum = [sumRule[ilag][-1]-sumRule[ilag][0]+1 for ilag in range(Nlags)]

                    # total integrations as a function of lag
                    K=scipy.array(nsum)*S['Acf']['PulsesIntegrated'][ibm]*S['Acf']['Kint']

                    # compute average
                    rng.append(scipy.mean(S['Acf']['Range'][sumRule[0]+htind]))
                    alt.append(scipy.mean(S['Acf']['Altitude'][ibm,sumRule[0]+htind]))
                    acf.append([scipy.mean(S['Acf']['Data'][ibm,ilag,sumRule[ilag]+htind]) for ilag in range(Nlags)])
                    snr.append(scipy.mean(S['Power']['SNR'][ibm,sumRule[0]+htind]))
                    k.append([nsum[ilag]*S['Acf']['PulsesIntegrated'][ibm]*S['Acf']['Kint'][ilag] for ilag in range(Nlags)])
                    psc.append(self.Tx['Power']*S['Pulsewidth']*S['BMCODES'][ibm,-1]/(rng[irng]**2.0))

                    # variance estimate
                    if self.acfopts['uselag1']==1:
                        sig=scipy.absolute(acf[irng][S['Acf']['Lag1Index']]) # first lag
                    else:
                        sig=scipy.absolute(acf[irng][0]) # 0 lag
                    acfVar.append([sig**2.0/k[irng][ilag]*(1.0+1.0/snr[irng]+S['Acf']['iSCR'][ilag])**2.0 for ilag in range(Nlags)])
                    ne.append(sig/psc[irng]*2.0)
                
                # output
                summed['Range'].append(rng)
                summed['Altitude'].append(alt)
                summed['Acf'].append(acf)
                summed['AcfVar'].append(acfVar)
                summed['SNR'].append(snr)
                summed['Nint'].append(k)
                summed['Psc'].append(psc)
                summed['Ne'].append(ne)

        for key in summed.keys():
            try:
                summed[key]=scipy.array(summed[key])
            except: ''

        return summed

""" Sondrestrom Long Pulse """
class processLongPulse_mot1(processDataClass):

    def process(self,doamb=1):

        for ifreq in range(self.nfreq):
            
            tfconts = self.fconts[ifreq]
            tirecs = self.Irecs[ifreq]
            
            nfiles = tirecs[-1][-1]-tirecs[0][-1]+1
            
            for ifile in range(nfiles):
            
                irecs = [tirecs[ir][0] for ir in range(len(tirecs)) if tirecs[ir][1]==ifile]
                Nrecs = len(irecs); Nbeams=1
            
                tfile = tfconts[ifile]

                Pcal=tfile.root.Rx.Bandwidth.read()*tfile.root.Rx.CalTemp.read()*v_Boltzmann # Cal power in Watts
                
                ### azimuth and elevation
                az=tfile.root.Antenna.Azimuth[irecs,:]
                el=tfile.root.Antenna.Elevation[irecs,:]
                
                ### Power profile
                power=tfile.root.S.Data.Power.Data[irecs,:,:]
                powerNint=tfile.root.S.Data.PulsesIntegrated[irecs,:]
                npower=tfile.root.S.Noise.Power.Data[irecs,:,:]
                npowerNint=tfile.root.S.Data.PulsesIntegrated[irecs,:]
                cpower=tfile.root.S.Cal.Power.Data[irecs,:,:]
                cpowerNint=tfile.root.S.Data.PulsesIntegrated[irecs,:]
                powerRange=scipy.squeeze(tfile.root.S.Data.Power.Range)
                Nranges = powerRange.shape[0]

                # Average the noise and cal power samples
                npower = scipy.median(npower,axis=2)/npowerNint
                cpower = scipy.median(cpower,axis=2)/cpowerNint
                cpower = cpower-npower
                
                # Noise subtract and calibrate power profle
                power=power/scipy.repeat(powerNint[:,:,scipy.newaxis],Nranges,axis=2)
                pulsesIntegrated=scipy.sum(powerNint,axis=0) # total number of pulses integrated
                snr = (power - scipy.repeat(npower[:,:,scipy.newaxis],Nranges,axis=2))/scipy.repeat(npower[:,:,scipy.newaxis],Nranges,axis=2)
                power = Pcal*(power - scipy.repeat(npower[:,:,scipy.newaxis],Nranges,axis=2))/scipy.repeat(cpower[:,:,scipy.newaxis],Nranges,axis=2)
                
                ### ACF                
                if self.acfopts['DO_FITS']:
                    acf=(tfile.root.S.Data.Acf.Data[irecs,:,:,:,0]+tfile.root.S.Data.Acf.Data[irecs,:,:,:,1]*1.0j).astype('complex64')
                    nacf=(tfile.root.S.Noise.Acf.Data[irecs,:,:,:,0]+tfile.root.S.Noise.Acf.Data[irecs,:,:,:,1]*1.0j).astype('complex64')
                    lags=scipy.squeeze(tfile.root.S.Data.Acf.Lags)
                    Nlags = lags.shape[0]
                    acfRange=scipy.squeeze(tfile.root.S.Data.Acf.Range)
                    
                    # Noise ACF
                    nacf=complex_median(nacf,axis=3)/scipy.repeat(npowerNint[:,:,scipy.newaxis],Nlags,axis=2)
                    
                    # Noise subtract and calibrate the ACF                    
                    acf=acf/scipy.repeat(scipy.repeat(powerNint[:,:,scipy.newaxis,scipy.newaxis],Nlags,axis=2),Nranges,axis=3)
                    acf = Pcal*(acf-scipy.repeat(nacf[:,:,:,scipy.newaxis],Nranges,axis=3))/scipy.repeat(scipy.repeat(cpower[:,:,scipy.newaxis,scipy.newaxis],Nlags,axis=2),Nranges,axis=3)
                
                # store data
                if ifile==0 and ifreq==0:
                    recSum=0
                    # initialize dict 
                    S={} 
                    S['Acf']={}
                    S['Power']={}                
                    # pulsewidth and baudlen
                    S['Pulsewidth'] = tfile.root.S.Data.Pulsewidth.read()
                    S['TxBaud'] = tfile.root.S.Data.TxBaud.read()
                    # az and el
                    allaz = az
                    allel = el
                    # Power
                    S['Power']['Kint']=1.0
                    S['Power']['iSCR']=0.0
                    S['Power']['Range'] = powerRange
                    S['Power']['PulsesIntegrated'] = scipy.zeros((Nbeams),dtype='float32')
                    allpower = power  
                    allsnr = snr   
                    # Acf
                    if self.acfopts['DO_FITS']:
                        S['Acf']['Kint']=scipy.ones((Nlags),dtype='float64')
                        S['Acf']['iSCR']=scipy.zeros((Nlags),dtype='float64')
                        S['Acf']['Lags'] = lags
                        S['Acf']['Range'] = acfRange
                        S['Acf']['PulsesIntegrated'] = scipy.zeros((Nbeams),dtype='float32')
                        allacf = acf
                else:
                    allaz = scipy.concatenate((allaz,az),axis=0)
                    allel = scipy.concatenate((allel,el),axis=0)
                    allpower = scipy.concatenate((allpower,power),axis=0)
                    allsnr = scipy.concatenate((allsnr,snr),axis=0)
                    if self.acfopts['DO_FITS']:
                        allacf = scipy.concatenate((allacf,acf),axis=0)
                 
                S['Power']['PulsesIntegrated'] += pulsesIntegrated
                if self.acfopts['DO_FITS']:
                    S['Acf']['PulsesIntegrated'] += pulsesIntegrated                                                            

                recSum += Nrecs
        
        # ambiguity function
        if doamb:
            try:
                ambPath=tfile.root.S.Data.Ambiguity.read()            
                S['Power']['Ambiguity']=amb_utils.copyAmbDict(tfile,ambPath)
                S['Acf']['Ambiguity']=amb_utils.copyAmbDict(tfile,ambPath)
                # for the power, we are dealing only with the zero lags
                S['Power']['Ambiguity']['Wlag']=S['Power']['Ambiguity']['Wlag'][0,:] 
                S['Power']['Ambiguity']['Wrange']=S['Power']['Ambiguity']['Wrange'][0,:] 
            except:
                #### TODO ACTUALLY RAISE AN EXCEPTION
                print 'Unable to load ambiguity function'        
                xxx
                
        # average                
        S['Power']['Data'] = scipy.median(allpower,axis=0)
        S['Power']['SNR'] =  scipy.median(allsnr,axis=0)
        S['Power']['fracErr'] = scipy.std(allpower,axis=0)/S['Power']['Data']/scipy.sqrt(recSum)
        if self.acfopts['DO_FITS']:
            S['Acf']['Data'] = complex_median(allacf,axis=0)

        # azimuth and elevation
        I=scipy.where(el>90.0)[0]; el[I]=180.0-el[I]; az[I]=az[I]+180.0
        I=scipy.where(az>360.0)[0]; az[I]=az[I]-360.0
        I=scipy.where(az<0.0)[0]; az[I]=az[I]+360.0                
        S['AvgAzimuth']=azAverage(az*pi/180.0)*180.0/pi
        S['AvgElevation']=scipy.mean(el)
        S['Azimuth']=scipy.array([az[0,0],az[-1,-1]])
        S['Elevation']=scipy.array([el[0,0],el[-1,-1]])

        # system constant and beamcodes
        if self.acfopts.has_key('Ksys'):
            S['Ksys']=self.acfopts['Ksys']
        else:            
            try:
                S['Ksys']=fconts[0][0].root.Rx.SysConst.read()
            except:
                S['Ksys']=self.acfopts['DEFOPTS']['KSYS_DEF']
        S['BMCODES']=scipy.array([[-1,S['AvgAzimuth'],S['AvgElevation'],S['Ksys']]]) 
                
        self.S=S
                
        return S    
        

