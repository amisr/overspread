#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys
import getopt
import optparse
import os.path
import ConfigParser
import math
import datetime
import tables
import scipy
import scipy.stats
import ctypes
import scipy.signal
import scipy.interpolate
import scipy.integrate
import scipy.fftpack
import scipy.ndimage
import glob

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import proc_utils
import flipchem
import scipy.ndimage as ndimage
import numpy

# some natural constants
v_lightspeed=299792458
v_Boltzmann=1.380658e-23
v_electronmass=9.1093897e-31
v_amu=1.6605402e-27
v_electronradius=2.81794092e-15
v_epsilon0=8.854188E-12
v_elemcharge=1.602177E-19

##############################

class iplot:

    def __init__(self):
        self.pl_x=[]
        self.pl_y=[]
        return
        
    def __call__(self,event):
        clickx=event.xdata
        clicky=event.ydata
        tb = pylab.get_current_fig_manager().toolbar
        if event.button==1 and event.inaxes and tb.mode == '':
            self.pl_x.append(clickx)
            self.pl_y.append(clicky)
            #pylab.ioff()
            v = pylab.axis()
            pylab.plot([clickx,clickx],[clicky,clicky],'.w')
            pylab.axis(v)
            #pylab.show()
            
            
def freq2ne(freq,Te=1000.0,f0=450.0e6,B=50000e-9,alph=0.0):
    k=4*math.pi/v_lightspeed*f0
    fc=v_elemcharge*B/v_electronmass/2.0/math.pi
    
    ne=4.0*math.pi**2*v_electronmass*v_epsilon0/v_elemcharge**2*(scipy.power(freq,2.0)-3*k**2/4.0/math.pi**2*v_Boltzmann*Te/v_electronmass-fc**2*scipy.sin(alph)**2)

    return ne
    
def chapman_func(z,Hch,nmax,zmax):

    ne=nmax*scipy.exp(0.5*(1.0-(z-zmax)/Hch-scipy.exp((zmax-z)/Hch)))
    
    return ne

def chap_fitfun(parameter,altitude,data,scaler,PW,mode=0):

    parameter=parameter*scaler

    tnmax=parameter[0]
    tzmax=parameter[1]
    thch=parameter[2]
    
    althr=scipy.arange(altitude[0]-PW,altitude[-1]+PW,100)
    
    # model
    m=chapman_func(althr,thch,tnmax,tzmax)
    
    # smear model
    m2=scipy.zeros(data.shape,dtype=data.dtype)
    for rr in range(altitude.shape[0]):
        I=scipy.where((althr>=(altitude[rr]-PW/2.0)) & (althr<=(altitude[rr]+PW/2.0)))[0]
        m2[rr]=scipy.mean(m[I]*(altitude[rr]**2.0)/scipy.power(althr[I],2.0))
    
    if mode==0:
        y=m2-data
    else:
        y=m2
        
    return y


def chap_fitfun2(parameter,altitude,data,scaler,mode=0):

    parameter=parameter*scaler

    tnmax=parameter[0]
    tzmax=parameter[1]
    thch=parameter[2]
        
    # model
    m=chapman_func(altitude,thch,tnmax,tzmax)
    
    if mode==0:
        y=m-data
    else:
        y=m
        
    return y
    
def canny(image, high_threshold, low_threshold):

    # Filter kernels for calculating the value of neighbors in several directions
    _N  = numpy.array([[0, 1, 0],[0, 0, 0],[0, 1, 0]], dtype=bool)
    _NE = numpy.array([[0, 0, 1],[0, 0, 0],[1, 0, 0]], dtype=bool)
    _W  = numpy.array([[0, 0, 0],[1, 0, 1],[0, 0, 0]], dtype=bool)
    _NW = numpy.array([[1, 0, 0],[0, 0, 0],[0, 0, 1]], dtype=bool)

    # After quantizing the angles, vertical (north-south) edges get values of 3,
    # northwest-southeast edges get values of 2, and so on, as below:
    _NE_d = 0
    _W_d = 1
    _NW_d = 2
    _N_d = 3

    grad_x = ndimage.sobel(image, 0)
    grad_y = ndimage.sobel(image, 1)
    grad_mag = numpy.sqrt(grad_x**2+grad_y**2)
    grad_angle = numpy.arctan2(grad_y, grad_x)
    # next, scale the angles in the range [0, 3] and then round to quantize
    quantized_angle = numpy.around(3 * (grad_angle + numpy.pi) / (numpy.pi * 2))
    # Non-maximal suppression: an edge pixel is only good if its magnitude is
    # greater than its neighbors normal to the edge direction. We quantize
    # edge direction into four angles, so we only need to look at four
    # sets of neighbors
    NE = ndimage.maximum_filter(grad_mag, footprint=_NE)
    W  = ndimage.maximum_filter(grad_mag, footprint=_W)
    NW = ndimage.maximum_filter(grad_mag, footprint=_NW)
    N  = ndimage.maximum_filter(grad_mag, footprint=_N)
    thinned = (((grad_mag > W)  & (quantized_angle == _N_d )) |
                ((grad_mag > N)  & (quantized_angle == _W_d )) |
                ((grad_mag > NW) & (quantized_angle == _NE_d)) |
                ((grad_mag > NE) & (quantized_angle == _NW_d)) )
    thinned_grad = thinned * grad_mag
    # Now, hysteresis thresholding: find seeds above a high threshold, then
    # expand out until we go below the low threshold
#	high = thinned_grad > high_threshold
#	low = thinned_grad > low_threshold
#	canny_edges = ndimage.binary_dilation(high, structure=numpy.ones((3,3)), iterations=-1, mask=low)
    return grad_mag, thinned_grad
    
def read_a_datafile(fname,p=None):
    
    print 'Reading file ' + fname
    
    # make sure the file exists
    if os.path.exists(fname)==False:
        print 'The input file does not exist.'
        sys.exit(1)
    
    # read the entire file
    h5file=tables.openFile(fname)
    output={}
    if p==None:
        for group in h5file.walkGroups("/"):
            output[group._v_pathname]={}
            for array in h5file.listNodes(group, classname = 'Array'):						
                output[group._v_pathname][array.name]=array.read()		
        h5file.close()
    else:
        for group in p:
            output[group]={}
            for array in h5file.listNodes(group, classname = 'Array'):						
                output[group][array.name]=array.read()		
        h5file.close()
        
    return output

def read_pl_file(fname):

    fid=open(fname,'r')
    
    pl=[]
    time=[]
    done=0
    icnt=0
    while (not done):
        try:
            line=fid.next()
            line=line.split(' ')
            if icnt==0 and (len(line)==4 or len(line)==5):
                bmcode=scipy.array(line).astype('float64')
            elif icnt==1 and len(line)==1:
                stime=float(line[0])
                print stime
            elif len(line)==2:
                pl.append(float(line[1])*1.0e6)
                time.append(stime+float(line[0]))
        except:
            done=1
        icnt+=1
    
    fid.close()
    
    return bmcode,time,pl

def cal_pl_all(dir, EXP):

    import matplotlib
    matplotlib.use('Agg')
    import pylab

    ST0=''
#    EXP='20150224.001'
    
#	INT='_lp_3min'; 	LONGPULSE=1; doChap=0; usePower=0; Ilen=1
#	INT='_ac_5min'; 	LONGPULSE=0; doChap=0; usePower=0; Ilen=3
#    INT='_lp_15min-64157'; 	LONGPULSE=1; doChap=0; usePower=0; Ilen=1; useAltPl=0
#    INT='_ac_15min-64157'; 	LONGPULSE=0; doChap=0; usePower=0; Ilen=3; useAltPl=0
#    INT = '_lp_5min'; 	LONGPULSE=1; doChap=0; usePower=0; Ilen=1; useAltPl=0
#    INT='_ac_5min'; 	LONGPULSE=0; doChap=0; usePower=0; Ilen=3; useAltPl=0
    
    pl_type = {'LP':{'INT': '_lp_5min', 'LONGPULSE': 1, 'doChap': 0, 'usePower': 0, 'Ilen': 1, 'useAltPl': 0, 'filter':'*_lp_5min_.txt'},
                'AC':{'INT': '_ac_5min', 'LONGPULSE': 0, 'doChap': 0, 'usePower': 0, 'Ilen': 3, 'useAltPl': 0, 'filter':'*_ac_5min_.txt'}
                }
                
    ST=''; 
    caldir='cal-0/'
    top_cal_dir = '/Volumes/ISR_DATA_02/calibration/AMISR/calibration_PFISR'
    ILdir = os.path.join(dir,EXP)
    allfiles=glob.glob(os.path.join(ILdir,caldir,'*-plline-[0-9][0-9][0-9].txt')) 
    
    for key, value in pl_type.items():
        filter      = value['filter']
        
        INT         = value['INT']
        LONGPULSE   = value['LONGPULSE']
        doChap      = value['doChap']
        usePower    = value['usePower']
        Ilen        = value['Ilen']
        useAltPl    = value['useAltPl']
        
        ILfname=ST+EXP+INT+'.h5'
        print ILfname
        
        for aa in range(len(allfiles)):
            try:
                #if 1==1:
                cal_pl(EXP,INT,LONGPULSE,allfiles[aa],ILfname,ILdir,doChap=doChap,usePower=usePower,ST='_'+ST0,Ilen=Ilen,useAltPl=useAltPl)
                pylab.close('all')
            except:
                print 'failed'
        
        full_cal_dir = os.path.join(top_cal_dir,EXP[0:6],'PLCal30',EXP)
        if not os.path.exists(full_cal_dir):
            os.makedirs(full_cal_dir)
        
        fid = open('%s/filelist_%s.txt' % (full_cal_dir,key.lower()),'w')
        for file in glob.glob(os.path.join(ILdir,caldir,filter)):
            fid.write(file)
            fid.write('\n')
        
    
            
def cal_pl(EXP,INT,LONGPULSE,PLfile,ILfname,ILdir,doChap=-1,usePower=-1,ST='',Ilen=10,useAltPl=0):

    import pylab

#	EXP='20071011.011'
#	INT='_ac_10min'
#	LONGPULSE=0
#	ILfname=EXP+INT+'.h5'
#	PLfile='cal-0/20071011.011-65162-plline-000.txt'
    oname=PLfile[:-4]+INT+ST

    thch=40.0e3
    if doChap==-1:
        doChap=0
    if usePower==-1:
        usePower=0
    PW=30.0e-6*v_lightspeed/2	
    ALTMIN=150.0e3
    ALTMAX=400.0e3

    if LONGPULSE:
        if doChap==-1:
            doChap=1
        if usePower==-1:
            usePower=1
        if usePower==0:
            Ilen=1
            ALTMIN=100.0e3
        PW=480.0e-6*v_lightspeed/2.0

    # read ion line data
    ILdata=read_a_datafile(os.path.join(ILdir,ILfname))
    TxPower=ILdata['/ProcessingParams']['TxPower']
    BMCODES=ILdata['/']['BeamCodes']
    Nbeams=BMCODES.shape[0]
    IlTime=scipy.mean(ILdata['/Time']['UnixTime'],axis=1)
    NePower=ILdata['/NeFromPower']['Ne_NoTr']
    AltPower=ILdata['/NeFromPower']['Altitude']
    Ne=ILdata['/FittedParams']['Ne']
    dNe=ILdata['/FittedParams']['dNe']
    Alt=ILdata['/FittedParams']['Altitude']
    Te=ILdata['/FittedParams']['Fits'][:,:,:,-1,1]
    Ti=ILdata['/FittedParams']['Fits'][:,:,:,0,1]
    Tr=Te/Ti
    Babs=ILdata['/Geomag']['Babs']
    kpar=ILdata['/Geomag']['kpar']
    kperp=scipy.sqrt(scipy.power(ILdata['/Geomag']['kpe'],2.0)+scipy.power(ILdata['/Geomag']['kpn'],2.0))
    alph=math.pi/2-scipy.arcsin(kpar/scipy.sqrt(scipy.power(kpar,2.0)+scipy.power(kperp,2.0))).real
    YR=ILdata['/Time']['Year'][0,0]
    DAY=ILdata['/Time']['Day'][0,0]
    MON=ILdata['/Time']['Month'][0,0]

    # read PL data
    print 'Reading file '+ PLfile
    bmcode,timepl,pl=read_pl_file(PLfile)
    if useAltPl:
        try:
            altpl = bmcode[-1]        
        except:
            altpl = -1
            useAltPl=0

    timepl=scipy.array(timepl)
    pl=scipy.array(pl)
    PW=PW*scipy.sin(bmcode[2]*scipy.pi/180.0)

    Ibeam=scipy.where(BMCODES[:,0]==bmcode[0])[0]
    if len(Ibeam)==0:
        print 'cant find beam!'
        sys.exit(-1)

    # get nemax from IL
    Itime=scipy.where((IlTime>=(timepl.min()-1000)) & (IlTime<=(timepl.max()+1000)))[0]
    timeil=IlTime[Itime]
    NeMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    dNeMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    htMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    TeMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    TiMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    TrMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    alphMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    BabsMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    TxPow=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
    for aa in range(len(Itime)):
        ra=Itime[aa]
        
        TxPow[aa]=TxPower[ra]
    
        taltpower=scipy.squeeze(AltPower[Ibeam,:])
        Ialtpower=scipy.where((taltpower>=ALTMIN) & (taltpower<=ALTMAX))[0]
        taltpower=taltpower[Ialtpower]
        tprofpower=scipy.squeeze(NePower[ra,Ibeam,:])[Ialtpower]			
        
        talt=scipy.squeeze(Alt[Ibeam,:])
        Ialt=scipy.where((talt>=ALTMIN) & (talt<=ALTMAX))[0]
        talt=talt[Ialt]

        tprof=scipy.squeeze(Ne[ra,Ibeam,:])[Ialt]
        dtprof=scipy.squeeze(dNe[ra,Ibeam,:])[Ialt]
                    
        tte=scipy.squeeze(Te[ra,Ibeam,:])[Ialt]
        tti=scipy.squeeze(Ti[ra,Ibeam,:])[Ialt]
        talph=scipy.squeeze(alph[Ibeam,:])[Ialt]
        tBabs=scipy.squeeze(Babs[Ibeam,:])[Ialt]
        ttr=tte/tti
            
        if usePower:
            ttprof=tprofpower
            ttalt=taltpower
        else:
            ttprof=tprof
            ttalt=talt			

        try:
#        if 1==1:
            
            if useAltPl:
            
                htMax[aa] = altpl*1e3

                NeMax[aa]=scipy.interpolate.interp1d(ttalt,ttprof,bounds_error=0)(htMax[aa])
                dNeMax[aa]=scipy.interpolate.interp1d(talt,dtprof,bounds_error=0)(htMax[aa])
                TeMax[aa]=scipy.interpolate.interp1d(talt,tte,bounds_error=0)(htMax[aa])
                TrMax[aa]=scipy.interpolate.interp1d(talt,ttr,bounds_error=0)(htMax[aa])           
                            
            else:
            
                # get maximum Ne and altitude of peak
                I=scipy.where(ttprof==ttprof.max())[0]
    #			pylab.plot(ttprof,ttalt)

                p=scipy.polyfit(ttalt[I-Ilen:I+Ilen+1], ttprof[I-Ilen:I+Ilen+1], 2, rcond=None, full=False)
                htMax[aa]=-p[1]/2.0/p[0]
                NeMax[aa]=p[0]*htMax[aa]**2+p[1]*htMax[aa]+p[2]
                if NeMax[aa]>1.0e13:
                    NeMax[aa]=1.0e11
                dNeMax[aa]=scipy.std(ttprof[I-Ilen:I+Ilen+1]-scipy.polyval(p,ttalt[I-Ilen:I+Ilen+1]))
    #			pylab.plot(scipy.polyval(p,ttalt[I-Ilen:I+Ilen+1]),ttalt[I-Ilen:I+Ilen+1],'.k-')
    #			pylab.plot(ttprof[I-Ilen:I+Ilen+1]-scipy.polyval(p,ttalt[I-Ilen:I+Ilen+1]),ttalt[I-Ilen:I+Ilen+1],'.b-')
                
                if doChap:
                    scaler=scipy.array([1.0e11,1.0e5,1.0e4])
                    params0=scipy.array([NeMax[aa],htMax[aa],thch])
                    (x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(chap_fitfun,params0/scaler,(ttalt[I-Ilen:I+Ilen+1],ttprof[I-Ilen:I+Ilen+1],scaler,PW),
                        full_output=1,epsfcn=1.0e-5,ftol=1.0e-5, xtol=1.0e-5, gtol=0.0, maxfev=15*params0.shape[0],factor=0.5,diag=None)
                    tz=scipy.arange(ttalt[I-Ilen],ttalt[I+Ilen+1],100)
                    nemod=chap_fitfun(x,tz,tz,scaler,PW,mode=1)
                    x=x*scaler
                    
                #	print x
                    tne=chapman_func(tz,x[2],x[0],x[1])
                    tne0=chapman_func(tz,params0[2],params0[0],params0[1]) # initial guess
                    pylab.plot(ttprof,ttalt,'.b-')
                    pylab.plot(tne,tz,'k')
                    pylab.plot(tne0,tz,'r')
                    pylab.plot(nemod,tz,'k--')
                
                    htMax[aa]=x[1]
                    NeMax[aa]=x[0]
                                
                TeMax[aa]=scipy.interpolate.interp1d(talt,tte,bounds_error=0)(htMax[aa])
                TrMax[aa]=scipy.interpolate.interp1d(talt,ttr,bounds_error=0)(htMax[aa])

#			print talt
#			print tte
#			print htMax[aa]
#			print TeMax[aa]			
#			xxx
            
            if usePower:
                NeMax[aa]=NeMax[aa]/2.0*(1.0+TrMax[aa])	
                    
            alphMax[aa]=scipy.interpolate.interp1d(talt,talph,bounds_error=0)(htMax[aa])
            BabsMax[aa]=scipy.interpolate.interp1d(talt,tBabs,bounds_error=0)(htMax[aa])		
        
        except:
            print 'Failed'
                
    # get sys constant
    pl_ne=scipy.zeros(timepl.shape,dtype='float64')
    il_ne=scipy.zeros(timepl.shape,dtype='float64')
    il_dne=scipy.zeros(timepl.shape,dtype='float64')
    il_tr=scipy.zeros(timepl.shape,dtype='float64')
    for isamp in range(timepl.shape[0]):
    
        # find the closest time
        tmp=scipy.absolute(timeil-timepl[isamp])				
        I=scipy.where(tmp==tmp.min())[0]
        I=I[0]	
            
        pl_ne[isamp]=float(freq2ne(pl[isamp],Te=TeMax[I],B=BabsMax[I],alph=alphMax[I]))
        il_ne[isamp]=NeMax[I]
        il_tr[isamp]=TrMax[I]
        il_dne[isamp]=dNeMax[I]


    KsysCor=il_ne/pl_ne
    KsysErr=il_dne/pl_ne
    Ksys_med=scipy.stats.stats.nanmedian(KsysCor)
    #    Ksys_std=scipy.stats.stats.nanmedian(KsysErr)
    Ksys_std=scipy.stats.stats.nanstd(KsysCor)

    pylab.figure()
    pylab.subplot(211)
    pylab.scatter(timepl,pl_ne,c='r')
    pylab.errorbar(timeil,NeMax,yerr=dNeMax)
    pylab.gca().set_ylabel('Ne (m-3)')
    pylab.title('%d (%2.2f,%2.2f)' % (bmcode[0],bmcode[1],bmcode[2]))
    pylab.xlim([(timepl.min()-1000),(timepl.max()+1000)])
    
    pylab.subplot(212)
    pylab.errorbar(timepl,KsysCor,yerr=KsysErr, marker='s', mfc='red', mec='green', ms=4, mew=1,fmt='k.')
    v=pylab.axis()
    pylab.plot(v[0:2],[Ksys_med,Ksys_med],'k')
    pylab.plot(v[0:2],[Ksys_med+Ksys_std,Ksys_med+Ksys_std],'k--')
    pylab.plot(v[0:2],[Ksys_med-Ksys_std,Ksys_med-Ksys_std],'k--')
    pylab.gca().set_ylabel('Ksys-cor')
    pylab.gca().set_xlabel('Time (secs)')
    pylab.xlim([(timepl.min()-1000),(timepl.max()+1000)])

    pylab.show()
    
    pylab.gcf().savefig(os.path.join(ILdir,oname+'.png'))

#	pylab.figure()
#	pylab.plot(timeil,TrMax)

    fH=open(oname+'.txt','w') 
    fH.write('%f %f %f %f\n' % (bmcode[0],bmcode[1],bmcode[2],bmcode[3]))
    fH.write('%f %f\n' % (Ksys_med,Ksys_std)) 
    fH.write('%d %d\n' % (timepl.shape[0],timeil.shape[0]))
    fH.write('%f %f\n' % (timepl[0],timepl[-1]))
    fH.write('%f %f\n' % (timeil[0],timeil[-1]))
    fH.write('%f %f\n' % (scipy.stats.stats.nanmedian(il_ne),scipy.stats.stats.nanmedian(il_tr)))
    fH.write('%f\n' % (scipy.stats.stats.nanmean(TxPow)))
    fH.write('\n')
    for i in range(len(KsysCor)):    
        fH.write('%f %f %f\n' % (timepl[i],KsysCor[i],KsysErr[i]))
    fH.close()


def plot_pl():

    FILE_PATH='/Volumes/ISR_Data-1/Data AMISR Poker/20070829.001'
    filepath_up='*.Dt2.h5'
    filepath_dn='*.Dt1.h5'
    Ifiles=range(0,1)

    files_up=glob.glob(os.path.join(FILE_PATH,filepath_up)) 
    if len(Ifiles)>0:
        files_up=files_up[Ifiles[0]:(Ifiles[-1]+1)]
    if dualpl:
        files_dn=glob.glob(os.path.join(FILE_PATH,filepath_dn)) 
        if len(Ifiles)>0:
            files_dn=files_dn[Ifiles[0]:(Ifiles[-1]+1)]
    else:
        files_dn=files_up
        
    files_dn.sort()
    files_up.sort()

    # loop over plasma line files
    for Ifile in range(NFILES):
        output1=read_a_datafile(os.path.join(FILE_PATH,files_up[Ifile]))
        if dualpl:
            output2=read_a_datafile(os.path.join(FILE_PATH,files_dn[Ifile]))

        if Ifile==0:
            rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
            fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
            if fr_off==output1['/Tx']['Frequency'][0,0]:
                fr_off=6.5e6
            freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
            
            pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
            bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
            time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
            
            if dualpl:
                freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
                pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
                bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
                time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
        else:
            pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
            bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
            time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
            if dualpl:
                pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
                bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
                time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)
    # deal the data
    pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
    if dualpl:
        pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

    Ntimes=time_up.size
    Nfreqs=freq_up.size

    if len(beams2do)==0:
        beams2do=BMCODES[:,0]

    # loop over beams
    for jjj in range(Nbeams):

        # get beamcode
        bmcode=BMCODES[jjj,:]
        
        # subtract highest altitude	
        tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
        tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-10][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2) 
        tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
        if dualpl:
            tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
            tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-10][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
            tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
        #
        alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)

def batchImagePl():

    import matplotlib
    matplotlib.use('Agg')
    import pylab

    """"""
    # IPY ***
    FILELIST_UP='/Volumes/AMISR_004/processed_data/PFISR/2007/Stromme01/20070323.002/filelist_PL2.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/PFISR/2007/Stromme01/20070323.002/filelist_PL1.txt'
    EXP='Stromme01'
    ODIR='/Volumes/AMISR_004/processed_data/PFISR/2007/Stromme01/20070323.002' + '/cal-0/batchPl/'
    DAT_PATH = ''
    DIR_SEARCH = 1
    Is=18
    beams2do=[]
    MAXFILES=5
    flip=1
    dualpl=1
    
    
    
    clim=[1.0e8,1.0e7]
    clim=[.5e7,.5e7]
    htstart=170.0e3	
    """"""
    
    """
    # RISR ***
    ENUM='20100504.001'
    FILELIST_UP='/Volumes/AMISR_004/processed_data/RISR/2010/WorldDay55m/'+ENUM+'/filelist_PL2.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/RISR/2010/WorldDay55m/'+ENUM+'/filelist_PL1.txt'
    EXP='WorldDay55m'
    ODIR='/Volumes/AMISR_004/processed_data/RISR/2010/WorldDay55m/'+ENUM+ '/cal-0/batchPl/'
    DAT_PATH = '/Volumes/AMISR_013/Data AMISR Resolute N/'+ENUM+'/'
    DIR_SEARCH = 1
    Is=0	
    beams2do=[64280]
    MAXFILES=5
    flip=0
    dualpl=1
    clim=[0.5e7,0.5e7]
    htstart=250.0e3
    """

    Ifiles=[]
    FR_OFF=[6.5e6,6.5e6]
    #FR_OFF=[3.5e6,3.5e6]
    KS=[5,3]

    # get file list
    f=open(FILELIST_UP); files_up=f.readlines(); f.close()
    f=open(FILELIST_DN); files_dn=f.readlines(); f.close()
    if len(files_up) != len(files_dn):
        print 'different number of up and down files!'
        sys.exit(-1)
    if DIR_SEARCH:
        files_up2=[]; files_dn2=[]
        for ifile in range(len(files_up)):
            files_up2.extend(glob.glob(os.path.join(DAT_PATH,files_up[ifile].rstrip('\n'))))
        for ifile in range(len(files_dn)):
            files_dn2.extend(glob.glob(os.path.join(DAT_PATH,files_dn[ifile].rstrip('\n'))))
        files_up=files_up2
        files_dn=files_dn2
        if len(files_up) != len(files_dn):
            print 'different number of up and down files!'
            sys.exit(-1)
        files_up.sort()
        files_dn.sort()
    files_up=files_up[Is:]
    files_dn=files_dn[Is:]

    NFILES=len(files_up)
    for ir in range(NFILES):
        files_up[ir]=files_up[ir].rstrip('\n')
        files_dn[ir]=files_dn[ir].rstrip('\n')

    pylab.figure()
    pylab.show()
    pylab.close('all')
    figg1=pylab.figure();
        
    done=0; IIfile=0
    while not done:
        tfiles_up=[]
        tfiles_dn=[]
        
        print "Spot: %d of %d" % (IIfile+Is,len(files_up))
        tfiles_up.append(files_up[IIfile])
        tfiles_dn.append(files_dn[IIfile])
        tmp=tfiles_up[0].split('/'); texp=tmp[-2]; tmp=tmp[-1]; tmp=int(tmp[1:-7]); tmp=tmp+1
        
        sett=0
        StartI=IIfile+Is
        while not sett:
            IIfile=IIfile+1
            if (IIfile<NFILES) and (len(tfiles_up)<=MAXFILES):
                ttexp=files_up[IIfile].split('/'); ttexp=ttexp[-2]
                if (ttexp==texp) and (files_up[IIfile][:-7].endswith('%(#)06d' % {"#": tmp})):
                    tfiles_up.append(files_up[IIfile])
                    tfiles_dn.append(files_dn[IIfile])
                    tmp=tmp+1
                else:
                    sett=1
            else:
                sett=1
        EndI=IIfile-1+Is

            
        if IIfile>=NFILES:
            done=1

        # loop over plasma line files
        for Ifile in range(len(tfiles_up)):
            
            # read file(s)
            output1=read_a_datafile(tfiles_up[Ifile])
            if dualpl:
                if tfiles_dn[Ifile]==tfiles_up[Ifile]:
                    output2=output1.copy()
                else:
                    output2=read_a_datafile(tfiles_dn[Ifile])

            if Ifile==0:
                rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
                fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
                if fr_off==output1['/Tx']['Frequency'][0,0]:
                    fr_off=FR_OFF[0]	
                freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
                
                try:
                    BeamcodeMap=output1['/Setup']['BeamcodeMap']
                except:
                    BeamcodeMap=[]
                
                pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
                bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
                time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
                
                if dualpl:
                    fr_off=scipy.absolute(output2['/Tx']['Frequency'][0,0]-output2['/Rx']['Frequency'][0,0])
                    if fr_off==output1['/Tx']['Frequency'][0,0]:
                        fr_off=FR_OFF[1]
                    freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
                    pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
                    bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
                    time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
            else:
                pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
                bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
                time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
                if dualpl:
                    pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
                    bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
                    time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)

        beamcodes=scipy.sort(bmcodes_up[0,:])
        Nbeams=beamcodes.shape[0]
        BMCODES=scipy.zeros((Nbeams,4),dtype='Float64') # beamcode table (beamcode,az,el,ksys)
        for i in range(Nbeams):
            I=scipy.where(BeamcodeMap[:,0]==beamcodes[i])[0]
            BMCODES[i,:]=BeamcodeMap[I,:]
                
        # deal the data
        pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
        if dualpl:
            pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

        
        Ntimes=time_up.size
        Nfreqs=freq_up.size

        if len(beams2do)==0:
            beams2do=BMCODES[:,0]

            
        # loop over beams
        for jjj in range(Nbeams):

            # get beamcode
            bmcode=BMCODES[jjj,:]
            rrI=scipy.where(bmcode[0]==beams2do)[0]
                    
            if len(rrI)>=1:

                #raw_input('Ready? hit enter')
                figg1.clf()
                
                # subtract highest altitude	
                tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
                tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-1][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2) 
                tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
                if dualpl:
                    tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
                    tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-1][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
                    tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
                #
                alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)

                # find plasma line spectra closest to peak
                tmp=scipy.absolute(alt_pl-htstart)
                Ialt=scipy.where(tmp==tmp.min())[0]
                pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
                if dualpl:
                    pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
                if dualpl:
                    if flip==0:
                        pl_dn=scipy.fliplr(pl_dn)
                if flip==1:
                        pl_up=scipy.fliplr(pl_up) # down is really up!!

                pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
                if dualpl:
                    pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)

                tmp1=datetime.datetime.utcfromtimestamp(time_up[0]); r1=datetime.datetime(tmp1.year,tmp1.month,tmp1.day, tmp1.hour, tmp1.minute, tmp1.second)
                x_up=tmp1.strftime('Secs from %D %H:%M:%S')
                
                pylab.figure(figg1.number)
                pylab.subplot(211)
                pylab.imshow(pl_up,vmin=0,vmax=clim[0],aspect='auto',origin='lower',extent=[freq_up[0]/1.0e6,freq_up[-1]/1.0e6,0,time_up[-1]-time_up[0]])	
                pylab.colorbar()
                ax1=pylab.gca()
                ax1.set_ylabel(x_up)
                ax1.set_xlabel('Freq (MHz)')
                pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
               
                if dualpl:
                
                    #tmp1=datetime.datetime.utcfromtimestamp(time_dn[0]); r1=datetime.datetime(tmp1.year,tmp1.month,tmp1.day, tmp1.hour, tmp1.minute, tmp1.second)
                    #x_dn=tmp1.strftime('Secs from %D %H:%M:%S')
                
                    pylab.subplot(212)
                    pylab.imshow(pl_dn,vmin=0,vmax=clim[1],aspect='auto',origin='lower',extent=[freq_dn[0]/1.0e6,freq_dn[-1]/1.0e6,0,time_dn[-1]-time_dn[0]])
                    pylab.colorbar()
                    ax2=pylab.gca()
                    #ax2.set_ylabel(x_dn)
                    ax2.set_xlabel('Freq (MHz)')

                pylab.show()
            
                if 1==1:
                            
                    oname='%s-%d-plline-%.3dkm_%.4d-%.4d' % (EXP,bmcode[0],int(alt_pl[Ialt]/1000.0),StartI,EndI) 
                    
                    try:
                        figg1.savefig(os.path.join(ODIR,oname+'.png'))
                    except:
                        print 'Could not save plot'				

if __name__ == '__main__':

    import matplotlib
    matplotlib.use('TkAgg'); matplotlib.interactive(True)
    import pylab
    pylab.interactive(True)
    
    filterBySza=0
    flipAuto=1
        
    """"""
    # IPY ***
    '''
    #EXPNAME='20091105.003'; Is=102 #284+154+756+116
    
    EXPNAME='20091201.001'; Is=135 
    FILELIST_UP='/Volumes/AMISR_004/processed_data/PFISR/2009/IPY17/' + EXPNAME +'/IPY17-filelist_PL2-64157.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/PFISR/2009/IPY17/' + EXPNAME +'/IPY17-filelist_PL1-64157.txt'
    EXP='IPY17'
    ODIR='/Volumes/AMISR_004/processed_data/PFISR/2009/IPY17/' + EXPNAME +'/cal-0'
    DAT_PATH = ''
    DIR_SEARCH = 1
    beams2do=[64157]
    MAXFILES=5
    flip=0
    dualpl=1
    filterBySza=1
    Ifiles=[]   
    '''
    """
    ddir = '20150804.003'
    EXP = 'PLCal30'
    dddir = '/Volumes/ISR_DATA_02/processed_data/PFISR/2015/08/'
    FILELIST_UP= os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')
    FILELIST_DN = os.path.join(dddir,EXP,ddir,'filelist_PL2.txt')
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/PFISR_004/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=5
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]
    filterBySza=1
    """
    ddir = '20141121.003'#'20100626.003'
    #EXP = 'PLCal30'#'IPY17_3dt'
    EXP = 'PLCal30'
    dddir = '/Volumes/ISR_DATA_02/processed_data/PFISR/2014/11/'#'/Volumes/ISR_DATA_02/processed_data/PFISR/2010/06/'
    FILELIST_UP= os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')
    FILELIST_DN = os.path.join(dddir,EXP,ddir,'filelist_PL2.txt')
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/PFISR_004/Data AMISR Poker/' + ddir #'/Volumes/AMISR_012/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]#[]
    MAXFILES=5
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]
    filterBySza=0
    """"""
    """
    EXPNAME='20101101.002'; Is=0
    FILELIST_UP='/Volumes/AMISR_004/processed_data/PFISR/2010/IPY17/' + EXPNAME +'/IPY17-filelist_PL2-64157.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/PFISR/2010/IPY17/' + EXPNAME +'/IPY17-filelist_PL1-64157.txt'
    EXP='IPY17'
    ODIR='/Volumes/AMISR_004/processed_data/PFISR/2010/IPY17/' + EXPNAME +'/cal-0'
    DAT_PATH = ''
    DIR_SEARCH = 1
    beams2do=[64157]
    MAXFILES=5
    flip=0
    dualpl=1
    filterBySza=1
    Ifiles=[]    
    """

    """
    EXPNAME='20110413.001'; Is=0
    ddir = '/Volumes/ISR_DATA_01/processed_data/PFISR/2011/04/IPY17/'
    FILELIST_UP= os.path.join(ddir,EXPNAME,'filelist_PL2.txt')
    FILELIST_DN= FILELIST_UP; #os.path.join(ddir,EXPNAME,'filelist_PL1.txt')
    EXP='IPY17'
    ODIR= os.path.join(ddir,EXPNAME,'cal-0')
    DAT_PATH = os.path.join('/Volumes/AMISR_014/Data AMISR Poker',EXPNAME)
    DIR_SEARCH = 1
    beams2do=[]
    MAXFILES=5
    flip=0
    dualpl=0
    filterBySza=1
    Ifiles=[]    
    """
        
    # LTCS 01/17 - stopped at 157
    """
    '''
    FILELIST_UP='/Volumes/AMISR_004/processed_data/LTCS01/20080117.016/filelist_PL2.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/LTCS01/20080117.016/filelist_PL1.txt'
    EXP='LTCS01'
    ODIR='/Volumes/AMISR_004/processed_data/LTCS01/20080117.016' + '/cal-0'
    DAT_PATH = '/Volumes/AMISR_006/Data AMISR Poker'
    DIR_SEARCH = 1
    Is=157
    beams2do=[]
    MAXFILES=10
    '''
    ddir='20120112.001'
    EXP='LTCS31'    
    dddir = '/Volumes/ISR_DATA_02/processed_data/PFISR/2012/01/'
    FILELIST_UP= os.path.join(dddir,EXP,ddir,'filelist_PL2.txt')
    FILELIST_DN = os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')    
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/AMISR_015/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=987
    beams2do=[]	
    MAXFILES=20
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]
    filterBySza=1       
    
    """
    

    """
    # Erickson
    ddir='20100531.001'
    EXP='Erickson30'
    FILELIST_UP='/Volumes/AMISR_004/processed_data/PFISR/2010/'+EXP+'/' + ddir + '/filelist_PL2.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/PFISR/2010/'+EXP+'/' + ddir + '/filelist_PL1.txt'	
    ODIR='/Volumes/AMISR_004/processed_data/PFISR/2010/'+EXP+'/' + ddir + '/cal-0'
    DAT_PATH = '/Volumes/AMISR_012/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[64157]
    MAXFILES=10
    flip=0
    dualpl=1
    Ifiles=[]
    """

    """
    # SGrid 01
    ddir='20061213.003'
    FILELIST_UP='/Volumes/AMISR_004/processed_data/SGrid01/' + ddir + '/filelist_PL1.txt'
    FILELIST_DN='/Volumes/AMISR_004/processed_data/SGrid01/' + ddir + '/filelist_PL1.txt'	
    EXP='SGrid01'
    ODIR='/Volumes/AMISR_004/processed_data/SGrid01/' + ddir + '/cal-0'
    DAT_PATH = '/Volumes/AMISR_005/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=10	
    """
    
    # PlCal
    """
    ddir='20111108.002'
    EXP='nodt3IPY17'    
    dddir = '/Volumes/ISR_DATA_01/processed_data/PFISR/2011/11/'    
    FILELIST_UP = os.path.join(dddir,EXP,ddir,'filelist_PL2.txt')
    FILELIST_DN = os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')	
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/AMISR_015/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=5
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]    
    filterBySza=1            
    """

    # PlCal31
    """
    ddir='20111116.002'
    EXP='nodt3IPY17'    
    dddir = '/Volumes/ISR_DATA_01/processed_data/PFISR/2011/11/'
    FILELIST_UP= os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')
    FILELIST_DN=FILELIST_UP		
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/AMISR_015/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=5
    flip=0 # down is really up.
    dualpl=0
    Ifiles=[]    
    filterBySza=1            
    """
    
    # Themis31
    """
    ddir='20110201.002'
    EXP='WorldDay30'    
    dddir = '/Volumes/ISR_DATA_01/processed_data/PFISR/2011/02/'
    FILELIST_UP= os.path.join(dddir,EXP,ddir,'filelist_PL2.txt')
    FILELIST_DN=FILELIST_UP #os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')    
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/AMISR_014/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=10
    flip=0 # down is really up.
    dualpl=0
    Ifiles=[]
    filterBySza=1        
    """
    
    # PLCal30
    """
    ddir='20120218.003'
    EXP='PLCal30'    
    dddir = '/Volumes/ISR_DATA_02/processed_data/PFISR/2012/02/'
    FILELIST_UP= os.path.join(dddir,EXP,ddir,'filelist_PL2.txt')
    FILELIST_DN = os.path.join(dddir,EXP,ddir,'filelist_PL1.txt')    
    ODIR=os.path.join(dddir,EXP,ddir,'cal-0')
    DAT_PATH = '/Volumes/AMISR_015/Data AMISR Poker/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=5
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]
    filterBySza=0     
    """

    # RISR
    """
    ddir='20100707.002'
    EXP='WorldDay54m'
    FILELIST_UP='/Volumes/ISR_DATA/processed_data/RISR/2010/' +EXP+ '/'+ddir+'/filelist_PL2.txt'
    FILELIST_DN='/Volumes/ISR_DATA/processed_data/RISR/2010/' +EXP+ '/'+ ddir+'/filelist_PL1.txt'
    ODIR='/Volumes/ISR_DATA/processed_data/RISR/2010/' +EXP+ '/'+ ddir+'/cal-0'
    DAT_PATH = '/Volumes/AMISR_013/Data AMISR Resolute N/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[64280]	
    MAXFILES=5
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]	
    """

    # sondre
    """
    ddir='20110217.001'
    EXP='WorldDay55m'
    FILELIST_UP='/Users/mnicolls/Desktop/file2.txt'
    FILELIST_DN='/Users/mnicolls/Desktop/file1.txt'
    ODIR='/Users/mnicolls/Desktop/'
    DAT_PATH = '/Volumes/Sondre_002/' + ddir
    DIR_SEARCH = 1
    Is=0
    beams2do=[]	
    MAXFILES=4
    flip=0 # down is really up.
    dualpl=1
    Ifiles=[]	
    """

    htstart=250.0e3
    FR_OFF=[5.0e6,5.0e6]
    move_window = 0
    KS=[5,3]

    # get file list
    f=open(FILELIST_UP); files_up=f.readlines(); f.close()
    f=open(FILELIST_DN); files_dn=f.readlines(); f.close()
    if len(files_up) != len(files_dn):
        print len(files_up)
        print len(files_dn)
        print 'different number of up and down files!'
        sys.exit(-1)
    if DIR_SEARCH:
        files_up2=[]; files_dn2=[]
        for ifile in range(len(files_up)):
            files_up2.extend(glob.glob(os.path.join(DAT_PATH,files_up[ifile].rstrip('\n'))))
            files_dn2.extend(glob.glob(os.path.join(DAT_PATH,files_dn[ifile].rstrip('\n'))))
        files_up=files_up2
        files_dn=files_dn2
        if len(files_up) != len(files_dn):
            print len(files_up)
            print len(files_dn)
            print 'different number of up and down files!'
            sys.exit(-1)
        files_up.sort()
        files_dn.sort()
    
    if filterBySza:
        print 'Filtering by Solar Zenith Angle'
        
        try:
            files_up=scipy.loadtxt(os.path.join(ODIR,'SZAlist_up.txt'),dtype='string',delimiter='\n').tolist()
            files_dn=scipy.loadtxt(os.path.join(ODIR,'SZAlist_dn.txt'),dtype='string',delimiter='\n').tolist()
            
        except:
        
            Ifiles=[]
            pathath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/flip-chem/libflip.dylib'
            ct_flipchem=ctypes.CDLL(pathath) # spectra library
            for ifile in range(len(files_up)):
                try:
                #if 1==1:
                    t=read_a_datafile(files_up[ifile],p=['/Time','/Site'])
                    stime=t['/Time']['UnixTime'][0,0]
                    tmp1=datetime.datetime.utcfromtimestamp(stime); r1=datetime.date(tmp1.year,tmp1.month,tmp1.day)
                    dtime=(float(tmp1.hour)+float(tmp1.minute)/60.0+float(tmp1.second)/3600.0)
                    doy=int(r1.strftime('%j'))
                    YYYYDDD = "%.4d%.3d" %(tmp1.year,doy); YYYYDDD = int(YYYYDDD)
                    LTHRS,SZAD,DEC=flipchem.getltsza(ct_flipchem,YYYYDDD,dtime*3600.0,t['/Site']['Latitude'],t['/Site']['Longitude'])
                    print str(ifile) + ' - ' + str(SZAD)
                    if SZAD < 80.0:
                        Ifiles.append(ifile)
                except:
                    print "Problem...."
                        
    if len(Ifiles)>0:
        files_up=scipy.array(files_up)[Ifiles].tolist()
        files_dn=scipy.array(files_dn)[Ifiles].tolist()

    if filterBySza:
        scipy.savetxt(os.path.join(ODIR,'SZAlist_dn.txt'),files_dn,fmt='%s')
        scipy.savetxt(os.path.join(ODIR,'SZAlist_up.txt'),files_up,fmt='%s')

    files_up=files_up[Is:]
    files_dn=files_dn[Is:]

    NFILES=len(files_up)
    for ir in range(NFILES):
        files_up[ir]=files_up[ir].rstrip('\n')
        files_dn[ir]=files_dn[ir].rstrip('\n')
        
    pylab.figure()
    #pylab.show()
    pylab.close('all')
    figg1=pylab.figure();

    done=0; IIfile=0
    while not done:
        tfiles_up=[]
        tfiles_dn=[]
        
        print "Spot: %d of %d" % (IIfile,len(files_up))
        tfiles_up.append(files_up[IIfile])
        tfiles_dn.append(files_dn[IIfile])
        tmp=tfiles_up[0].split('/'); texp=tmp[-2]; tmp=tmp[-1]; tmp=int(tmp[1:-7]); tmp=tmp+1
        
        sett=0
        while not sett:
            IIfile=IIfile+1
            if (IIfile<NFILES) and (len(tfiles_up)<=MAXFILES):
                ttexp=files_up[IIfile].split('/'); ttexp=ttexp[-2]
                if (ttexp==texp) and (files_up[IIfile][:-7].endswith('%(#)06d' % {"#": tmp})):
                    tfiles_up.append(files_up[IIfile])
                    tfiles_dn.append(files_dn[IIfile])
                    tmp=tmp+1
                else:
                    sett=1
            else:
                sett=1
            
        
            
        if IIfile>=NFILES:
            done=1
                        
        # loop over plasma line files
        for Ifile in range(len(tfiles_up)):
                        
            # read file(s)
            output1=read_a_datafile(tfiles_up[Ifile])
            if dualpl:
                if tfiles_dn[Ifile]==tfiles_up[Ifile]:
                    output2=output1.copy()
                else:
                    output2=read_a_datafile(tfiles_dn[Ifile])

            if Ifile==0:
                if flipAuto:
                    flip=scipy.sign(output1['/Rx']['Frequency'][0,0]-output1['/Tx']['Frequency'][0,0])
                    print flip
                    if flip<0:
                        flip=1
                    else:
                        flip=0
            
                rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
                if 1==1:
                    fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
                    if fr_off==output1['/Tx']['Frequency'][0,0]:
                        fr_off=FR_OFF[0]	
                else:
                    fr_off=FR_OFF[0]
                freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
                
                
                try:
                    BeamcodeMap=scipy.array(output1['/Setup']['BeamcodeMap'])
                except:
                    az=scipy.mean(output1['/Antenna']['Azimuth'])
                    el=scipy.mean(output1['/Antenna']['Elevation'])
                    BeamcodeMap=scipy.array([[32768,az,el,0.0]])
                
                pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
                bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
                time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
                
                if dualpl:
                    if 1==1:
                        fr_off=scipy.absolute(output2['/Tx']['Frequency'][0,0]-output2['/Rx']['Frequency'][0,0])
                        if fr_off==output1['/Tx']['Frequency'][0,0]:
                            fr_off=FR_OFF[1]
                    else:
                        fr_off=FR_OFF[1]
                    freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
                    pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
                    bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
                    time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
            else:
                pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
                bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
                time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
                if dualpl:
                    pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
                    bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
                    time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)

        beamcodes=scipy.sort(bmcodes_up[0,:])
        Nbeams=beamcodes.shape[0]
        BMCODES=scipy.zeros((Nbeams,4),dtype='Float64') # beamcode table (beamcode,az,el,ksys)
        for i in range(Nbeams):
            try:
                I=scipy.where(BeamcodeMap[:,0]==beamcodes[i])[0]
                BMCODES[i,:]=BeamcodeMap[I,:]
            except:
                BMCODES[i,:]=[0.0,0.0,0.0,0.0]
                xxxx
                
        # deal the data
        pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
        if dualpl:
            pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

        
        Ntimes=time_up.size
        Nfreqs=freq_up.size

        if len(beams2do)==0:
            beams2do=BMCODES[:,0]

            
        # loop over beams
        for jjj in range(Nbeams):

            # get beamcode
            bmcode=BMCODES[jjj,:]
            rrI=scipy.where(bmcode[0]==beams2do)[0]
                    
            if len(rrI)>=1:
            
                #Altitude summation
                altSum=numpy.array((235e3,270e3))
                nalt = -3
                
                #raw_input('Ready? hit enter')
                figg1.clf()
                
                
                #
                alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)
                
                #Ialt = scipy.where( (altSum[0] < alt_pl[:]) & (alt_pl[:] < altSum[-1]))
                
                # subtract highest altitude
                
                tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
                
                tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-1][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2)
                '''
                tpl_spect_up_noise = scipy.repeat(scipy.mean(tpl_spect_up[:,:,-1+nalt:],axis=2)[:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2)*scipy.size(Ialt)
                tpl_spect_up= scipy.squeeze(scipy.sum(tpl_spect_up[:,:,Ialt],axis=3))
                tpl_spect_up = tpl_spect_up-tpl_spect_up_noise
                '''
                tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
                
                
                if dualpl:
                    tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
                    tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-1][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
                    '''
                    tpl_spect_dn_noise = scipy.repeat(scipy.mean(tpl_spect_dn[:,:,-1+nalt:],axis=2)[:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)*scipy.size(Ialt)
                    tpl_spect_dn= scipy.squeeze(scipy.sum(tpl_spect_dn[:,:,Ialt],axis=3))
                    tpl_spect_dn = tpl_spect_dn-tpl_spect_dn_noise
                    '''
                    tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
 


                # find plasma line spectra closest to peak
                tmp=scipy.absolute(alt_pl-htstart)
                Ialt=scipy.where(tmp==tmp.min())[0]
                pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
                if dualpl:
                    pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
                if dualpl:
                    if flip==0:
                        pl_dn=scipy.fliplr(pl_dn)
                if flip==1:
                        pl_up=scipy.fliplr(pl_up) # down is really up!!

                START_TIME=time_up[0]

                tmp1=datetime.datetime.utcfromtimestamp(time_up[0]); r1=datetime.datetime(tmp1.year,tmp1.month,tmp1.day, tmp1.hour, tmp1.minute, tmp1.second)
                x_up=tmp1.strftime('Secs from %D %H:%M:%S')

                pylab.figure(figg1.number)
                pylab.subplot(211)
                pylab.imshow(pl_up,vmin=0,vmax=.5e7,aspect='auto',origin='lower',extent=[freq_up[0]/1e6,freq_up[-1]/1e6,0,time_up[-1]-time_up[0]])
                pylab.colorbar()
                ax1=pylab.gca()
                ax1.set_ylabel(x_up)
                ax1.set_xlabel('Freq (MHz)')
                pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
               
                if dualpl:
                    pylab.subplot(212)
                    pylab.imshow(pl_dn,vmin=0,vmax=0.5e7,aspect='auto',origin='lower',extent=[freq_dn[0]/1e6,freq_dn[-1]/1e6,0.0,time_dn[-1]-time_dn[0]])
                    pylab.colorbar()
                    ax2=pylab.gca()
                    #ax2.set_ylabel('Time (secs)')
                    ax2.set_xlabel('Freq (MHz)')

                #pylab.show()

                clim=[0.1e8,0.1e8]
#				clim=[1e8,1e8]
                not_ok=1
                while not_ok:

                    info=raw_input("Scale OK? Yes hit enter, No enter new clim.u/d to go down/up in alt.cfilt/mfilt for filters.")
                    if info=='':
                        not_ok=0
                    else:
                        if (info=='u') or (info=='d'):
                            if info=='u':
                                Ialt=Ialt+1					
                            elif info=='d':
                                Ialt=Ialt-1	
                                                    
                            pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
                            if dualpl:
                                pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
                            if dualpl:
                                if flip==0:
                                    pl_dn=scipy.fliplr(pl_dn)
                            if flip==1:
                                pl_up=scipy.fliplr(pl_up) # down is really up!!
                            pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
                            if dualpl:
                                pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)
                            pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
                            if dualpl:
                                pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)
                        elif (info=='mfilt'):					
                            pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
                            if dualpl:
                                pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)
                        elif (info=='cfilt'):
                            pl_up,b=canny(pl_up, 0.0,0.0)
                            if dualpl:
                                pl_dn,b=canny(pl_dn, 0.0,0.0)
                        elif (info=='gfilt'):
                            pl_up = scipy.ndimage.filters.median_filter(pl_up, 7)
                            for i in range(3):
                                pl_up = scipy.ndimage.filters.gaussian_filter(pl_up,1, mode='nearest')
                            if dualpl:
                                pl_dn = scipy.ndimage.filters.median_filter(pl_dn, 7)
                                for i in range(3):
                                    pl_dn = scipy.ndimage.filters.gaussian_filter(pl_dn,1, mode='nearest')
                        elif (info=='flip'):
                            pl_up=scipy.fliplr(pl_up)
                            if dualpl:
                                pl_dn=scipy.fliplr(pl_dn)
                        else:
                            try:
                                clim=eval('['+info+']')
                            except:
                                print 'invalid input'	
                    
                        try:
                            pylab.clf()
                            pylab.subplot(211)
                            pylab.imshow(pl_up,vmin=0,vmax=clim[0],aspect='auto',origin='lower',extent=[freq_up[0]/1e6,freq_up[-1]/1e6,0.0,time_up[-1]-time_up[0]])	
                            pylab.colorbar()
                            ax1=pylab.gca()
                            ax1.set_ylabel(x_up)
                            ax1.set_xlabel('Freq (MHz)')
                            pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
                       
                            if dualpl:
                                pylab.subplot(212)
                                pylab.imshow(pl_dn,vmin=0,vmax=clim[1],aspect='auto',origin='lower',extent=[freq_dn[0]/1e6,freq_dn[-1]/1e6,0.0,time_dn[-1]-time_dn[0]])
                                pylab.colorbar()
                                ax2=pylab.gca()
                                #ax2.set_ylabel('Time (secs)')
                                ax2.set_xlabel('Freq (MHz)')
                        except:
                            print 'invalid input'	
                            
                Cplup=iplot()
                a=pylab.connect('button_press_event', Cplup)
                pylab.axes(ax1)
                info=raw_input('Doing top plot. Hit enter when done.')
                pylab.disconnect(a)

                if dualpl:
                    Cpldn=iplot()
                    a=pylab.connect('button_press_event', Cpldn)
                    pylab.axes(ax2)
                    info=raw_input('Doing bottom plot. Hit enter when done.')
                    pylab.disconnect(a)
            
                if ((len(Cplup.pl_x)>0) or (dualpl and (len(Cpldn.pl_x)>0))):# or (1==1)):
                    
                    oname='%s-%d-plline*.txt' % (EXP,bmcode[0]) 
                    g=glob.glob(os.path.join(ODIR,oname))
                    oname='%s-%d-plline-%.3d' % (EXP,bmcode[0],len(g)) 
                    
                    try:
                        figg1.savefig(os.path.join(ODIR,oname+'.png'))
                    except:
                        print 'Could not save plot'				
                    
                    try:
                        fH=open(os.path.join(ODIR,oname+'.txt'),'w') 
                        fH.write('%d %f %f %f %f\n' % (bmcode[0],bmcode[1],bmcode[2],bmcode[3],alt_pl[Ialt]/1000.0))
                        fH.write('%f\n' % (START_TIME))

                        if len(Cplup.pl_x)>0:
                            fH.write('\n')
                            for aa in range(len(Cplup.pl_x)):
                                fH.write('%.1f %.3f\n' % (Cplup.pl_y[aa],Cplup.pl_x[aa]))
            
                        if dualpl and (len(Cpldn.pl_x)>0):
                            fH.write('\n')
                            for aa in range(len(Cpldn.pl_x)):
                                fH.write('%.1f %.3f\n' % (Cpldn.pl_y[aa],Cpldn.pl_x[aa]))
            
                        fH.close()

                    except:
                        print 'Could not write file'
