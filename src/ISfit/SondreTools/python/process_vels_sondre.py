#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import os, copy
import sys
import math
import scipy
import ctypes 
import scipy.fftpack
import scipy.interpolate
import scipy.optimize

import struct
import time
import datetime
import array
import glob
import tables
import matplotlib
matplotlib.use('Agg')
import pylab


import os
fitter_base_path = os.environ['AMISR_FITTER_PATH'].split('AMISR_fitter_py')[0]
amisr_tools_path = os.path.join(fitter_base_path,'AMISRtools/python')
fitter_path = os.path.join(fitter_base_path,'AMISR_fitter_py/src')
sys.path.append(amisr_tools_path)
sys.path.append(fitter_path)

#sys.path.append('/Volumes/mnicolls/Documents/Work/ISfit/AMISRtools/python')
#sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import vvels
import plot_utils

CMAP='RdBu'

# some natural constants
v_lightspeed=299792458
v_Boltzmann=1.380658e-23
v_electronmass=9.1093897e-31
v_amu=1.6605402e-27
v_electronradius=2.81794092e-15
v_epsilon0=8.854188E-12
v_elemcharge=1.602177E-19
pi=math.pi

def gmag2geo_covar(input,dec_all,dip_all,direction=0):

    # Create Jacobian matrix
    Jac=scipy.zeros(input.shape,dtype=input.dtype)
    
    if direction==0:
        for ii in range(dec_all.size):
            dec=dec_all[ii]
            dip=dip_all[ii]
            Rgmag=scipy.array([
                [scipy.cos(dec),scipy.sin(dip)*scipy.sin(dec),-scipy.cos(dip)*scipy.sin(dec)],	# east
                [-scipy.sin(dec),scipy.cos(dec)*scipy.sin(dip),-scipy.cos(dip)*scipy.cos(dec)], # north
                [0.0,scipy.cos(dip),scipy.sin(dip)]])											# up
            Jac[ii*3:ii*3+3,ii*3:ii*3+3]=Rgmag
            
    elif direction==1:
        for ii in range(dec_all.size):
            dec=dec_all[ii]
            dip=dip_all[ii]
            Rgeo=scipy.array([
                [scipy.cos(dec),-scipy.sin(dec),0.0],	# perp east
                [scipy.sin(dec)*scipy.sin(dip),scipy.cos(dec)*scipy.sin(dip),scipy.cos(dip)], # perp north
                [-scipy.sin(dec)*scipy.cos(dip),-scipy.cos(dec)*scipy.cos(dip),scipy.sin(dip)]]) # par
            Jac[ii*3:ii*3+3,ii*3:ii*3+3]=Rgeo

    Jac=scipy.matrix(Jac)
    covar=Jac*scipy.matrix(input)*scipy.transpose(Jac)

    return covar

def gmag2geo(input,dec,dip,direction=0):
    # direction:
    # 0 - gmag2geo
    # 1 - geo2gmag

    output=[]
    if direction==0:
        Rgmag=scipy.matrix([
            [scipy.cos(dec),scipy.sin(dip)*scipy.sin(dec),-scipy.cos(dip)*scipy.sin(dec)],	# east
            [-scipy.sin(dec),scipy.cos(dec)*scipy.sin(dip),-scipy.cos(dip)*scipy.cos(dec)], # north
            [0.0,scipy.cos(dip),scipy.sin(dip)]])											# up
        output=Rgmag*scipy.matrix(input)

    elif direction==1:
        Rgeo=scipy.matrix([
            [scipy.cos(dec),-scipy.sin(dec),0.0],	# perp east
            [scipy.sin(dec)*scipy.sin(dip),scipy.cos(dec)*scipy.sin(dip),scipy.cos(dip)], # perp north
            [-scipy.sin(dec)*scipy.cos(dip),-scipy.cos(dec)*scipy.cos(dip),scipy.sin(dip)]]) # par
        output=Rgeo*scipy.matrix(input)
    
    return output
    

def getmlt(time,plong):
    ct_aacgm=ctypes.CDLL('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/aacgm/libaacgm.1.09.dylib') # AACGM library
    os.putenv('AACGM_DAT_PREFIX','/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/aacgm/aacgm_coeffs')
    ct_aacgm.AACGMConvertMLT.restype=ctypes.c_double

    MLTtime=scipy.zeros(time.size,dtype='float64')

    for aa in range(time.size):
        tmp=datetime.datetime.utcfromtimestamp(time[aa])
        r1=datetime.date(tmp.year,tmp.month,tmp.day)
        doy=int(r1.strftime('%j'))
        secs=doy*24*3600+tmp.hour*3600+tmp.minute*60+tmp.second
        MLTtime[aa]=ct_aacgm.AACGMConvertMLT(ctypes.c_int(tmp.year),ctypes.c_int(secs),ctypes.c_double(plong))
        if aa>0:
            if MLTtime[aa]<MLTtime[aa-1]:
                MLTtime[aa]=MLTtime[aa]+24.0

    return MLTtime
    
def readafile(fname):

    h5file=tables.open_file(fname)
    output={}
    for group in h5file.walk_groups("/"):
        output[group._v_pathname]={}
        for array in h5file.list_nodes(group, classname = 'Array'):						
            output[group._v_pathname][array.name]=array.read()		
    h5file.close()
    
    return output
    
def write_outputfile(fhandle,dict2do,keys2do=[],groupname='',name=''):

    if groupname == '':
        group=fhandle.root
    else:
        if fhandle.__contains__('/'+groupname):
            group='/'+groupname
        else:
            group=fhandle.create_group(fhandle.root, groupname, 'Dataset')

    if len(keys2do)==0:
        try:
            fhandle.remove_node(group,name)
        except:
            ''
        fhandle.create_array(group,name, dict2do, "Dataset")
    else:
        for key in keys2do:
            try:
                fhandle.remove_node(group,key)
            except:
                ''		
            if type(dict2do[key]) is long:
                fhandle.create_array(group, key, int(dict2do[key]), "Dataset")
            else:
                fhandle.create_array(group, key, dict2do[key], "Dataset")

def dovelsAlt(fnameIn='',makeplot=1,plot2=0,zoom=[],saveout=1,clim=[-1000,1000], \
    sc=[15.0,15.0],swaphr=0,MinAlt=175.0,MaxAlt=450.0,PPP=[200.0,0.5,2000.0,100.0],COVAR=[3000.*3000.,3000.*3000.,15.*15.], \
    zoomWhole=[],extraApp='',neMin=2.0e10,CHIRP=0.0):

    if fnameIn!='':
        fname=fnameIn
        
    ofname='%s-vvelsAlt%s' % (os.path.basename(fname)[:-3],extraApp)
    
    odir=os.path.join(os.path.dirname(fname),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    # read data
    dat1=readafile(fname)

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
    Bmed = scipy.nanmedian(Babs)
    for i in range(Bmed.ndim):
        Bmed=scipy.nanmedian(Bmed)		

    # fitted params
    ht=scipy.squeeze(dat1['/FittedParams']['Altitude'])
    Range=scipy.squeeze(dat1['/FittedParams']['Range'])
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+CHIRP
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
    ne1=dat1['/FittedParams']['Ne']

    # time
    time1=dat1['/Time']['UnixTime']
    doy1=dat1['/Time']['doy']
    dtime1=dat1['/Time']['dtime']+(doy1-doy1[0,0])*24.0
    MLT=dat1['/Time']['MagneticLocalTimeSite']
    yr=dat1['/Time']['Year']
    mon=dat1['/Time']['Month']
    day=dat1['/Time']['Day']
    
    # low densities
    I=scipy.where((ne1<neMin))
    vlos1[I]=scipy.nan
    dvlos1[I]=scipy.nan
    
    # just do a portion of data
    if len(zoomWhole)!=0:
        I=scipy.where((dtime1[:,0]>=zoomWhole[0]) & (dtime1[:,1]<=zoomWhole[1]))[0]	
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
    yr=yr[0,0]
    mon=mon[0,0]
    day=day[0,0]
    title=' %d-%d-%d' % (mon, day, yr)
    
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
        for irng in range(Nrngs):
            tht = ht[Irecs[Iel],irng]
            if scipy.isfinite(tht):
                htin=[]; vlosin=[]; dvlosin=[]; kin=[]; bin=[];
                for ism in range(len(Irecs)):
                    Ihts = scipy.where(scipy.isfinite(ht[Irecs[ism],:]))[0]
                    Iht=scipy.argmin(scipy.absolute(ht[Irecs[ism],:][Ihts]-tht))
                    Iht=Ihts[Iht]
                    htin.append(ht[Irecs[ism],Iht])
                    vlosin.append(vlos1[Irecs[ism],0,Iht])
                    dvlosin.append(dvlos1[Irecs[ism],0,Iht])					
                    kin.append([kpn[Irecs[ism],0,Iht],kpe[Irecs[ism],0,Iht],kpar[Irecs[ism],0,Iht]])
                    bin.append(Babs[Irecs[ism],0,Iht])
                htin=scipy.array(htin); vlosin=scipy.array(vlosin); dvlosin=scipy.array(dvlosin); kin=scipy.array(kin); bin=scipy.array(bin)
                (tAlt_out,tVest,tdVest,xx,tNmeas)=vvels.compute_velvec2([[0.0*1000,2000.0*1000]],vlosin,dvlosin,kin,htin,[],htin,htmin=0.0*1000,htmax=2000.0*1000,covar=COVAR,p=PPP)		
                Vest[irng,:]=tVest; dVest[irng,:]=tdVest
                Nmeas[irng]=tNmeas; Alts[irng]=scipy.mean(htin)
                Bavg[irng]=scipy.mean(bin)
                
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
        else:
            vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)
            Nall1=scipy.concatenate((Nall1,Nmeas[scipy.newaxis,:]),axis=0)
            Alt1=scipy.concatenate((Alt1,Alts[scipy.newaxis,:]),axis=0)
            Ball1=scipy.concatenate((Ball1,Bavg[scipy.newaxis,:]),axis=0)
            
        if scipy.mod(IIrec,100)==0 or done==1:
            print 'Record %d of %d' % (IIrec,Nrecs)
        IIrec=IIrec+1		

    # Electric field
    Eavg=scipy.zeros((Ball1.shape[0],1,2))*scipy.nan
    dEavg=scipy.zeros((Ball1.shape[0],1,2))*scipy.nan
    for itime in range(Eavg.shape[0]):
        Ialt=scipy.where((Alt1[itime,:]>=MinAlt*1000.0) & (Alt1[itime,:]<=MaxAlt*1000.0))[0]
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
        
    Param={}
    Param['MinAlt']=MinAlt*1000.0
    Param['MaxAlt']=MaxAlt*1000.0
    Param['Covar']=COVAR
    Param['ErrorElim']=PPP
    Param['SourceFile']=os.path.basename(fname)
    try: Param['PulseLength']=dat1['/ProcessingParams']['PulseLength']
    except: Param['PulseLength']=0.0
    try: Param['BaudLength']=dat1['/ProcessingParams']['BaudLength']
    except: Param['BaudLength']=0.0
    try: Param['RxFrequency']=dat1['/ProcessingParams']['RxFrequency']
    except: Param['RxFrequency']=0.0
    try: Param['TxFrequency']=dat1['/ProcessingParams']['TxFrequency']
    except: Param['TxFrequency']=0.0
    Param['ProcessingTime']=time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
        
    if saveout==1:
        print 'Writing output to ' + os.path.join(odir,ofname+'.h5')
        outh5file=tables.open_file(os.path.join(odir,ofname+'.h5'), mode = "a", title = "Fit File")
        # Time
        write_outputfile(outh5file,timeout,groupname='Time',name='UnixTime')
        write_outputfile(outh5file,dtimeout,groupname='Time',name='dtime')
        write_outputfile(outh5file,MLTtime1,groupname='Time',name='MagneticLocalTime')
        # vector velocities
        write_outputfile(outh5file,Nall1.astype('int32'),groupname='VectorVels',name='Nmeas')
        write_outputfile(outh5file,Eavg,groupname='VectorVels',name='AvgElectricField')
        write_outputfile(outh5file,dEavg,groupname='VectorVels',name='dAvgElectricField')
        write_outputfile(outh5file,vvels1,groupname='VectorVels',name='Vest')
        write_outputfile(outh5file,dvvels1,groupname='VectorVels',name='dVest')
        write_outputfile(outh5file,Vmag,groupname='VectorVels',name='Vmag')
        write_outputfile(outh5file,dVmag,groupname='VectorVels',name='dVmag')
        write_outputfile(outh5file,Vdir,groupname='VectorVels',name='Vdir')
        write_outputfile(outh5file,dVdir,groupname='VectorVels',name='dVdir')
        write_outputfile(outh5file,Alt1,groupname='VectorVels',name='Altitude')
        write_outputfile(outh5file,Param,keys2do=Param.keys(),groupname='ProcessingParams')
        write_outputfile(outh5file,dat1['/Site'],keys2do=dat1['/Site'].keys(),groupname='Site')
        outh5file.close()
        
    if makeplot==1:	
        talt=scipy.nanmedian(Alt1,axis=0)
        # velocity vector
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
            title='Vector Velocities' + title,p=PPP,sc=sc[0],cax=clim,ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2],vzsc=10.0,label='Altitude (km)')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vvec.png'))
        # velocity magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),talt/1e3,Vmag,Vdir,dVmag,dVdir,
            title='Vector Velocities' + title,cax1=[0.0,clim[1]],cax2=[-180.0,180.0],label='Altitude (km)')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vmag.png'))
    
    return
        
def dovels(fnameIn='',makeplot=1,plot2=0,zoom=[],saveout=1,plats=[[70.0,80.0,0.25,0.25],[]],clim=[-1000,1000], \
    sc=[15.0,15.0],swaphr=0,MinAlt=175.0,MaxAlt=450.0,PPP=[200.0,0.5,2000.0,100.0],COVAR=[3000.*3000.,3000.*3000.,15.*15.], \
    zoomWhole=[],extraApp='',neMin=2.0e10,CHIRP=0.0):
    
    if fnameIn!='':
        fname=fnameIn
        
    ofname='%s-vvels%s' % (os.path.basename(fname)[:-3],extraApp)

    odir=os.path.join(os.path.dirname(fname),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    # magnetic latitudes
    x=scipy.arange(plats[0][0],plats[0][1],plats[0][3])[:,scipy.newaxis]
    PLAT_OUT=scipy.concatenate((x,x+plats[0][2]),axis=1)
    if len(plats[1])>0:
        x=scipy.arange(plats[1][0],plats[1][1],plats[1][3])[:,scipy.newaxis]
        x=scipy.concatenate((x,x+plats[1][2]),axis=1)
        PLAT_OUT=scipy.concatenate((PLAT_OUT,x),axis=0)
    #print PLAT_OUT

    # read data
    dat1=readafile(fname)
    
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
    Bmed = scipy.nanmedian(Babs)
    for i in range(Bmed.ndim):
        Bmed=scipy.nanmedian(Bmed)		
    PPPe=(scipy.array(PPP).copy()).tolist(); PPPe[0]*=Bmed; PPPe[2]*=Bmed; PPPe[3]*=Bmed
    COVARe=(scipy.array(COVAR).copy()).tolist(); COVARe[0]*=Bmed*Bmed; COVARe[1]*=Bmed*Bmed; COVARe[2]*=Bmed*Bmed
    
    # fitted params
    ht=scipy.squeeze(dat1['/FittedParams']['Altitude'])
    Range=scipy.squeeze(dat1['/FittedParams']['Range'])
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+CHIRP
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
    ne1=dat1['/FittedParams']['Ne']

    # time
    time1=dat1['/Time']['UnixTime']
    doy1=dat1['/Time']['doy']
    dtime1=dat1['/Time']['dtime']+(doy1-doy1[0,0])*24.0
    MLT=dat1['/Time']['MagneticLocalTimeSite']
    yr=dat1['/Time']['Year']
    mon=dat1['/Time']['Month']
    day=dat1['/Time']['Day']
    
    # low densities
    I=scipy.where((ne1<neMin))
    vlos1[I]=scipy.nan
    dvlos1[I]=scipy.nan
    
    # just do a portion of data
    if len(zoomWhole)!=0:
        I=scipy.where((dtime1[:,0]>=zoomWhole[0]) & (dtime1[:,1]<=zoomWhole[1]))[0]	
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
    yr=yr[0,0]
    mon=mon[0,0]
    day=day[0,0]
    title=' %d-%d-%d' % (mon, day, yr)

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
            IrecsScan1=range(Irec,Irec+I[0])

        # get scan 2 records
        Irec=IrecsScan1[-1]+1
        I=scipy.where((Event[Irec:] != Event[Irec]))[0]
        if len(I)>0:
            IrecsScan2=range(Irec,Irec+I[0])
        else:
            IrecsScan2=range(Irec,time1.shape[0])
            done=1
            
        Irecs=IrecsScan1
        Irecs.extend(IrecsScan2)
        
        if Irecs[-1] > scipy.shape(vlos1)[0]-1:
            Irecs = Irecs[:-1]
        # line of sight velocities and errors
        tvlos=vlos1[Irecs,:,:]
        tdvlos=dvlos1[Irecs,:,:]
        vlosin=scipy.reshape(scipy.squeeze(tvlos),(Nrngs*len(Irecs)))
        dvlosin=scipy.reshape(scipy.squeeze(tdvlos),(Nrngs*len(Irecs)))
        
        igood = scipy.sum(scipy.isfinite(vlosin))
        
        # input params
        kin=scipy.zeros((len(Irecs),1,Nrngs,3),dtype=kpn.dtype)
        platin=scipy.zeros((len(Irecs),1,Nrngs))
        plongin=scipy.zeros((len(Irecs),1,Nrngs)) 
        bin=scipy.zeros((len(Irecs),1,Nrngs)) 
        kin[:,:,:,0]=kpn[Irecs]
        kin[:,:,:,1]=kpe[Irecs]
        kin[:,:,:,2]=kpar[Irecs]
        platin[:,:,:]=plat[Irecs]
        plongin[:,:,:]=plong[Irecs]
        bin[:,:,:]=Babs[Irecs]

        """
        for i in range(len(Irecs)):
            kin[i,0,:,0]=scipy.interpolate.interp1d(RangeGmag,kpn[Irecs[i],:])(Range)
            kin[i,0,:,1]=scipy.interpolate.interp1d(RangeGmag,kpe[Irecs[i],:])(Range)
            kin[i,0,:,2]=scipy.interpolate.interp1d(RangeGmag,kpar[Irecs[i],:])(Range)
            platin[i,0,:]=scipy.interpolate.interp1d(RangeGmag,plat[Irecs[i],:])(Range)
            plongin[i,0,:]=scipy.interpolate.interp1d(RangeGmag,plong[Irecs[i],:])(Range)
        """
        kin=scipy.reshape(kin,(len(Irecs)*Nrngs,3))
        platin=scipy.reshape(platin,(len(Irecs)*Nrngs))
        plongin=scipy.reshape(plongin,(len(Irecs)*Nrngs))
        htin=scipy.reshape(ht[Irecs,:],(len(Irecs)*Nrngs))
        bin=scipy.reshape(bin,(len(Irecs)*Nrngs))

        # compute vectors
        (plat_out1,Vest,dVest,xx,Nmeas)=vvels.compute_velvec2(PLAT_OUT,vlosin,dvlosin,kin,platin,plongin,htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVAR,p=PPP)
        (plat_out1,tEest,tdEest,xx,Nmeas1)=vvels.compute_velvec2(PLAT_OUT,vlosin*bin,dvlosin*bin,kin,platin,plongin,htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVARe,p=PPPe)
        Eest=scipy.zeros(tEest.shape)*scipy.nan; dEest=scipy.zeros(tEest.shape)*scipy.nan;
        Eest[:,0]=-tEest[:,1] # Enorth = -Veast*B
        Eest[:,1]=tEest[:,0] # Eeast = Vnorth*B
        dEest[:,0]=tdEest[:,1]; dEest[:,1]=tdEest[:,0]

        timeout.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
        dtimeout.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])
        MLTtime1.append([MLT[Irecs[0],0],MLT[Irecs[-1],1]])
        if IIrec>0:
            if MLTtime1[IIrec][0]<MLTtime1[IIrec-1][0]:
                MLTtime1[IIrec][0]=MLTtime1[IIrec][0]+24.0
                MLTtime1[IIrec][1]=MLTtime1[IIrec][1]+24.0

        if IIrec==0:
            vvels1=Vest[scipy.newaxis,:,:]
            dvvels1=dVest[scipy.newaxis,:,:]
            Nall1=Nmeas[scipy.newaxis,:]
            evec1=Eest[scipy.newaxis,:,:]
            devec1=dEest[scipy.newaxis,:,:]
        else:
            vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)
            Nall1=scipy.concatenate((Nall1,Nmeas[scipy.newaxis,:]),axis=0)
            evec1=scipy.concatenate((evec1,Eest[scipy.newaxis,:,:]),axis=0)
            devec1=scipy.concatenate((devec1,dEest[scipy.newaxis,:,:]),axis=0)

        print IIrec
        IIrec=IIrec+1		
        
    MLTtime1=scipy.array(MLTtime1)
    timeout=scipy.array(timeout)
    dtimeout=scipy.array(dtimeout)
    
    Vmag = scipy.sqrt( scipy.power(vvels1[:,:,0],2.0) + scipy.power(vvels1[:,:,1],2.0) ).real
    dVmag = scipy.sqrt( scipy.power(dvvels1[:,:,0],2.0)*scipy.power(vvels1[:,:,0]/Vmag,2.0) + scipy.power(dvvels1[:,:,1],2.0)*scipy.power(vvels1[:,:,1]/Vmag,2.0) ).real
    Vdir = 180.0/pi*scipy.arctan2(vvels1[:,:,1],vvels1[:,:,0]).real
    dVdir=180.0/pi*((1.0/scipy.absolute(vvels1[:,:,0]))*(1.0/(1.0+scipy.power(vvels1[:,:,1]/vvels1[:,:,0],2.0)))*scipy.sqrt(scipy.power(dvvels1[:,:,1],2.0)+scipy.power(vvels1[:,:,1]/vvels1[:,:,0]*dvvels1[:,:,0],2.0))).real
        
    Emag = scipy.sqrt( scipy.power(evec1[:,:,0],2.0) + scipy.power(evec1[:,:,1],2.0) ).real
    dEmag = scipy.sqrt( scipy.power(devec1[:,:,0],2.0)*scipy.power(evec1[:,:,0]/Emag,2.0) + scipy.power(devec1[:,:,1],2.0)*scipy.power(evec1[:,:,1]/Emag,2.0) ).real
    Edir = 180.0/pi*scipy.arctan2(evec1[:,:,1],evec1[:,:,0]).real
    dEdir=180.0/pi*((1.0/scipy.absolute(evec1[:,:,0]))*(1.0/(1.0+scipy.power(evec1[:,:,1]/evec1[:,:,0],2.0)))*scipy.sqrt(scipy.power(devec1[:,:,1],2.0)+scipy.power(evec1[:,:,1]/evec1[:,:,0]*devec1[:,:,0],2.0))).real		

    Param={}
    Param['MinAlt']=MinAlt*1000.0
    Param['MaxAlt']=MaxAlt*1000.0
    Param['Covar']=COVAR
    Param['ErrorElim']=PPP
    Param['CovarEfield']=COVARe
    Param['ErrorElimEfield']=PPPe
    Param['SourceFile']=os.path.basename(fname)
    try: Param['PulseLength']=dat1['/ProcessingParams']['PulseLength']
    except: Param['PulseLength']=0.0
    try: Param['BaudLength']=dat1['/ProcessingParams']['BaudLength']
    except: Param['BaudLength']=0.0
    try: Param['RxFrequency']=dat1['/ProcessingParams']['RxFrequency']
    except: Param['RxFrequency']=0.0
    try: Param['TxFrequency']=dat1['/ProcessingParams']['TxFrequency']
    except: Param['TxFrequency']=0.0
    Param['ProcessingTime']=time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())
        
    if saveout==1:
        print 'Writing output to ' + os.path.join(odir,ofname+'.h5')
        outh5file=tables.open_file(os.path.join(odir,ofname+'.h5'), mode = "a", title = "Fit File")
        # Time
        write_outputfile(outh5file,timeout,groupname='Time',name='UnixTime')
        write_outputfile(outh5file,dtimeout,groupname='Time',name='dtime')
        write_outputfile(outh5file,MLTtime1,groupname='Time',name='MagneticLocalTime')
        # vector velocities
        write_outputfile(outh5file,Nall1.astype('int32'),groupname='VectorVels',name='Nmeas')
        write_outputfile(outh5file,vvels1,groupname='VectorVels',name='Vest')
        write_outputfile(outh5file,dvvels1,groupname='VectorVels',name='dVest')
        write_outputfile(outh5file,Vmag,groupname='VectorVels',name='Vmag')
        write_outputfile(outh5file,dVmag,groupname='VectorVels',name='dVmag')
        write_outputfile(outh5file,Vdir,groupname='VectorVels',name='Vdir')
        write_outputfile(outh5file,dVdir,groupname='VectorVels',name='dVdir')
        write_outputfile(outh5file,PLAT_OUT,groupname='VectorVels',name='Plat')
        write_outputfile(outh5file,evec1,groupname='VectorVels',name='Eest')
        write_outputfile(outh5file,devec1,groupname='VectorVels',name='dEest')
        write_outputfile(outh5file,Emag,groupname='VectorVels',name='Emag')
        write_outputfile(outh5file,dEmag,groupname='VectorVels',name='dEmag')
        write_outputfile(outh5file,Edir,groupname='VectorVels',name='Edir')
        write_outputfile(outh5file,dEdir,groupname='VectorVels',name='dEdir')
        write_outputfile(outh5file,Param,keys2do=Param.keys(),groupname='ProcessingParams')
        write_outputfile(outh5file,dat1['/Site'],keys2do=dat1['/Site'].keys(),groupname='Site')
        outh5file.close()
        
    if makeplot==1:	
        # velocity vector
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
            title='Vector Velocities' + title,p=PPP,sc=sc[0],cax=clim,ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2],vzsc=10.0)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vvec.png'))
        # velocity magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,Vmag,Vdir,dVmag,dVdir,
            title='Vector Velocities' + title,cax1=[0.0,clim[1]],cax2=[-180.0,180.0])
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vmag.png'))
            
        # electric field
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,evec1[:,:,1]*1e3,evec1[:,:,0]*1e3,devec1[:,:,1]*1e3,devec1[:,:,0]*1e3,
            title='Electric Fields' + title,p=[PPPe[0]*1e3,PPPe[1],PPPe[2]*1e3,PPPe[3]*1e3],sc=sc[0]*5e-2,cax=[clim[0]*5e-2,clim[1]*5e-2],ncols=2,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-evec.png'))
        # electric field magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,Emag*1e3,Edir,dEmag*1e3,dEdir,
            title='Electric Fields' + title,cax1=[0.0,clim[1]*5e-2],cax2=[-180.0,180.0],units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-emag.png'))


def plot_vvels1(time,MLTtime,lat,vx,vy,dvx,dvy,title='Vector Vels',units='m/s',parm='V',\
    p=[200.0,0.25,4000.0,500.0],sc=15.0,cax=[-1000,1000],label='Mag. Lat. (degrees)',\
    ncols=2,vz=[],dvz=[],vzsc=1.0,nrows=4,geo=0,doQuiv=1):

    vx=vx.copy()
    vy=vy.copy()
    dvx=dvx.copy()
    dvy=dvy.copy()
    if ncols==3:
        vz=vz.copy()*vzsc
        dvz=dvz.copy()*vzsc
    
    if lat.ndim==2:
        lat2=scipy.nanmean(lat,axis=1)
        lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
    else:
        lat2=lat
    
    time2=scipy.mean(time,axis=1)

    textsize = 8        # size for axes text
    labsize = 10

    pylab.ioff()
    
    figBG   = 'w'        # the figure background color
    axesBG  = '#f6f6f6'  # the axies background color
    figsz = (7,9)
    if ncols==3:
        figsz = (11,12)

    dx, dy= 0.015, 0.05	
    POS=[0.1,0.75,1.0/(ncols+0.6)-dx,1.0/(nrows)-dy*1.5]	
    if ncols==3:
        POS=[0.075,0.75,1.0/(ncols+0.6)-dx,1.0/(nrows)-dy*1.5]	
        
    figg=pylab.figure(figsize=figsz, facecolor=figBG)
    
    ax=[]
    for aa in range(nrows-1-doQuiv):
        for bb in range(ncols):
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))
    
    ii=0
    x,dat=plot_utils.timegaps(time,vx)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[scipy.nanmin(lat),scipy.nanmax(lat)]
    pc=ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    if geo==1:
        ax[ii].set_title(parm+' east '+units, fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title(parm+' perp east '+units, fontsize=labsize, horizontalalignment='center')
    ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')
    if ncols==1:
        cl=pylab.colorbar(pc)
        cl.set_label(units,fontsize=labsize)
                
    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,vy)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    #	pylab.colorbar()
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    pylab.setp(ax[ii], yticklabels=[])
    if geo==1:
        ax[ii].set_title(parm+' north '+units, fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title(parm+' perp north '+units, fontsize=labsize, horizontalalignment='center')

    ii=ii+1

    if ncols==3:
        x,dat=plot_utils.timegaps(time,vz)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        #	pylab.colorbar()
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        if geo==1:
            ax[ii].set_title('%s up (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')	
        else:
            ax[ii].set_title('%s anti par (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')	
        
        ii=ii+1
    
    x,dat=plot_utils.timegaps(time,dvx)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    pc2=ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=0,vmax=cax[1]/5)
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    if geo==1:
        ax[ii].set_title('err %s east %s' % (parm,units), fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title('err %s perp east %s' % (parm,units), fontsize=labsize, horizontalalignment='center')

    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,dvy)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=0,vmax=cax[1]/5)
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    pylab.setp(ax[ii], yticklabels=[])
    if geo==1:
        ax[ii].set_title('err %s north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title('err %s perp north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
            
    ii=ii+1

    if ncols==3:
        x,dat=plot_utils.timegaps(time,dvz)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=0,vmax=cax[1]/5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        if geo==1:
            ax[ii].set_title('err %s up (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')					
        else:
            ax[ii].set_title('err %s anti par (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')
        
        ii=ii+1

    # quiver plot
    if doQuiv:
        aa=aa+1
        bb=0
        ss=2
        sss=1
        if ncols==33:
            ss=3
            sss=0.8

        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa-dy/1*sss-POS[3]*(sss-1),POS[2]*ss+dx,POS[3]*sss]
        ax.append(pylab.axes(rect, axisbg=axesBG))
        
        I=scipy.where(scipy.isnan(vx))
        vx[I]=0
        vy[I]=0
        I=scipy.where(scipy.isnan(vy))
        vx[I]=0
        vy[I]=0
        I=scipy.where(scipy.absolute(dvx)/(scipy.absolute(vx)+p[0])>p[1])
        vx[I]=0
        vy[I]=0
        I=scipy.where(scipy.absolute(dvy)/(scipy.absolute(vy)+p[0])>p[1])
        vx[I]=0
        vy[I]=0
        I=scipy.where(scipy.sqrt(vx*vx+vy*vy)>p[2])
        vx[I]=0
        vy[I]=0
        I=scipy.where((scipy.absolute(dvx)>p[3]) | (scipy.absolute(dvy)>p[3]))
        vx[I]=0
        vy[I]=0
                    
        C=scipy.ones(scipy.transpose(vx).shape)-scipy.sign(scipy.transpose(vx))
        x=matplotlib.dates.epoch2num(time2)
        [X,Y]=scipy.meshgrid(x,lat2)
        Q=ax[ii].quiver(X,Y,scipy.transpose(vx),scipy.transpose(vy),C,scale=1000.0*sc,width=rect[2]*0.005)
        ax[ii].quiverkey(Q, 1.1, 1, cax[1], str(cax[1]) + ' ' + units,fontproperties={'size' : labsize},labelpos='S')
        ax[ii].xaxis.tick_bottom()
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        pylab.hold(1)
        pylab.xlim((x[0],x[-1]))
        pylab.ylim((scipy.nanmin(lat2),scipy.nanmax(lat2)))	
        
        ax22 = figg.add_axes(ax[ii].get_position(), sharey=ax[ii], frameon=False)
        pylab.plot(MLTtime,MLTtime,'k')
        ax22.set_xlim([MLTtime[0],MLTtime[-1]])
        ax22.xaxis.tick_top()
        ax22.set_xlabel('Magnetic Local Time', fontsize=labsize)
        ax22.xaxis.set_label_position('top')
        labels = pylab.getp(ax22, 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax22, 'yticklabels')
        pylab.setp(labels, fontsize=textsize)
        pylab.ylim((scipy.nanmin(lat2),scipy.nanmax(lat2)))	

        ii=ii+1
    
    if ncols==1:
        npls=ii-1		
        bb=1
        for aa in range(npls):
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))	
            cl=pylab.colorbar(pc,ax[ii])
            cl.set_label(units,fontsize=labsize)
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)
            ii+=1
        
    else:
        aa=aa-2
        bb=2
        if ncols==3:
            bb=3
        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
        ax.append(pylab.axes(rect, axisbg=axesBG))
        
        cl=pylab.colorbar(pc,ax[ii])
        cl.set_label(units,fontsize=labsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)

        ii=ii+1
        
        aa=aa+1
        bb=2
        if ncols==3:
            bb=3
        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
        ax.append(pylab.axes(rect, axisbg=axesBG))
        
        cl=pylab.colorbar(pc2,ax[ii])
        cl.set_label(units,fontsize=labsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)
        
    locator = matplotlib.dates.HourLocator(interval=1)
#	locator = matplotlib.dates.MinuteLocator(interval=5)
    formatter = matplotlib.dates.DateFormatter("%H:%M")
    
    dx=(time2[-1]-time2[0])/3600.0
    dx2=dx/7.0
    
    ss=5
    if ncols==3:
        ss=7
    for rr in range(len(ax)):
    
        if dx2>0.5:
            interval=scipy.ceil(dx/7.0)
            locator = matplotlib.dates.HourLocator(interval=interval)
            formatter = matplotlib.dates.DateFormatter("%H:%M")
        elif dx2<0.5:
            interval=scipy.ceil(dx*60.0/7.0)
            locator = matplotlib.dates.MinuteLocator(interval=interval)
            formatter = matplotlib.dates.DateFormatter("%H:%M")
            
        ax[rr].xaxis.set_major_locator(locator)
        ax[rr].xaxis.set_major_formatter(formatter)
    
    """
    rr=rr+1
    locator = matplotlib.dates.MinuteLocator(interval=30)
    formatter = matplotlib.dates.DateFormatter("%H:%M")
    ax[rr].xaxis.set_major_locator(locator)
    ax[rr].xaxis.set_major_formatter(formatter)
    """
    
    pylab.show()
    
    return figg

def plot_vvels1_mag(time,MLTtime,lat,vx,vy,dvx,dvy,title='Vector Vels',units='m/s',parm='V',cax1=[-1000,1000],cax2=[-180,180],label='Mag. Lat. (degrees)'):

    vx=vx.copy()
    vy=vy.copy()
    dvx=dvx.copy()
    dvy=dvy.copy()
    
    if lat.ndim==2:
        lat2=scipy.nanmean(lat,axis=1)
        lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
    else:
        lat2=lat
    
    time2=scipy.mean(time,axis=1)

    textsize = 8        # size for axes text
    labsize = 10

    pylab.ioff()
    
    figBG   = 'w'        # the figure background color
    axesBG  = '#f6f6f6'  # the axies background color
    figsz = (7,9)

    dx, dy= 0.015, 0.05	
    nrows=4; ncols=1
    POS=[0.07,0.75,0.8-dx,1.0/(nrows)-dy*1.5]	
        
    figg=pylab.figure(figsize=figsz, facecolor=figBG)
    
    ax=[]
    for aa in range(nrows):
        for bb in range(ncols):
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))
    
    pc=[]
    
    ii=0
    x,dat=plot_utils.timegaps(time,vx)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[scipy.nanmin(lat),scipy.nanmax(lat)]
    pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax1[0],vmax=cax1[1]))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    ax[ii].set_title('%s mag (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
    ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')
                
    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,dvx)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=0.0,vmax=cax1[1]/5))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    #	pylab.colorbar()
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    ax[ii].set_title('err %s mag (%s)' %(parm,units), fontsize=labsize, horizontalalignment='center')

    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,vy)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax2[0],vmax=cax2[1],cmap=pylab.get_cmap('hsv')))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    ax[ii].set_title('%s dir (%s)' % (parm,'degrees'), fontsize=labsize, horizontalalignment='center')

    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,dvy)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=0,vmax=cax2[1]/5))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    ax[ii].set_title('err %s dir (%s)' % (parm,'degrees'), fontsize=labsize, horizontalalignment='center')
            
    ii=ii+1
    
    if ncols==1:
        bb=1
        for aa in range(len(pc)):
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/20,POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))	
            cl=pylab.colorbar(pc[aa],ax[ii])
            if aa<2:
                cl.set_label(units,fontsize=labsize)
            else:
                cl.set_label('degrees',fontsize=labsize)			
            pylab.setp(pylab.getp(ax[ii], 'yticklabels'), fontsize=textsize)
            ii+=1
                
    locator = matplotlib.dates.HourLocator(interval=1)
#	locator = matplotlib.dates.MinuteLocator(interval=5)
    formatter = matplotlib.dates.DateFormatter("%H:%M")
    
    dx=(time2[-1]-time2[0])/3600.0
    dx2=dx/7.0
    
    for rr in range(len(ax)):
    
        if dx2>0.5:
            interval=scipy.ceil(dx/7.0)
            locator = matplotlib.dates.HourLocator(interval=interval)
            formatter = matplotlib.dates.DateFormatter("%H:%M")
        elif dx2<0.5:
            interval=scipy.ceil(dx*60.0/7.0)
            locator = matplotlib.dates.MinuteLocator(interval=interval)
            formatter = matplotlib.dates.DateFormatter("%H:%M")
            
        ax[rr].xaxis.set_major_locator(locator)
        ax[rr].xaxis.set_major_formatter(formatter)
    
    pylab.show()
    
    return figg