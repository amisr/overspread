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
import scipy.stats

import struct
import datetime
import array
import glob
import tables
import matplotlib
matplotlib.use('Agg')
import pylab

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

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

    h5file=tables.openFile(fname)
    output={}
    for group in h5file.walkGroups("/"):
        output[group._v_pathname]={}
        for array in h5file.listNodes(group, classname = 'Array'):						
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
            group=fhandle.createGroup(fhandle.root, groupname, 'Dataset')

    if len(keys2do)==0:
        try:
            fhandle.removeNode(group,name)
        except:
            ''
        fhandle.createArray(group,name, dict2do, "Dataset")
    else:
        for key in keys2do:
            try:
                fhandle.removeNode(group,key)
            except:
                ''		
            fhandle.createArray(group, key, dict2do[key], "Dataset")

def dovels_replot(fname,txMax=1.0e6,clim=[-1000.0,1000.0],sc=[15.0,15.0],saveout=1,binByDay=0):

    dat1=readafile(fname)
    odir=''
    ofname=fname[:-3]
        
    byGeo=dat1['/ProcessingParams']['GeographicBinning']
    PPP=dat1['/ProcessingParams']['ErrorElim']
    PPPe=[PPP[0]*5e-5,PPP[1],PPP[2]*5e-5,PPP[3]*5e-5]

    vvels1=dat1['/VectorVels']['Vest']
    dvvels1=dat1['/VectorVels']['dVest']
    Vmag=dat1['/VectorVels']['Vmag']
    dVmag=dat1['/VectorVels']['dVmag']
    Vdir=dat1['/VectorVels']['Vdir']
    dVdir=dat1['/VectorVels']['dVdir']
    evec1=dat1['/VectorVels']['Eest']
    devec1=dat1['/VectorVels']['dEest']
    Emag=dat1['/VectorVels']['Emag']
    dEmag=dat1['/VectorVels']['dEmag']
    Edir=dat1['/VectorVels']['Edir']
    dEdir=dat1['/VectorVels']['dEdir']
    
    if byGeo==2:
        PLAT_OUT = dat1['/VectorVels']['Latitude']
    else:
        PLAT_OUT = dat1['/VectorVels']['MagneticLatitude']

    MLTtime1 = dat1['/Time']['MagneticLocalTime']
    timeout = dat1['/Time']['UnixTime']

    dates=[]; yrs=[]; mths=[]; days=[]; dtime=[]
    for i in range(timeout.shape[0]):
        dates.append([datetime.datetime.utcfromtimestamp(timeout[i,0]),datetime.datetime.utcfromtimestamp(timeout[i,1])])
        yrs.append([dates[i][0].year,dates[i][1].year])
        mths.append([dates[i][0].month,dates[i][1].month])
        days.append([dates[i][0].day,dates[i][1].day])
        dtime.append([dates[i][0].hour+dates[i][0].minute/60.0+dates[i][0].second/3600.0,dates[i][1].hour+dates[i][1].minute/60.0+dates[i][1].second/3600.0])
    yrs=scipy.array(yrs); mths=scipy.array(mths); days=scipy.array(days); dtime=scipy.array(dtime);

    if binByDay:
        r1=datetime.date(dates[0][0].year,dates[0][0].month,dates[0][0].day)
        r2=datetime.date(dates[-1][-1].year,dates[-1][-1].month,dates[-1][-1].day)
        Ntrecs=int(r2.strftime('%j'))-int(r1.strftime('%j'))+1
        tplus='byDay'
    else:
        Ttotal=(timeout[-1,-1]-timeout[0,0])/3600.0
        Ntrecs=scipy.ceil(Ttotal/txMax)
        tplus='by' + str(txMax) + 'hr'
    print Ntrecs

    iStart=0; 
    for iTime in range(Ntrecs):
        if binByDay:
            iEnd=scipy.where((yrs[:,0]==yrs[iStart,0]) & (mths[:,0]==mths[iStart,0]) & (days[:,0]==days[iStart,0]))[0]
            iEnd=iEnd[-1]
        else:
            iEnd=scipy.where(timeout[:,-1]<=(timeout[iStart,0]+txMax*3600.0))[0]
            iEnd=iEnd[-1]
        tlim=[iStart,iEnd]
        iStart=iEnd+1
        if Ntrecs>1:
            txtra='-' + tplus + '-' + str(iTime)
        else:
            txtra=''
            
        title= " %d-%d-%d %.3f UT - %d-%d-%d %.3f UT" % (dates[tlim[0]][0].month,dates[tlim[0]][0].day,dates[tlim[0]][0].year,dtime[tlim[0]][0],dates[tlim[-1]][1].month,dates[tlim[-1]][1].day,dates[tlim[-1]][1].year,dtime[tlim[-1],1])
        
        if byGeo:
            latlabel='Geographic Lat. (deg)'		
        else:
            latlabel='Magnetic Lat. (deg)'		
        
        # velocity vector
        figg=plot_vvels1(timeout[tlim[0]:(tlim[1]+1)],scipy.mean(MLTtime1[tlim[0]:(tlim[1]+1)],axis=1),PLAT_OUT,vvels1[tlim[0]:(tlim[1]+1),:,1],vvels1[tlim[0]:(tlim[1]+1),:,0],dvvels1[tlim[0]:(tlim[1]+1),:,1],dvvels1[tlim[0]:(tlim[1]+1),:,0],
            title='Vector Velocities' + title,p=PPP,sc=sc[0],cax=clim,ncols=3,vz=vvels1[tlim[0]:(tlim[1]+1),:,2],dvz=dvvels1[tlim[0]:(tlim[1]+1),:,2],vzsc=10.0,geo=byGeo,label=latlabel)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vvec' +txtra +'.png'))
        # velocity magnitude
        figg=plot_vvels1_mag(timeout[tlim[0]:(tlim[1]+1)],scipy.mean(MLTtime1[tlim[0]:(tlim[1]+1)],axis=1),PLAT_OUT,Vmag[tlim[0]:(tlim[1]+1)],Vdir[tlim[0]:(tlim[1]+1)],dVmag[tlim[0]:(tlim[1]+1)],dVdir[tlim[0]:(tlim[1]+1)],
            title='Vector Velocities' + title,cax1=[0.0,clim[1]],cax2=[-180.0,180.0],label=latlabel)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vmag' +txtra+ '.png'))

        # electric field
        figg=plot_vvels1(timeout[tlim[0]:(tlim[1]+1)],scipy.mean(MLTtime1[tlim[0]:(tlim[1]+1)],axis=1),PLAT_OUT,evec1[tlim[0]:(tlim[1]+1),:,1]*1e3,evec1[tlim[0]:(tlim[1]+1),:,0]*1e3,devec1[tlim[0]:(tlim[1]+1),:,1]*1e3,devec1[tlim[0]:(tlim[1]+1),:,0]*1e3,
            title='Electric Fields' + title,p=[PPPe[0]*1e3,PPPe[1],PPPe[2]*1e3,PPPe[3]*1e3],sc=sc[0]*5e-2,cax=[clim[0]*5e-2,clim[1]*5e-2],ncols=2,geo=byGeo,label=latlabel,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-evec' +txtra +'.png'))
        # electric field magnitude
        figg=plot_vvels1_mag(timeout[tlim[0]:(tlim[1]+1)],scipy.mean(MLTtime1[tlim[0]:(tlim[1]+1)],axis=1),PLAT_OUT,Emag[tlim[0]:(tlim[1]+1)]*1e3,Edir[tlim[0]:(tlim[1]+1)],dEmag[tlim[0]:(tlim[1]+1)]*1e3,dEdir[tlim[0]:(tlim[1]+1)],
            title='Electric Fields' + title,cax1=[0.0,clim[1]*5e-2],cax2=[-180.0,180.0],label=latlabel,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-emag' +txtra +'.png'))


def dovels(fnameIn=None,makeplot=1,zoom=[],saveout=1,plats=[[66.0,69.0,0.25,0.25],[]],clim=[-1000,1000],sc=[15.0,15.0],Time2Integrate=[600.0],\
    MinAlt=175.0,MaxAlt=450.0,PPP=[200.0,0.5,2000.0,100.0],COVAR=[3000.*3000.,3000.*3000.,15.*15.],\
    CorrectVap=1,upBcode=64157.,noUpB=0,zoomWhole=[],extraApp='',neMin=2.0e10,byGeo=0,CHIRP=0.0):
    
    if fnameIn != None:
        fname=fnameIn
    
    if byGeo:
        extraApp=extraApp + '-geo'
            
    ofname='%s-vvels-%dsec%s' % (os.path.basename(fname)[:-3],Time2Integrate[0],extraApp)
    
    odir=os.path.join(os.path.dirname(fname),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    x=scipy.arange(plats[0][0],plats[0][1],plats[0][3])[:,scipy.newaxis]
    PLAT_OUT=scipy.concatenate((x,x+plats[0][2]),axis=1)
    if len(plats[1])>0:
        x=scipy.arange(plats[1][0],plats[1][1],plats[1][3])[:,scipy.newaxis]
        x=scipy.concatenate((x,x+plats[1][2]),axis=1)
        PLAT_OUT=scipy.concatenate((PLAT_OUT,x),axis=0)

    print PLAT_OUT

    dat1=readafile(fname)

    BeamCodes=dat1['/']['BeamCodes']
    if CorrectVap:
        IupB=scipy.where(BeamCodes[:,0]==upBcode)[0]
        InotUpB=scipy.where(BeamCodes[:,0]!=upBcode)[0]
        print IupB
        
    if byGeo:
        kpn=dat1['/Geomag']['kn']
        kpe=dat1['/Geomag']['ke']
        kpar=dat1['/Geomag']['kz']
        if byGeo==2:
            plat=dat1['/Geomag']['Latitude']
            plong=dat1['/Geomag']['Longitude']
        else:
            plat=dat1['/Geomag']['MagneticLatitude']
            plong=dat1['/Geomag']['MagneticLongitude']        
    else:
        kpn=dat1['/Geomag']['kpn']
        kpe=dat1['/Geomag']['kpe']
        kpar=dat1['/Geomag']['kpar']
        try:
            plat=dat1['/Geomag']['MagneticLatitude']
            plong=dat1['/Geomag']['MagneticLongitude']
        except:
            plat=dat1['/Geomag']['plat']
            plong=dat1['/Geomag']['plong']
    Babs=dat1['/Geomag']['Babs']
    Bmed = scipy.stats.nanmedian(Babs)
    for i in range(Bmed.ndim):
        Bmed=scipy.stats.nanmedian(Bmed)		
    PPPe=(scipy.array(PPP).copy()).tolist(); PPPe[0]*=Bmed; PPPe[2]*=Bmed; PPPe[3]*=Bmed
    COVARe=(scipy.array(COVAR).copy()).tolist(); COVARe[0]*=Bmed*Bmed; COVARe[1]*=Bmed*Bmed; COVARe[2]*=Bmed*Bmed

    ht=dat1['/FittedParams']['Altitude']
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+CHIRP
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
    ne1=dat1['/FittedParams']['Ne']
    time1=dat1['/Time']['UnixTime']
    doy1=dat1['/Time']['doy']
    dtime1=dat1['/Time']['dtime']+(doy1-doy1[0,0])*24.0
    MLT=dat1['/Time']['MagneticLocalTimeSite']
    yr=dat1['/Time']['Year']
    mon=dat1['/Time']['Month']
    day=dat1['/Time']['Day']
    
    I=scipy.where((ne1<neMin))
    vlos1[I]=scipy.nan
    dvlos1[I]=scipy.nan
    
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
    
    yr=yr[0,0]
    mon=mon[0,0]
    day=day[0,0]
    title=' %d-%d-%d' % (mon, day, yr)

    (Nbeams,Nhts)=ht.shape

    if CorrectVap:
        Nbeams=Nbeams-1
        # up B
        vlos1upB=vlos1[:,IupB[0],:]
        dvlos1upB=dvlos1[:,IupB[0],:]
        htupB=ht[IupB[0],:]
        # not
        vlos1=vlos1[:,InotUpB,:]
        dvlos1=dvlos1[:,InotUpB,:]
        ht=ht[InotUpB,:]	
        kpn=kpn[InotUpB,:]
        kpe=kpe[InotUpB,:]
        kpar=kpar[InotUpB,:]
        plat=plat[InotUpB,:]
        plong=plong[InotUpB,:]
    
    timeout=[]
    dtimeout=[]
    MLTtime1=[]
    
    
    done=0 # flag to say when we are done
    Irec=0 # record counter
    IIrec=0 # record counter
    while not done:
        
        Irecs=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[0])))[0]
        
        tvlos=vlos1[Irecs,:,:]
        tdvlos=dvlos1[Irecs,:,:]
                
        if CorrectVap:
            vUpB=vlos1upB[Irecs]
            dvUpB=dvlos1upB[Irecs]
            for i in range(Nbeams):
                tupB=scipy.interpolate.interp1d(scipy.squeeze(htupB),vUpB,bounds_error=0,fill_value=0.0)(ht[i,:])
                tvlos[:,i,:]=tvlos[:,i,:]-tupB*kpar[i,:]
                tdvlos[:,i,:]=scipy.sqrt(scipy.power(tdvlos[:,i,:],2.0)+scipy.power(dvUpB*kpar[i,:],2.0))	
        
        # line of sight velocities and errors
        vlosin=scipy.reshape(scipy.squeeze(tvlos),(Nhts*Nbeams*len(Irecs)))
        dvlosin=scipy.reshape(scipy.squeeze(tdvlos),(Nhts*Nbeams*len(Irecs)))

        # k vector (this is in mag coords)
        kin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
        kin[:,:,0]=kpn
        kin[:,:,1]=kpe
        kin[:,:,2]=kpar
        kin=scipy.reshape(scipy.repeat(kin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))
        
        # magnetic field
        bin=scipy.reshape(scipy.repeat(Babs[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        
        # mag lat and long, altitude
        platin=scipy.reshape(scipy.repeat(plat[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        plongin=scipy.reshape(scipy.repeat(plong[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        htin=scipy.reshape(scipy.repeat(ht[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))

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

        Irec=Irecs[-1]+1 # increment counters
        IIrec=IIrec+1

        if Irec>=time1.shape[0]:
            done=1
        
    MLTtime1=scipy.array(MLTtime1)
    timeout=scipy.array(timeout)
    dtimeout=scipy.array(dtimeout)
    
    # magnitude and direction
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
    Param['SourceFile']=os.path.basename(fname)
    Param['IntegrationTime']=Time2Integrate[0]
    Param['CorrectVap']=CorrectVap
    Param['GeographicBinning']=byGeo
    Param['MinimumElectronDensity']=neMin
    Param['VelocityOffsetCorrection']=CHIRP
    Paramhr=copy.copy(Param)
    Paramhr['IntegrationTime']=Time2Integrate[-1]
    
    if saveout==1:
        print 'Writing output to' + os.path.join(odir,ofname+'.h5')
        outh5file=tables.openFile(os.path.join(odir,ofname+'.h5'), mode = "w", title = "Fit File")
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
        write_outputfile(outh5file,evec1,groupname='VectorVels',name='Eest')
        write_outputfile(outh5file,devec1,groupname='VectorVels',name='dEest')
        write_outputfile(outh5file,Emag,groupname='VectorVels',name='Emag')
        write_outputfile(outh5file,dEmag,groupname='VectorVels',name='dEmag')
        write_outputfile(outh5file,Edir,groupname='VectorVels',name='Edir')
        write_outputfile(outh5file,dEdir,groupname='VectorVels',name='dEdir')			
        if byGeo==2:
            write_outputfile(outh5file,PLAT_OUT,groupname='VectorVels',name='Latitude')
        else:
            write_outputfile(outh5file,PLAT_OUT,groupname='VectorVels',name='MagneticLatitude')
        write_outputfile(outh5file,Param,keys2do=Param.keys(),groupname='ProcessingParams')
        outh5file.close()
                
    if makeplot==1:		
        if byGeo==2:
            latlabel='Geographic Lat. (deg)'		
        else:
            latlabel='Magnetic Lat. (deg)'		
        
        # velocity vector
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
            title='Vector Velocities' + title,p=PPP,sc=sc[0],cax=clim,ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2],vzsc=10.0,geo=byGeo,label=latlabel)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vvec.png'))
        # velocity magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,Vmag,Vdir,dVmag,dVdir,
            title='Vector Velocities' + title,cax1=[0.0,clim[1]],cax2=[-180.0,180.0],label=latlabel)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vmag.png'))

        # electric field
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,evec1[:,:,1]*1e3,evec1[:,:,0]*1e3,devec1[:,:,1]*1e3,devec1[:,:,0]*1e3,
            title='Electric Fields' + title,p=[PPPe[0]*1e3,PPPe[1],PPPe[2]*1e3,PPPe[3]*1e3],sc=sc[0]*5e-2,cax=[clim[0]*5e-2,clim[1]*5e-2],ncols=2,geo=byGeo,label=latlabel,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-evec.png'))
        # electric field magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,Emag*1e3,Edir,dEmag*1e3,dEdir,
            title='Electric Fields' + title,cax1=[0.0,clim[1]*5e-2],cax2=[-180.0,180.0],label=latlabel,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-emag.png'))

def dovels_risr(fnameIn=None,makeplot=1,zoom=[],saveout=1,plats=[[66.0,69.0,0.25,0.25],[]],clim=[-1000,1000],sc=[15.0,15.0],Time2Integrate=[600.0],\
    MinAlt=175.0,MaxAlt=450.0,PPP=[200.0,0.5,2000.0,100.0],COVAR=[3000.*3000.,3000.*3000.,15.*15.],\
    CorrectVap=1,upBcode=64157.,noUpB=0,zoomWhole=[],extraApp='',neMin=2.0e10,byGeo=0,CHIRP=0.0):
    
    if fnameIn != None:
        fname=fnameIn
    
    if byGeo:
        extraApp=extraApp + '-geo'
            
    ofname='%s-vvels-%dsec%s' % (os.path.basename(fname)[:-3],Time2Integrate[0],extraApp)
    
    odir=os.path.join(os.path.dirname(fname),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    x=scipy.arange(plats[0][0],plats[0][1],plats[0][3])[:,scipy.newaxis]
    PLAT_OUT=scipy.concatenate((x,x+plats[0][2]),axis=1)
    if len(plats[1])>0:
        x=scipy.arange(plats[1][0],plats[1][1],plats[1][3])[:,scipy.newaxis]
        x=scipy.concatenate((x,x+plats[1][2]),axis=1)
        PLAT_OUT=scipy.concatenate((PLAT_OUT,x),axis=0)

    print PLAT_OUT

    dat1=readafile(fname)

    BeamCodes=dat1['/']['BeamCodes']
    if CorrectVap:
        IupB=scipy.where(BeamCodes[:,0]==upBcode)[0]
        InotUpB=scipy.where(BeamCodes[:,0]!=upBcode)[0]
        print IupB
        
    if byGeo:
        kpn=dat1['/Geomag']['kn']
        kpe=dat1['/Geomag']['ke']
        kpar=dat1['/Geomag']['kz']
        if byGeo==2:
            plat=dat1['/Geomag']['Latitude']
            plong=dat1['/Geomag']['Longitude']
        else:
            plat=dat1['/Geomag']['MagneticLatitude']
            plong=dat1['/Geomag']['MagneticLongitude']        
    else:
        kpn=dat1['/Geomag']['kpn']
        kpe=dat1['/Geomag']['kpe']
        kpar=dat1['/Geomag']['kpar']
        try:
            plat=dat1['/Geomag']['MagneticLatitude']
            plong=dat1['/Geomag']['MagneticLongitude']
        except:
            plat=dat1['/Geomag']['plat']
            plong=dat1['/Geomag']['plong']
    Bn=dat1['/Geomag']['Bx']; Be=dat1['/Geomag']['By']; Bz=dat1['/Geomag']['Bz']; 
    Babs=dat1['/Geomag']['Babs']
    Bmed = scipy.stats.nanmedian(Babs)
    for i in range(Bmed.ndim):
        Bmed=scipy.stats.nanmedian(Bmed)		
    PPPe=(scipy.array(PPP).copy()).tolist(); PPPe[0]*=Bmed; PPPe[2]*=Bmed; PPPe[3]*=Bmed
    COVARe=(scipy.array(COVAR).copy()).tolist(); COVARe[0]*=Bmed*Bmed; COVARe[1]*=Bmed*Bmed; COVARe[2]*=Bmed*Bmed

    ht=dat1['/FittedParams']['Altitude']
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+CHIRP
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
    ne1=dat1['/FittedParams']['Ne']
    time1=dat1['/Time']['UnixTime']
    doy1=dat1['/Time']['doy']
    dtime1=dat1['/Time']['dtime']+(doy1-doy1[0,0])*24.0
    MLT=dat1['/Time']['MagneticLocalTimeSite']
    yr=dat1['/Time']['Year']
    mon=dat1['/Time']['Month']
    day=dat1['/Time']['Day']
    
    I=scipy.where((ne1<neMin))
    vlos1[I]=scipy.nan
    dvlos1[I]=scipy.nan
    
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
    
    yr=yr[0,0]
    mon=mon[0,0]
    day=day[0,0]
    title=' %d-%d-%d' % (mon, day, yr)

    (Nbeams,Nhts)=ht.shape

    if CorrectVap:
        Nbeams=Nbeams-1
        # up B
        vlos1upB=vlos1[:,IupB[0],:]
        dvlos1upB=dvlos1[:,IupB[0],:]
        htupB=ht[IupB[0],:]
        # not
        vlos1=vlos1[:,InotUpB,:]
        dvlos1=dvlos1[:,InotUpB,:]
        ht=ht[InotUpB,:]	
        kpn=kpn[InotUpB,:]
        kpe=kpe[InotUpB,:]
        kpar=kpar[InotUpB,:]
        plat=plat[InotUpB,:]
        plong=plong[InotUpB,:]
    
    timeout=[]
    dtimeout=[]
    MLTtime1=[]
    
    
    done=0 # flag to say when we are done
    Irec=0 # record counter
    IIrec=0 # record counter
    while not done:
        
        Irecs=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[0])))[0]
        
        tvlos=vlos1[Irecs,:,:]
        tdvlos=dvlos1[Irecs,:,:]
                
        if CorrectVap:
            vUpB=vlos1upB[Irecs]
            dvUpB=dvlos1upB[Irecs]
            for i in range(Nbeams):
                tupB=scipy.interpolate.interp1d(scipy.squeeze(htupB),vUpB,bounds_error=0,fill_value=0.0)(ht[i,:])
                tvlos[:,i,:]=tvlos[:,i,:]-tupB*kpar[i,:]
                tdvlos[:,i,:]=scipy.sqrt(scipy.power(tdvlos[:,i,:],2.0)+scipy.power(dvUpB*kpar[i,:],2.0))	
        
        # line of sight velocities and errors
        vlosin=scipy.reshape(scipy.squeeze(tvlos),(Nhts*Nbeams*len(Irecs)))
        dvlosin=scipy.reshape(scipy.squeeze(tdvlos),(Nhts*Nbeams*len(Irecs)))

        # k vector (this is in mag coords)
        kin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
        kin[:,:,0]=kpn
        kin[:,:,1]=kpe
        kin[:,:,2]=kpar
        kin=scipy.reshape(scipy.repeat(kin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))

        ekin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
        ekin[:,:,0]=(-Be*kpar-Bz*kpe)/Babs**2.0
        ekin[:,:,1]=(Bz*kpn+Bn*kpar)/Babs**2.0
        ekin[:,:,2]=(Bn*kpe-Be*kpn)/Babs**2.0
        ekin=scipy.reshape(scipy.repeat(ekin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))
        
        # magnetic field
        bin=scipy.reshape(scipy.repeat(Babs[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        
        # mag lat and long, altitude
        platin=scipy.reshape(scipy.repeat(plat[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        plongin=scipy.reshape(scipy.repeat(plong[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        htin=scipy.reshape(scipy.repeat(ht[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))

        # compute vectors
        (plat_out1,Vest,dVest,xx,Nmeas)=vvels.compute_velvec2(PLAT_OUT,vlosin,dvlosin,kin,platin,plongin,htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVAR,p=PPP)
        (plat_out1,Eest,dEest,xx,Nmeas1)=vvels.compute_velvec2(PLAT_OUT,vlosin,dvlosin,ekin,platin,plongin,htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVARe,p=PPP)
        Eest[:,-1]*=-1

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

        Irec=Irecs[-1]+1 # increment counters
        IIrec=IIrec+1

        if Irec>=time1.shape[0]:
            done=1
        
    MLTtime1=scipy.array(MLTtime1)
    timeout=scipy.array(timeout)
    dtimeout=scipy.array(dtimeout)
    
    # magnitude and direction
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
    Param['SourceFile']=os.path.basename(fname)
    Param['IntegrationTime']=Time2Integrate[0]
    Param['CorrectVap']=CorrectVap
    Param['GeographicBinning']=byGeo
    Param['MinimumElectronDensity']=neMin
    Param['VelocityOffsetCorrection']=CHIRP
    Paramhr=copy.copy(Param)
    Paramhr['IntegrationTime']=Time2Integrate[-1]
    
    if saveout==1:
        print 'Writing output to' + os.path.join(odir,ofname+'.h5')
        outh5file=tables.openFile(os.path.join(odir,ofname+'.h5'), mode = "w", title = "Fit File")
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
        write_outputfile(outh5file,evec1,groupname='VectorVels',name='Eest')
        write_outputfile(outh5file,devec1,groupname='VectorVels',name='dEest')
        write_outputfile(outh5file,Emag,groupname='VectorVels',name='Emag')
        write_outputfile(outh5file,dEmag,groupname='VectorVels',name='dEmag')
        write_outputfile(outh5file,Edir,groupname='VectorVels',name='Edir')
        write_outputfile(outh5file,dEdir,groupname='VectorVels',name='dEdir')			
        if byGeo==2:
            write_outputfile(outh5file,PLAT_OUT,groupname='VectorVels',name='Latitude')
        else:
            write_outputfile(outh5file,PLAT_OUT,groupname='VectorVels',name='MagneticLatitude')
        write_outputfile(outh5file,Param,keys2do=Param.keys(),groupname='ProcessingParams')
        outh5file.close()
                
    if makeplot==1:		
        if byGeo==2:
            latlabel='Geographic Lat. (deg)'		
        else:
            latlabel='Magnetic Lat. (deg)'		
        
        # velocity vector
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
            title='Vector Velocities' + title,p=PPP,sc=sc[0],cax=clim,ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2],vzsc=10.0,geo=byGeo,label=latlabel)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vvec.png'))
        # velocity magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,Vmag,Vdir,dVmag,dVdir,
            title='Vector Velocities' + title,cax1=[0.0,clim[1]],cax2=[-180.0,180.0],label=latlabel)
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-vmag.png'))

        # electric field
        figg=plot_vvels1(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,evec1[:,:,1]*1e3,evec1[:,:,0]*1e3,devec1[:,:,1]*1e3,devec1[:,:,0]*1e3,
            title='Electric Fields' + title,p=[PPPe[0]*1e3,PPPe[1],PPPe[2]*1e3,PPPe[3]*1e3],sc=sc[0]*5e-2,cax=[clim[0]*5e-2,clim[1]*5e-2],
            ncols=3,vz=evec1[:,:,2]*1e3,dvz=devec1[:,:,2]*1e3,vzsc=10.0,geo=byGeo,label=latlabel,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-evec.png'))
        # electric field magnitude
        figg=plot_vvels1_mag(timeout,scipy.mean(MLTtime1,axis=1),PLAT_OUT,Emag*1e3,Edir,dEmag*1e3,dEdir,
            title='Electric Fields' + title,cax1=[0.0,clim[1]*5e-2],cax2=[-180.0,180.0],label=latlabel,units='mV/m',parm='E')
        if saveout:
            figg.savefig(os.path.join(odir,ofname+'-emag.png'))
    

def dovelsAltcomb(makeplot=1,plot2=0,zoom=[],saveout=1,alts=[[80.0,200.0,5.0],[200.0,400.0,50.0]],
    clim=[-1000,1000],sc=[15.0,15.0],Time2Integrate=[600.0],swaphr=0,MinAlt=80.0,MaxAlt=450.0,MinEl=0.0,MaxEl=90.0,MinAz=-180.0,MaxAz=180.0,
    COVAR=[3000.*3000.,3000.*3000.,3000.*3000.],PPP=[200.0,0.5,2000.0,100.0]):

#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20070301.013/20070301.013_ac_5min.h5'
    fname1='/Volumes/AMISR_004/processed_data/PFISR/2007/Joule2/20070119.001/20070119.001_ac_5min-phaseerrs-cal.h5'
    fname2='/Volumes/AMISR_004/processed_data/PFISR/2007/Joule2/20070119.001/20070119.001_lp_2min-cal.h5'

#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Thayer01/20070521.001/20070521.001_lp_1min.h5'
#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Lyons01/20070827.001/20070827.001_lp_3min.h5'
#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/WorldDay02/20070327.004/20070327.004_lp_2min.h5'
#	fname="/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070811.001/20070811.001_lp_8min.h5"

    odir=os.path.join(os.path.dirname(fname1),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    ofname='%s-%s-vvels-altcomb-%dsec.h5' % (os.path.basename(fname1)[:-3],os.path.basename(fname2)[:-3],Time2Integrate[0])
    
    x=scipy.arange(alts[0][0]*1000.0,alts[0][1]*1000.0,alts[0][2]*1000.0)[:,scipy.newaxis]
    ALT_OUT=scipy.concatenate((x,x+alts[0][2]*1000.0),axis=1)
    x=scipy.arange(alts[1][0]*1000.0,alts[1][1]*1000.0,alts[1][2]*1000.0)[:,scipy.newaxis]
    x=scipy.concatenate((x,x+alts[1][2]*1000.0),axis=1)
    ALT_OUT=scipy.concatenate((ALT_OUT,x),axis=0)

    # read alt code
    dat1=readafile(fname1)

    BMCODES=dat1['/']['BeamCodes']
    Ibm=scipy.where((BMCODES[:,2]>=MinEl) & (BMCODES[:,2]<=MaxEl) & (BMCODES[:,1]>=MinAz) & (BMCODES[:,1]<=MaxAz))[0]
    BMCODES=BMCODES[Ibm,:]
    Nbeams=BMCODES.shape[0]

    ht1=dat1['/FittedParams']['Altitude'][Ibm,:]
    Iht=scipy.where((ht1>=alts[0][0]*1000.) & (ht1<=alts[0][1]*1000.))
    Ihtd=scipy.transpose(Iht)
    ht1=ht1[Iht]
    Nhts1=ht1.shape[0]

    kpn1=dat1['/Geomag']['kpn'][Ibm,:][Iht]
    kpe1=dat1['/Geomag']['kpe'][Ibm,:][Iht]
    kpar1=dat1['/Geomag']['kpar'][Ibm,:][Iht]
    plat1=dat1['/Geomag']['MagneticLatitude'][Ibm,:][Iht]
    plong1=dat1['/Geomag']['MagneticLongitude'][Ibm,:][Iht]
    Babs1=dat1['/Geomag']['Babs'][Ibm,:][Iht]
    dec1=dat1['/Geomag']['Declination'][Ibm,:][Iht]
    dip1=dat1['/Geomag']['Dip'][Ibm,:][Iht]

    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3][:,Ibm,:][:,Ihtd[:,0],Ihtd[:,1]]
    nuin1=dat1['/FittedParams']['Fits'][:,:,:,0,2][:,Ibm,:][:,Ihtd[:,0],Ihtd[:,1]]
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3][:,Ibm,:][:,Ihtd[:,0],Ihtd[:,1]]
    time1=dat1['/Time']['UnixTime']
    dtime1=dat1['/Time']['dtime']
    yr=dat1['/Time']['Year'][0,0]
    mon=dat1['/Time']['Month'][0,0]
    day=dat1['/Time']['Day'][0,0]
    title='Vector Velocities %d-%d-%d' % (mon, day, yr)

    # read lp
    dat2=readafile(fname2)

    ht2=dat2['/FittedParams']['Altitude'][Ibm,:]
    Iht=scipy.where((ht2>=alts[1][0]*1000.) & (ht2<=alts[1][1]*1000.))
    Ihtd=scipy.transpose(Iht)
    ht2=ht2[Iht]
    Nhts2=ht2.shape[0]

    kpn2=dat2['/Geomag']['kpn'][Ibm,:][Iht]
    kpe2=dat2['/Geomag']['kpe'][Ibm,:][Iht]
    kpar2=dat2['/Geomag']['kpar'][Ibm,:][Iht]
    plat2=dat2['/Geomag']['MagneticLatitude'][Ibm,:][Iht]
    plong2=dat2['/Geomag']['MagneticLongitude'][Ibm,:][Iht]
    Babs2=dat2['/Geomag']['Babs'][Ibm,:][Iht]
    dec2=dat2['/Geomag']['Declination'][Ibm,:][Iht]
    dip2=dat2['/Geomag']['Dip'][Ibm,:][Iht]

    vlos2=dat2['/FittedParams']['Fits'][:,:,:,0,3][:,Ibm,:][:,Ihtd[:,0],Ihtd[:,1]]
    nuin2=dat2['/FittedParams']['Fits'][:,:,:,0,2][:,Ibm,:][:,Ihtd[:,0],Ihtd[:,1]]
    dvlos2=dat2['/FittedParams']['Errors'][:,:,:,0,3][:,Ibm,:][:,Ihtd[:,0],Ihtd[:,1]]
    time2=dat2['/Time']['UnixTime']
    dtime2=dat2['/Time']['dtime']

    # concatenate what we can
    htAll=scipy.concatenate((ht1,ht2),axis=0)
    kpn=scipy.concatenate((kpn1,kpn2),axis=0)
    kpe=scipy.concatenate((kpe1,kpe2),axis=0)
    kpar=scipy.concatenate((kpar1,kpar2),axis=0)
    plat=scipy.concatenate((plat1,plat2),axis=0)
    plong=scipy.concatenate((plong1,plong2),axis=0)
    Babs=scipy.concatenate((Babs1,Babs2),axis=0)
    dec=scipy.concatenate((dec1,dec2),axis=0)
    dip=scipy.concatenate((dip1,dip2),axis=0)
    NhtsAll=htAll.shape[0]
    
    # get some params
    BabsOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    PlatOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    PlongOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    DipOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    DecOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    for aa in range(ALT_OUT.shape[0]):
        I=scipy.where((htAll>=ALT_OUT[aa,0]) & (htAll<=ALT_OUT[aa,1]))
        BabsOut[aa]=scipy.mean(Babs[I])
        PlatOut[aa]=scipy.mean(plat[I])
        PlongOut[aa]=scipy.mean(plong[I])
        DecOut[aa]=scipy.mean(dec[I])
        DipOut[aa]=scipy.mean(dip[I])
            
    timeout=[]
    dtimeout=[]
    
    done=0 # flag to say when we are done
    Irec=0 # record counter
    IIrec=0 # record counter
    while not done:
        
        Irecs1=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[0])))[0]
        Irecs2=scipy.where((time2[:,0]>=time1[Irec,0]) & (time2[:,0]<=(time1[Irec,0]+Time2Integrate[0])))[0]
                
        # line of sight velocities and errors
        vlosin1=scipy.reshape(scipy.squeeze(vlos1[Irecs1,:]),(Nhts1*len(Irecs1)))
        dvlosin1=scipy.reshape(scipy.squeeze(dvlos1[Irecs1,:]),(Nhts1*len(Irecs1)))
        vlosin2=scipy.reshape(scipy.squeeze(vlos2[Irecs2,:]),(Nhts2*len(Irecs2)))
        dvlosin2=scipy.reshape(scipy.squeeze(dvlos2[Irecs2,:]),(Nhts2*len(Irecs2)))
        vlosin=scipy.concatenate((vlosin1,vlosin2),axis=0)
        dvlosin=scipy.concatenate((dvlosin1,dvlosin2),axis=0)

        # collision frequency
        nuinin1=scipy.reshape(scipy.squeeze(nuin1[Irecs1,:]),(Nhts1*len(Irecs1)))
        nuinin2=scipy.reshape(scipy.squeeze(nuin2[Irecs2,:]),(Nhts2*len(Irecs2)))
        nuinin=scipy.concatenate((nuinin1,nuinin2),axis=0)

        # k vector (this is in mag coords)
        kin1=scipy.zeros((len(Irecs1)*Nhts1,3))
        kin2=scipy.zeros((len(Irecs2)*Nhts2,3))
        kin1[:,0]=scipy.reshape(scipy.repeat(kpn1[scipy.newaxis,:],len(Irecs1),axis=0),(len(Irecs1)*Nhts1))
        kin1[:,1]=scipy.reshape(scipy.repeat(kpe1[scipy.newaxis,:],len(Irecs1),axis=0),(len(Irecs1)*Nhts1))
        kin1[:,2]=scipy.reshape(scipy.repeat(kpar1[scipy.newaxis,:],len(Irecs1),axis=0),(len(Irecs1)*Nhts1))
        kin2[:,0]=scipy.reshape(scipy.repeat(kpn2[scipy.newaxis,:],len(Irecs2),axis=0),(len(Irecs2)*Nhts2))
        kin2[:,1]=scipy.reshape(scipy.repeat(kpe2[scipy.newaxis,:],len(Irecs2),axis=0),(len(Irecs2)*Nhts2))
        kin2[:,2]=scipy.reshape(scipy.repeat(kpar2[scipy.newaxis,:],len(Irecs2),axis=0),(len(Irecs2)*Nhts2))
        kin=scipy.concatenate((kin1,kin2),axis=0)
                
        # mag lat and long, altitude
        platin1=scipy.reshape(scipy.repeat(plat1[scipy.newaxis,:],len(Irecs1),axis=0),(len(Irecs1)*Nhts1))
        platin2=scipy.reshape(scipy.repeat(plat2[scipy.newaxis,:],len(Irecs2),axis=0),(len(Irecs2)*Nhts2))
        platin=scipy.concatenate((platin1,platin2),axis=0)		
        plongin1=scipy.reshape(scipy.repeat(plong1[scipy.newaxis,:],len(Irecs1),axis=0),(len(Irecs1)*Nhts1))
        plongin2=scipy.reshape(scipy.repeat(plong2[scipy.newaxis,:],len(Irecs2),axis=0),(len(Irecs2)*Nhts2))
        plongin=scipy.concatenate((plongin1,plongin2),axis=0)		
        htin1=scipy.reshape(scipy.repeat(ht1[scipy.newaxis,:],len(Irecs1),axis=0),(len(Irecs1)*Nhts1))
        htin2=scipy.reshape(scipy.repeat(ht2[scipy.newaxis,:],len(Irecs2),axis=0),(len(Irecs2)*Nhts2))
        htin=scipy.concatenate((htin1,htin2),axis=0)

        # compute vectors
        #(alt_out1,Vest,dVest,dVestAll,xx)=vvels.compute_velvec2(ALT_OUT,vlosin,dvlosin,kin,htin,[],htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=[3000.*3000.,3000.*3000.,3000.*3000.],femax=0.5)
        (alt_out1,Vest,dVest,dVestAll,xxx)=vvels.compute_velvec2(ALT_OUT,vlosin,dvlosin,kin,htin,[],htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVAR,p=PPP)

        # geographic coords
        VestGeo=scipy.zeros(Vest.shape,Vest.dtype)
        dVestGeo=scipy.zeros(dVest.shape,dVest.dtype)
        tnuin=scipy.zeros(ALT_OUT.shape[0],dtype='float64')
        for bb in range(ALT_OUT.shape[0]):
            terrgmag=scipy.squeeze(dVestAll[bb,:,:])
            terrgmag=scipy.array([[terrgmag[1,1],terrgmag[1,0],terrgmag[1,2]],[terrgmag[0,1],terrgmag[0,0],terrgmag[0,2]],[terrgmag[2,1],terrgmag[2,0],terrgmag[2,2]]])
            test = scipy.squeeze(gmag2geo(scipy.matrix([[Vest[bb,1]],[Vest[bb,0]],[Vest[bb,2]]]),DecOut[bb]*pi/180.0,DipOut[bb]*pi/180.0))
            VestGeo[bb,0]=test[0,1]
            VestGeo[bb,1]=test[0,0]
            VestGeo[bb,2]=test[0,2]
            terrgeo = gmag2geo_covar(scipy.matrix(terrgmag),scipy.array(DecOut[bb]*pi/180.0)[scipy.newaxis],scipy.array(DipOut[bb]*pi/180.0)[scipy.newaxis])
            terr=scipy.sqrt(scipy.diag(terrgeo))
            dVestGeo[bb,0]=terr[1]
            dVestGeo[bb,1]=terr[0]
            dVestGeo[bb,2]=terr[2]
            #
            I=scipy.where((htin>=ALT_OUT[bb,0]) & (htin<=ALT_OUT[bb,1]))
            tnuin[bb]=scipy.mean(nuinin[I])
            
        timeout.append([time1[Irecs1[0],0],time1[Irecs1[-1],1]])
        dtimeout.append([dtime1[Irecs1[0],0],dtime1[Irecs1[-1],1]])

        if IIrec==0:
            vvels1=Vest[scipy.newaxis,:,:]
            dvvels1=dVest[scipy.newaxis,:,:]
            vvels1geo=VestGeo[scipy.newaxis,:,:]
            dvvels1geo=dVestGeo[scipy.newaxis,:,:]
            nuinOut=tnuin[scipy.newaxis,:]
        else:
            vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)
            vvels1geo=scipy.concatenate((vvels1geo,VestGeo[scipy.newaxis,:,:]),axis=0)
            dvvels1geo=scipy.concatenate((dvvels1geo,dVestGeo[scipy.newaxis,:,:]),axis=0)
            nuinOut=scipy.concatenate((nuinOut,tnuin[scipy.newaxis,:]),axis=0)

        Irec=Irecs1[-1]+1 # increment counters
        IIrec=IIrec+1

        if Irec>=time1.shape[0]:
            done=1
        
    timeout=scipy.array(timeout)
    dtimeout=scipy.array(dtimeout)
    
    MLTtime1=getmlt(scipy.mean(timeout,axis=1),plong[0])

    if saveout==1:
        print 'Writing output to' + ofname
        outh5file=tables.openFile(os.path.join(odir,ofname), mode = "a", title = "Fit File")
        write_outputfile(outh5file,BMCODES,groupname='',name='BeamCodes')
        write_outputfile(outh5file,PlatOut,groupname='',name='MagneticLatitude')
        write_outputfile(outh5file,PlongOut,groupname='',name='MagneticLongitude')
        write_outputfile(outh5file,BabsOut,groupname='',name='Babs')
        write_outputfile(outh5file,DecOut,groupname='',name='Declination')
        write_outputfile(outh5file,DipOut,groupname='',name='Dip')
        write_outputfile(outh5file,nuinOut,groupname='',name='CollisonFrequency')
        # Time
        write_outputfile(outh5file,timeout,groupname='Time',name='UnixTime')
        write_outputfile(outh5file,dtimeout,groupname='Time',name='dtime')
        write_outputfile(outh5file,MLTtime1,groupname='Time',name='MagneticLocalTime')
        # vector velocities
        write_outputfile(outh5file,vvels1,groupname='VectorVels',name='VestGmag')
        write_outputfile(outh5file,dvvels1,groupname='VectorVels',name='dVestGmag')
        write_outputfile(outh5file,ALT_OUT,groupname='VectorVels',name='Altitude')
        write_outputfile(outh5file,vvels1geo,groupname='VectorVels',name='VestGeo')
        write_outputfile(outh5file,dvvels1geo,groupname='VectorVels',name='dVestGeo')
    
        outh5file.close()

    if makeplot==1:	
        figg=plot_vvels1(timeout,MLTtime1,ALT_OUT/1000.0,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],title=title,p=[200.0,0.5,4000.0,500.0],
            sc=sc[0],cax=clim,label='Altitude (km)',ncols=3,nrows=4,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2])
        oname=ofname[:-3]+'-gmag.png'
        figg.savefig(os.path.join(odir,oname))
        
        figg=plot_vvels1(timeout,MLTtime1,ALT_OUT/1000.0,vvels1geo[:,:,1],vvels1geo[:,:,0],dvvels1geo[:,:,1],dvvels1geo[:,:,0],title=title,p=[200.0,0.5,2000.0,500.0],
            sc=sc[0],cax=clim,label='Altitude (km)',ncols=3,nrows=4,vz=vvels1geo[:,:,2],dvz=dvvels1geo[:,:,2],geo=1)
        oname=ofname[:-3]+'-geo.png'
        figg.savefig(os.path.join(odir,oname))



def dovelsAlt(fnameIn='',makeplot=1,plot2=0,zoom=[],saveout=1,alts=[80.0,250.0,5.0],clim=[-1000,1000],sc=[15.0,15.0],
    Time2Integrate=[600.0],swaphr=0,MinAlt=0.0,MaxAlt=450.0,MinEl=0.0,MaxEl=90.0,MinAz=-180.0,MaxAz=180.0,
    COVAR=[3000.*3000.,3000.*3000.,3000.*3000.],PPP=[200.0,0.5,2000.0,100.0]):

#	fname='/Volumes/AMISR_004/processed_data/joule2/20070117.004/20070117.004_ac_10min-phaseerrs-noteti.h5'
    fname="/Volumes/AMISR_004/processed_data/PFISR/2007/Joule2/20070119.001/20070119.001_ac_5min-phaseerrs-cal.h5"
   # fname='/Users/mnicolls/Documents/Work/AmisrProc/A16flTest10/20090115.005/20090115.005_acfl_15min-hrng.h5'
    #fname='/Users/mnicolls/Documents/Work/AmisrProc/ACES10/20090126.004/20090126.004_ac_15min.h5'
    
    if fnameIn!='':
        fname=fnameIn

    odir=os.path.join(os.path.dirname(fname),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    ofname='%s-vvels-alt-%dsec' % (os.path.basename(fname)[:-3],Time2Integrate[0])
    
    x=scipy.arange(alts[0]*1000.0,alts[1]*1000.0,alts[2]*1000.0)[:,scipy.newaxis]
    ALT_OUT=scipy.concatenate((x,x+alts[2]*1000.0),axis=1)

    dat1=readafile(fname)

    BMCODES=dat1['/']['BeamCodes']
    Ibm=scipy.where((BMCODES[:,2]>=MinEl) & (BMCODES[:,2]<=MaxEl) & (BMCODES[:,1]>=MinAz) & (BMCODES[:,1]<=MaxAz))[0]
    BMCODES=BMCODES[Ibm,:]

    kpn=dat1['/Geomag']['kpn'][Ibm,:]
    kpe=dat1['/Geomag']['kpe'][Ibm,:]
    kpar=dat1['/Geomag']['kpar'][Ibm,:]
    plat=dat1['/Geomag']['MagneticLatitude'][Ibm,:]
    plong=dat1['/Geomag']['MagneticLongitude'][Ibm,:]
    Babs=dat1['/Geomag']['Babs'][Ibm,:]
    dec=dat1['/Geomag']['Declination'][Ibm,:]
    dip=dat1['/Geomag']['Dip'][Ibm,:]

    ht=dat1['/FittedParams']['Altitude'][Ibm,:]
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3][:,Ibm,:]-20.0
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3][:,Ibm,:]
    time1=dat1['/Time']['UnixTime']
    dtime1=dat1['/Time']['dtime']
    yr=dat1['/Time']['Year'][0,0]
    mon=dat1['/Time']['Month'][0,0]
    day=dat1['/Time']['Day'][0,0]
    title='Vector Velocities %d-%d-%d' % (mon, day, yr)

    BabsOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    PlatOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    PlongOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    DipOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    DecOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    for aa in range(ALT_OUT.shape[0]):
        I=scipy.where((ht>=ALT_OUT[aa,0]) & (ht<=ALT_OUT[aa,1]))
        BabsOut[aa]=scipy.mean(Babs[I])
        PlatOut[aa]=scipy.mean(plat[I])
        PlongOut[aa]=scipy.mean(plong[I])
        DecOut[aa]=scipy.mean(dec[I])
        DipOut[aa]=scipy.mean(dip[I])
    
    (Nbeams,Nhts)=ht.shape
    
    timeout=[]
    dtimeout=[]
    
    done=0 # flag to say when we are done
    Irec=0 # record counter
    IIrec=0 # record counter
    while not done:
        
        Irecs=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[0])))[0]
        
        # line of sight velocities and errors
        vlosin=scipy.reshape(scipy.squeeze(vlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))
        dvlosin=scipy.reshape(scipy.squeeze(dvlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))

        # k vector (this is in mag coords)
        kin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
        kin[:,:,0]=kpn
        kin[:,:,1]=kpe
        kin[:,:,2]=kpar
        kin=scipy.reshape(scipy.repeat(kin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))
        
        # mag lat and long, altitude
        platin=scipy.reshape(scipy.repeat(plat[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        plongin=scipy.reshape(scipy.repeat(plong[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        htin=scipy.reshape(scipy.repeat(ht[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))

        # compute vectors
        (alt_out1,Vest,dVest,xx,xxx)=vvels.compute_velvec2(ALT_OUT,vlosin,dvlosin,kin,htin,[],htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVAR,p=PPP)

        timeout.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
        dtimeout.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])

        if IIrec==0:
            vvels1=Vest[scipy.newaxis,:,:]
            dvvels1=dVest[scipy.newaxis,:,:]
        else:
            vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)

        Irec=Irecs[-1]+1 # increment counters
        IIrec=IIrec+1

        if Irec>=time1.shape[0]:
            done=1
        
    timeout=scipy.array(timeout)
    dtimeout=scipy.array(dtimeout)
    
    MLTtime1=getmlt(scipy.mean(timeout,axis=1),plong[0,0])

    
    if plot2==1 and len(Time2Integrate)==1:
        if len(zoom)==0:
            I=range(timeout.shape[0])
        else:
            I=scipy.where((dtimeout[:,0]>=zoom[0]) & (dtimeout[:,1]<=zoom[1]))[0]
        timeouthr=timeout[I,:]
        MLTtimehr=MLTtime1[I,:]
        vvelshr=vvels1[I,:,:]
        dvvelshr=dvvels1[I,:,:]
    elif plot2==1 and len(Time2Integrate)==2:

        timeouthr2=[]
        dtimeouthr2=[]
                
        done=0 # flag to say when we are done
        Irec=0 # record counter
        IIrec=0 # record counter
        
        while not done:
        
            Irecs=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[1])))[0]
        
            # line of sight velocities and errors
            vlosin=scipy.reshape(scipy.squeeze(vlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))
            dvlosin=scipy.reshape(scipy.squeeze(dvlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))

            # k vector (this is in mag coords)
            kin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
            kin[:,:,0]=kpn
            kin[:,:,1]=kpe
            kin[:,:,2]=kpar
            kin=scipy.reshape(scipy.repeat(kin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))
        
            # mag lat and long, altitude
            platin=scipy.reshape(scipy.repeat(plat[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
            plongin=scipy.reshape(scipy.repeat(plong[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
            htin=scipy.reshape(scipy.repeat(ht[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))

            # compute vectors
            (alt_out1,Vest,dVest)=vvels.compute_velvec2(ALT_OUT,vlosin,dvlosin,kin,htin,[],htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=[3000.*3000.,3000.*3000.,5.*5.],femax=0.5)

            timeouthr2.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
            dtimeouthr2.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])

            if IIrec==0:
                vvelshr2=Vest[scipy.newaxis,:,:]
                dvvelshr2=dVest[scipy.newaxis,:,:]
            else:
                vvelshr2=scipy.concatenate((vvelshr2,Vest[scipy.newaxis,:,:]),axis=0)
                dvvelshr2=scipy.concatenate((dvvelshr2,dVest[scipy.newaxis,:,:]),axis=0)

            Irec=Irecs[-1]+1 # increment counters
            IIrec=IIrec+1

            if Irec>=time1.shape[0]:
                done=1
        
        timeouthr2=scipy.array(timeouthr2)
        dtimeouthr2=scipy.array(dtimeouthr2)
    
        MLTtimehr2=getmlt(scipy.mean(timeouthr2,axis=1),plong[0,0])
        
        if len(zoom)==0:
            I=range(timeouthr2.shape[0])
        else:
            I=scipy.where((dtimeouthr2[:,0]>=zoom[0]) & (dtimeouthr2[:,1]<=zoom[1]))[0]
        timeouthr=timeouthr2[I,:]
        dtimeouthr=dtimeouthr2[I,:]
        MLTtimehr=MLTtimehr2[I,:]
        vvelshr=vvelshr2[I,:,:]
        dvvelshr=dvvelshr2[I,:,:]
    
    if saveout==1:
        print 'Writing output to ' + ofname + '.h5'
        outh5file=tables.openFile(os.path.join(odir,ofname+'.h5'), mode = "a", title = "Fit File")
        write_outputfile(outh5file,BMCODES,groupname='',name='BeamCodes')
        write_outputfile(outh5file,PlatOut,groupname='',name='MagneticLatitude')
        write_outputfile(outh5file,PlongOut,groupname='',name='MagneticLongitude')
        write_outputfile(outh5file,BabsOut,groupname='',name='Babs')
        write_outputfile(outh5file,DecOut,groupname='',name='Declination')
        write_outputfile(outh5file,DipOut,groupname='',name='Dip')
        # Time
        write_outputfile(outh5file,timeout,groupname='Time',name='UnixTime')
        write_outputfile(outh5file,dtimeout,groupname='Time',name='dtime')
        write_outputfile(outh5file,MLTtime1,groupname='Time',name='MagneticLocalTime')
        # vector velocities
        write_outputfile(outh5file,vvels1,groupname='VectorVels',name='Vest')
        write_outputfile(outh5file,dvvels1,groupname='VectorVels',name='dVest')
        write_outputfile(outh5file,ALT_OUT,groupname='VectorVels',name='Altitude')
        # hr
        if plot2==1:
            write_outputfile(outh5file,timeouthr,groupname='Time',name='UnixTimeHR')
            write_outputfile(outh5file,dtimeouthr,groupname='Time',name='dtimeHR')
            write_outputfile(outh5file,MLTtimehr,groupname='Time',name='MagneticLocalTimeHR')
            write_outputfile(outh5file,vvelshr,groupname='VectorVels',name='VestHR')
            write_outputfile(outh5file,dvvelshr,groupname='VectorVels',name='dVestHR')
    
        outh5file.close()
        
    if makeplot==1:	
        figg=plot_vvels1(timeout,MLTtime1,ALT_OUT/1000.0,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],title=title,p=[200.0,0.5,2000.0,500.0],
            sc=sc[0],cax=clim,label='Altitude (km)',ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2])
        oname=ofname+'.png'
        figg.savefig(os.path.join(odir,oname))
        if plot2:
            if swaphr==0:
                TI=Time2Integrate[0]
                txt1='  %d sec, top' % (TI)
                if len(Time2Integrate)==2:
                    TI=Time2Integrate[1]
                txt2='  %d sec' % (TI)
                
                figg=plot_vvels(timeout,timeouthr,timeout,MLTtime1,MLTtimehr,MLTtime1,ALT_OUT/1000.0,ALT_OUT/1000.0,
                    vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
                    vvelshr[:,:,1],vvelshr[:,:,0],dvvelshr[:,:,1],dvvelshr[:,:,0],
                    vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
                    title=title,p=[200.0,0.5,2000.0,500.0],sc=sc,txt1=txt1,txt2=txt2,cax=clim,label='Altitude (km)')
            else:
                TI=Time2Integrate[0]
                txt1='  %d sec' % (TI)
                if len(Time2Integrate)==2:
                    TI=Time2Integrate[1]
                txt2='  %d sec, top' % (TI)			
            
                figg=plot_vvels(timeout,timeouthr,timeouthr2,MLTtime1,MLTtimehr,MLTtimehr2,ALT_OUT/1000.0,ALT_OUT/1000.0,
                    vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
                    vvelshr[:,:,1],vvelshr[:,:,0],dvvelshr[:,:,1],dvvelshr[:,:,0],
                    vvelshr2[:,:,1],vvelshr2[:,:,0],dvvelshr2[:,:,1],dvvelshr2[:,:,0],
                    title=title,p=[200.0,0.5,2000.0,500.0],sc=sc,txt1=txt1,txt2=txt2,cax=clim,label='Altitude (km)')
            oname=ofname+'.png'
            figg.savefig(os.path.join(odir,oname))

def dovelsAltGeo(makeplot=1,plot2=0,zoom=[],saveout=1,alts=[80.0,250.0,5.0],clim=[-1000,1000],sc=[15.0,15.0],Time2Integrate=[600.0],swaphr=0,MinAlt=0.0,MaxAlt=450.0,MinEl=0.0,MaxEl=90.0,MinAz=-180.0,MaxAz=180.0,COVAR=[3000.*3000.,3000.*3000.,3000.*3000.],PPP=[200.0,0.5,2000.0,100.0]):

#	fname='/Volumes/AMISR_004/processed_data/joule2/20070117.004/20070117.004_ac_10min-phaseerrs-noteti.h5'
    fname="/Volumes/AMISR_004/processed_data/2008/Cascades10/20081108.001/20081108.001_ac_15min.h5"
    fname='/Users/mnicolls/Documents/Work/AmisrProc/A16flTest10/20090115.005/20090115.005_acfl_15min-hrng.h5'
    fname='/Users/mnicolls/Documents/Work/AmisrProc/A16flTest10/20090116.002/20090116.002_acfl_15min-hrng.h5'
    fname='/Users/mnicolls/Documents/Work/AmisrProc/Hedden10/20090120.003/20090120.003_acfl_60min-hrng.h5'
    fname='/Users/mnicolls/Documents/Work/AmisrProc/ACES10/20090126.004/20090126.004_acfl_15min-hrng.h5'
    fname='/Users/mnicolls/Documents/Work/AmisrProc/ACES10/20090126.004/20090126.004_ac_15min.h5'
    fname='/Volumes/AMISR_004/processed_data/2009/ACES10/20090129.002/20090129.002_acfl_15min-hrng.h5'
    fname='/Volumes/AMISR_004/processed_data/2009/MSWinds23/20090509.001/20090509.001_acfl_15min.h5'

    odir=os.path.join(os.path.dirname(fname),'derivedParams')
    if not os.path.exists(odir):
        os.mkdir(odir)

    ofname='%s-vvels-alt-%dsec' % (os.path.basename(fname)[:-3],Time2Integrate[0])
    
    x=scipy.arange(alts[0]*1000.0,alts[1]*1000.0,alts[2]*1000.0)[:,scipy.newaxis]
    ALT_OUT=scipy.concatenate((x,x+alts[2]*1000.0),axis=1)

    dat1=readafile(fname)

    BMCODES=dat1['/']['BeamCodes']
    Ibm=scipy.where((BMCODES[:,2]>=MinEl) & (BMCODES[:,2]<=MaxEl) & (BMCODES[:,1]>=MinAz) & (BMCODES[:,1]<=MaxAz))[0]
    BMCODES=BMCODES[Ibm,:]

    kpn=dat1['/Geomag']['kn'][Ibm,:]
    kpe=dat1['/Geomag']['ke'][Ibm,:]
    kpar=dat1['/Geomag']['kz'][Ibm,:]
    plat=dat1['/Geomag']['MagneticLatitude'][Ibm,:]
    plong=dat1['/Geomag']['MagneticLongitude'][Ibm,:]
    Babs=dat1['/Geomag']['Babs'][Ibm,:]
    dec=dat1['/Geomag']['Declination'][Ibm,:]
    dip=dat1['/Geomag']['Dip'][Ibm,:]

    ht=dat1['/FittedParams']['Altitude'][Ibm,:]
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3][:,Ibm,:]-20.0
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3][:,Ibm,:]
    time1=dat1['/Time']['UnixTime']
    dtime1=dat1['/Time']['dtime']
    yr=dat1['/Time']['Year'][0,0]
    mon=dat1['/Time']['Month'][0,0]
    day=dat1['/Time']['Day'][0,0]
    title='Vector Velocities %d-%d-%d' % (mon, day, yr)

    BabsOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    PlatOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    PlongOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    DipOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    DecOut=scipy.zeros(ALT_OUT.shape[0],dtype=ALT_OUT.dtype)
    for aa in range(ALT_OUT.shape[0]):
        I=scipy.where((ht>=ALT_OUT[aa,0]) & (ht<=ALT_OUT[aa,1]))
        BabsOut[aa]=scipy.mean(Babs[I])
        PlatOut[aa]=scipy.mean(plat[I])
        PlongOut[aa]=scipy.mean(plong[I])
        DecOut[aa]=scipy.mean(dec[I])
        DipOut[aa]=scipy.mean(dip[I])
    
    (Nbeams,Nhts)=ht.shape
    
    timeout=[]
    dtimeout=[]
    
    done=0 # flag to say when we are done
    Irec=0 # record counter
    IIrec=0 # record counter
    while not done:
        
        Irecs=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[0])))[0]
        
        # line of sight velocities and errors
        vlosin=scipy.reshape(scipy.squeeze(vlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))
        dvlosin=scipy.reshape(scipy.squeeze(dvlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))

        # k vector (this is in mag coords)
        kin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
        kin[:,:,0]=kpn
        kin[:,:,1]=kpe
        kin[:,:,2]=kpar
        kin=scipy.reshape(scipy.repeat(kin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))
        
        # mag lat and long, altitude
        platin=scipy.reshape(scipy.repeat(plat[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        plongin=scipy.reshape(scipy.repeat(plong[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
        htin=scipy.reshape(scipy.repeat(ht[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))

        # compute vectors
        (alt_out1,Vest,dVest,xx,xxx)=vvels.compute_velvec2(ALT_OUT,vlosin,dvlosin,kin,htin,[],htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=COVAR,p=PPP)

        timeout.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
        dtimeout.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])

        if IIrec==0:
            vvels1=Vest[scipy.newaxis,:,:]
            dvvels1=dVest[scipy.newaxis,:,:]
        else:
            vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)

        Irec=Irecs[-1]+1 # increment counters
        IIrec=IIrec+1

        if Irec>=time1.shape[0]:
            done=1
        
    timeout=scipy.array(timeout)
    dtimeout=scipy.array(dtimeout)
    
    MLTtime1=getmlt(scipy.mean(timeout,axis=1),plong[0,0])

    
    if plot2==1 and len(Time2Integrate)==1:
        if len(zoom)==0:
            I=range(timeout.shape[0])
        else:
            I=scipy.where((dtimeout[:,0]>=zoom[0]) & (dtimeout[:,1]<=zoom[1]))[0]
        timeouthr=timeout[I,:]
        MLTtimehr=MLTtime1[I,:]
        vvelshr=vvels1[I,:,:]
        dvvelshr=dvvels1[I,:,:]
    elif plot2==1 and len(Time2Integrate)==2:

        timeouthr2=[]
        dtimeouthr2=[]
                
        done=0 # flag to say when we are done
        Irec=0 # record counter
        IIrec=0 # record counter
        
        while not done:
        
            Irecs=scipy.where((time1[:,0]>=time1[Irec,0]) & (time1[:,0]<=(time1[Irec,0]+Time2Integrate[1])))[0]
        
            # line of sight velocities and errors
            vlosin=scipy.reshape(scipy.squeeze(vlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))
            dvlosin=scipy.reshape(scipy.squeeze(dvlos1[Irecs,:,:]),(Nhts*Nbeams*len(Irecs)))

            # k vector (this is in mag coords)
            kin=scipy.zeros((Nbeams,Nhts,3),dtype=kpn.dtype)
            kin[:,:,0]=kpn
            kin[:,:,1]=kpe
            kin[:,:,2]=kpar
            kin=scipy.reshape(scipy.repeat(kin[scipy.newaxis,:,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams,3))
        
            # mag lat and long, altitude
            platin=scipy.reshape(scipy.repeat(plat[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
            plongin=scipy.reshape(scipy.repeat(plong[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))
            htin=scipy.reshape(scipy.repeat(ht[scipy.newaxis,:,:],len(Irecs),axis=0),(len(Irecs)*Nhts*Nbeams))

            # compute vectors
            (alt_out1,Vest,dVest)=vvels.compute_velvec2(ALT_OUT,vlosin,dvlosin,kin,htin,[],htin,htmin=MinAlt*1000,htmax=MaxAlt*1000,covar=[3000.*3000.,3000.*3000.,5.*5.],femax=0.5)

            timeouthr2.append([time1[Irecs[0],0],time1[Irecs[-1],1]])
            dtimeouthr2.append([dtime1[Irecs[0],0],dtime1[Irecs[-1],1]])

            if IIrec==0:
                vvelshr2=Vest[scipy.newaxis,:,:]
                dvvelshr2=dVest[scipy.newaxis,:,:]
            else:
                vvelshr2=scipy.concatenate((vvelshr2,Vest[scipy.newaxis,:,:]),axis=0)
                dvvelshr2=scipy.concatenate((dvvelshr2,dVest[scipy.newaxis,:,:]),axis=0)

            Irec=Irecs[-1]+1 # increment counters
            IIrec=IIrec+1

            if Irec>=time1.shape[0]:
                done=1
        
        timeouthr2=scipy.array(timeouthr2)
        dtimeouthr2=scipy.array(dtimeouthr2)
    
        MLTtimehr2=getmlt(scipy.mean(timeouthr2,axis=1),plong[0,0])
        
        if len(zoom)==0:
            I=range(timeouthr2.shape[0])
        else:
            I=scipy.where((dtimeouthr2[:,0]>=zoom[0]) & (dtimeouthr2[:,1]<=zoom[1]))[0]
        timeouthr=timeouthr2[I,:]
        dtimeouthr=dtimeouthr2[I,:]
        MLTtimehr=MLTtimehr2[I,:]
        vvelshr=vvelshr2[I,:,:]
        dvvelshr=dvvelshr2[I,:,:]
    
    if saveout==1:
        print 'Writing output to ' + ofname + '.h5'
        outh5file=tables.openFile(os.path.join(odir,ofname+'.h5'), mode = "a", title = "Fit File")
        write_outputfile(outh5file,BMCODES,groupname='',name='BeamCodes')
        write_outputfile(outh5file,PlatOut,groupname='',name='MagneticLatitude')
        write_outputfile(outh5file,PlongOut,groupname='',name='MagneticLongitude')
        write_outputfile(outh5file,BabsOut,groupname='',name='Babs')
        write_outputfile(outh5file,DecOut,groupname='',name='Declination')
        write_outputfile(outh5file,DipOut,groupname='',name='Dip')
        # Time
        write_outputfile(outh5file,timeout,groupname='Time',name='UnixTime')
        write_outputfile(outh5file,dtimeout,groupname='Time',name='dtime')
        write_outputfile(outh5file,MLTtime1,groupname='Time',name='MagneticLocalTime')
        # vector velocities
        write_outputfile(outh5file,vvels1,groupname='VectorVels',name='Vest')
        write_outputfile(outh5file,dvvels1,groupname='VectorVels',name='dVest')
        write_outputfile(outh5file,ALT_OUT,groupname='VectorVels',name='Altitude')
        # hr
        if plot2==1:
            write_outputfile(outh5file,timeouthr,groupname='Time',name='UnixTimeHR')
            write_outputfile(outh5file,dtimeouthr,groupname='Time',name='dtimeHR')
            write_outputfile(outh5file,MLTtimehr,groupname='Time',name='MagneticLocalTimeHR')
            write_outputfile(outh5file,vvelshr,groupname='VectorVels',name='VestHR')
            write_outputfile(outh5file,dvvelshr,groupname='VectorVels',name='dVestHR')
    
        outh5file.close()
        
    if makeplot==1:	
        figg=plot_vvels1(timeout,MLTtime1,ALT_OUT/1000.0,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],title=title,p=PPP,
            sc=sc[0],cax=clim,label='Altitude (km)',ncols=3,vz=vvels1[:,:,2],dvz=dvvels1[:,:,2],geo=1,vzsc=10.0)
        oname=ofname+'.png'
        figg.savefig(os.path.join(odir,oname))
        if plot2:
            if swaphr==0:
                TI=Time2Integrate[0]
                txt1='  %d sec, top' % (TI)
                if len(Time2Integrate)==2:
                    TI=Time2Integrate[1]
                txt2='  %d sec' % (TI)
                
                figg=plot_vvels(timeout,timeouthr,timeout,MLTtime1,MLTtimehr,MLTtime1,ALT_OUT/1000.0,ALT_OUT/1000.0,
                    vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
                    vvelshr[:,:,1],vvelshr[:,:,0],dvvelshr[:,:,1],dvvelshr[:,:,0],
                    vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
                    title=title,p=[200.0,0.5,2000.0,500.0],sc=sc,txt1=txt1,txt2=txt2,cax=clim,label='Altitude (km)',geo=1)
            else:
                TI=Time2Integrate[0]
                txt1='  %d sec' % (TI)
                if len(Time2Integrate)==2:
                    TI=Time2Integrate[1]
                txt2='  %d sec, top' % (TI)			
            
                figg=plot_vvels(timeout,timeouthr,timeouthr2,MLTtime1,MLTtimehr,MLTtimehr2,ALT_OUT/1000.0,ALT_OUT/1000.0,
                    vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],
                    vvelshr[:,:,1],vvelshr[:,:,0],dvvelshr[:,:,1],dvvelshr[:,:,0],
                    vvelshr2[:,:,1],vvelshr2[:,:,0],dvvelshr2[:,:,1],dvvelshr2[:,:,0],
                    title=title,p=[200.0,0.5,2000.0,500.0],sc=sc,txt1=txt1,txt2=txt2,cax=clim,label='Altitude (km)',geo=1)
            oname=ofname+'.png'
            figg.savefig(os.path.join(odir,oname))


def plot_vvels(time,timehr,timecol,MLTtime,MLTtimehr,MLTtimecol,lat,lat2,vx,vy,dvx,dvy,vxhr,vyhr,dvxhr,dvyhr,vxcol,vycol,dvxcol,dvycol,\
    title='Vector Vels',units='m/s',parm='V',p=[200.0,0.25,4000.0,500.0],sc=[15.0,10.0],txt1='',txt2='',cax=[-1000,1000],label='Mag. Lat. (degrees)',\
    ncols=2,vz=[],dvz=[],vzhr=[],dvzhr=[],vzsc=1.0,nrows=4,geo=0):

    vx=vx.copy()
    vy=vy.copy()
    dvx=dvx.copy()
    dvy=dvy.copy()
    vxhr=vxhr.copy()
    vyhr=vyhr.copy()
    dvxhr=dvxhr.copy()
    dvyhr=dvyhr.copy()
    vxcol=vxcol.copy()
    vycol=vycol.copy()
    dvxcol=dvxcol.copy()
    dvycol=dvycol.copy()
    if ncols==3:
        vzcol=vzhr.copy()*vzsc
        dvzcol=dvzhr.copy()*vzsc	
        
    latt=scipy.mean(lat,axis=1)
    latt2=scipy.mean(lat2,axis=1)
    
    lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
    lat2=scipy.concatenate((lat2[:,0],lat2[[-1],1]))

    time2=scipy.mean(time,axis=1)
    time2hr=scipy.mean(timehr,axis=1)

    textsize = 8        # size for axes text
    labsize = 10

    pylab.ioff()
    
    figBG   = 'w'        # the figure background color
    axesBG  = '#f6f6f6'  # the axies background color
    figsz = (7,9)
    if ncols==3:
        figsz = (11,12)

    nrows=4		
    dx, dy= 0.015, 0.05	
    POS=[0.1,0.75,1.0/(ncols+0.6)-dx,1.0/(nrows)-dy*1.5]	
    if ncols==3:
        POS=[0.075,0.75,1.0/(ncols+0.6)-dx,1.0/(nrows)-dy*1.5]			
        
    figg=pylab.figure(figsize=figsz, facecolor=figBG)
    
    ax=[]
    for aa in range(nrows-2):
        for bb in range(ncols):
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))
    
    ii=0
    x,dat=plot_utils.timegaps(timecol,vxcol)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[lat[0],lat[-1]]
    pc=ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    if geo==1:
        ax[ii].set_title(parm+' east ' + units, fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title(parm+' perp east ' + units, fontsize=labsize, horizontalalignment='center')
    ax[ii].text(xlim[0],(lat.max()-lat.min())*0.15+lat.max(),title,fontsize=labsize, horizontalalignment='left')
                
    ii=ii+1
    
    x,dat=plot_utils.timegaps(timecol,vycol)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[lat[0],lat[-1]]
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
        ax[ii].set_title('V north (m/s)', fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title('V perp north (m/s)', fontsize=labsize, horizontalalignment='center')
        
    ii=ii+1

    if ncols==3:
        x,dat=plot_utils.timegaps(timecol,vzcol)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[lat[0],lat[-1]]
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
            ax[ii].set_title('V up (m/s)', fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('V anti par (m/s) x %d' % (vzsc), fontsize=labsize, horizontalalignment='center')	
                
    ii=ii+1
    
    x,dat=plot_utils.timegaps(timecol,dvxcol)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[lat[0],lat[-1]]
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
        ax[ii].set_title('err V east (m/s)', fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title('err V perp east (m/s)', fontsize=labsize, horizontalalignment='center')

    ii=ii+1
    
    x,dat=plot_utils.timegaps(timecol,dvycol)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[lat[0],lat[-1]]
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
        ax[ii].set_title('err V north (m/s)', fontsize=labsize, horizontalalignment='center')
    else:
        ax[ii].set_title('err V perp north (m/s)', fontsize=labsize, horizontalalignment='center')
            
    ii=ii+1

    if ncols==3:
        x,dat=plot_utils.timegaps(timecol,dvzcol)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[lat[0],lat[-1]]
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
            ax[ii].set_title('err V up (m/s)', fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('err V anti par (m/s) x %d' % (vzsc), fontsize=labsize, horizontalalignment='center')		
            
    ii=ii+1

    # quiver plot
    aa=aa+1; bb=0
    rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa-dy/3,POS[2]*2+dx,POS[3]]
    ax.append(pylab.axes(rect, axisbg=axesBG))
    
    I=scipy.where(scipy.isnan(vx)); vx[I]=0; vy[I]=0
    I=scipy.where(scipy.isnan(vy)); vx[I]=0; vy[I]=0
    I=scipy.where(scipy.absolute(dvx)/(scipy.absolute(vx)+p[0])>p[1]); vx[I]=0; vy[I]=0
    I=scipy.where(scipy.absolute(dvy)/(scipy.absolute(vy)+p[0])>p[1]); vx[I]=0; vy[I]=0
    I=scipy.where(scipy.sqrt(vx*vx+vy*vy)>p[2]); vx[I]=0; vy[I]=0
    I=scipy.where((scipy.absolute(dvx)>p[3]) | (scipy.absolute(dvy)>p[3])); vx[I]=0; vy[I]=0	
                
    C=scipy.ones(scipy.transpose(vx).shape)-scipy.sign(scipy.transpose(vx))
    x=matplotlib.dates.epoch2num(time2)
    [X,Y]=scipy.meshgrid(x,latt)
    Q=ax[ii].quiver(X,Y,scipy.transpose(vx),scipy.transpose(vy),C,scale=1000*sc[0],width=rect[2]*0.005)
    ax[ii].quiverkey(Q, 1.1, 1, 1000.0, '1000 m/s',fontproperties={'size' : labsize},labelpos='S')
    ax[ii].text(x[-1],lat[0]+(lat[-1]-lat[0])/2.0,txt1,fontsize=textsize-1)
    ax[ii].xaxis.tick_top()
#	ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    pylab.hold(1)
    pylab.plot([matplotlib.dates.epoch2num(time2hr[0]),matplotlib.dates.epoch2num(time2hr[0])],[lat[0],lat[-1]],'k--')
    pylab.plot([matplotlib.dates.epoch2num(time2hr[-1]),matplotlib.dates.epoch2num(time2hr[-1])],[lat[0],lat[-1]],'k--')
#	ax[ii].set_ylim([lat[0]-0.25,lat[-1]])
    pylab.xlim((x[0],x[-1]))
    pylab.ylim((lat2[0],lat2[-1]))
    
    ax22 = figg.add_axes(ax[ii].get_position(), sharey=ax[ii], frameon=False)
    pylab.plot(MLTtime,MLTtime,'k')
    ax22.set_xlim([MLTtime[0],MLTtime[-1]])
    ax22.xaxis.tick_bottom()
    ax22.xaxis.set_label_position('top')
    labels = pylab.getp(ax22, 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax22, 'yticklabels')
    pylab.setp(labels, fontsize=textsize)
    pylab.ylim((lat2[0],lat2[-1]))


    ii=ii+1

    aa=aa+1; bb=0
    rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa-dy/2,POS[2]*2+dx,POS[3]]
    ax.append(pylab.axes(rect, axisbg=axesBG))	
        
    I=scipy.where(scipy.isnan(vxhr)); vxhr[I]=0; vyhr[I]=0
    I=scipy.where(scipy.isnan(vyhr)); vxhr[I]=0; vyhr[I]=0
    I=scipy.where(scipy.absolute(dvxhr)/(scipy.absolute(vxhr)+p[0])>p[1]); vxhr[I]=0; vyhr[I]=0
    I=scipy.where(scipy.absolute(dvyhr)/(scipy.absolute(vyhr)+p[0])>p[1]); vxhr[I]=0; vyhr[I]=0
    I=scipy.where(scipy.sqrt(vxhr*vxhr+vyhr*vyhr)>p[2]); vxhr[I]=0; vyhr[I]=0
    I=scipy.where((scipy.absolute(dvxhr)>p[3]) | (scipy.absolute(dvyhr)>p[3])); vxhr[I]=0; vyhr[I]=0	
        
    C=scipy.ones(scipy.transpose(vxhr).shape)-scipy.sign(scipy.transpose(vxhr))
    x=matplotlib.dates.epoch2num(time2hr)
    [X,Y]=scipy.meshgrid(x,latt2)
    Q=ax[ii].quiver(X,Y,scipy.transpose(vxhr),scipy.transpose(vyhr),C,scale=1000*sc[1],width=rect[2]*0.005)
    ax[ii].quiverkey(Q, 1.1, 1, 1000.0, '1000 m/s',fontproperties={'size' : labsize},labelpos='S')
    ax[ii].text(x[-1],lat[0]+(lat[-1]-lat[0])/2.0,txt2,fontsize=textsize-1)
    ax[-1].set_xlabel('Time (UT)', fontsize=labsize)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    ax[ii].xaxis.tick_bottom()
    pylab.xlim((x[0],x[-1]))
    pylab.ylim((lat2[0],lat2[-1]))	
    
    ax2 = figg.add_axes(ax[ii].get_position(), sharey=ax[ii], frameon=False)
    pylab.plot(MLTtimehr,MLTtimehr,'k')
    ax2.set_xlim([MLTtimehr[0],MLTtimehr[-1]])
    ax2.xaxis.tick_top()
    ax2.xaxis.set_label_position('top')
    labels = pylab.getp(ax2, 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax2, 'yticklabels')
    pylab.setp(labels, fontsize=textsize)
    ax2.set_xlabel('Magnetic Local Time (Hours)', fontsize=labsize)
    pylab.ylim((lat2[0],lat2[-1]))	
            
    ii=ii+1

    aa=aa-3
    bb=2
    if ncols==3:
        bb=3	
    rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
    ax.append(pylab.axes(rect, axisbg=axesBG))

    cl=pylab.colorbar(pc,ax[ii])
    cl.set_label('m/s',fontsize=labsize)
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
    cl.set_label('m/s',fontsize=labsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)

    # set time ticks
    ss=6
    if ncols==3:
        ss=8
    for rr in range(ss):
        dx=(time2[-1]-time2[0])/3600.0
        dx2=dx/7.0
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
            
    rr=rr+1
    dx=(time2hr[-1]-time2hr[0])/3600.0
    dx2=dx/7.0
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
    
    lat2=scipy.mean(lat,axis=1)
    lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
    
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
    ylim=[lat[0],lat[-1]]
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
    ax[ii].text(xlim[0],(lat.max()-lat.min())*0.15+lat.max(),title,fontsize=labsize, horizontalalignment='left')
    if ncols==1:
        cl=pylab.colorbar(pc)
        cl.set_label(units,fontsize=labsize)
                
    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,vy)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[lat[0],lat[-1]]
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
        ylim=[lat[0],lat[-1]]
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
    ylim=[lat[0],lat[-1]]
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
    ylim=[lat[0],lat[-1]]
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
        ylim=[lat[0],lat[-1]]
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
        pylab.ylim((lat2[0],lat2[-1]))	
        
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
        pylab.ylim((lat2[0],lat2[-1]))

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
    
    lat2=scipy.mean(lat,axis=1)
    lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
    
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
    ylim=[lat[0],lat[-1]]
    pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),shading='flat',vmin=cax1[0],vmax=cax1[1]))
    ax[ii].set_xlim(xlim)
    ax[ii].set_ylim(ylim)
    labels = pylab.getp(ax[ii], 'xticklabels')
    pylab.setp(labels, fontsize=textsize)
    labels = pylab.getp(ax[ii], 'yticklabels')
    pylab.setp(labels, fontsize=textsize)	
    ax[ii].set_ylabel(label, fontsize=labsize)
    ax[ii].set_title('%s mag (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
    ax[ii].text(xlim[0],(lat.max()-lat.min())*0.15+lat.max(),title,fontsize=labsize, horizontalalignment='left')
                
    ii=ii+1
    
    x,dat=plot_utils.timegaps(time,dvx)
    dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]	
    ylim=[lat[0],lat[-1]]
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
    ylim=[lat[0],lat[-1]]
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
    ylim=[lat[0],lat[-1]]
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


def doproc():

#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20061215.008/20061215.008_lp_10sec.h5'
#	fname_hr='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20061215.008/20061215.008_lp_10sec.h5'
    
#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20070214/20070214_lp_3min.h5'
#	fname_hr='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20070214/20070214_lp_1min.h5'

#	fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20070119.001/20070119.001_lp_3min.h5'
#	fname_hr='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/jouleII/20070119.001/20070119.001_lp_1min.h5'

    fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/WorldDay02/20070327.004/20070327.004_lp_3min.h5'
    fname_hr='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/WorldDay02/20070327.004/20070327.004_lp_1min.h5'
    oname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/WorldDay02/20070327.004/20070327.004_lp_3min1min-vvels.png'
    saveout=1
    
    PLAT_OUT1=scipy.arange(65.75,69,0.25)
    PLAT_OUT2=scipy.arange(65.75,69,0.25)

    dat1=readafile(fname)
    dat2=readafile(fname_hr)

    kpn=dat1['/Geomag']['kpn']
    kpe=dat1['/Geomag']['kpe']
    kpar=dat1['/Geomag']['kpar']
    plat=dat1['/Geomag']['plat']
    plong=dat1['/Geomag']['plong']

    ht=dat1['/FittedParams']['Altitude']
    vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]
    dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
    time1=dat1['/Time']['UnixTime']
    yr=dat1['/Time']['Year'][0,0]
    mon=dat1['/Time']['Month'][0,0]
    day=dat1['/Time']['Day'][0,0]
    title='Vector Velocities %d-%d-%d' % (mon, day, yr)

    vlos2=dat2['/FittedParams']['Fits'][:,:,:,0,3]
    dvlos2=dat2['/FittedParams']['Errors'][:,:,:,0,3]
    time2=dat2['/Time']['UnixTime']
    dtime2=dat2['/Time']['dtime']
    
    I=scipy.where((dtime2[:,0]>=10.5) & (dtime2[:,1]<=11.5))[0]
    time2=time2[I,:]
    vlos2=vlos2[I,:,:]
    dvlos2=dvlos2[I,:,:]

    MLTtime1=getmlt(scipy.mean(time1,axis=1),plong[0,0])
    MLTtime2=getmlt(scipy.mean(time2,axis=1),plong[0,0])
    
    for aa in range(vlos1.shape[0]):
        vlosin=scipy.squeeze(vlos1[aa,:,:])
        dvlosin=scipy.squeeze(dvlos1[aa,:,:])
        (plat_out1,Vest,dVest)=vvels.compute_velvec2(PLAT_OUT1,vlosin,dvlosin,kpn,kpe,kpar,plat,plong,ht,htmin=150,htmax=350,covar=[5000.*5000.,5000.*5000.,5.*5.])
        if aa==0:
            vvels1=Vest[scipy.newaxis,:,:]
            dvvels1=dVest[scipy.newaxis,:,:]
        else:
            vvels1=scipy.concatenate((vvels1,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels1=scipy.concatenate((dvvels1,dVest[scipy.newaxis,:,:]),axis=0)

    for aa in range(vlos2.shape[0]):
        vlosin=scipy.squeeze(vlos2[aa,:,:])
        dvlosin=scipy.squeeze(dvlos2[aa,:,:])
        (plat_out2,Vest,dVest)=vvels.compute_velvec2(PLAT_OUT2,vlosin,dvlosin,kpn,kpe,kpar,plat,plong,ht,htmin=150,htmax=350,covar=[5000.*5000.,5000.*5000.,5.*5.])
        if aa==0:
            vvels2=Vest[scipy.newaxis,:,:]
            dvvels2=dVest[scipy.newaxis,:,:]
        else:
            vvels2=scipy.concatenate((vvels2,Vest[scipy.newaxis,:,:]),axis=0)
            dvvels2=scipy.concatenate((dvvels2,dVest[scipy.newaxis,:,:]),axis=0)

    figg=plot_vvels(time1,time2,MLTtime1,MLTtime2,PLAT_OUT1,PLAT_OUT2,vvels1[:,:,1],vvels1[:,:,0],dvvels1[:,:,1],dvvels1[:,:,0],vvels2[:,:,1],vvels2[:,:,0],dvvels2[:,:,1],dvvels2[:,:,0],title=title,p=[100.0,0.25,2000.0])

    if saveout==1:
        figg.savefig(oname)

    return MLTtime1
    
    
