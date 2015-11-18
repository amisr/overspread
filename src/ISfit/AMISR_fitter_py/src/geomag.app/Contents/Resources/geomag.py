#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import ctypes, datetime
import scipy, scipy.interpolate
import geolib

from constants import *

R_EARTH=6371.2

# getmlt
def getmlt(aacgm,time,plong):

    aacgm.AACGMConvertMLT.restype=ctypes.c_double
        
    Nbeams,Nranges=plong.shape
    Ntimes=time.shape[0]
    
    MLTtime=scipy.zeros((Ntimes,Nbeams,Nranges),dtype='float64')
    
    for itm in range(Ntimes):
        tmp=datetime.datetime.utcfromtimestamp(time[itm])
        r1=datetime.date(tmp.year,tmp.month,tmp.day)
        doy=int(r1.strftime('%j'))
        secs=doy*24*3600+tmp.hour*3600+tmp.minute*60+tmp.second
        for ibm in range(Nbeams):
            for irng in range(Nranges):    
                MLTtime[itm,ibm,irng]=aacgm.AACGMConvertMLT(ctypes.c_int(tmp.year),ctypes.c_int(secs),ctypes.c_double(plong[ibm,irng]))
        
    return MLTtime

# geomag
def geomag(ct_geolib,YR,BMCODES,CLAT,CLONG,CALT=0.0,rng=scipy.arange(0.,1050.,50.)):
    #
    # This function computes geomagnetic information for each of the beams.
    # Includes calls to .
    #
    
    Nbeams=BMCODES.shape[0]
        
    # initialize all the output vars
    kvec=scipy.zeros((Nbeams,3),'Float64') # k vector (geographic)
    lat=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # lat
    long=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # long
    plat=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # mag lat
    plong=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # mag long
    dip=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # dip angle
    dec=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # dec angle
    ht=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # altitude
    kpn=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # k component, perp north
    kpe=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # k component, perp east
    kpar=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # k component, anti-parallel
    kn=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # k component, north
    ke=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # k component, east
    kz=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # k component, up    
    kgeo=scipy.zeros((Nbeams,rng.shape[0],3),'Float64') # k vector, geodetic coords
    kgmag=scipy.zeros((Nbeams,rng.shape[0],3),'Float64') # k vector, geomag coords
    Bx=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # B north
    By=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # B east
    Bz=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # B down
    B=scipy.zeros((Nbeams,rng.shape[0],3),'Float64') # B vector
    Babs=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # magnitude of B
    Lshell=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # L shell value in Re
    MagMN=scipy.zeros((Nbeams,rng.shape[0]),'Float64') # Magnetic local time midnight in UT hours
                
    # get station geocentric lat and distance 
    SLAT,SR=geolib.convrt(ct_geolib,CLAT,CALT,dir=1)
                    
    # loop over beams
    for i in range(Nbeams):
        AZ=BMCODES[i,1]; EL=BMCODES[i,2]
        az=AZ*pi/180.; el=EL*pi/180.
        a=scipy.array([scipy.cos(el)*scipy.cos(az),scipy.cos(el)*scipy.sin(az),scipy.sin(el)],'Float64') # unit vector in k direction (geographic)
        kvec[i]=a
               
        # loop over ranges
        for j in range(rng.shape[0]):
            
            # geodetic lat, long, altitude
            PR,GCLAT,GLON,GDLAT,GDALT = geolib.point(ct_geolib,SR,SLAT,CLONG,AZ,EL,rng[j])
            ht[i,j]=GDALT; lat[i,j]=GDLAT; long[i,j]=GLON

            RCOR=geolib.coord(ct_geolib,CLAT,CLONG,SR,SLAT,YR,AZ,EL,rng[j],GDLAT,GLON,GDALT)
            dat1,dat2,dat3,dat4=geolib.geocgm01(ct_geolib,YR,GDALT,GCLAT,GLON)
            
            # magnetic field
            br=RCOR[7]*1e-4; bt=RCOR[8]*1e-4; bp=RCOR[9]*1e-4 # r, theta, phi
            Bx[i,j] = -bt; By[i,j] = bp; Bz[i,j] = -br
            B[i,j,0]=Bx[i,j]; B[i,j,1]=By[i,j]; B[i,j,2]=Bz[i,j]
            Babs[i,j] = RCOR[6]*1e-4
                        
            # dip and dec angles
            dip[i,j] = RCOR[28]
            dec[i,j] = RCOR[29]        
            
            # magnetic latitude and longitude
            plat[i,j] = dat3[2] 
            plong[i,j] = dat3[3] 
            
            # Lshell & mag midnight
            Lshell[i,j] = dat3[4] # apex of magnetic field line in Re
            MagMN[i,j] = dat1[10] # local magnetic midnight in UT hours
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
            # compute k vectors using direction cosines 
            kgmag[i,j,0]=RCOR[24]; kpar[i,j]=RCOR[24] # anti-parallel
            kgmag[i,j,1]=RCOR[23]; kpe[i,j]=RCOR[23] # perp-east
            kgmag[i,j,2]=-RCOR[22]; kpn[i,j]=-RCOR[22] # perp-north
            
            kgeo[i,j,0]=-RCOR[19]; kn[i,j]=-RCOR[19] # north
            kgeo[i,j,1]=RCOR[20]; ke[i,j]=RCOR[20] # east
            kgeo[i,j,2]=RCOR[21]; kz[i,j]=RCOR[21] # up
      
    # assign output parameters
    gmag={}
    gmag['Range']=rng*1000.0; gmag['Altitude']=ht*1000.0
    gmag['Latitude']=lat; gmag['Longitude']=long
    gmag['MagneticLatitude']=plat; gmag['MagneticLongitude']=plong
    gmag['kpn']=kpn; gmag['kpe']=kpe; gmag['kpar']=kpar
    gmag['kn']=kn; gmag['ke']=ke; gmag['kz']=kz
    gmag['kvec']=kvec; gmag['kgmag']=kgmag; gmag['kgeo']=kgeo
    gmag['Dip']=dip; gmag['Declination']=dec
    gmag['Bx']=Bx; gmag['By']=By; gmag['Bz']=Bz
    gmag['B']=B; gmag['Babs']=Babs
    gmag['LshellRe']=Lshell; gmag['MLTMidnightUT']=MagMN
        
    return gmag
    
# geomag
def geomagTime(ct_geolib,YR,AZ,EL,CLAT,CLONG,CALT=0.0,rng=scipy.arange(0.,1050.,50.)):
    #
    # This function computes geomagnetic information for each of the beams.
    # Includes calls to .
    #
        
    Ntimes=AZ.shape[0]
    if rng.ndim==1:
        Nranges=rng.shape[0]
        rngIt=rng
    else:
        Nranges=rng.shape[1]    
        
    # initialize all the output vars
    kvec=scipy.zeros((Ntimes,3),'Float64')*scipy.nan # k vector (geographic)
    lat=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # lat
    long=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # long
    plat=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # mag lat
    plong=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # mag long
    dip=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # dip angle
    dec=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # dec angle
    ht=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # altitude
    kpn=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # k component, perp north
    kpe=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # k component, perp east
    kpar=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # k component, anti-parallel
    kn=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # k component, north
    ke=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # k component, east
    kz=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # k component, up    
    kgeo=scipy.zeros((Ntimes,Nranges,3),'Float64')*scipy.nan # k vector, geodetic coords
    kgmag=scipy.zeros((Ntimes,Nranges,3),'Float64')*scipy.nan # k vector, geomag coords
    Bx=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # B north
    By=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # B east
    Bz=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # B down
    B=scipy.zeros((Ntimes,Nranges,3),'Float64')*scipy.nan # B vector
    Babs=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # magnitude of B
    Lshell=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # L shell value in Re
    MagMN=scipy.zeros((Ntimes,Nranges),'Float64')*scipy.nan # Magnetic local time midnight in UT hours
                
    # get station geocentric lat and distance 
    SLAT,SR=geolib.convrt(ct_geolib,CLAT,CALT,dir=1)
                    
    # loop over beams
    for i in range(Ntimes):
        azp=AZ[i]; elp=EL[i]
        az=AZ[i]*pi/180.; el=EL[i]*pi/180.
        a=scipy.array([scipy.cos(el)*scipy.cos(az),scipy.cos(el)*scipy.sin(az),scipy.sin(el)],'Float64') # unit vector in k direction (geographic)
        kvec[i]=a
             
        if rng.ndim==2:
            rngIt=scipy.squeeze(rng[i,:])
                   
        # loop over ranges
        for j in range(Nranges):
            
            if scipy.isfinite(rngIt[j]):
            
                # geodetic lat, long, altitude
                PR,GCLAT,GLON,GDLAT,GDALT = geolib.point(ct_geolib,SR,SLAT,CLONG,azp,elp,rngIt[j])
                ht[i,j]=GDALT; lat[i,j]=GDLAT; long[i,j]=GLON

                RCOR=geolib.coord(ct_geolib,CLAT,CLONG,SR,SLAT,YR,azp,elp,rngIt[j],GDLAT,GLON,GDALT)
                dat1,dat2,dat3,dat4=geolib.geocgm01(ct_geolib,YR,GDALT,GCLAT,GLON)
            
                # magnetic field
                br=RCOR[7]*1e-4; bt=RCOR[8]*1e-4; bp=RCOR[9]*1e-4 # r, theta, phi
                Bx[i,j] = -bt; By[i,j] = bp; Bz[i,j] = -br
                B[i,j,0]=Bx[i,j]; B[i,j,1]=By[i,j]; B[i,j,2]=Bz[i,j]
                Babs[i,j] = RCOR[6]*1e-4
                        
                # dip and dec angles
                dip[i,j] = RCOR[28]
                dec[i,j] = RCOR[29]        
            
                # magnetic latitude and longitude
                plat[i,j] = dat3[2] 
                plong[i,j] = dat3[3] 
            
                # Lshell & mag midnight
                Lshell[i,j] = dat3[4] # apex of magnetic field line in Re
                MagMN[i,j] = dat1[10] # local magnetic midnight in UT hours
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                # compute k vectors using direction cosines 
                kgmag[i,j,0]=RCOR[24]; kpar[i,j]=RCOR[24] # anti-parallel
                kgmag[i,j,1]=RCOR[23]; kpe[i,j]=RCOR[23] # perp-east
                kgmag[i,j,2]=-RCOR[22]; kpn[i,j]=-RCOR[22] # perp-north
            
                kgeo[i,j,0]=-RCOR[19]; kn[i,j]=-RCOR[19] # north
                kgeo[i,j,1]=RCOR[20]; ke[i,j]=RCOR[20] # east
                kgeo[i,j,2]=RCOR[21]; kz[i,j]=RCOR[21] # up
      
    # assign output parameters
    gmag={}
    gmag['Range']=rngIt*1000.0; gmag['Altitude']=ht*1000.0
    gmag['Latitude']=lat; gmag['Longitude']=long
    gmag['MagneticLatitude']=plat; gmag['MagneticLongitude']=plong
    gmag['kpn']=kpn; gmag['kpe']=kpe; gmag['kpar']=kpar
    gmag['kn']=kn; gmag['ke']=ke; gmag['kz']=kz
    gmag['kvec']=kvec; #gmag['kgmag']=kgmag; gmag['kgeo']=kgeo
    gmag['Dip']=dip; gmag['Declination']=dec
    gmag['Bx']=Bx; gmag['By']=By; gmag['Bz']=Bz
    #gmag['B']=B; 
    gmag['Babs']=Babs
    gmag['LshellRe']=Lshell; gmag['MLTMidnightUT']=MagMN
        
    return gmag	
    
def blankGmag(Nx=1,Ny=1):
    
    gmag={}
    gmag['Range']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Altitude']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Latitude']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Longitude']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['MagneticLatitude']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan 
    gmag['MagneticLongitude']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['kpn']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan 
    gmag['kpe']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['kpar']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['kn']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan 
    gmag['ke']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['kz']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['kvec']=scipy.zeros((Nx,3),dtype='Float32')*scipy.nan
    gmag['Dip']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Declination']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Bx']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['By']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Bz']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['Babs']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['LshellRe']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan
    gmag['MLTMidnightUT']=scipy.zeros((Nx,Ny),dtype='Float32')*scipy.nan

    return gmag	
    
# rturn_gmag
def rturn_gmag(gmag,Range,extras=['kvec']):
    #
    # This function just interpolates over the geomagnetic data 
    # keeping only 2 dimensional arrays (throws out some redundant info)
    #

    dd={}
    dd['Range']=Range
    for key in gmag.keys():
        if (scipy.ndim(scipy.asarray(gmag[key]))==2) and (gmag[key].shape[1]==gmag['Range'].shape[0]):
            dd[key]=scipy.zeros(Range.shape)
            for ibm in range(Range.shape[0]):
                dd[key][ibm,:]=scipy.interpolate.interp1d(gmag['Range'],gmag[key][ibm,:],bounds_error=0)(Range[ibm,:])

    for key in range(len(extras)):
        try:
            dd[extras[key]]=gmag[extras[key]]
        except:
            ''
            
    return dd

def trim_gmag(gmag,Nbeams,Ibeams):
    
    dd=gmag
    for key in gmag.keys():
        if (gmag[key].shape[0]==Nbeams):
            dd[key]=dd[key][Ibeams,:]
    
    return dd
