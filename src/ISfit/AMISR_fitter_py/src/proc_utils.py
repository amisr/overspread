#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import scipy, ctypes
import geolib

from constants import *

def azAverage(input):
    x=scipy.mean(scipy.sin(input))
    y=scipy.mean(scipy.cos(input))
    return scipy.arctan2(x,y)

def complex_median(input,axis=0):
    
    return scipy.stats.stats.nanmedian(input.real,axis=axis)+scipy.stats.stats.nanmedian(input.imag,axis=axis)*1.0j

def ne_prof(Power,Range,Altitude,Model,TxPower,Pulsewidth,TxFrequency,KSYS):
    
    Nbeams=scipy.size(KSYS)
    Nranges=scipy.size(Range)

    k=2.0*pi*2.0*TxFrequency/v_lightspeed # k vector

    Tr=Model['Te']/Model['Ti']

    k2D2=k*k*((v_epsilon0*v_Boltzmann*Model['Te'])/(Model['Ne']*v_elemcharge*v_elemcharge))
    
    Range2=scipy.repeat(scipy.reshape(Range,(1,Nranges)),Nbeams,axis=0)
    if Nbeams>1:
        Ksys2=scipy.repeat(scipy.reshape(KSYS,(Nbeams,1)),Nranges,axis=1)
    else:
        Ksys2=KSYS

    Ne_Parm=scipy.real(Power*Range2*Range2/(Pulsewidth*TxPower*Ksys2)*(1.0+k2D2)*(1.0+k2D2+Tr))
    Ne_NoTr=scipy.real(Power*Range2*Range2/(Pulsewidth*TxPower*Ksys2)*2.0)
    
    Psc=TxPower*Pulsewidth*Ksys2/(Range2*Range2)
    
    return Ne_Parm,Ne_NoTr,Psc
        
def range2heightSimple(rng,el):
    # 
    # Converts range to height assuming flat earth
    #
        
    Nhts=rng.size
    Nbeams=el.size
    el2=scipy.repeat(scipy.reshape(el,(Nbeams,1)),Nhts,axis=1)
    range2=scipy.repeat(scipy.reshape(rng,(1,Nhts)),Nbeams,axis=0)
    alt=range2*scipy.sin(el2*pi/180.)
    
    return alt

def range2height(ct_geolib,rng,az,el,CLAT,CLONG,CALT,m=1):
    # 
    # Converts range to geodetic altitude
    #

	# get station geocentric lat and distance 
    SLAT,SR=geolib.convrt(ct_geolib,CLAT,CALT,dir=1)
    
    Nhts=len(rng); Nbeams=len(el)
    alt=scipy.zeros((Nbeams,Nhts),dtype='Float64')   
    for ibm in range(Nbeams):
        for iht in range(Nhts):
            PR,GCLAT,GLON,GDLAT,GDALT = geolib.point(ct_geolib,SR,SLAT,CLONG,az[ibm],el[ibm],rng[iht])
            alt[ibm,iht] = GDALT

    if m==1:
        alt=alt*1000.0
    
    return alt

# deal_data
def deal_data(beamcodes,data,BMCODES):
    #
    # This function sorts the data based on beamcode
    #
    # Brute force, not pretty
        
    nbeams=BMCODES.size
    ndims=data.ndim
    nrecs=data.shape[0]
    output=scipy.zeros(data.shape, dtype=data.dtype)
    for i in range(nbeams):
        bm=BMCODES[i]
        for j in range(nrecs):
            try:
                cols=scipy.where(beamcodes[j,:]==bm)[0]
                if len(cols)==1:
                    cols=int(cols)
                    if ndims==3:
                        output[j,i,:]=data[j,cols,:]
                    elif ndims==2:
                        output[j,i]=data[j,cols]
                    elif ndims==4:
                        output[j,i,:,:]=data[j,cols,:,:]
            except:
                raise RuntimeError, 'Beamcode error in deal_data()...'

    return output
    
