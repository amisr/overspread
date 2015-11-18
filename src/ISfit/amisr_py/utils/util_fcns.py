#!/usr/bin/env python

"""

"""

from amisr_py.constants.constants import *
from amisr_py.lib import *

import scipy, scipy.stats

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
    
# azAverage
def azAverage(input):
    """ Averages a series of azimuth angles """
    
    x=scipy.mean(scipy.sin(input))
    y=scipy.mean(scipy.cos(input))
    
    return scipy.arctan2(x,y)

# complex_median
def complex_median(input,axis=0):
    """ Performs median of real and imaginary parts separately """
    
    return scipy.median(input.real,axis=axis)+scipy.median(input.imag,axis=axis)*1.0j

# range2heightSimple
def range2heightSimple(rng,el):
    """ Converts range to height assuming flat earth """
        
    Nhts=rng.size
    Nbeams=el.size
    el2=scipy.repeat(scipy.reshape(el,(Nbeams,1)),Nhts,axis=1)
    range2=scipy.repeat(scipy.reshape(rng,(1,Nhts)),Nbeams,axis=0)
    alt=range2*scipy.sin(el2*pi/180.)
    
    return alt

# range2height
def range2height(ct_geolib,rng,az,el,CLAT,CLONG,CALT,m=1):
    """ Converts range to geodetic altitude """

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
    
# ne_prof
def ne_prof(Power,Range,Altitude,Model,TxPower,Pulsewidth,TxFrequency,KSYS):
    """ converts power to electron density """
    
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