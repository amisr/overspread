#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import os
import sys
import math
import scipy
import ctypes 
import scipy.fftpack
import scipy.interpolate
import scipy.optimize

import struct
import datetime
import array
import glob
import tables
import pylab
import matplotlib

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import vvels
import plot_utils
import proc_utils

# some natural constants
v_lightspeed=299792458
v_Boltzmann=1.380658e-23
v_electronmass=9.1093897e-31
v_amu=1.6605402e-27
v_electronradius=2.81794092e-15
v_epsilon0=8.854188E-12
v_elemcharge=1.602177E-19
pi=math.pi


def readafile(fname):

	h5file=tables.openFile(fname)
	output={}
	for group in h5file.walkGroups("/"):
		output[group._v_pathname]={}
		for array in h5file.listNodes(group, classname = 'Array'):						
			output[group._v_pathname][array.name]=array.read()		
	h5file.close()
	
	return output
	
if __name__ == '__main__':

#	ddir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Lyons02/20071130.001/'
#	ddir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic03/20071018.001/'
	ddir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Grid11x11_01/20071110.001/'
	fname='20071110.001_bc_5min-ne.h5'
	odir='plots_bc_5min-ne-AltLat'
	saveout=1
		
	cmax=[10.0,12.0]
	BW=1.0*scipy.pi/180.0

	AltLims=scipy.array([70.0,250.0])*1000.0
	LatLims=scipy.array([65.0,67.0])

#	AltLims=scipy.array([70.0,250.0])*1000.0
#	LatLims=scipy.array([65.0,68.0])

#	AltLims=scipy.array([100.0,400.0])*1000.0
#	LatLims=scipy.array([65.0,70.0])
		
	output=readafile(os.path.join(ddir,fname))
	
	BeamCodes=output['/']['BeamCodes']
	
	kpe=output['/Geomag']['kpe']
	kpn=output['/Geomag']['kpn']
	
	geoang=scipy.arcsin(scipy.absolute(kpn[:,-1])/scipy.sqrt(scipy.power(kpn[:,-1],2.0)+scipy.power(kpe[:,-1],2.0)))*180.0/scipy.pi	
	geoang=geoang.real
	Ibm=scipy.where(geoang>70.0)[0]

	geoang2=scipy.arcsin(scipy.absolute(kpe[:,-1])/scipy.sqrt(scipy.power(kpn[:,-1],2.0)+scipy.power(kpe[:,-1],2.0)))*180.0/scipy.pi	
	geoang2=geoang2.real
	Ibm2=scipy.where(geoang2>70.0)[0]
	xxx
	
	Ne=output['/NeFromPower']['Ne_NoTr'][:,Ibm,:]
	Altitude=output['/NeFromPower']['Altitude'][Ibm,:]
#	Plat=output['/Geomag']['plat']
	Plat=output['/Geomag']['MagneticLatitude'][Ibm,:]
	GeomagRange=output['/Geomag']['Range']
	if GeomagRange[-1]<1.0e4:
		GeomagRange=GeomagRange*1000.0
	GeomagAlt=proc_utils.range2height(GeomagRange,BeamCodes[:,2])
	Time=output['/Time']
	
	BeamCodes=BeamCodes[Ibm,:]
	
	Nbeams=BeamCodes.shape[0]
	Nrecs=Time['UnixTime'].shape[0]
	
	figg=pylab.figure()
	for irec in range(Nrecs):
	
		pylab.ioff()
		pylab.clf()
		
		for ibeam in range(Nbeams):
		
			tAlt=scipy.squeeze(Altitude[ibeam,:])
			Ialt=scipy.where((tAlt>=AltLims[0]) & (tAlt<=AltLims[1]))[0]
			tAlt=tAlt[Ialt]

			tNe=scipy.real(scipy.log10(Ne[irec,ibeam,Ialt][:,scipy.newaxis]))

			dAlt=scipy.median(scipy.diff(tAlt))
			tAlt=scipy.arange(tAlt[0]-dAlt/2.0,tAlt[-1]+dAlt/2.0,dAlt)
			tRange=tAlt/scipy.sin(BeamCodes[ibeam,2]*scipy.pi/180.0)
			tAlt=scipy.repeat(tAlt[:,scipy.newaxis],2,axis=1)
			
			p=pylab.polyfit(GeomagAlt[ibeam,:],Plat[ibeam,:],1)	
			tPlat=pylab.polyval(p,tAlt[:,0])
			tPlat=scipy.repeat(tPlat[:,scipy.newaxis],2,axis=1)
			dPlat=BW*tRange/110000.0
			tPlat[:,0]=tPlat[:,0]-dPlat/2.0
			tPlat[:,1]=tPlat[:,1]+dPlat/2.0

			dat=scipy.ma.masked_where(scipy.isnan(tNe),tNe)

			pylab.hold
			pylab.pcolor(tPlat,tAlt/1000.0,dat,shading='flat',vmin=cmax[0],vmax=cmax[1])
		
		pylab.ylim(AltLims/1000.0)
		pylab.xlim(LatLims)
		pylab.ylabel('Altitude (km)')
		pylab.xlabel('Magnetic Latitude')
		pylab.title('%d/%d/%d %2.2f-%2.2f UT' % (Time['Month'][irec,0],Time['Day'][irec,0],Time['Year'][irec,0],Time['dtime'][irec,0],Time['dtime'][irec,1]))
		pylab.colorbar()
		pylab.show()

		xxx

		if saveout:
			oname='%s_%.3d.png' % (fname[:-3],irec)
			figg.savefig(os.path.join(ddir,odir,oname))
