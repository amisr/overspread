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
import scipy.stats

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

	ddir='/Volumes/AMISR_004/processed_data/RISR/2010/WorldDay55m/20100315.001'
	fname='20100315.001_lp_3min.h5'; ofname='20090129.002'
	odir='plots_ac_2min-LongLat'
	saveout=1
		
	cmax=[10.0,12.0]
	#cmax=[0,1.0]
	BW=1.25*scipy.pi/180.0

	AltLims=scipy.array([80.0,250.0])*1000.0
	LatLims=scipy.array([65.0,68.0])
	LonLims=scipy.array([263.0,268.0])

#	AltLims=scipy.array([70.0,250.0])*1000.0
#	LatLims=scipy.array([65.0,68.0])

#	AltLims=scipy.array([100.0,400.0])*1000.0
#	LatLims=scipy.array([65.0,70.0])
		
	output=readafile(os.path.join(ddir,fname))
	
	BeamCodes=output['/']['BeamCodes']; Nbeams=BeamCodes.shape[0]
	
	kpe=output['/Geomag']['kpe']
	kpn=output['/Geomag']['kpn']
	kpar=output['/Geomag']['kpar']
	
	geoang=scipy.arcsin(kpn/scipy.sqrt(scipy.power(kpn,2.0)+scipy.power(kpe,2.0)+scipy.power(kpar,2.0)))*180.0/scipy.pi	
	geoang=geoang.real
	geoang=scipy.stats.stats.nanmedian(geoang,axis=1)
	Ibm=scipy.where(geoang>0.0)[0]
	
	Ne=output['/NeFromPower']['Ne_NoTr'][:,Ibm,:]
	Altitude=output['/NeFromPower']['Altitude'][Ibm,:]
	Plat=output['/Geomag']['MagneticLatitude'][Ibm,:]
	Plon=output['/Geomag']['MagneticLongitude'][Ibm,:]
		
	GeomagRange=output['/Geomag']['Range']
	GeomagAlt=output['/Geomag']['Altitude']
#	GeomagAlt=scipy.zeros(GeomagRange.shape)
#	for ibm in range(Nbeams):
#		GeomagAlt[ibm,:]=proc_utils.range2heightSimple(GeomagRange[ibm,:],BeamCodes[ibm,2])
	Time=output['/Time']
	
	BeamCodes=BeamCodes[Ibm,:]
	
	Nbeams=BeamCodes.shape[0]
	Nrecs=Time['UnixTime'].shape[0]
	
	figg=pylab.figure()
	for irec in range(Nrecs): #range(Nrecs):
	
		pylab.ioff()
		pylab.clf()
		
		for ibeam in range(Nbeams):
			
			az=BeamCodes[ibeam,1]
			el=BeamCodes[ibeam,2]
		
			tAlt=scipy.squeeze(Altitude[ibeam,:])
			Ialt=scipy.where((tAlt>=AltLims[0]) & (tAlt<=AltLims[1]))[0]
			tAlt=tAlt[Ialt]

			tNe=scipy.real(scipy.log10(Ne[irec,ibeam,Ialt][:,scipy.newaxis]))
			#tNe=scipy.real((Ne[irec,ibeam,Ialt][:,scipy.newaxis]))/1.0e11
			dat=scipy.ma.masked_where(scipy.isnan(tNe),tNe)

			dAlt=scipy.median(scipy.diff(tAlt))
			tAlt=scipy.arange(tAlt[0]-dAlt/2.0,tAlt[-1]+dAlt/2.0,dAlt)
			tRange=tAlt/scipy.sin(BeamCodes[ibeam,2]*scipy.pi/180.0)
			tAlt=scipy.repeat(tAlt[:,scipy.newaxis],2,axis=1)
			
			I=scipy.where(scipy.isfinite(GeomagAlt[ibeam,:]))[0]
			
			p0=pylab.polyfit(GeomagAlt[ibeam,I],Plat[ibeam,I],1)	
			tPlat0=pylab.polyval(p0,tAlt[:,0])
			p1=pylab.polyfit(GeomagAlt[ibeam,I],Plon[ibeam,I],1)	
			tPlon0=pylab.polyval(p1,tAlt[:,0])
			dif=tPlon0[-1]-tPlon0[0]
			sg=-scipy.sign(dif)
			inMer=0
			if scipy.absolute(scipy.median(tPlon0)-265.5)<=0.5:
				inMer=1
			
			
			tPlat=scipy.repeat(tPlat0[:,scipy.newaxis],2,axis=1)
			dPlat=BW*tRange/110000.0
			tPlat[:,0]=tPlat[:,0]-dPlat/2.0
			tPlat[:,1]=tPlat[:,1]+dPlat/2.0

			pylab.subplot(2,1,1)
			pylab.hold
			pylab.pcolor(tPlat,tAlt/1000.0,dat,shading='flat',vmin=cmax[0],vmax=cmax[1])

			tPlon=scipy.repeat(tPlon0[:,scipy.newaxis],2,axis=1)
			dPlon=2*BW*tRange/110000.0
			
			tPlon[:,0]=tPlon[:,0]-sg*dPlon/2.0
			tPlon[:,1]=tPlon[:,1]+sg*dPlon/2.0

			if inMer==1:
				tPlat=scipy.repeat(tPlat0[:,scipy.newaxis],2,axis=1)

			pylab.subplot(2,1,2)
			pylab.hold
#			for ipl in range(tPlon.shape[0]):
#				tlat=scipy.array([[tPlat0[ipl]+dPlat[ipl]/2.0,tPlat0[ipl]-dPlat[ipl]/2.0],[tPlat0[ipl]+dPlat[ipl]/2.0,tPlat0[ipl]-dPlat[ipl]/2.0]])
#				tlon=scipy.array([[tPlon0[ipl]+dPlon[ipl]/2.0,tPlon0[ipl]+dPlon[ipl]/2.0],[tPlon0[ipl]-dPlon[ipl]/2.0,tPlon0[ipl]-dPlon[ipl]/2.0]])
#				pylab.pcolor(tlat,tlon,[dat[ipl],dat[ipl]],shading='flat',vmin=cmax[0],vmax=cmax[1])

			pylab.pcolor(tPlat,tPlon,dat,shading='flat',vmin=cmax[0],vmax=cmax[1])
			for ialt in scipy.arange(100.0,300.0,100.0):
				tlon=pylab.polyval(p1,[ialt*1e3])
				tlat=pylab.polyval(p0,[ialt*1e3])
				if inMer==0:
					pylab.plot([tlat-dPlat,tlat+dPlat],[tlon-sg*dPlon,tlon+sg*dPlon],'k-')
				else:
					pylab.plot([tlat,tlat],[tlon-sg*dPlon,tlon+sg*dPlon],'k-')



		pylab.subplot(2,1,1)
		pylab.ylim(AltLims/1000.0)
		pylab.xlim(LatLims)
		pylab.ylabel('Altitude (km)')
		#pylab.xlabel('Magnetic Latitude')
		pylab.title('%d/%d/%d %2.2f-%2.2f UT' % (Time['Month'][irec,0],Time['Day'][irec,0],Time['Year'][irec,0],Time['dtime'][irec,0],Time['dtime'][irec,1]))
		pylab.colorbar()

		pylab.subplot(2,1,2)
		pylab.ylim(LonLims)
		pylab.xlim(LatLims)
		pylab.ylabel('Magnetic Longitude')
		pylab.xlabel('Magnetic Latitude')
		pylab.colorbar()

		#pylab.show()
		if saveout:
			oname='%s_%.3d.png' % (ofname,irec)
			figg.savefig(os.path.join(ddir,odir,oname))
