#! /usr/bin/env python

"""
Real time computations and plots.

~M. Nicolls
last revised: 12/14/2006

"""

import sys
import getopt
import optparse
import os.path
import ConfigParser
import math
import datetime
import tables
import scipy as sc
import ctypes as ct
import scipy.interpolate
import scipy.integrate
import scipy.fftpack
#import ctypes.util
		
# compute_velvec
def compute_velvec(self,Vlosin,klat,klon,plat,plon,SNR,ht):
	# 
	# This function takes the line of sight velocities and resolves them into vector velocities as a function of magnetic latitude.  
	# Needs to know the pairs (self.PAIRS) of beams over which to do the computation.
	#
				
	Vlos=sc.copy(Vlosin)
	(Nrecs,Nbeams,Nhts)=Vlos.shape
		
	# resize some stuff
	klat=sc.repeat(sc.reshape(klat,(1,Nbeams,Nhts)),Nrecs,axis=0)
	klon=sc.repeat(sc.reshape(klon,(1,Nbeams,Nhts)),Nrecs,axis=0)
	plat=sc.repeat(sc.reshape(plat,(1,Nbeams,Nhts)),Nrecs,axis=0)
	plon=sc.repeat(sc.reshape(plon,(1,Nbeams,Nhts)),Nrecs,axis=0)
		
	# this is the magnetic latitude over which the function bins the data
	plat_out=sc.copy(self.PLAT_OUT)
	dp=plat_out[1]-plat_out[0]
	
	# if the data is outside some altitude range or the power is lower than some SNR, set the velocity to nan
	tmp=sc.repeat(sc.reshape(ht,(1,Nbeams,Nhts)),Nrecs,axis=0)/1000.
	I=sc.where((tmp<150.)|(tmp>500.))
	Vlos[I]=sc.nan
	Vlos[sc.where((SNR<0)|(10.*sc.log10(SNR)<-10))]=sc.nan
	
	# loop over the pairs
	for i in range(self.PAIRS.shape[0]):

		# find the beamcode indicies
		a1=sc.where(self.PAIRS[i,0]==self.BMCODES[:,0])[0]
		a2=sc.where(self.PAIRS[i,1]==self.BMCODES[:,0])[0]
		
		if len(a1)==0 or len(a2)==0:
			print 'One of the beamcode pairs you specified isn`t part of the experiment.... can`t compute vvels!'
			sys.exit(-32)
			
		# resolve pairs into perp east and perp north components
		vlat1=sc.squeeze((Vlos[:,a1,:]-klon[:,a1,:]/klon[:,a2,:]*Vlos[:,a2,:])/(klat[:,a1,:]-klon[:,a1,:]/klon[:,a2,:]*klat[:,a2,:]))
		vlon1=sc.squeeze((Vlos[:,a1,:]-klat[:,a1,:]/klat[:,a2,:]*Vlos[:,a2,:])/(klon[:,a1,:]-klat[:,a1,:]/klat[:,a2,:]*klon[:,a2,:]))
		SN1=sc.squeeze((SNR[:,a1,:]+SNR[:,a2,:])/2.0)		
		pl1=sc.squeeze((plat[:,a1,:]+plat[:,a2,:])/2.0)
			
		# just make sure
		vlat1=sc.reshape(vlat1,(Nrecs,Nhts))
		vlon1=sc.reshape(vlon1,(Nrecs,Nhts))
		SN1=sc.reshape(SN1,(Nrecs,Nhts))
		pl1=sc.reshape(pl1,(Nrecs,Nhts))
			
		if i==0:
			vlat=vlat1
			vlon=vlon1
			pl=pl1
			SN=SN1
		else:
			vlat=sc.concatenate((vlat,vlat1),axis=1)
			vlon=sc.concatenate((vlon,vlon1),axis=1)
			pl=sc.concatenate((pl,pl1),axis=1)
			SN=sc.concatenate((SN,SN1),axis=1)
		
	# now bin the results 
	Vpn=sc.zeros((Nrecs,plat_out.shape[0]),dtype=Vlos.dtype)
	Vpe=sc.zeros((Nrecs,plat_out.shape[0]),dtype=Vlos.dtype)
	for g in range(plat_out.shape[0]):
		I=sc.where((pl>=plat_out[g]-dp/2.)&(pl<=plat_out[g]+dp/2.))[1]
		print I
		if len(I)==0:
			Vpn[:,g]=sc.nan
			Vpe[:,g]=sc.nan
		elif len(I)==1:
			Vpn[:,g]=vlat[:,I]
			Vpe[:,g]=vlat[:,I]
		else:
			Vpn[:,g]=sc.nansum(vlat[:,I],axis=1)/sc.nansum(vlat[:,I]/vlat[:,I],axis=1)
			Vpe[:,g]=sc.nansum(vlon[:,I],axis=1)/sc.nansum(vlon[:,I]/vlon[:,I],axis=1)
				
	return plat_out, Vpn, Vpe
		
# compute_velvec2
def compute_velvec2(PLAT,AllVlos,AlldVlos,Allk,AllPlat,AllPlong,Allht,htmin=150.0*1000,htmax=400.0*1000,covar=[1000.*1000.,1000.*1000.,5.*5.],p=[200.0,0.5,2000.0,100.0]):
	# 
	# This function takes the line of sight velocities and resolves them into vector velocities as a function of magnetic latitude.  
	# It uses the Bayesian approach and doesn't care about pairs or anything like that.
	# It does care about the magnetic latitude that you choose for binning, so be wise, eh.
	#
	
	# this is the magnetic latitude over which the function bins the data
	plat_in=scipy.copy(PLAT)
	Nplout=plat_in.shape[0]
	plat_out=scipy.zeros((Nplout),dtype='Float64')
	
	Nparms=Allk.shape[1]
		
	# a priori covariance matrix
	SigmaV=scipy.matrix(scipy.diagflat(covar)) 
	
	# other options
#	snrl=-20. # min SNR in db
#	snru=20. # max SNR in db	
		
	fracerrs=scipy.absolute(AlldVlos)/(scipy.absolute(AllVlos)+p[0])
	abserrs=scipy.absolute(AlldVlos)
	
	# loop over output latitudes
	Nmeas=scipy.zeros((Nplout))
	Vest=scipy.zeros((Nplout,Nparms),dtype=AllVlos.dtype)
	dVest=scipy.zeros((Nplout,Nparms),dtype=AllVlos.dtype)
	dVestAll=scipy.zeros((Nplout,Nparms,Nparms),dtype=AllVlos.dtype)
	for i in range(Nplout):
		plat_out[i]=(plat_in[i,0]+plat_in[i,1])/2.0
#		tsnrdb=scipy.reshape(AllSNRdb[j,:],(Nhts*Nbeams,1))
#		I=scipy.where((AllPlat>plat_in[i])&(AllPlat<plat_in[i+1])&(Allht>htmin)&(Allht<htmax)&(tsnrdb>snrl)&(tsnrdb<snru)&(scipy.isfinite(tvlos)))[0]				
		I=scipy.where((AllPlat>plat_in[i,0])&(AllPlat<plat_in[i,1])&(Allht>htmin)&(Allht<htmax)&(scipy.isfinite(AllVlos))&(fracerrs<p[1])&(abserrs<p[3]))[0]				
		Vest[i,:]=scipy.nan
		dVest[i,:]=scipy.nan
		if len(I) != 0:
			try:
				tvlos=scipy.transpose(scipy.matrix(AllVlos[I]))
				SigmaE=scipy.matrix(scipy.diagflat(AlldVlos[I]*AlldVlos[I]))
				A=scipy.matrix(Allk[I,:])
				tv=SigmaV*scipy.transpose(A)*scipy.linalg.inv(A*SigmaV*scipy.transpose(A)+SigmaE)*tvlos
				ts=scipy.linalg.inv(scipy.transpose(A)*scipy.linalg.inv(SigmaE)*A+scipy.linalg.inv(SigmaV))
				Vest[i,:]=scipy.reshape(scipy.array(tv),(1,Nparms))
				dVest[i,:]=scipy.reshape(scipy.sqrt(scipy.array(scipy.diag(ts))),(1,Nparms))
				dVestAll[i,:,:]=ts
				Nmeas[i]=len(I)
			except:
				''
		
#	Vpn=scipy.reshape(Vest[:,:,0],(Nrecs,Nplout))
#	dVpn=scipy.reshape(dVest[:,:,0],(Nrecs,Nplout))
#	Vpe=scipy.reshape(Vest[:,:,1],(Nrecs,Nplout))
#	dVpe=scipy.reshape(dVest[:,:,1],(Nrecs,Nplout))
#	Vpar=scipy.reshape(Vest[:,:,2],(Nrecs,Nplout))
#	dVpar=scipy.reshape(dVest[:,:,2],(Nrecs,Nplout))
		
	return plat_out, Vest, dVest, dVestAll, Nmeas