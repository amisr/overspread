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
import scipy.linalg

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

def forwardmodel(parm,htout,ht,k,mob,kappa):

	Nmeas=ht.size
	Neqs=htout.shape[0]*3+3

	# form C matrix
	C=scipy.array([[1.0/(1.0+kappa*kappa),-kappa/(1.0+kappa*kappa),scipy.zeros(kappa.shape,kappa.dtype)],
		[kappa/(1.0+kappa*kappa),1.0/(1.0+kappa*kappa),scipy.zeros(kappa.shape,kappa.dtype)],
		[scipy.zeros(kappa.shape,kappa.dtype),scipy.zeros(kappa.shape,kappa.dtype),scipy.ones(kappa.shape,kappa.dtype)]])	
		
	# build the A matrix
	Amatrix=scipy.matrix(scipy.zeros((Nmeas,Neqs),dtype='float64'))
	for aa in range(Nmeas):
		Amatrix[aa,0:3]=mob[aa]*scipy.matrix(k[aa,:])*scipy.matrix(C[:,:,aa]) # electric field terms
	for aa in range(htout.shape[0]):
		I=scipy.where((ht>=htout[aa,0])&(ht<=htout[aa,1]))[0]
		for bb in range(len(I)):
			Amatrix[I[bb],((aa+1)*3):((aa+1)*3+3)]=scipy.matrix(k[I[bb],:])*scipy.matrix(C[:,:,I[bb]]) # neutral wind terms
		
	VlosEst=scipy.matrix(Amatrix)*scipy.matrix(parm) # the forward model

	return scipy.array(VlosEst)

def fit_fun(nuScaler,htout,Allht,AllVlos,AlldVlos,Allk,Allmob,Allkappa,decht,dipht):

	#print nuScaler

	(htout,test,terr,VlosEst,Idid)=invertwinds(htout,Allht,AllVlos,AlldVlos,Allk,Allmob/nuScaler,Allkappa/nuScaler,decht,dipht)
				
	y=(AllVlos[Idid]-scipy.squeeze(VlosEst))/scipy.sqrt(AlldVlos[Idid])
	y=y.real
	y=y[scipy.where(scipy.isfinite(y))]
	
	return y.astype('float64')

def invertwinds(htout,ht,Vlos,dVlos,k,mob,kappa,dec,dip):

	htmin=htout[0,0]
	htmax=htout[-1,1]

	decall=scipy.zeros(htout.shape[0]+1,dtype='float64')
	dipall=scipy.zeros(htout.shape[0]+1,dtype='float64')
	decall[0]=scipy.stats.stats.nanmean(dec)
	decall[1:]=dec
	dipall[0]=scipy.stats.stats.nanmean(dip)
	dipall[1:]=dip

	decall[scipy.where(scipy.isnan(decall))]=scipy.stats.stats.nanmean(dec)
	dipall[scipy.where(scipy.isnan(dipall))]=scipy.stats.stats.nanmean(dip)

	ht=ht.copy()
	Vlos=Vlos.copy()
	dVlos=dVlos.copy()
	k=k.copy()
	mob=mob.copy()
	kappa=kappa.copy()
	
	fracerrs=scipy.absolute(dVlos)/(scipy.absolute(Vlos)+100.0)
	abserrs=scipy.absolute(dVlos)

	# get rid of some obvious bad points.
	I=scipy.where((ht>=htmin) & (ht<=htmax) & (abserrs<100.0))
	ht=ht[I]
	Vlos=Vlos[I]
	dVlos=dVlos[I]
	k=k[I]
	mob=mob[I]
	kappa=kappa[I]
	Iout=I

	Nmeas=Vlos.size
	Neqs=htout.shape[0]*3+3

	# initialize output matrices
	xout=scipy.zeros((Neqs),dtype=Vlos.dtype)
	dxout=scipy.zeros((Neqs),dtype=Vlos.dtype)
	
	# a priori covariance matrix
	windvar=1000.0*1000.0
	Zwindvar=5.0*5.0
	te=10.0e-1
	tep=1e-5
	# create the cov matrix in geographic coordinates
	covar=scipy.ones(Neqs,dtype='float64')*windvar
	covar[5::3]=Zwindvar
	covar[0:3]=0.0 
	covar[-9:]=Zwindvar
	SigmaV=scipy.array(scipy.diagflat(covar)) # full matrix in geo coords
	# rotate to geomagnetic coordinates
	SigmaV=gmag2geo_covar(SigmaV,decall*pi/180.0,dipall*pi/180.0,direction=1)
	# put in electric field guy
	efieldvar=scipy.matrix(scipy.diagflat([te*te,te*te,tep*tep]))
	SigmaV[0:3,0:3]=efieldvar

	# form C matrix
	C=scipy.array([[1.0/(1.0+kappa*kappa),-kappa/(1.0+kappa*kappa),scipy.zeros(kappa.shape,kappa.dtype)],
		[kappa/(1.0+kappa*kappa),1.0/(1.0+kappa*kappa),scipy.zeros(kappa.shape,kappa.dtype)],
		[scipy.zeros(kappa.shape,kappa.dtype),scipy.zeros(kappa.shape,kappa.dtype),scipy.ones(kappa.shape,kappa.dtype)]])	
	
	# build the A matrix
	Amatrix=scipy.matrix(scipy.zeros((Nmeas,Neqs),dtype='float64'))
	for aa in range(Nmeas):
		Amatrix[aa,0:3]=mob[aa]*scipy.matrix(k[aa,:])*scipy.matrix(C[:,:,aa]) # electric field terms
	for aa in range(htout.shape[0]):
		I=scipy.where((ht>=htout[aa,0])&(ht<=htout[aa,1]))[0]
		for bb in range(len(I)):
			Amatrix[I[bb],((aa+1)*3):((aa+1)*3+3)]=scipy.matrix(k[I[bb],:])*scipy.matrix(C[:,:,I[bb]]) # neutral wind terms

	# error covariance matrix
	SigmaE=scipy.matrix(scipy.diagflat(dVlos*dVlos))
	
	Vlos=scipy.matrix(Vlos[:,scipy.newaxis])

	# do the inversion
	try:
		test=SigmaV*scipy.transpose(Amatrix)*scipy.linalg.inv(Amatrix*SigmaV*scipy.transpose(Amatrix)+SigmaE)*Vlos # estimate
		terr=scipy.linalg.inv(scipy.transpose(Amatrix)*scipy.linalg.inv(SigmaE)*Amatrix+scipy.linalg.inv(SigmaV)) # covariance matrix
	except:
		test=scipy.nan*scipy.zeros((Neqs))
		terr=scipy.nan*scipy.zeros((Neqs,Neqs))
		
	VlosEst=scipy.matrix(Amatrix)*scipy.matrix(test) # the forward model
		
	return htout,scipy.array(test),scipy.array(terr),scipy.array(VlosEst),Iout

def eregwinds2():

	datdir='/Volumes/AMISR_004/processed_data/joule2/20070117.004'

	fname_lpvvels='derivedParams/20070117.004_lp_1min-vvels-180sec.h5'
	fname_acvvels='derivedParams/20070117.004_ac_10min-phaseerrs-noteti-vvels-alt-600sec.h5'
	fname_ac='20070117.004_ac_10min-phaseerrs-noteti.h5'
	oname=os.path.join(datdir,'derivedParams','winds-lp_1min-ac_10min.h5')

	dat=readafile(os.path.join(datdir,fname_ac))
	dat1=readafile(os.path.join(datdir,fname_acvvels))
	dat2=readafile(os.path.join(datdir,fname_lpvvels))
	
	# alternating code vvels
	BeamCodes=dat1['/']['BeamCodes']
	Babs=dat1['/']['Babs']
	dip=dat1['/']['Dip']
	dec=dat1['/']['Declination']
	plat=dat1['/']['MagneticLatitude']
	time=dat1['/Time']['UnixTime']
	dtime=dat1['/Time']['dtime']
	vvelsac=dat1['/VectorVels']['Vest']
	dvvelsac=dat1['/VectorVels']['dVest']
	ht=dat1['/VectorVels']['Altitude']
	
	# alternating code il
	timeac=dat['/Time']['UnixTime']
	Ibm=[]
	for ii in range(BeamCodes.shape[0]):
		Ibm.append(int(scipy.where(dat['/']['BeamCodes'][:,0]==BeamCodes[ii,0])[0]))
	nuin=dat['/FittedParams']['Fits'][:,:,:,0,2][:,Ibm,:]
	htac=dat['/FittedParams']['Altitude'][Ibm,:]
	
	# long pulse vvels
	timelp=dat2['/Time']['UnixTime']
	vvelslp=dat2['/VectorVels']['Vest']
	dvvelslp=dat2['/VectorVels']['dVest']
	platlp=dat2['/VectorVels']['Plat']
	platlpmean=scipy.mean(platlp,axis=1)

	Ugmag=scipy.zeros(vvelsac.shape,dtype=vvelsac.dtype)
	errUgmag=scipy.zeros(vvelsac.shape,dtype=vvelsac.dtype)
	Ugeo=scipy.zeros(vvelsac.shape,dtype=vvelsac.dtype)
	errUgeo=scipy.zeros(vvelsac.shape,dtype=vvelsac.dtype)
	EfieldAll=scipy.zeros(vvelsac.shape,dtype=vvelsac.dtype)
	errEfieldAll=scipy.zeros(vvelsac.shape,dtype=vvelsac.dtype)
	for irec in range(time.shape[0]):
	
		# get nuin
		Itm=scipy.where((timeac[:,0]>=time[irec,0]) & (timeac[:,1]<=time[irec,1]))[0]
		tnuin=scipy.mean(nuin[Itm,:,:],axis=0)
	
		# get AC vvels
		tvvelsac=vvelsac[irec,:,:]
	
		# get LP vvels
		Itm=scipy.where((timelp[:,0]>=time[irec,0]) & (timelp[:,1]<=time[irec,1]))[0]
		if len(Itm)==0:
			Itm=scipy.where((timelp[:,0]<=scipy.mean(time[irec,:])) & (timelp[:,1]>=scipy.mean(time[irec,:])))[0]
		tvvelslp=scipy.stats.stats.nanmean(vvelslp[Itm,:,:],axis=0)
		
		for iht in range(ht.shape[0]):
			# compute electric field
			tbabs=Babs[iht]
			tdec=dec[iht]
			tdip=dip[iht]
			tplat=plat[iht]
			tv=scipy.array([0.0,0.0])*scipy.nan
			if tplat>=platlpmean[0]:
				tv[0]=scipy.interpolate.interp1d(platlpmean,tvvelslp[:,0],bounds_error=0)(tplat)
				tv[1]=scipy.interpolate.interp1d(platlpmean,tvvelslp[:,1],bounds_error=0)(tplat)
			if scipy.isnan(tv[0]):
				Inan=scipy.where(scipy.isfinite(tvvelslp[:,0]))[0]
				if len(Inan)>0:
					tv[0]=tvvelslp[Inan[2],0]
			if scipy.isnan(tv[1]):
				Inan=scipy.where(scipy.isfinite(tvvelslp[:,1]))[0]
				if len(Inan)>0:
					tv[1]=tvvelslp[Inan[2],1]	
			E=scipy.array([[tv[0]*tbabs],[-tv[1]*tbabs],[0.0]]) # pe, pn, par
			EfieldAll[irec,iht,:]=scipy.squeeze(E)
			errEfieldAll[irec,iht,:]=scipy.squeeze(E)*0.0
			
			# mobility and ratio of gyro to collision freq
			Iht=scipy.where((htac>=ht[iht,0]) & (htac<=ht[iht,1]))
			ttnuin=scipy.mean(tnuin[Iht])
			mob=v_elemcharge/(v_amu*30.5*ttnuin)
			kappa=mob*tbabs
			# form C matrix 
			C=scipy.array([[1.0/(1.0+kappa*kappa),-kappa/(1.0+kappa*kappa),scipy.zeros(kappa.shape,kappa.dtype)],
				[kappa/(1.0+kappa*kappa),1.0/(1.0+kappa*kappa),scipy.zeros(kappa.shape,kappa.dtype)],
				[scipy.zeros(kappa.shape,kappa.dtype),scipy.zeros(kappa.shape,kappa.dtype),scipy.ones(kappa.shape,kappa.dtype)]])	
			Cinv=scipy.linalg.inv(scipy.matrix(C))
			vi=scipy.array([[tvvelsac[iht,1]],[tvvelsac[iht,0]],[tvvelsac[iht,2]]]) # pe, pn, par
			
			# estimate winds
			u=-mob*scipy.matrix(E)+Cinv*scipy.matrix(vi)
			Ugmag[irec,iht,:]=scipy.squeeze(u)
			errUgmag[irec,iht,0]=0.0 # perp east
			errUgmag[irec,iht,1]=0.0 # perp north
			errUgmag[irec,iht,2]=0.0 # par
			# wind in geographic coords
			Ugeo[irec,iht,:] = scipy.squeeze(gmag2geo(scipy.array([[Ugmag[irec,iht,0]],[Ugmag[irec,iht,1]],[Ugmag[irec,iht,2]]]),tdec*pi/180.0,tdip*pi/180.0))
#			terrgeo = gmag2geo_covar(terr[3:,3:],decht*pi/180.0,dipht*pi/180.0)
#			errWind=scipy.sqrt(scipy.diag(terrgeo))
			errUgeo[irec,iht,0]=0.0 # east
			errUgeo[irec,iht,1]=0.0 # north
			errUgeo[irec,iht,2]=0.0 # up

	Efield=scipy.mean(EfieldAll,axis=1)
	errEfield=scipy.mean(errEfieldAll,axis=1)
	
	print 'Writing output to' + oname
	outh5file=tables.openFile(oname, mode = "w", title = "Fit File")
	write_outputfile(outh5file,Ugmag,groupname='',name='WindGmag')
	write_outputfile(outh5file,Ugeo,groupname='',name='WindGeo')
	write_outputfile(outh5file,errUgmag,groupname='',name='errWindGmag')
	write_outputfile(outh5file,errUgeo,groupname='',name='errWindGeo')
	write_outputfile(outh5file,Efield,groupname='',name='Efield')
	write_outputfile(outh5file,errEfield,groupname='',name='errEfield')
	write_outputfile(outh5file,time,groupname='',name='UnixTime')
	write_outputfile(outh5file,dtime,groupname='',name='dTime')
	write_outputfile(outh5file,ht,groupname='',name='Altitude')
	outh5file.close()	
	
	
	figg=plot_winds(Efield,errEfield,Ugmag,errUgmag,ht/1000.0,time)
	figg.savefig(oname[:-3]+'-WindGmag.png')	
	figg=plot_winds(Efield,errEfield,Ugeo,errUgeo,ht/1000.0,time,geo=1)
	figg.savefig(oname[:-3]+'-WindGeo.png')	
		

def eregwinds():

	nuinScaler=1.0	; do_iterate=0;
	
	"""
	fname_ac='/Volumes/AMISR_004/processed_data/Joule2/20070119.001/20070119.001_ac_15min-phaseerrs.h5'
	fname_lp='/Volumes/AMISR_004/processed_data/Joule2/20070119.001/20070119.001_lp_5min.h5'
	oname='/Volumes/AMISR_004/processed_data/Joule2/20070119.001/derivedParams/20070119.001-winds_lp5min_ac15min.h5'

	fname_ac='/Volumes/AMISR_004/processed_data/joule2/20070213.001/20070213.001_ac_5min-cal.h5'
	fname_lp='/Volumes/AMISR_004/processed_data/joule2/20070213.001/20070213.001_lp_2min-cal.h5'
	oname='/Volumes/AMISR_004/processed_data/joule2/20070213.001/derivedParams/20070213.001-winds_lp2min_ac5min.h5'
	
	fname_ac='/Volumes/AMISR_004/processed_data/LTCS01/20071209.006/20071209.006_ac_15min.h5'
	fname_lp='/Volumes/AMISR_004/processed_data/LTCS01/20071209.006/20071209.006_lp_5min.h5'
	oname='/Volumes/AMISR_004/processed_data/LTCS01/20071209.006/derivedParams/20071209.006-winds_lp5min_ac15min.h5'
	"""
	"""
	fname_ac='/Volumes/AMISR_004/processed_data/2007/LTCS01/20071209.006/20071209.006_ac_15min-cal.h5'
	fname_lp='/Volumes/AMISR_004/processed_data/2007/LTCS01/20071209.006/20071209.006_lp_15min-cal.h5'
	oname='/Volumes/AMISR_004/processed_data/2007/LTCS01/20071209.006/derivedParams/20071209.006-winds_lp15min_ac15min.h5'
	
	fname_ac='/Volumes/AMISR_004/processed_data/2008/IPY05/20080404.004.done/IPY05-20080404.004_ac_15min-cal.h5'
	fname_lp='/Volumes/AMISR_004/processed_data/2008/IPY05/20080404.004.done/IPY05-20080404.004_lp_15min-cal.h5'
	oname='/Volumes/AMISR_004/processed_data/2008/IPY05/20080404.004.done/derivedParams/IPY05-20080404.004-winds_lp15min_ac15min.h5'
	"""

	exp='20080227.005'
	fname_ac='/Volumes/AMISR_004/processed_data/2008/Erickson01/'+exp+'.done/' + exp +'_ac_5min-cal.h5'
	fname_lp='/Volumes/AMISR_004/processed_data/2008/Erickson01/'+exp+'.done/' + exp + '_lp_2min-cal.h5'
	oname='/Volumes/AMISR_004/processed_data/2008/Erickson01/'+exp+'.done/derivedParams/nuin1.0/'+exp+'-winds_lp2min_ac5min.h5'
	

	CHIRP=0.0

	
	dat1=readafile(fname_ac)
	dat2=readafile(fname_lp)

	# alternating code
	ne1=dat1['/FittedParams']['Ne']
	dne1=dat1['/FittedParams']['dNe']
	vlos1=dat1['/FittedParams']['Fits'][:,:,:,0,3]+CHIRP
	dvlos1=dat1['/FittedParams']['Errors'][:,:,:,0,3]
	time1=dat1['/Time']['UnixTime']
	dtime1=dat1['/Time']['dtime']
	(Nrecs1,Nbeams1,Nhts1)=vlos1.shape
	ht1=dat1['/FittedParams']['Altitude']
	kpn1=dat1['/Geomag']['kpn']; kpe1=dat1['/Geomag']['kpe']; kpar1=dat1['/Geomag']['kpar']
	k1=scipy.zeros((Nbeams1,Nhts1,3),dtype=kpn1.dtype)
	k1[:,:,0]=kpe1; k1[:,:,1]=kpn1; k1[:,:,2]=kpar1
	Babs1=dat1['/Geomag']['Babs']; 
	if Babs1[0,0]<1.0e-5: 
		Babs1=Babs1*1.0e5
	dec1=dat1['/Geomag']['Declination']; dip1=dat1['/Geomag']['Dip']
	mass=dat1['/FittedParams']['IonMass']
	fraction=dat1['/FittedParams']['Fits'][:,:,:,0:2,0] # O+ fraction
	nuin=dat1['/FittedParams']['Fits'][:,:,:,0:2,2]
	mob=v_elemcharge/(v_amu*nuin); mob[:,:,:,0]=mob[:,:,:,0]/mass[0]; mob[:,:,:,1]=mob[:,:,:,1]/mass[1]
	kappa=mob*1.0; kappa[:,:,:,0]=kappa[:,:,:,0]*Babs1; kappa[:,:,:,1]=kappa[:,:,:,1]*Babs1
	# mass-weighted params
	nuin1=nuin[:,:,:,0]*fraction[:,:,:,0] + nuin[:,:,:,1]*fraction[:,:,:,1] # mass weighted collision frequency
	mob1=mob[:,:,:,0]*fraction[:,:,:,0]+mob[:,:,:,1]*fraction[:,:,:,1] # mass weighted mobility
	kappa1=kappa[:,:,:,0]*fraction[:,:,:,0]+kappa[:,:,:,1]*fraction[:,:,:,1] # mass weighted kappa
	sp1=ne1*v_elemcharge*v_elemcharge/(v_amu*mass[0]*nuinScaler*nuin[:,:,:,0]*(1.0+(kappa[:,:,:,0]/nuinScaler)**2.0))*fraction[:,:,:,0]
	+ne1*v_elemcharge*v_elemcharge/(v_amu*mass[1]*nuinScaler*nuin[:,:,:,1]*(1.0+(kappa[:,:,:,1]/nuinScaler)**2.0))*fraction[:,:,:,1] # Pedersen conductance
	
	BMCODES=dat1['/']['BeamCodes']; Nbeams0=BMCODES.shape[0]
	Ibm=scipy.where((BMCODES[:,2]>=50.0) & (BMCODES[:,2]<=90.0) & (BMCODES[:,1]>=-180.0) & (BMCODES[:,1]<=180.0))[0]
	Nbeams1=len(Ibm)
	print Nbeams1
	print BMCODES[Ibm,:]
#	xxx

	# long pulse
	ne2=dat2['/FittedParams']['Ne']
	dne2=dat2['/FittedParams']['dNe']	
	vlos2=dat2['/FittedParams']['Fits'][:,:,:,0,3]+CHIRP
	dvlos2=dat2['/FittedParams']['Errors'][:,:,:,0,3]
	time2=dat2['/Time']['UnixTime']; time2m=scipy.mean(time2,axis=1); 
	(Nrecs2,Nbeams2,Nhts2)=vlos2.shape
	ht2=dat2['/FittedParams']['Altitude']
	kpn2=dat2['/Geomag']['kpn']
	kpe2=dat2['/Geomag']['kpe']
	kpar2=dat2['/Geomag']['kpar']
	k2=scipy.zeros((Nbeams2,Nhts2,3),dtype=kpn1.dtype)
	k2[:,:,0]=kpe2; k2[:,:,1]=kpn2; k2[:,:,2]=kpar2
	Babs2=dat2['/Geomag']['Babs']
	if Babs2[0,0]<1.0e-5: 
		Babs2=Babs2*1.0e5
	dec2=dat2['/Geomag']['Declination']
	dip2=dat2['/Geomag']['Dip']
	mass=dat2['/FittedParams']['IonMass']
	fraction=dat2['/FittedParams']['Fits'][:,:,:,0:2,0] # O+ fraction
	nuin=dat2['/FittedParams']['Fits'][:,:,:,0:2,2]
	mob=v_elemcharge/(v_amu*nuin); mob[:,:,:,0]=mob[:,:,:,0]/mass[0]; mob[:,:,:,1]=mob[:,:,:,1]/mass[1]
	kappa=mob*1.0; kappa[:,:,:,0]=kappa[:,:,:,0]*Babs2; kappa[:,:,:,1]=kappa[:,:,:,1]*Babs2
	# mass-weighted params
	nuin2=nuin[:,:,:,0]*fraction[:,:,:,0] + nuin[:,:,:,1]*fraction[:,:,:,1] # mass weighted collision frequency
	mob2=mob[:,:,:,0]*fraction[:,:,:,0]+mob[:,:,:,1]*fraction[:,:,:,1] # mass weighted mobility
	kappa2=kappa[:,:,:,0]*fraction[:,:,:,0]+kappa[:,:,:,1]*fraction[:,:,:,1] # mass weighted kappa
	sp2=ne2*v_elemcharge*v_elemcharge/(v_amu*mass[0]*nuin[:,:,:,0]*(1.0+kappa[:,:,:,0]**2.0))*fraction[:,:,:,0]
	+ne2*v_elemcharge*v_elemcharge/(v_amu*mass[1]*nuin[:,:,:,1]*(1.0+kappa[:,:,:,1]**2.0))*fraction[:,:,:,1] # Pedersen conductance
		
	Nbeams2=Nbeams1
	
	#
	LPminAlt=200.0
	LPmaxAlt=400.0 
	tm1=scipy.arange(80.0,150.0,5.0)
#	tm2=scipy.arange(200.0,400.0,50)
#	htout=scipy.zeros((tm1.size+tm2.size,2),dtype='float64')
#	htout[:,0]=scipy.concatenate((tm1,tm2))
#	htout[:,1]=scipy.concatenate((tm1+tm1[1]-tm1[0],tm2+tm2[1]-tm2[0]))
	htout=scipy.zeros((tm1.size,2),dtype='float64')
	htout[:,0]=tm1
	htout[:,1]=tm1+tm1[1]-tm1[0]
	htoutm=scipy.mean(htout,axis=1)

	print htout

	NuinScaler=scipy.zeros((time1.shape[0]),dtype='float64')*scipy.nan
	errNuinScaler=scipy.zeros((time1.shape[0]),dtype='float64')*scipy.nan
	Efield=scipy.zeros((time1.shape[0],3),dtype='float64')*scipy.nan
	errEfield=scipy.zeros((time1.shape[0],3),dtype='float64')*scipy.nan
	WindGmag=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	errWindGmag=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	WindGeo=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	errWindGeo=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	VestGmag=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	errVestGmag=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	VestGeo=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	errVestGeo=scipy.zeros((time1.shape[0],htout.shape[0],3),dtype='float64')*scipy.nan
	
	CurrentGmag=scipy.zeros((ne1.shape[0],ne1.shape[1],ne1.shape[2],2),dtype='float64')*scipy.nan
	errCurrentGmag=scipy.zeros((ne1.shape[0],ne1.shape[1],ne1.shape[2],2),dtype='float64')*scipy.nan
	JouleHeatingE=scipy.zeros((ne1.shape[0],ne1.shape[1],ne1.shape[2]),dtype='float64')*scipy.nan
	errJouleHeatingE=scipy.zeros((ne1.shape[0],ne1.shape[1],ne1.shape[2]),dtype='float64')*scipy.nan
	JouleHeatingTotal=scipy.zeros((ne1.shape[0],ne1.shape[1],ne1.shape[2]),dtype='float64')*scipy.nan
	errJouleHeatingTotal=scipy.zeros((ne1.shape[0],ne1.shape[1],ne1.shape[2]),dtype='float64')*scipy.nan
	IntJouleHeatingE=scipy.zeros((ne1.shape[0],ne1.shape[1]),dtype='float64')*scipy.nan
	errIntJouleHeatingE=scipy.zeros((ne1.shape[0],ne1.shape[1]),dtype='float64')*scipy.nan
	IntJouleHeatingTotal=scipy.zeros((ne1.shape[0],ne1.shape[1]),dtype='float64')*scipy.nan
	errIntJouleHeatingTotal=scipy.zeros((ne1.shape[0],ne1.shape[1]),dtype='float64')*scipy.nan
	
	VlosEst1=scipy.zeros(vlos1.shape,dtype=vlos1.dtype)*scipy.nan

	for itm in range(time1.shape[0]):
		print itm
	
		try:
			# alternating code information
			AllBabs=scipy.reshape(Babs1[Ibm,:],(Nhts1*Nbeams1))
			Alldec=scipy.reshape(dec1[Ibm,:],(Nhts1*Nbeams1))
			Alldip=scipy.reshape(dip1[Ibm,:],(Nhts1*Nbeams1))
			Allk=scipy.reshape(k1[Ibm,:],(Nhts1*Nbeams1,3))
			Allht=scipy.reshape(ht1[Ibm,:],(Nhts1*Nbeams1))/1000.0
			AllVlos=scipy.reshape(vlos1[itm,Ibm,:],(Nhts1*Nbeams1))
			AlldVlos=scipy.reshape(dvlos1[itm,Ibm,:],(Nhts1*Nbeams1))
			Allmob=scipy.reshape(mob1[itm,Ibm,:],(Nhts1*Nbeams1))
			Allkappa=scipy.reshape(kappa1[itm,Ibm,:],(Nhts1*Nbeams1))
#			Iht=scipy.where(Allht>=tm2[0])[0]
			Iht=scipy.where(Allht>htout[-1,-1])[0]
			AllVlos[Iht]=scipy.nan
			# long pulse information
			I=scipy.where((time1[itm,0]<=time2m) & (time1[itm,1]>=time2m))[0]
			print I
			tht=ht2[Ibm,:]
			tht=scipy.repeat(tht[scipy.newaxis,:,:],len(I),axis=0)
			tht=scipy.reshape(tht,(len(I)*Nhts2*Nbeams2))/1000.0
			tBabs=Babs2[Ibm,:]
			tBabs=scipy.repeat(tBabs[scipy.newaxis,:,:],len(I),axis=0)
			tBabs=scipy.reshape(tBabs,(len(I)*Nhts2*Nbeams2))
			tdec=dec2[Ibm,:]
			tdec=scipy.repeat(tdec[scipy.newaxis,:,:],len(I),axis=0)
			tdec=scipy.reshape(tdec,(len(I)*Nhts2*Nbeams2))
			tdip=dip2[Ibm,:]
			tdip=scipy.repeat(tdip[scipy.newaxis,:,:],len(I),axis=0)
			tdip=scipy.reshape(tdip,(len(I)*Nhts2*Nbeams2))
			tk=k2[Ibm,:,:]
			tk=scipy.repeat(tk[scipy.newaxis,:,:,:],len(I),axis=0)
			tk=scipy.reshape(tk,(len(I)*Nhts2*Nbeams2,3))
			tvlos=vlos2[:,Ibm,:]
			tvlos=scipy.reshape(tvlos[I,:,:],(len(I)*Nhts2*Nbeams2))
			tdvlos=dvlos2[:,Ibm,:]
			tdvlos=scipy.reshape(tdvlos[I,:,:],(len(I)*Nhts2*Nbeams2))
			tmob=mob2[:,Ibm,:]
			tmob=scipy.reshape(tmob[I,:,:],(len(I)*Nhts2*Nbeams2))
			tkappa=kappa2[:,Ibm,:]
			tkappa=scipy.reshape(tkappa[I,:,:],(len(I)*Nhts2*Nbeams2))
#			Iht=scipy.where(tht<=tm2[0])[0]
			Iht=scipy.where(tht<=LPminAlt)[0]
			tvlos[Iht]=scipy.nan
			#
#			Iht1=scipy.where(Allht<=tm1[-1]+tm1[1]-tm1[0])[0]
			Iht1=scipy.where((Allht<=htout[-1,-1]) & (Allht>=htout[0,0]))
#			Iht2=scipy.where(tht>=tm1[-1]+tm1[1]-tm1[0])[0]
			Iht2=scipy.where((tht>=LPminAlt) & (tht<=LPmaxAlt))[0]
			# combine
			AllBabs=scipy.concatenate((AllBabs[Iht1],tBabs[Iht2]))
			Alldec=scipy.concatenate((Alldec[Iht1],tdec[Iht2]))
			Alldip=scipy.concatenate((Alldip[Iht1],tdip[Iht2]))		
			Allk=scipy.concatenate((Allk[Iht1],tk[Iht2]))
			Allht=scipy.concatenate((Allht[Iht1],tht[Iht2]))
			AllVlos=scipy.concatenate((AllVlos[Iht1],tvlos[Iht2]))
			AlldVlos=scipy.concatenate((AlldVlos[Iht1],tdvlos[Iht2]))
			Allmob=scipy.concatenate((Allmob[Iht1],tmob[Iht2]))
			Allkappa=scipy.concatenate((Allkappa[Iht1],tkappa[Iht2]))
			# avg dec and dip
			decht=scipy.zeros(htout.shape[0],dtype='Float64')
			dipht=scipy.zeros(htout.shape[0],dtype='Float64')
			Babsht=scipy.zeros(htout.shape[0],dtype='Float64')
			for bb in range(htout.shape[0]):
				I=scipy.where((Allht>=htout[bb,0])&(Allht<=htout[bb,1]))[0]
				decht[bb]=scipy.stats.stats.nanmean(Alldec[I])
				dipht[bb]=scipy.stats.stats.nanmean(Alldip[I])
				Babsht[bb]=scipy.stats.stats.nanmean(AllBabs[I])
			
			# velocity vector
			(alt_out1,tVest,tdVest,tdVestAll,xx)=vvels.compute_velvec2(htout,AllVlos,AlldVlos,Allk,Allht,[],Allht,htmin=0.0,htmax=1000.0*1000,covar=[3000.*3000.,3000.*3000.,30.*30.],femax=0.5)
			
			# vector in geomag coords
			VestGmag[itm,:,:]=tVest # perp east, perp north, par
			errVestGmag[itm,:,:]=tdVest # perp east, perp north, par
			
			# vector in geographic coords
			for bb in range(htout.shape[0]):
				terrgmag=scipy.squeeze(tdVestAll[bb,:,:])
				test = scipy.squeeze(gmag2geo(scipy.matrix(scipy.array([[tVest[bb,0]],[tVest[bb,1]],[tVest[bb,2]]])),decht[bb]*pi/180.0,dipht[bb]*pi/180.0))
				VestGeo[itm,bb,:]=test
				terrgeo = gmag2geo_covar(scipy.matrix(terrgmag),scipy.array(decht[bb]*pi/180.0)[scipy.newaxis],scipy.array(dipht[bb]*pi/180.0)[scipy.newaxis])
				terr=scipy.sqrt(scipy.diag(terrgeo))
				errVestGeo[itm,bb,:]=terr
			
			if do_iterate:
				params0=scipy.array([0.5])
				(x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(fit_fun,params0,(htout,Allht,AllVlos,AlldVlos,Allk,Allmob,Allkappa,decht,dipht),
					full_output=1,epsfcn=1.0e-5,ftol=1.0e-5, xtol=1.0e-5, gtol=0.0, maxfev=20*params0.shape[0],factor=1.0,diag=None)		
				NuinScaler[itm]=x
				errNuinScaler[itm]=scipy.sqrt(cov_x[0])
			else:
				NuinScaler[itm]=nuinScaler
			
			# invert winds
			(htout,test,terr,xx,yy)=invertwinds(htout,Allht,AllVlos,AlldVlos,Allk,Allmob/NuinScaler[itm],Allkappa/NuinScaler[itm],decht,dipht)
			Efield[itm,:]=scipy.transpose(test[0:3])
			errEfield[itm,:]=scipy.sqrt(scipy.diag(terr)[0:3])
			twind=test[3:]
			errWind=scipy.sqrt(scipy.diag(terr)[3:])
			
			# forward model
			for ibm in range(Nbeams0):
				VlosEst1[itm,ibm,:]=scipy.squeeze(forwardmodel(test,htout,ht1[ibm,:]/1000.0,k1[ibm,:,:],mob1[itm,ibm,:]/NuinScaler[itm],kappa1[itm,ibm,:]/NuinScaler[itm]))
			
			# wind in geomag coords
			WindGmag[itm,:,0]=twind[0::3][scipy.newaxis,:,0] # perp east
			WindGmag[itm,:,1]=twind[1::3][scipy.newaxis,:,0] # perp north
			WindGmag[itm,:,2]=twind[2::3][scipy.newaxis,:,0] # par
			errWindGmag[itm,:,0]=errWind[0::3] # perp east
			errWindGmag[itm,:,1]=errWind[1::3] # perp north
			errWindGmag[itm,:,2]=errWind[2::3] # par
			
			# wind in geographic coords
			for bb in range(htout.shape[0]):
				I=scipy.where((Allht>=htout[bb,0])&(Allht<=htout[bb,1]))[0]
				WindGeo[itm,bb,:] = scipy.squeeze(gmag2geo(scipy.array([[WindGmag[itm,bb,0]],[WindGmag[itm,bb,1]],[WindGmag[itm,bb,2]]]),decht[bb]*pi/180.0,dipht[bb]*pi/180.0))
			terrgeo = gmag2geo_covar(terr[3:,3:],decht*pi/180.0,dipht*pi/180.0)
			errWind=scipy.sqrt(scipy.diag(terrgeo))
			errWindGeo[itm,:,0]=errWind[0::3] # east
			errWindGeo[itm,:,1]=errWind[1::3] # north
			errWindGeo[itm,:,2]=errWind[2::3] # up

			# current
			for ibm in range(Nbeams1):
				tvx=scipy.interpolate.interp1d(scipy.mean(htout,axis=1),VestGmag[itm,:,0],bounds_error=0)(ht1[ibm,:]/1000.0)
				tvy=scipy.interpolate.interp1d(scipy.mean(htout,axis=1),VestGmag[itm,:,1],bounds_error=0)(ht1[ibm,:]/1000.0)
				tux=scipy.interpolate.interp1d(scipy.mean(htout,axis=1),WindGmag[itm,:,0],bounds_error=0)(ht1[ibm,:]/1000.0)
				tuy=scipy.interpolate.interp1d(scipy.mean(htout,axis=1),WindGmag[itm,:,1],bounds_error=0)(ht1[ibm,:]/1000.0)
				tB=scipy.interpolate.interp1d(scipy.mean(htout,axis=1),Babsht,bounds_error=0)(ht1[ibm,:]/1000.0)
				# current
				CurrentGmag[itm,ibm,:,0]=v_elemcharge*ne1[itm,ibm,:]*(tvx+Efield[itm,1]/tB)
				CurrentGmag[itm,ibm,:,1]=v_elemcharge*ne1[itm,ibm,:]*(tvy-Efield[itm,0]/tB)
				errCurrentGmag[itm,ibm,:,0]=CurrentGmag[itm,ibm,:,0]*0.0
				errCurrentGmag[itm,ibm,:,1]=CurrentGmag[itm,ibm,:,1]*0.0
				# joule heating E
				JouleHeatingE[itm,ibm,:]=sp1[itm,ibm,:]*scipy.sum(scipy.power(Efield[itm,:],2.0))
				errJouleHeatingE[itm,ibm,:]=JouleHeatingE[itm,ibm,:]*0.0
				# joule heating U
				JouleHeatingTotal[itm,ibm,:]=sp1[itm,ibm,:]*(scipy.power(Efield[itm,0]-tuy*tB,2.0)+scipy.power(Efield[itm,1]+tux*tB,2.0))
				errJouleHeatingTotal[itm,ibm,:]=JouleHeatingTotal[itm,ibm,:]*0.0	
				# Integrated joule heating rates
				I=scipy.where(scipy.isfinite(JouleHeatingE[itm,ibm,:]))[0]
				IntJouleHeatingE[itm,ibm]=scipy.trapz(JouleHeatingE[itm,ibm,I], x=ht1[ibm,I])
				errIntJouleHeatingE[itm,ibm]=IntJouleHeatingE[itm,ibm]*0.0
				I=scipy.where(scipy.isfinite(JouleHeatingTotal[itm,ibm,:]))[0]				
				IntJouleHeatingTotal[itm,ibm]=scipy.trapz(JouleHeatingTotal[itm,ibm,I], x=ht1[ibm,I])
				errIntJouleHeatingTotal[itm,ibm]=IntJouleHeatingTotal[itm,ibm]*0.0
							
		except:
			print 'Failed'
	
	print 'Writing output to' + oname
	outh5file=tables.openFile(oname, mode = "w", title = "Fit File")
	# winds
	write_outputfile(outh5file,NuinScaler,groupname='Winds',name='NuinScaler')
	write_outputfile(outh5file,errNuinScaler,groupname='Winds',name='errNuinScaler')
	write_outputfile(outh5file,WindGmag,groupname='Winds',name='WindGmag')
	write_outputfile(outh5file,WindGeo,groupname='Winds',name='WindGeo')
	write_outputfile(outh5file,errWindGmag,groupname='Winds',name='errWindGmag')
	write_outputfile(outh5file,errWindGeo,groupname='Winds',name='errWindGeo')
	write_outputfile(outh5file,htout,groupname='Winds',name='Altitude')
	# vector vels
	write_outputfile(outh5file,VestGmag,groupname='VectorVels',name='VestGmag')
	write_outputfile(outh5file,VestGeo,groupname='VectorVels',name='VestGeo')
	write_outputfile(outh5file,errVestGmag,groupname='VectorVels',name='errVestGmag')
	write_outputfile(outh5file,errVestGeo,groupname='VectorVels',name='errVestGeo')
	write_outputfile(outh5file,htout,groupname='VectorVels',name='Altitude')
	# LOS vels
	write_outputfile(outh5file,VlosEst1,groupname='LineOfSightVels',name='VlosEst')
	write_outputfile(outh5file,vlos1,groupname='LineOfSightVels',name='VlosAC')	
	write_outputfile(outh5file,dvlos1,groupname='LineOfSightVels',name='dVlosAC')	
	write_outputfile(outh5file,ne1,groupname='LineOfSightVels',name='NeAC')	
	write_outputfile(outh5file,dne1,groupname='LineOfSightVels',name='dNeAC')		
	write_outputfile(outh5file,ht1,groupname='LineOfSightVels',name='Altitude')	
	write_outputfile(outh5file,k1,groupname='LineOfSightVels',name='Kvec')	
	write_outputfile(outh5file,mob1,groupname='LineOfSightVels',name='Mobility')	
	write_outputfile(outh5file,kappa1,groupname='LineOfSightVels',name='Kappa')	
	write_outputfile(outh5file,dec1,groupname='LineOfSightVels',name='Declination')	
	write_outputfile(outh5file,dip1,groupname='LineOfSightVels',name='Dip')	
	write_outputfile(outh5file,BMCODES,groupname='LineOfSightVels',name='BeamCodes')	
	# electric fields
	write_outputfile(outh5file,Efield,groupname='ElectricFields',name='Efield')
	write_outputfile(outh5file,errEfield,groupname='ElectricFields',name='errEfield')
	# products
	write_outputfile(outh5file,CurrentGmag,groupname='JouleHeating',name='CurrentGmag')
	write_outputfile(outh5file,errCurrentGmag,groupname='JouleHeating',name='errCurrentGmag')
	write_outputfile(outh5file,JouleHeatingE,groupname='JouleHeating',name='JouleHeatingE')
	write_outputfile(outh5file,errJouleHeatingE,groupname='JouleHeating',name='errJouleHeatingE')
	write_outputfile(outh5file,IntJouleHeatingE,groupname='JouleHeating',name='IntJouleHeatingE')
	write_outputfile(outh5file,errIntJouleHeatingE,groupname='JouleHeating',name='errIntJouleHeatingE')
	write_outputfile(outh5file,JouleHeatingTotal,groupname='JouleHeating',name='JouleHeatingTotal')
	write_outputfile(outh5file,errJouleHeatingTotal,groupname='JouleHeating',name='errJouleHeatingTotal')
	write_outputfile(outh5file,IntJouleHeatingTotal,groupname='JouleHeating',name='IntJouleHeatingTotal')
	write_outputfile(outh5file,errIntJouleHeatingTotal,groupname='JouleHeating',name='errIntJouleHeatingTotal')
	write_outputfile(outh5file,sp1,groupname='JouleHeating',name='PedersenConductance')	
	write_outputfile(outh5file,ht1,groupname='JouleHeating',name='Altitude')	
	# time
	write_outputfile(outh5file,time1,groupname='Time',name='UnixTime')
	write_outputfile(outh5file,dtime1,groupname='Time',name='dTime')
	outh5file.close()
	
	figg=plot_winds(Efield,errEfield,WindGmag,errWindGmag,htout,time1,clim=[-500.0,500.0])
	figg.savefig(oname[:-3]+'-WindGmag.png')	
	figg=plot_winds(Efield,errEfield,WindGeo,errWindGeo,htout,time1,geo=1,clim=[-500.0,500.0])
	figg.savefig(oname[:-3]+'-WindGeo.png')	
	
	# plot drift
	clim=[-500.0,500.0]; xlim=[dat1['/Time']['UnixTime'][0,0],dat1['/Time']['UnixTime'][-1,1]]; ylim=[90.0,150.0]
	title= "%d-%d-%d %.3f UT - %d-%d-%d %.3f UT" % (dat1['/Time']['Month'][0,0],dat1['/Time']['Day'][0,0],dat1['/Time']['Year'][0,0],dat1['/Time']['dtime'][0,0],dat1['/Time']['Month'][-1,1],dat1['/Time']['Day'][-1,1],dat1['/Time']['Year'][-1,1],dat1['/Time']['dtime'][-1,1])
	txt=r'$\rm{Vlos} \ (\rm{m/s})$'
	dat=VlosEst1
	time,dat=plot_utils.timegaps(dat1['/Time']['UnixTime'],dat)
	dat=scipy.ma.masked_where(scipy.isnan(dat),dat)     
	(figg,ax)=plot_utils.pcolor_plot(time,dat1['/FittedParams']['Altitude']/1000,dat,clim,xlim,ylim,'Time (UT)','Altitude (km)',title,txt,dat1['/']['BeamCodes'])
	figg.savefig(oname[:-3]+'-VlosForwardModel.png')
	
	# plot drift
	clim=[-500.0,500.0]; xlim=[dat1['/Time']['UnixTime'][0,0],dat1['/Time']['UnixTime'][-1,1]]; ylim=[90.0,150.0]
	title= "%d-%d-%d %.3f UT - %d-%d-%d %.3f UT" % (dat1['/Time']['Month'][0,0],dat1['/Time']['Day'][0,0],dat1['/Time']['Year'][0,0],dat1['/Time']['dtime'][0,0],dat1['/Time']['Month'][-1,1],dat1['/Time']['Day'][-1,1],dat1['/Time']['Year'][-1,1],dat1['/Time']['dtime'][-1,1])
	txt=r'$\rm{Vlos} \ (\rm{m/s})$'
	dat=vlos1
	time,dat=plot_utils.timegaps(dat1['/Time']['UnixTime'],dat)
	dat=scipy.ma.masked_where(scipy.isnan(dat),dat)     
	(figg,ax)=plot_utils.pcolor_plot(time,dat1['/FittedParams']['Altitude']/1000,dat,clim,xlim,ylim,'Time (UT)','Altitude (km)',title,txt,dat1['/']['BeamCodes'])
	figg.savefig(oname[:-3]+'-Vlos.png')		

	return Efield,errEfield,WindGmag,errWindGmag,WindGeo,errWindGeo,htout,time1
	
def plot_winds(Efield,errEfield,Wind,errWind,htout,time1,geo=0,clim=[-200,200]):

	Wpe=Wind[:,:,0].copy()
	eWpe=errWind[:,:,0].copy()
	Wpn=Wind[:,:,1].copy()
	eWpn=errWind[:,:,1].copy()	
	Wpar=Wind[:,:,2].copy()
	eWpar=errWind[:,:,2].copy()

	ht=htout[:,0]
	ht=scipy.hstack((ht,htout[-1,1]))

	te1=50.0
	te2=0.5
	
	I=scipy.where((scipy.absolute(eWpn)/(scipy.absolute(Wpn)+te1)>te2))
	Wpn[I]=scipy.nan
	I=scipy.where(scipy.absolute(eWpe)/(scipy.absolute(Wpe)+te1)>te2)
	Wpe[I]=scipy.nan
	I=scipy.where(scipy.absolute(eWpar)/(scipy.absolute(Wpar)+te1)>te2)
	Wpar[I]=scipy.nan
		
	time,Wpn=plot_utils.timegaps(time1,Wpn)
	time,Wpe=plot_utils.timegaps(time1,Wpe)
	time,Wpar=plot_utils.timegaps(time1,Wpar)
	time,eWpn=plot_utils.timegaps(time1,eWpn)
	time,eWpe=plot_utils.timegaps(time1,eWpe)
	time,eWpar=plot_utils.timegaps(time1,eWpar)
	timex=matplotlib.dates.epoch2num(time)

	xlim=[timex[0],timex[-1]]
	ylim=[htout[0,0],htout[-1,1]]
	ylim=[htout[0,0],150]

	Wpn=scipy.ma.masked_where(scipy.isnan(Wpn),Wpn)
	Wpe=scipy.ma.masked_where(scipy.isnan(Wpe),Wpe)
	Wpar=scipy.ma.masked_where(scipy.isnan(Wpar),Wpar)
	eWpn=scipy.ma.masked_where(scipy.isnan(eWpn),eWpn)
	eWpe=scipy.ma.masked_where(scipy.isnan(eWpe),eWpe)
	eWpar=scipy.ma.masked_where(scipy.isnan(eWpar),eWpar)

	figg,ax=plot_utils.multi_axes(3,2)
	
	labsize=12
	textsize=10

	ii=0
	
	pylab.axes(ax[ii])
	pc=ax[ii].pcolor(timex,ht,scipy.transpose(Wpn),shading='flat',vmin=clim[0],vmax=clim[1])
	ax[ii].set_xlim(xlim)
	ax[ii].set_ylim(ylim)	
	ax[ii].set_ylabel('Altitude (km)', fontsize=labsize)
	labels = pylab.getp(ax[ii], 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	labels = pylab.getp(ax[ii], 'xticklabels')
	pylab.setp(labels, fontsize=textsize)
	a=pylab.colorbar(pc)
	labels = pylab.getp(a.ax, 'yticklabels')
	pylab.setp(labels, fontsize=textsize)	
	if geo:
		ax[ii].set_title('U north (m/s)', fontsize=labsize, horizontalalignment='center')
	else:
		ax[ii].set_title('U perp north (m/s)', fontsize=labsize, horizontalalignment='center')
	
	ii=ii+1
	
	pylab.axes(ax[ii])
	pc=ax[ii].pcolor(timex,ht,scipy.transpose(eWpn),shading='flat',vmin=0,vmax=clim[1]/2)
	ax[ii].set_xlim(xlim)
	ax[ii].set_ylim(ylim)	
	labels = pylab.getp(ax[ii], 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	labels = pylab.getp(ax[ii], 'xticklabels')
	pylab.setp(labels, fontsize=textsize)
	a=pylab.colorbar(pc)
	labels = pylab.getp(a.ax, 'yticklabels')
	pylab.setp(labels, fontsize=textsize)	
	if geo:
		ax[ii].set_title('err U north (m/s)', fontsize=labsize, horizontalalignment='center')
	else:
		ax[ii].set_title('err U perp north (m/s)', fontsize=labsize, horizontalalignment='center')	

	ii=ii+1
	
	pylab.axes(ax[ii])
	pc=ax[ii].pcolor(timex,ht,scipy.transpose(Wpe),shading='flat',vmin=clim[0],vmax=clim[1])
	ax[ii].set_xlim(xlim)
	ax[ii].set_ylim(ylim)	
	ax[ii].set_ylabel('Altitude (km)', fontsize=labsize)
	labels = pylab.getp(ax[ii], 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	labels = pylab.getp(ax[ii], 'xticklabels')
	pylab.setp(labels, fontsize=textsize)
	a=pylab.colorbar(pc)
	labels = pylab.getp(a.ax, 'yticklabels')
	pylab.setp(labels, fontsize=textsize)	
	if geo:
		ax[ii].set_title('U east (m/s)', fontsize=labsize, horizontalalignment='center')
	else:
		ax[ii].set_title('U perp east (m/s)', fontsize=labsize, horizontalalignment='center')
	
	ii=ii+1
	
	pylab.axes(ax[ii])
	pc=ax[ii].pcolor(timex,ht,scipy.transpose(eWpe),shading='flat',vmin=0,vmax=clim[1]/2)
	ax[ii].set_xlim(xlim)
	ax[ii].set_ylim(ylim)	
	labels = pylab.getp(ax[ii], 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	labels = pylab.getp(ax[ii], 'xticklabels')
	pylab.setp(labels, fontsize=textsize)
	a=pylab.colorbar(pc)
	labels = pylab.getp(a.ax, 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	if geo:
		ax[ii].set_title('err U east (m/s)', fontsize=labsize, horizontalalignment='center')
	else:
		ax[ii].set_title('err U perp east (m/s)', fontsize=labsize, horizontalalignment='center')

	ii=ii+1
	
	pylab.axes(ax[ii])
	pc=ax[ii].pcolor(timex,ht,scipy.transpose(Wpar),shading='flat',vmin=clim[0]/10.0,vmax=clim[1]/10.0)
	ax[ii].set_xlim(xlim)
	ax[ii].set_ylim(ylim)	
	ax[ii].set_ylabel('Altitude (km)', fontsize=labsize)
	labels = pylab.getp(ax[ii], 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	labels = pylab.getp(ax[ii], 'xticklabels')
	pylab.setp(labels, fontsize=textsize)
	a=pylab.colorbar(pc)
	labels = pylab.getp(a.ax, 'yticklabels')
	pylab.setp(labels, fontsize=textsize)	
	ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
	if geo:
		ax[ii].set_title('U up (m/s)', fontsize=labsize, horizontalalignment='center')
	else:
		ax[ii].set_title('U par (m/s)', fontsize=labsize, horizontalalignment='center')
	
	
	ii=ii+1
	
	pylab.axes(ax[ii])
	pc=ax[ii].pcolor(timex,ht,scipy.transpose(eWpar),shading='flat',vmin=0,vmax=clim[1]/2.0/10.0)
	ax[ii].set_xlim(xlim)
	ax[ii].set_ylim(ylim)	
	labels = pylab.getp(ax[ii], 'yticklabels')
	pylab.setp(labels, fontsize=textsize)
	labels = pylab.getp(ax[ii], 'xticklabels')
	pylab.setp(labels, fontsize=textsize)
	a=pylab.colorbar(pc)
	labels = pylab.getp(a.ax, 'yticklabels')
	pylab.setp(labels, fontsize=textsize)	
	ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
	if geo:
		ax[ii].set_title('err U up (m/s)', fontsize=labsize, horizontalalignment='center')
	else:
		ax[ii].set_title('err U par (m/s)', fontsize=labsize, horizontalalignment='center')
	
	
	
	locator = matplotlib.dates.HourLocator(interval=1)
	formatter = matplotlib.dates.DateFormatter("%H:%M")
	ax[0].xaxis.set_major_locator(locator)
	ax[0].xaxis.set_major_formatter(formatter)
	ax[1].xaxis.set_major_locator(locator)
	ax[1].xaxis.set_major_formatter(formatter)
	ax[2].xaxis.set_major_locator(locator)
	ax[2].xaxis.set_major_formatter(formatter)
	ax[3].xaxis.set_major_locator(locator)
	ax[3].xaxis.set_major_formatter(formatter)
	ax[4].xaxis.set_major_locator(locator)
	ax[4].xaxis.set_major_formatter(formatter)
	ax[5].xaxis.set_major_locator(locator)
	ax[5].xaxis.set_major_formatter(formatter)
	
	pylab.show()
	
	return figg

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
			fhandle.createArray(group, key, dict2do[key], "Dataset")