#!/usr/bin/env python

"""

"""

import scipy
				
# compute_velvec2
def compute_velvec2(PLAT,AllVlos,AlldVlos,Allk,AllPlat,AllPlong,Allht,htmin=150.0*1000,htmax=400.0*1000,\
    covar=[1000.*1000.,1000.*1000.,5.*5.],p=[200.0,0.5,2000.0,100.0]):
	
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
		
	fracerrs=scipy.absolute(AlldVlos)/(scipy.absolute(AllVlos)+p[0])
	abserrs=scipy.absolute(AlldVlos)
	
	# loop over output latitudes
	Nmeas=scipy.zeros((Nplout))
	Vest=scipy.zeros((Nplout,Nparms),dtype=AllVlos.dtype)
	dVest=scipy.zeros((Nplout,Nparms),dtype=AllVlos.dtype)
	dVestAll=scipy.zeros((Nplout,Nparms,Nparms),dtype=AllVlos.dtype)
	for i in range(Nplout):
		plat_out[i]=(plat_in[i,0]+plat_in[i,1])/2.0
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
		
	return plat_out, Vest, dVest, dVestAll, Nmeas