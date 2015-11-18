#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys
import getopt
import optparse
import os.path
import ConfigParser
import math
import datetime
import tables
import scipy
import scipy.stats
import ctypes
import scipy.signal
import scipy.interpolate
import scipy.integrate
import scipy.fftpack
import matplotlib
#matplotlib.use('Agg')
#matplotlib.use('GTKAgg'); matplotlib.interactive(True)
import pylab
import glob

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import proc_utils


# some natural constants
v_lightspeed=299792458
v_Boltzmann=1.380658e-23
v_electronmass=9.1093897e-31
v_amu=1.6605402e-27
v_electronradius=2.81794092e-15
v_epsilon0=8.854188E-12
v_elemcharge=1.602177E-19

##############################

class iplot:

	def __init__(self):
		self.pl_x=[]
		self.pl_y=[]
		return
		
	def __call__(self,event):
		clickx=event.xdata
		clicky=event.ydata
		tb = pylab.get_current_fig_manager().toolbar
		if event.button==1 and event.inaxes and tb.mode == '':
			self.pl_x.append(clickx)
			self.pl_y.append(clicky)
			pylab.ioff()
			v = pylab.axis()
			pylab.plot([clickx,clickx],[clicky,clicky],'.w')
			pylab.axis(v)
			pylab.show()
			
			
def freq2ne(freq,Te=1000.0,f0=450.0e6,B=50000e-9,alph=0.0):
	k=4*math.pi/v_lightspeed*f0
	fc=v_elemcharge*B/v_electronmass/2.0/math.pi
	
	ne=4.0*math.pi**2*v_electronmass*v_epsilon0/v_elemcharge**2*(scipy.power(freq,2.0)-3*k**2/4.0/math.pi**2*v_Boltzmann*Te/v_electronmass-fc**2*scipy.sin(alph)**2)

	return ne
	
def chapman_func(z,Hch,nmax,zmax):

	ne=nmax*scipy.exp(0.5*(1.0-(z-zmax)/Hch-scipy.exp((zmax-z)/Hch)))
	
	return ne

def chap_fitfun(parameter,altitude,data,scaler,PW,mode=0):

	parameter=parameter*scaler

	tnmax=parameter[0]
	tzmax=parameter[1]
	thch=parameter[2]
	
	althr=scipy.arange(altitude[0]-PW,altitude[-1]+PW,100)
	
	# model
	m=chapman_func(althr,thch,tnmax,tzmax)
	
	# smear model
	m2=scipy.zeros(data.shape,dtype=data.dtype)
	for rr in range(altitude.shape[0]):
		I=scipy.where((althr>=(altitude[rr]-PW/2.0)) & (althr<=(altitude[rr]+PW/2.0)))[0]
		m2[rr]=scipy.mean(m[I]*(altitude[rr]**2.0)/scipy.power(althr[I],2.0))
	
	if mode==0:
		y=m2-data
	else:
		y=m2
		
	return y


def chap_fitfun2(parameter,altitude,data,scaler,mode=0):

	parameter=parameter*scaler

	tnmax=parameter[0]
	tzmax=parameter[1]
	thch=parameter[2]
		
	# model
	m=chapman_func(altitude,thch,tnmax,tzmax)
	
	if mode==0:
		y=m-data
	else:
		y=m
		
	return y
	
def read_a_datafile(fname):
	
	print 'Reading file ' + fname
	
	# make sure the file exists
	if os.path.exists(fname)==False:
		print 'The input file does not exist.'
		sys.exit(1)
	
	# read the entire file
	h5file=tables.openFile(fname)
	output={}
	for group in h5file.walkGroups("/"):
		output[group._v_pathname]={}
		for array in h5file.listNodes(group, classname = 'Array'):						
			output[group._v_pathname][array.name]=array.read()		
	h5file.close()

	return output

def read_pl_file(fname):

	fid=open(fname,'r')
	
	pl=[]
	time=[]
	done=0
	while (not done):
		try:
			line=fid.next()
			line=line.split(' ')
			if len(line)==4:
				bmcode=scipy.array(line).astype('float64')
			elif len(line)==2:
				pl.append(float(line[1]))
				time.append(float(line[0]))
		except:
			done=1
	
	fid.close()
	
	return bmcode,time,pl

def cal_pl_all():

	ST0=''
	EXP='20090315.003'
	INT='_lp_2min'; 	LONGPULSE=1; doChap=0; usePower=0; Ilen=1
#	INT='_ac_5min'; 	LONGPULSE=0; doChap=0; usePower=0; Ilen=7
#	INT='_lp_15min-64157'; 	LONGPULSE=1; doChap=0; usePower=0; Ilen=1
#	INT='_ac_15min-64157'; 	LONGPULSE=0; doChap=0; usePower=0; Ilen=7
	
#	ST=''; ILdir='/Volumes/AMISR_004/processed_data/2008/PlCal03'
	ST=''; ILdir='/Volumes/AMISR_004/processed_data/2009/Erickson12/';
#	ST='IPY15'+ST0 + '-'; ILdir='/Volumes/AMISR_004/processed_data/2008/IPY15/';
	ILdir=os.path.join(ILdir,EXP)
	caldir='cal-0/'
	
	allfiles=glob.glob(os.path.join(ILdir,caldir,'*-plline-[0-9][0-9][0-9].txt')) 
	ILfname=ST+EXP+INT+'.h5'
	
	for aa in range(len(allfiles)):
		try:
#		if 1==1:
			cal_pl(EXP,INT,LONGPULSE,allfiles[aa],ILfname,ILdir,doChap=doChap,usePower=usePower,ST='_'+ST0,Ilen=Ilen)
			pylab.close('all')
		except:
			print 'failed'
			
def cal_pl(EXP,INT,LONGPULSE,PLfile,ILfname,ILdir,doChap=-1,usePower=-1,ST='',Ilen=10):

#	EXP='20071011.011'
#	INT='_ac_10min'
#	LONGPULSE=0
#	ILfname=EXP+INT+'.h5'
#	PLfile='cal-0/20071011.011-65162-plline-000.txt'
	oname=PLfile[:-4]+INT+ST

	thch=40.0e3
	if doChap==-1:
		doChap=0
	if usePower==-1:
		usePower=0
	PW=30.0e-6*v_lightspeed/2	
	ALTMIN=150.0e3
	ALTMAX=400.0e3

	if LONGPULSE:
		if doChap==-1:
			doChap=1
		if usePower==-1:
			usePower=1
		if usePower==0:
			Ilen=1
			ALTMIN=100.0e3
		PW=480.0e-6*v_lightspeed/2.0

	# read ion line data
	ILdata=read_a_datafile(os.path.join(ILdir,ILfname))
	TxPower=ILdata['/ProcessingParams']['TxPower']
	BMCODES=ILdata['/']['BeamCodes']
	Nbeams=BMCODES.shape[0]
	IlTime=scipy.mean(ILdata['/Time']['UnixTime'],axis=1)
	NePower=ILdata['/NeFromPower']['Ne_NoTr']
	AltPower=ILdata['/NeFromPower']['Altitude']
	Ne=ILdata['/FittedParams']['Ne']
	Alt=ILdata['/FittedParams']['Altitude']
	Te=ILdata['/FittedParams']['Fits'][:,:,:,2,1]
	Ti=ILdata['/FittedParams']['Fits'][:,:,:,0,1]
	Tr=Te/Ti
	Babs=ILdata['/Geomag']['Babs']
	kpar=ILdata['/Geomag']['kpar']
	kperp=scipy.sqrt(scipy.power(ILdata['/Geomag']['kpe'],2.0)+scipy.power(ILdata['/Geomag']['kpn'],2.0))
	alph=math.pi/2-scipy.arcsin(kpar/scipy.sqrt(scipy.power(kpar,2.0)+scipy.power(kperp,2.0))).real
	YR=ILdata['/Time']['Year'][0,0]
	DAY=ILdata['/Time']['Day'][0,0]
	MON=ILdata['/Time']['Month'][0,0]

	# read PL data
	print 'Reading file '+ PLfile
	bmcode,timepl,pl=read_pl_file(PLfile)	
	timepl=scipy.array(timepl)
	pl=scipy.array(pl)
	PW=PW*scipy.sin(bmcode[2]*scipy.pi/180.0)

	Ibeam=scipy.where(BMCODES[:,0]==bmcode[0])[0]
	if len(Ibeam)==0:
		print 'cant find beam!'
		sys.exit(-1)

	# get nemax from IL
	Itime=scipy.where((IlTime>=(timepl.min()-1000)) & (IlTime<=(timepl.max()+1000)))[0]
	timeil=IlTime[Itime]
	NeMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	dNeMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	htMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	TeMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	TiMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	TrMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	alphMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	BabsMax=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	TxPow=scipy.zeros(timeil.shape,dtype='float64')*scipy.nan
	for aa in range(len(Itime)):
		ra=Itime[aa]
		
		TxPow[aa]=TxPower[ra]
	
		taltpower=scipy.squeeze(AltPower[Ibeam,:])
		Ialtpower=scipy.where((taltpower>=ALTMIN) & (taltpower<=ALTMAX))[0]
		taltpower=taltpower[Ialtpower]
		tprofpower=scipy.squeeze(NePower[ra,Ibeam,:])[Ialtpower]			
		
		talt=scipy.squeeze(Alt[Ibeam,:])
		Ialt=scipy.where((talt>=ALTMIN) & (talt<=ALTMAX))[0]
		talt=talt[Ialt]

		tprof=scipy.squeeze(Ne[ra,Ibeam,:])[Ialt]
					
		tte=scipy.squeeze(Te[ra,Ibeam,:])[Ialt]
		tti=scipy.squeeze(Ti[ra,Ibeam,:])[Ialt]
		talph=scipy.squeeze(alph[Ibeam,:])[Ialt]
		tBabs=scipy.squeeze(Babs[Ibeam,:])[Ialt]
		ttr=tte/tti
			
		if usePower:
			ttprof=tprofpower
			ttalt=taltpower
		else:
			ttprof=tprof
			ttalt=talt			

		try:
#		if 1==1:
			
			# get maximum Ne and altitude of peak
			I=scipy.where(ttprof==ttprof.max())[0]
#			pylab.plot(ttprof,ttalt)

			p=scipy.polyfit(ttalt[I-Ilen:I+Ilen+1], ttprof[I-Ilen:I+Ilen+1], 2, rcond=None, full=False)
			htMax[aa]=-p[1]/2.0/p[0]
			NeMax[aa]=p[0]*htMax[aa]**2+p[1]*htMax[aa]+p[2]
			if NeMax[aa]>1.0e13:
				NeMax[aa]=1.0e11
			dNeMax[aa]=scipy.std(ttprof[I-Ilen:I+Ilen+1]-scipy.polyval(p,ttalt[I-Ilen:I+Ilen+1]))
#			pylab.plot(scipy.polyval(p,ttalt[I-Ilen:I+Ilen+1]),ttalt[I-Ilen:I+Ilen+1],'.k-')
#			pylab.plot(ttprof[I-Ilen:I+Ilen+1]-scipy.polyval(p,ttalt[I-Ilen:I+Ilen+1]),ttalt[I-Ilen:I+Ilen+1],'.b-')
			
			if doChap:
				scaler=scipy.array([1.0e11,1.0e5,1.0e4])
				params0=scipy.array([NeMax[aa],htMax[aa],thch])
				(x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(chap_fitfun,params0/scaler,(ttalt[I-Ilen:I+Ilen+1],ttprof[I-Ilen:I+Ilen+1],scaler,PW),
					full_output=1,epsfcn=1.0e-5,ftol=1.0e-5, xtol=1.0e-5, gtol=0.0, maxfev=15*params0.shape[0],factor=0.5,diag=None)
				tz=scipy.arange(ttalt[I-Ilen],ttalt[I+Ilen+1],100)
				nemod=chap_fitfun(x,tz,tz,scaler,PW,mode=1)
				x=x*scaler
				
			#	print x
				tne=chapman_func(tz,x[2],x[0],x[1])
				tne0=chapman_func(tz,params0[2],params0[0],params0[1]) # initial guess
				pylab.plot(ttprof,ttalt,'.b-')
				pylab.plot(tne,tz,'k')
				pylab.plot(tne0,tz,'r')
				pylab.plot(nemod,tz,'k--')
			
				htMax[aa]=x[1]
				NeMax[aa]=x[0]
							
			TeMax[aa]=scipy.interpolate.interp1d(talt,tte,bounds_error=0)(htMax[aa])
			TrMax[aa]=scipy.interpolate.interp1d(talt,ttr,bounds_error=0)(htMax[aa])

#			print talt
#			print tte
#			print htMax[aa]
#			print TeMax[aa]			
#			xxx
			
			if usePower:
				NeMax[aa]=NeMax[aa]/2.0*(1.0+TrMax[aa])	
					
			alphMax[aa]=scipy.interpolate.interp1d(talt,talph,bounds_error=0)(htMax[aa])
			BabsMax[aa]=scipy.interpolate.interp1d(talt,tBabs,bounds_error=0)(htMax[aa])		
		
		except:
			print 'Failed'
				
	# get sys constant
	pl_ne=scipy.zeros(timepl.shape,dtype='float64')
	il_ne=scipy.zeros(timepl.shape,dtype='float64')
	il_dne=scipy.zeros(timepl.shape,dtype='float64')
	il_tr=scipy.zeros(timepl.shape,dtype='float64')
	for isamp in range(timepl.shape[0]):
	
		# find the closest time
		tmp=scipy.absolute(timeil-timepl[isamp])				
		I=scipy.where(tmp==tmp.min())[0]
		I=I[0]	
			
		pl_ne[isamp]=float(freq2ne(pl[isamp],Te=TeMax[I],B=BabsMax[I],alph=alphMax[I]))
		il_ne[isamp]=NeMax[I]
		il_tr[isamp]=TrMax[I]
		il_dne[isamp]=dNeMax[I]


	KsysCor=il_ne/pl_ne
	KsysErr=il_dne/pl_ne
	Ksys_med=scipy.stats.stats.nanmedian(KsysCor)
	Ksys_std=scipy.stats.stats.nanmedian(KsysErr)
#	Ksys_std=scipy.stats.stats.nanstd(KsysCor)

	pylab.figure()
	pylab.subplot(211)
	pylab.scatter(timepl,pl_ne,c='r')
	pylab.errorbar(timeil,NeMax,yerr=dNeMax)
	pylab.gca().set_ylabel('Ne (m-3)')
	pylab.title('%d (%2.2f,%2.2f)' % (bmcode[0],bmcode[1],bmcode[2]))
	pylab.xlim([(timepl.min()-1000),(timepl.max()+1000)])
	
	pylab.subplot(212)
	pylab.errorbar(timepl,KsysCor,yerr=KsysErr, marker='s', mfc='red', mec='green', ms=4, mew=1,fmt='k.')
	v=pylab.axis()
	pylab.plot(v[0:2],[Ksys_med,Ksys_med],'k')
	pylab.plot(v[0:2],[Ksys_med+Ksys_std,Ksys_med+Ksys_std],'k--')
	pylab.plot(v[0:2],[Ksys_med-Ksys_std,Ksys_med-Ksys_std],'k--')
	pylab.gca().set_ylabel('Ksys-cor')
	pylab.gca().set_xlabel('Time (secs)')
	pylab.xlim([(timepl.min()-1000),(timepl.max()+1000)])

	pylab.show()
	
	pylab.gcf().savefig(os.path.join(ILdir,oname+'.png'))

#	pylab.figure()
#	pylab.plot(timeil,TrMax)

	fH=open(oname+'.txt','w') 
	fH.write('%f %f %f %f\n' % (bmcode[0],bmcode[1],bmcode[2],bmcode[3]))
	fH.write('%f %f\n' % (Ksys_med,Ksys_std)) 
	fH.write('%d %d\n' % (timepl.shape[0],timeil.shape[0]))
	fH.write('%f %f\n' % (timepl[0],timepl[-1]))
	fH.write('%f %f\n' % (timeil[0],timeil[-1]))
	fH.write('%f %f\n' % (scipy.stats.stats.nanmedian(il_ne),scipy.stats.stats.nanmedian(il_tr)))
	fH.write('%f\n' % (scipy.stats.stats.nanmean(TxPow)))
	fH.close()


def plot_pl():

	FILE_PATH='/Volumes/ISR_Data-1/Data AMISR Poker/20070829.001'
	filepath_up='*.Dt2.h5'
	filepath_dn='*.Dt1.h5'
	Ifiles=range(0,1)

	files_up=glob.glob(os.path.join(FILE_PATH,filepath_up)) 
	if len(Ifiles)>0:
		files_up=files_up[Ifiles[0]:(Ifiles[-1]+1)]
	if dualpl:
		files_dn=glob.glob(os.path.join(FILE_PATH,filepath_dn)) 
		if len(Ifiles)>0:
			files_dn=files_dn[Ifiles[0]:(Ifiles[-1]+1)]
	else:
		files_dn=files_up
		
	files_dn.sort()
	files_up.sort()

	# loop over plasma line files
	for Ifile in range(NFILES):
		output1=read_a_datafile(os.path.join(FILE_PATH,files_up[Ifile]))
		if dualpl:
			output2=read_a_datafile(os.path.join(FILE_PATH,files_dn[Ifile]))

		if Ifile==0:
			rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
			fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
			if fr_off==output1['/Tx']['Frequency'][0,0]:
				fr_off=5.0e6			
			freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
			
			pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
			bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
			time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
			
			if dualpl:
				freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
				pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
				bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
				time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
		else:
			pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
			bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
			time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
			if dualpl:
				pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
				bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
				time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)
	# deal the data
	pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
	if dualpl:
		pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

	Ntimes=time_up.size
	Nfreqs=freq_up.size

	if len(beams2do)==0:
		beams2do=BMCODES[:,0]

	# loop over beams
	for jjj in range(Nbeams):

		# get beamcode
		bmcode=BMCODES[jjj,:]
		
		# subtract highest altitude	
		tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
		tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-1][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2) 
		tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
		if dualpl:
			tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
			tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-1][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
			tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
		#
		alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)

def proc_pl2_IPY():

	# IPY04 ***
	FILELIST_UP='/Volumes/AMISR_004/processed_data/2008/IPY15/20080912.001/IPY15-filelist_PL2-64157.txt'
	FILELIST_DN='/Volumes/AMISR_004/processed_data/2008/IPY15/20080912.001/IPY15-filelist_PL1-64157.txt'
	EXP='IPY05'
	ODIR='/Volumes/AMISR_004/processed_data/2008/IPY15/20080912.001' + '/cal-0'
	DAT_PATH = ''
	DIR_SEARCH = 1
	Is=1392		
	beams2do=[64157]
	MAXFILES=5
	flip=1

	# LTCS 01/17 - stopped at 157
	"""
	FILELIST_UP='/Volumes/AMISR_004/processed_data/LTCS01/20080117.016/filelist_PL2.txt'
	FILELIST_DN='/Volumes/AMISR_004/processed_data/LTCS01/20080117.016/filelist_PL1.txt'
	EXP='LTCS01'
	ODIR='/Volumes/AMISR_004/processed_data/LTCS01/20080117.016' + '/cal-0'
	DAT_PATH = '/Volumes/AMISR_006/Data AMISR Poker'
	DIR_SEARCH = 1
	Is=157
	beams2do=[]
	MAXFILES=10
	"""
	
	"""
	# Erickson
	ddir='20090315.003'
	FILELIST_UP='/Volumes/AMISR_004/processed_data/2009/Erickson12/' + ddir + '/filelist_PL2.txt'
	FILELIST_DN='/Volumes/AMISR_004/processed_data/2009/Erickson12/' + ddir + '/filelist_PL1.txt'	
	EXP='Erickson12'
	ODIR='/Volumes/AMISR_004/processed_data/2009/Erickson12/' + ddir + '/cal-0'
	DAT_PATH = '/Volumes/AMISR_008/Data AMISR Poker/' + ddir
	DIR_SEARCH = 1
	Is=0
	beams2do=[64157]	
	MAXFILES=10
	"""

	"""
	# SGrid 01
	ddir='20061213.003'
	FILELIST_UP='/Volumes/AMISR_004/processed_data/SGrid01/' + ddir + '/filelist_PL1.txt'
	FILELIST_DN='/Volumes/AMISR_004/processed_data/SGrid01/' + ddir + '/filelist_PL1.txt'	
	EXP='SGrid01'
	ODIR='/Volumes/AMISR_004/processed_data/SGrid01/' + ddir + '/cal-0'
	DAT_PATH = '/Volumes/AMISR_005/Data AMISR Poker/' + ddir
	DIR_SEARCH = 1
	Is=0
	beams2do=[]	
	MAXFILES=10	
	"""
	
	# PlCal
	"""

	ddir='20090127.011'
#	FILELIST_UP='/Volumes/AMISR_004/processed_data/2008/PlCal03/' + ddir + '/filelist_PL2.txt'
#	FILELIST_DN='/Volumes/AMISR_004/processed_data/2008/PlCal03/' + ddir + '/filelist_PL1.txt'	
	FILELIST_UP='/Users/mnicolls/Documents/Work/AmisrProc/A16PlTest10/'+ddir+'/filelist_PL2.txt'
	FILELIST_DN='/Users/mnicolls/Documents/Work/AmisrProc/A16PlTest10/'+ddir+'/filelist_PL1.txt'
	ODIR='/Users/mnicolls/Documents/Work/AmisrProc/MSWinds23/'+ddir+'/cal-0'
	DAT_PATH = '/Volumes/IOMEGA HDD/Data/' + ddir
	DIR_SEARCH = 1
	Is=0
	beams2do=[]	
	MAXFILES=5
	"""

	"""	
	ddir='20090425.002'
#	FILELIST_UP='/Volumes/AMISR_004/processed_data/2008/PlCal03/' + ddir + '/filelist_PL2.txt'
#	FILELIST_DN='/Volumes/AMISR_004/processed_data/2008/PlCal03/' + ddir + '/filelist_PL1.txt'	
	FILELIST_UP='/Volumes/AMISR_004/RbISR/2009/WorldDay51m/'+ddir+'/filelist_PL2.txt'
	FILELIST_DN='/Volumes/AMISR_004/RbISR/2009/WorldDay51m/'+ddir+'/filelist_PL1.txt'
#	ODIR='/Volumes/AMISR_004/RbISR/2009/PLCal13m/'+ddir+'/cal-0'
	ODIR='/Volumes/AMISR_004/RbISR/2009/WorldDay51m/'+ddir+'/cal-0'
	DAT_PATH = '/Volumes/AMISR_010/AMISR Data Resolute N/' + ddir
	DIR_SEARCH = 1
	Is=0
	beams2do=[]	
	MAXFILES=10
	flip=0 # down is really up.
	"""

	Ifiles=[] #range(0,10)
	dualpl=1
	htstart=250.0e3
	FR_OFF=[5.0e6,5.0e6]
	KS=[5,3]

	# get file list
	f=open(FILELIST_UP); files_up=f.readlines(); f.close()
	f=open(FILELIST_DN); files_dn=f.readlines(); f.close()
	if len(files_up) != len(files_dn):
		print 'different number of up and down files!'
		sys.exit(-1)
	if DIR_SEARCH:
		files_up2=[]; files_dn2=[]
		for ifile in range(len(files_up)):
			files_up2.extend(glob.glob(os.path.join(DAT_PATH,files_up[ifile].rstrip('\n'))))
			files_dn2.extend(glob.glob(os.path.join(DAT_PATH,files_dn[ifile].rstrip('\n'))))
		files_up=files_up2
		files_dn=files_dn2
		if len(files_up) != len(files_dn):
			print 'different number of up and down files!'
			sys.exit(-1)
		files_up.sort()
		files_dn.sort()
	files_up=files_up[Is:]
	files_dn=files_dn[Is:]

	NFILES=len(files_up)
	for ir in range(NFILES):
		files_up[ir]=files_up[ir].rstrip('\n')
		files_dn[ir]=files_dn[ir].rstrip('\n')

	pylab.figure()
	pylab.show()
	pylab.close('all')
	figg1=pylab.figure();

	done=0; IIfile=0
	while not done:
		tfiles_up=[]
		tfiles_dn=[]
		
		print "Spot: %d" % (IIfile+Is)
		tfiles_up.append(files_up[IIfile])
		tfiles_dn.append(files_dn[IIfile])
		tmp=tfiles_up[0].split('/'); texp=tmp[-2]; tmp=tmp[-1]; tmp=int(tmp[1:-7]); tmp=tmp+1
		
		sett=0
		while not sett:
			IIfile=IIfile+1
			if (IIfile<NFILES) and (len(tfiles_up)<=MAXFILES):
				ttexp=files_up[IIfile].split('/'); ttexp=ttexp[-2]
				if (ttexp==texp) and (files_up[IIfile][:-7].endswith('%(#)06d' % {"#": tmp})):
					tfiles_up.append(files_up[IIfile])
					tfiles_dn.append(files_dn[IIfile])
					tmp=tmp+1
				else:
					sett=1
			else:
				sett=1
			
		
			
		if IIfile>=NFILES:
			done=1
						
		# loop over plasma line files
		for Ifile in range(len(tfiles_up)):
			
			# read file(s)
			output1=read_a_datafile(tfiles_up[Ifile])
			if dualpl:
				output2=read_a_datafile(tfiles_dn[Ifile])

			if Ifile==0:
				rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
				fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
				if fr_off==output1['/Tx']['Frequency'][0,0]:
					fr_off=FR_OFF[0]	
				freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
				
				BeamcodeMap=output1['/Setup']['BeamcodeMap']
				
				pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
				bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
				time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
				
				if dualpl:
					fr_off=scipy.absolute(output2['/Tx']['Frequency'][0,0]-output2['/Rx']['Frequency'][0,0])
					if fr_off==output1['/Tx']['Frequency'][0,0]:
						fr_off=FR_OFF[1]
					freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
					pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
					bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
					time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
			else:
				pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
				bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
				time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
				if dualpl:
					pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
					bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
					time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)

		beamcodes=scipy.sort(bmcodes_up[0,:])
		Nbeams=beamcodes.shape[0]
		BMCODES=scipy.zeros((Nbeams,4),dtype='Float64') # beamcode table (beamcode,az,el,ksys)
		for i in range(Nbeams):
			I=scipy.where(BeamcodeMap[:,0]==beamcodes[i])[0]
			BMCODES[i,:]=BeamcodeMap[I,:]
				
		# deal the data
		pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
		if dualpl:
			pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

		
		Ntimes=time_up.size
		Nfreqs=freq_up.size

		if len(beams2do)==0:
			beams2do=BMCODES[:,0]

			
		# loop over beams
		for jjj in range(Nbeams):

			# get beamcode
			bmcode=BMCODES[jjj,:]
			rrI=scipy.where(bmcode[0]==beams2do)[0]
					
			if len(rrI)>=1:

				#raw_input('Ready? hit enter')
				figg1.clf()
				
				# subtract highest altitude	
				tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
				tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-1][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2) 
				tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
				if dualpl:
					tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
					tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-1][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
					tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
				#
				alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)

				# find plasma line spectra closest to peak
				tmp=scipy.absolute(alt_pl-htstart)
				Ialt=scipy.where(tmp==tmp.min())[0]
				pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
				if dualpl:
					pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
				if dualpl:
					if flip==0:
						pl_dn=scipy.fliplr(pl_dn)
				if flip==1:
						pl_up=scipy.fliplr(pl_up) # down is really up!!
			
				pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
				if dualpl:
					pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)
					pl_up=scipy.signal.medfilt2d(pl_up+10.0*pl_dn, kernel_size=KS)
					#pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)

#				tmp=scipy.signal.correlate2d(pl_up,pl_dn)
#				return tmp


				pylab.figure(figg1.number)
				pylab.subplot(211)
				pylab.imshow(pl_up,vmin=0,vmax=1e8,aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
				pylab.colorbar()
				ax1=pylab.gca()
				ax1.set_ylabel('Time (secs)')
				ax1.set_xlabel('Freq (Hz)')
				pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
			   
				if dualpl:
					pylab.subplot(212)
					pylab.imshow(pl_dn,vmin=0,vmax=0.1e8,aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
					pylab.colorbar()
					ax2=pylab.gca()
					ax2.set_ylabel('Time (secs)')
					ax2.set_xlabel('Freq (Hz)')

				pylab.show()

				clim=[0.1e8,0.1e8]
#				clim=[1e8,1e8]
				not_ok=1
				while not_ok:

					info=raw_input("Scale OK? Yes hit enter, No enter new clim.u/d to go down/up in alt.")
					if info=='':
						not_ok=0
					elif (info=='u') or (info=='d'):
						if info=='u':
							Ialt=Ialt+1					
						elif info=='d':
							Ialt=Ialt-1
							
						pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
						if dualpl:
							pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
						if dualpl:
							if flip==0:
								pl_dn=scipy.fliplr(pl_dn)
						if flip==1:
								pl_up=scipy.fliplr(pl_up) # down is really up!!
					
						pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
						if dualpl:
							pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)
							pl_up=scipy.signal.medfilt2d(pl_up+10.0*pl_dn, kernel_size=KS)
							#pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
					
						pylab.clf()
						pylab.subplot(211)
						pylab.imshow(pl_up,vmin=0,vmax=clim[0],aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
						pylab.colorbar()
						ax1=pylab.gca()
						ax1.set_ylabel('Time (secs)')
						ax1.set_xlabel('Freq (Hz)')
						pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
					   
						if dualpl:
							pylab.subplot(212)
							pylab.imshow(pl_dn,vmin=0,vmax=clim[1],aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
							pylab.colorbar()
							ax2=pylab.gca()
							ax2.set_ylabel('Time (secs)')
							ax2.set_xlabel('Freq (Hz)')
					
					else:
						try:
							clim=eval('['+info+']')
							pylab.clf()
							pylab.subplot(211)
							pylab.imshow(pl_up,vmin=0,vmax=clim[0],aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
							pylab.colorbar()
							ax1=pylab.gca()
							ax1.set_ylabel('Time (secs)')
							ax1.set_xlabel('Freq (Hz)')
							pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
					   
							if dualpl:
								pylab.subplot(212)
								pylab.imshow(pl_dn,vmin=0,vmax=clim[1],aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
								pylab.colorbar()
								ax2=pylab.gca()
								ax2.set_ylabel('Time (secs)')
								ax2.set_xlabel('Freq (Hz)')							
							
							"""
							print clim[0]
							for im in ax1.get_images(): 
								pylab.subplot(211)
								pylab.imshow(scipy.signal.medfilt2d(pl_up+10.0*pl_dn, kernel_size=KS),vmin=0,vmax=clim[0],aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
								#im.set_clim(0.0,clim[0])
							if dualpl:
								for im in ax2.get_images(): 
									pylab.subplot(212)
									pylab.imshow(pl_dn,vmin=0,vmax=clim[1],aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
									#im.set_clim(0.0,clim[1])					
							#pylab.show()
							"""
						except:
							print 'invalid input'	
			
				Cplup=iplot()
				a=pylab.connect('button_press_event', Cplup)
				pylab.axes(ax1)
				info=raw_input('Doing top plot. Hit enter when done.')
				pylab.disconnect(a)

				if dualpl:
					Cpldn=iplot()
					a=pylab.connect('button_press_event', Cpldn)
					pylab.axes(ax2)
					info=raw_input('Doing bottom plot. Hit enter when done.')
					pylab.disconnect(a)
			

			
				if ((len(Cplup.pl_x)>0) or (dualpl and (len(Cpldn.pl_x)>0)) or (1==1)):
					
					oname='%s-%d-plline*.txt' % (EXP,bmcode[0]) 
					g=glob.glob(os.path.join(ODIR,oname))
					oname='%s-%d-plline-%.3d' % (EXP,bmcode[0],len(g)) 
					
					try:
						figg1.savefig(os.path.join(ODIR,oname+'.png'))
					except:
						print 'Could not save plot'				
					
					try:
						fH=open(os.path.join(ODIR,oname+'.txt'),'w') 
						fH.write('%d %f %f %f\n' % (bmcode[0],bmcode[1],bmcode[2],bmcode[3]))

						if len(Cplup.pl_x)>0:
							fH.write('\n')
							for aa in range(len(Cplup.pl_x)):
								fH.write('%.1f %.3f\n' % (Cplup.pl_y[aa],Cplup.pl_x[aa]))
			
						if dualpl and (len(Cpldn.pl_x)>0):
							fH.write('\n')
							for aa in range(len(Cpldn.pl_x)):
								fH.write('%.1f %.3f\n' % (Cpldn.pl_y[aa],Cpldn.pl_x[aa]))
			
						fH.close()

					except:
						print 'Could not write file'
						
	return #Cplup,Cpldn

def proc_pl2():

#	ODIR='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles2/plasmaline_cal/20071019.006/cal-0'
	EXP='20070321.001'
	ODIR='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/WorldDay02/'+EXP+'/cal-0'
	FILE_PATH=os.path.join('/Volumes/ISR_Data-1/Data AMISR Poker',EXP)
	filepath_up='D001895*.Dt2.h5'
	filepath_dn='D001895*.Dt1.h5'
	Ifiles=[] #range(0,10)
	dualpl=1
	flip=1 # down is really up.
	beams2do=[]
	htstart=250.0e3
	FR_OFF=[5.0e6,5.0e6]
	KS=[3,3]

	# get file list
	files_up=glob.glob(os.path.join(FILE_PATH,filepath_up)) 
	files_up.sort()
	print files_up
	if len(Ifiles)>0:
		files_up=files_up[Ifiles[0]:(Ifiles[-1]+1)]
	if dualpl:
		files_dn=glob.glob(os.path.join(FILE_PATH,filepath_dn)) 
		files_dn.sort()
		if len(Ifiles)>0:
			files_dn=files_dn[Ifiles[0]:(Ifiles[-1]+1)]
	else:
		files_dn=files_up
		
	
	# check numbers
	if len(files_up) != len(files_dn):
		print 'different number of up and down files!'
		sys.exit(-1)
	NFILES=len(files_up)

	# loop over plasma line files
	for Ifile in range(NFILES):
		
		# read file(s)
		output1=read_a_datafile(os.path.join(FILE_PATH,files_up[Ifile]))
		if dualpl:
			output2=read_a_datafile(os.path.join(FILE_PATH,files_dn[Ifile]))

		if Ifile==0:
			rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
			fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
			if fr_off==output1['/Tx']['Frequency'][0,0]:
				fr_off=FR_OFF[0]	
			freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
			
			BeamcodeMap=output1['/Setup']['BeamcodeMap']
			
			pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
			bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
			time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
			
			if dualpl:
				fr_off=scipy.absolute(output2['/Tx']['Frequency'][0,0]-output2['/Rx']['Frequency'][0,0])
				if fr_off==output1['/Tx']['Frequency'][0,0]:
					fr_off=FR_OFF[1]
				freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
				pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
				bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
				time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
		else:
			pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
			bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
			time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
			if dualpl:
				pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
				bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
				time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)

	beamcodes=scipy.sort(bmcodes_up[0,:])
	Nbeams=beamcodes.shape[0]
	BMCODES=scipy.zeros((Nbeams,4),dtype='Float64') # beamcode table (beamcode,az,el,ksys)
	for i in range(Nbeams):
		I=scipy.where(BeamcodeMap[:,0]==beamcodes[i])[0]
		BMCODES[i,:]=BeamcodeMap[I,:]
			
	# deal the data
	pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
	if dualpl:
		pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

	
	Ntimes=time_up.size
	Nfreqs=freq_up.size

	if len(beams2do)==0:
		beams2do=BMCODES[:,0]

	pylab.figure()
#	pylab.show()
	pylab.close('all')
	figg1=pylab.figure();
		
	# loop over beams
	for jjj in range(Nbeams):

		# get beamcode
		bmcode=BMCODES[jjj,:]
		rrI=scipy.where(bmcode[0]==beams2do)[0]
				
		if len(rrI)>=1:

			raw_input('Ready? hit enter')
			figg1.clf()
			
			# subtract highest altitude	
			tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
			tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-1][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2) 
			tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
			if dualpl:
				tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
				tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-1][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
				tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
			#
			alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)


			# find plasma line spectra closest to peak
			tmp=scipy.absolute(alt_pl-htstart)
			Ialt=scipy.where(tmp==tmp.min())[0]
			pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
			if dualpl:
				pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
			if dualpl:
				if flip==0:
					pl_dn=scipy.fliplr(pl_dn)
			if flip==1:
					pl_up=scipy.fliplr(pl_up) # down is really up!!
		
			pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
			if dualpl:
				pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)

			pylab.figure(figg1.number)

			pylab.subplot(211)
			pylab.imshow(pl_up,vmin=0,vmax=1e8,aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
			pylab.colorbar()
			ax1=pylab.gca()
			ax1.set_ylabel('Time (secs)')
			ax1.set_xlabel('Freq (Hz)')
			pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
		   
			if dualpl:
				pylab.subplot(212)
				pylab.imshow(pl_dn,vmin=0,vmax=1e8,aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
				pylab.colorbar()
				ax2=pylab.gca()
				ax2.set_ylabel('Time (secs)')
				ax2.set_xlabel('Freq (Hz)')
		
			pylab.show()

			clim=[1e8,1e8]
			not_ok=1
			while not_ok:

				info=raw_input("Scale OK? Yes hit enter, No enter new clim.u/d to go down/up in alt.")
				if info=='':
					not_ok=0
				elif (info=='u') or (info=='d'):
					if info=='u':
						Ialt=Ialt+1					
					elif info=='d':
						Ialt=Ialt-1
						
					pl_up=scipy.squeeze(tpl_spect_up[:,:,Ialt])
					if dualpl:
						pl_dn=scipy.squeeze(tpl_spect_dn[:,:,Ialt])
					if dualpl:
						if flip==0:
							pl_dn=scipy.fliplr(pl_dn)
					if flip==1:
							pl_up=scipy.fliplr(pl_up) # down is really up!!
				
					pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
					if dualpl:
						pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)
				
					pylab.subplot(211)
					pylab.imshow(pl_up,vmin=0,vmax=clim[0],aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
					ax1=pylab.gca()
					ax1.set_ylabel('Time (secs)')
					ax1.set_xlabel('Freq (Hz)')
					pylab.title('%d (%2.2f,%2.2f) - %2.2f km' % (bmcode[0],bmcode[1],bmcode[2],alt_pl[Ialt]/1000.0))
				   
					if dualpl:
						pylab.subplot(212)
						pylab.imshow(pl_dn,vmin=0,vmax=clim[1],aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
						ax2=pylab.gca()
						ax2.set_ylabel('Time (secs)')
						ax2.set_xlabel('Freq (Hz)')
				
					pylab.show()				
				
				
				
				else:
					try:
						clim=eval('['+info+']')
						for im in ax1.get_images(): 
							im.set_clim(0.0,clim[0])
						if dualpl:
							for im in ax2.get_images(): 
								im.set_clim(0.0,clim[1])					
						pylab.show()
					except:
						print 'invalid input'	
		
			Cplup=iplot()
			a=pylab.connect('button_press_event', Cplup)
			pylab.axes(ax1)
			info=raw_input('Doing top plot. Hit enter when done.')
			pylab.disconnect(a)

			if dualpl:
				Cpldn=iplot()
				a=pylab.connect('button_press_event', Cpldn)
				pylab.axes(ax2)
				info=raw_input('Doing bottom plot. Hit enter when done.')
				pylab.disconnect(a)
		

		
			if ((len(Cplup.pl_x)>0) or (dualpl and (len(Cpldn.pl_x)>0))):
				
				oname='%s-%d-plline*.txt' % (EXP,bmcode[0]) 
				g=glob.glob(os.path.join(ODIR,oname))
				oname='%s-%d-plline-%.3d' % (EXP,bmcode[0],len(g)) 
				
				try:
					figg1.savefig(os.path.join(ODIR,oname+'.png'))
				except:
					print 'Could not save plot'				
				
				try:
					fH=open(os.path.join(ODIR,oname+'.txt'),'w') 
					fH.write('%d %f %f %f\n' % (bmcode[0],bmcode[1],bmcode[2],bmcode[3]))
				except:
					print 'Could not write file'

				if len(Cplup.pl_x)>0:
					fH.write('\n')
					for aa in range(len(Cplup.pl_x)):
						fH.write('%.1f %.3f\n' % (Cplup.pl_y[aa],Cplup.pl_x[aa]))
		
				if dualpl and (len(Cpldn.pl_x)>0):
					fH.write('\n')
					for aa in range(len(Cpldn.pl_x)):
						fH.write('%.1f %.3f\n' % (Cpldn.pl_y[aa],Cpldn.pl_x[aa]))
		
				fH.close()
						
	return #Cplup,Cpldn


def proc_pl():

	ILdir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles2/plasmaline_cal/20071011.010'
	ILfname='20071011.010_lp_2min.h5'
	ILfname='20071011.010_ac_5min.h5'
	#	FILE_PATH='/Volumes/ISR_Data-1/Data AMISR Poker/20070301.013/'
	FILE_PATH='/Volumes/ISR_Data-1/Data AMISR Poker/20071011.010'
	filepath_up='*.dt2.h5'
	filepath_dn='*.dt1.h5'
	Ifiles=range(0,3)
	dualpl=1
	flip=1 # down is really up.
	outdir='cal-1'
	beams2do=[]
	PW=480.0e-6*v_lightspeed/2
	PW=30e-6*v_lightspeed/2
	Ilen=10 # around peak to fit for
	usePower=0 # use NeFromPower or fits
	doChap=1 # do a chapman fit?


	#	ILdir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/plasmaline_cal/20070206.004'
	#	ILfname='20070206.004_lp_1min.h5'
	#	FILE_PATH='/Volumes/ISR_Data/Data AMISR Poker Flat/20070206.004/'
	#	filepath_up='*.Dt2.h5'
	#	filepath_dn='*.Dt1.h5'
	#	dualpl=1
	#	flip=0 # down is really up.
	#	outdir='cal'



	files_up=glob.glob(os.path.join(FILE_PATH,filepath_up)) 
	print files_up
	if len(Ifiles)>0:
		files_up=files_up[Ifiles[0]:(Ifiles[-1]+1)]
	if dualpl:
		files_dn=glob.glob(os.path.join(FILE_PATH,filepath_dn)) 
		if len(Ifiles)>0:
			files_dn=files_dn[Ifiles[0]:(Ifiles[-1]+1)]
	else:
		files_dn=files_up
		
	if len(files_up) != len(files_dn):
		print 'different number of up and down files!'
		sys.exit(-1)
		
	files_dn.sort()
	files_up.sort()
	
	NFILES=len(files_up)

	# read ion line data
	ILdata=read_a_datafile(os.path.join(ILdir,ILfname))
	BMCODES=ILdata['/']['BeamCodes']
	Nbeams=BMCODES.shape[0]
	IlTime=scipy.mean(ILdata['/Time']['UnixTime'],axis=1)
	NePower=ILdata['/NeFromPower']['Ne_NoTr']
	AltPower=ILdata['/NeFromPower']['Altitude']
	Ne=ILdata['/FittedParams']['Ne']
	Alt=ILdata['/FittedParams']['Altitude']
	Te=ILdata['/FittedParams']['Fits'][:,:,:,2,1]
	Ti=ILdata['/FittedParams']['Fits'][:,:,:,0,1]
	Tr=Te/Ti
	Babs=ILdata['/Geomag']['Babs']
	kpar=ILdata['/Geomag']['kpar']
	kperp=scipy.sqrt(scipy.power(ILdata['/Geomag']['kpe'],2.0)+scipy.power(ILdata['/Geomag']['kpn'],2.0))
	alph=math.pi/2-scipy.arcsin(kpar/scipy.sqrt(scipy.power(kpar,2.0)+scipy.power(kperp,2.0))).real

	YR=ILdata['/Time']['Year'][0,0]
	DAY=ILdata['/Time']['Day'][0,0]
	MON=ILdata['/Time']['Month'][0,0]

	# loop over plasma line files
	for Ifile in range(NFILES):
		output1=read_a_datafile(os.path.join(FILE_PATH,files_up[Ifile]))
		if dualpl:
			output2=read_a_datafile(os.path.join(FILE_PATH,files_dn[Ifile]))

		if Ifile==0:
			rng=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Range'])
			fr_off=scipy.absolute(output1['/Tx']['Frequency'][0,0]-output1['/Rx']['Frequency'][0,0])
			if fr_off==output1['/Tx']['Frequency'][0,0]:
				fr_off=5.0e6			
			freq_up=scipy.squeeze(output1['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
			
			pl_spect_up=output1['/PLFFTS/Data/Spectra']['Data']
			bmcodes_up=output1['/PLFFTS/Data']['Beamcodes']
			time_up=scipy.mean(output1['/Time']['UnixTime'],axis=1)
			
			if dualpl:
				freq_dn=scipy.squeeze(output2['/PLFFTS/Data/Spectra']['Frequency'])+fr_off
				pl_spect_dn=output2['/PLFFTS/Data/Spectra']['Data']
				bmcodes_dn=output2['/PLFFTS/Data']['Beamcodes']
				time_dn=scipy.mean(output2['/Time']['UnixTime'],axis=1)
		else:
			pl_spect_up=scipy.concatenate((pl_spect_up,output1['/PLFFTS/Data/Spectra']['Data']),axis=0)
			bmcodes_up=scipy.concatenate((bmcodes_up,output1['/PLFFTS/Data']['Beamcodes']),axis=0)
			time_up=scipy.concatenate((time_up,scipy.mean(output1['/Time']['UnixTime'],axis=1)),axis=0)
			if dualpl:
				pl_spect_dn=scipy.concatenate((pl_spect_dn,output2['/PLFFTS/Data/Spectra']['Data']),axis=0)
				bmcodes_dn=scipy.concatenate((bmcodes_dn,output2['/PLFFTS/Data']['Beamcodes']),axis=0)
				time_dn=scipy.concatenate((time_dn,scipy.mean(output2['/Time']['UnixTime'],axis=1)),axis=0)
	# deal the data
	pl_spect_up=proc_utils.deal_data(bmcodes_up,pl_spect_up,BMCODES[:,0])
	if dualpl:
		pl_spect_dn=proc_utils.deal_data(bmcodes_dn,pl_spect_dn,BMCODES[:,0])

	Ntimes=time_up.size
	Nfreqs=freq_up.size

	if len(beams2do)==0:
		beams2do=BMCODES[:,0]

	pylab.figure()
	pylab.show()
	figg1=pylab.figure();
	figg2=pylab.figure()
		
	# loop over beams
	for jjj in range(Nbeams):

		# get beamcode
		bmcode=BMCODES[jjj,:]
		rrI=scipy.where(bmcode[0]==beams2do)[0]
		
		tPW=PW*scipy.sin(bmcode[2]*scipy.pi/180.0)
		
		if len(rrI)>=1:

			raw_input('Ready? hit enter')
			figg1.clf()
			figg2.clf()
			
			tAlt=scipy.squeeze(Alt[jjj,:])
			tAltPower=scipy.squeeze(AltPower[jjj,:])
			Ialt=scipy.where((tAlt>=150.0e3) & (tAlt<=350.0e3))
			IaltPower=scipy.where((tAltPower>=150.0e3) & (tAltPower<=350.0e3))
			tAlt=tAlt[Ialt]
			tAltPower=tAltPower[IaltPower]
			tNe=scipy.squeeze(Ne[:,jjj,Ialt])
			tNePower=scipy.squeeze(NePower[:,jjj,IaltPower])
			tTe=scipy.squeeze(Te[:,jjj,Ialt])
			tTr=scipy.squeeze(Tr[:,jjj,Ialt])
			talph=scipy.squeeze(alph[jjj,Ialt])
			tBabs=scipy.squeeze(Babs[jjj,Ialt])
			
			# subtract highest altitude	
			tpl_spect_up=scipy.squeeze(pl_spect_up[:,jjj,:,:])
			tpl_spect_up=tpl_spect_up-scipy.repeat(tpl_spect_up[:,:,-1][:,:,scipy.newaxis],tpl_spect_up.shape[2],axis=2) 
			tpl_spect_up[scipy.where(tpl_spect_up<0.0)]=0.0
			if dualpl:
				tpl_spect_dn=scipy.squeeze(pl_spect_dn[:,jjj,:,:])
				tpl_spect_dn=tpl_spect_dn-scipy.repeat(tpl_spect_dn[:,:,-1][:,:,scipy.newaxis],tpl_spect_dn.shape[2],axis=2)
				tpl_spect_dn[scipy.where(tpl_spect_dn<0.0)]=0.0
			#
			alt_pl=rng*scipy.sin(bmcode[2]*math.pi/180)
			
			itime=scipy.zeros(Ntimes,dtype='float64')
			pl_up=scipy.zeros((Ntimes,Nfreqs),dtype='float64')*scipy.nan
			pl_dn=scipy.zeros((Ntimes,Nfreqs),dtype='float64')*scipy.nan
			htMax=scipy.zeros(Ntimes,dtype='float64')*scipy.nan
			NeMax=scipy.zeros(Ntimes,dtype='float64')*scipy.nan
			TeMax=scipy.zeros(Ntimes,dtype='float64')*scipy.nan
			TrMax=scipy.zeros(Ntimes,dtype='float64')*scipy.nan
			BabsMax=scipy.zeros(Ntimes,dtype='float64')*scipy.nan
			alphMax=scipy.zeros(Ntimes,dtype='float64')*scipy.nan
			
			for aa in range(Ntimes):
				# find the closest time
				tmp=scipy.absolute(IlTime-time_up[aa])				
				I=scipy.where(tmp==tmp.min())[0]
				I=I[0]
				if usePower:
					tprof=scipy.squeeze(tNePower[I,:])
					ttalt=tAltPower
				else:
					tprof=scipy.squeeze(tNe[I,:])
					ttalt=tAlt
				itime[aa]=IlTime[I]
				tte=scipy.squeeze(tTe[I,:])
				ttr=scipy.squeeze(tTr[I,:])
				
				I=scipy.where(itime==itime[aa])[0]
				if len(I)>1:
					tI=I[0]
					htMax[aa]=htMax[tI]
					NeMax[aa]=NeMax[tI]
					TeMax[aa]=TeMax[tI]
					TrMax[aa]=TrMax[tI]
					BabsMax[aa]=BabsMax[tI]
					alphMax[aa]=alphMax[tI]
				else:
		#			pylab.clf()
		#			pylab.plot(tAlt,tprof)
		#			pylab.plot(AltPower[jjj,:],scipy.squeeze(NePower[I,jjj,:]))
#					try:
					if 1==1:
						# get maximum Ne and altitude of peak
						I=scipy.where(tprof==tprof.max())[0]
						p=scipy.polyfit(ttalt[I-Ilen:I+Ilen+1], tprof[I-Ilen:I+Ilen+1], 2, rcond=None, full=False)
						htMax[aa]=-p[1]/2.0/p[0]
		#					htMax=200.0e3
						NeMax[aa]=p[0]*htMax[aa]**2+p[1]*htMax[aa]+p[2]
						if NeMax[aa]>1.0e13:
							NeMax[aa]=1.0e11
							
						if doChap:
						# fit range-smeared chapman
							thch=40.0e3
							scaler=scipy.array([1.0e11,1.0e5,1.0e4])
							params0=scipy.array([NeMax[aa],htMax[aa],thch])
							(x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(chap_fitfun,params0/scaler,(ttalt[I-Ilen:I+Ilen+1],tprof[I-Ilen:I+Ilen+1],scaler,tPW),
								full_output=1,epsfcn=1.0e-5,ftol=1.0e-5, xtol=1.0e-5, gtol=0.0, maxfev=15*params0.shape[0],factor=0.5,diag=None)
							tz=scipy.arange(ttalt[I-Ilen],ttalt[I+Ilen+1],100)
							nemod=chap_fitfun(x,tz,tz,scaler,tPW,mode=1)
							x=x*scaler
							"""	
							print x
							tne=chapman_func(tz,x[2],x[0],x[1])
							tne0=chapman_func(tz,params0[2],params0[0],params0[1]) # initial guess
							pylab.plot(tprof[I-Ilen:I+Ilen+1],ttalt[I-Ilen:I+Ilen+1],'.b-')
							pylab.plot(tne,tz,'k')
							pylab.plot(tne0,tz,'r')
							pylab.plot(nemod,tz,'k--')
							"""

							htMax[aa]=x[1]
							NeMax[aa]=x[0]
						
						TeMax[aa]=scipy.interpolate.interp1d(tAlt,tte)(htMax[aa])
						TrMax[aa]=scipy.interpolate.interp1d(tAlt,ttr)(htMax[aa])
						if TrMax[aa]>4.0:
							TrMax[aa]=1.0
						if NeMax[aa]>1.0e13:
							NeMax[aa]=scipy.nan
						if usePower:
							NeMax[aa]=NeMax[aa]/2.0*(1.0+TrMax[aa])
												
						alphMax[aa]=scipy.interpolate.interp1d(tAlt,talph)(htMax[aa])
						BabsMax[aa]=scipy.interpolate.interp1d(tAlt,tBabs)(htMax[aa])			
			#			pylab.plot(scipy.linspace(Alt[I-1],Alt[I+1],500),scipy.polyval(p,scipy.linspace(Alt[I-1],Alt[I+1],500)))

				#	except:
				#		''

				# find plasma line spectra closest to peak
				tmp=scipy.absolute(alt_pl-htMax[aa])
				print htMax[aa]
				I=scipy.where(tmp==tmp.min())[0]
				pl_up[aa,:]=tpl_spect_up[aa,:,I]
				if dualpl:
					pl_dn[aa,:]=tpl_spect_dn[aa,:,I]

			if dualpl:
				if flip==0:
					pl_dn=scipy.fliplr(pl_dn)
			if flip==1:
					pl_up=scipy.fliplr(pl_up) # down is really up!!
		
			pl_up=scipy.signal.medfilt2d(pl_up, kernel_size=KS)
			if dualpl:
				pl_dn=scipy.signal.medfilt2d(pl_dn, kernel_size=KS)

			pylab.figure(figg1.number)
			pylab.subplot(211)
			pylab.imshow(pl_up,vmin=0,vmax=1e8,aspect='auto',origin='lower',extent=[freq_up[0],freq_up[-1],time_up[0],time_up[-1]])	
			pylab.colorbar()
			ax1=pylab.gca()
			ax1.set_ylabel('Time (secs)')
			ax1.set_xlabel('Freq (Hz)')
			pylab.title('%d (%2.2f,%2.2f)' % (bmcode[0],bmcode[1],bmcode[2]))
		   
			if dualpl:
				pylab.subplot(212)
				pylab.imshow(pl_dn,vmin=0,vmax=1e8,aspect='auto',origin='lower',extent=[freq_dn[0],freq_dn[-1],time_dn[0],time_dn[-1]])
				pylab.colorbar()
				ax2=pylab.gca()
				ax2.set_ylabel('Time (secs)')
				ax2.set_xlabel('Freq (Hz)')
		
			pylab.show()
		
			not_ok=1
			while not_ok:
				info=raw_input('Scale OK? Yes hit enter, No enter new clim.')
				if info=='':
					not_ok=0
				else:
					try:
						clim=eval('['+info+']')
						for im in ax1.get_images(): 
							im.set_clim(0.0,clim[0])
						if dualpl:
							for im in ax2.get_images(): 
								im.set_clim(0.0,clim[1])					
						pylab.show()
					except:
						print 'invalid input'	
		
			Cplup=iplot()
			a=pylab.connect('button_press_event', Cplup)
			pylab.axes(ax1)
			info=raw_input('Doing top plot. Hit enter when done.')
			pylab.disconnect(a)

			if dualpl:
				Cpldn=iplot()
				a=pylab.connect('button_press_event', Cpldn)
				pylab.axes(ax2)
				info=raw_input('Doing bottom plot. Hit enter when done.')
				pylab.disconnect(a)
		
			try:
				oname='%d-%d-%d-%d-plline.png' % (DAY,MON,YR,bmcode[0])
				figg1.savefig(os.path.join(ILdir,outdir,oname))
			except:
				'Couldnt save plot'
		
			if len(Cplup.pl_x)>0:
		
				pl_ne_up=[]
				NeIl_up=[]
				TrIl_up=[]
				for aa in range(len(Cplup.pl_x)):
					tmp=scipy.absolute(Cplup.pl_y[aa]-time_up)
					ttm=scipy.where(tmp==tmp.min())
					NeIl_up.append(float(NeMax[ttm]))
					TrIl_up.append(float(TrMax[ttm]))
					TeIl=TeMax[ttm]
					pl_ne_up.append(float(freq2ne(Cplup.pl_x[aa],Te=TeIl,B=BabsMax[ttm],alph=alphMax[aa])))
				pl_ne_up=scipy.array(pl_ne_up)
				NeIl_up=scipy.array(NeIl_up)
				KsysCor_up=NeIl_up/pl_ne_up
				Ksys_all=KsysCor_up.copy()

				if dualpl:
					pl_ne_dn=[]
					NeIl_dn=[]
					TrIl_dn=[]
					for aa in range(len(Cpldn.pl_x)):
						tmp=scipy.absolute(Cpldn.pl_y[aa]-time_up)
						ttm=scipy.where(tmp==tmp.min())
						NeIl_dn.append(float(NeMax[ttm]))
						TrIl_dn.append(float(TrMax[ttm]))
						TeIl=TeMax[ttm]
						pl_ne_dn.append(float(freq2ne(Cpldn.pl_x[aa],Te=TeIl,B=BabsMax[ttm],alph=alphMax[aa])))
					pl_ne_dn=scipy.array(pl_ne_dn)
					NeIl_dn=scipy.array(NeIl_dn)
					KsysCor_dn=NeIl_dn/pl_ne_dn
					Ksys_all=scipy.concatenate((KsysCor_up,KsysCor_dn))
								
				Ksys_med=scipy.stats.stats.nanmedian(Ksys_all)
				Ksys_std=scipy.stats.stats.nanstd(Ksys_all)

				pylab.figure(figg2.number)
				pylab.subplot(211)
				pylab.scatter(Cplup.pl_y,pl_ne_up,c='r')
				if dualpl:
					pylab.scatter(Cpldn.pl_y,pl_ne_dn,c='b')
				pylab.plot(time_up,NeMax)
				pylab.gca().set_ylabel('Ne (m-3)')
				pylab.title('%d (%2.2f,%2.2f)' % (bmcode[0],bmcode[1],bmcode[2]))
				
				pylab.subplot(212)
				pylab.scatter(Cplup.pl_y,KsysCor_up,c='r')
				if dualpl:
					pylab.scatter(Cpldn.pl_y,KsysCor_dn,c='b')
				v=pylab.axis()
				pylab.plot(v[0:2],[Ksys_med,Ksys_med],'k')
				pylab.plot(v[0:2],[Ksys_med+Ksys_std,Ksys_med+Ksys_std],'k--')
				pylab.plot(v[0:2],[Ksys_med-Ksys_std,Ksys_med-Ksys_std],'k--')
				pylab.gca().set_ylabel('Ksys-cor')
				pylab.gca().set_xlabel('Time (secs)')
						
				try:
					oname='%d-%d-%d-%d-ksys.png' % (DAY,MON,YR,bmcode[0])
					figg2.savefig(os.path.join(ILdir,outdir,oname))
				except:
					print 'Couldnt save plot'
				try:
					oname='%d-%d-%d-%d-ksys.txt' % (DAY,MON,YR,bmcode[0])
					fH=open(os.path.join(ILdir,outdir,oname),'w') 
					fH.write('%f %f %f %f\n' % (bmcode[0],bmcode[1],bmcode[2],bmcode[3]))
					fH.write('%f %f\n' % (Ksys_med,Ksys_std)) 
					fH.write('%f %f\n' % (scipy.stats.stats.nanmedian(NeIl_up),scipy.stats.stats.nanmedian(TrIl_up)))
					if dualpl:
						fH.write('%f %f\n' % (scipy.stats.stats.nanmedian(NeIl_dn),scipy.stats.stats.nanmedian(TrIl_dn)))
					fH.close()
				except:
					print 'Couldnt write file'
		
	return #Cplup,Cpldn


