#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys
import os.path
import tables
import scipy
import scipy.signal
import scipy.interpolate
import scipy.stats
import scipy.io
import glob
import shutil
import matplotlib
matplotlib.use('Agg')
import pylab
import datetime

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import plot_utils

##############################

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
			fhandle.createArray(group, key, dict2do[key], "Dataset")

if __name__ == '__main__':

		replot=0;

		corrChirp=1; chirpCorr=-20.0

		# Erickson
		odir='/Volumes/AMISR_004/processed_data/2008/Erickson12'; exp='20080929.001'; 
		tpAll=['lp_2min','ac_5min']
		calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/Erickson02/Erickson02-calibration-10.13.2008.txt'
		st=''
		subDir='cal-0'
		useScaler=1 # scaler from plasma line
		useCalData=1 # beam-dependent factor
		"""
		"""

		"""
		# IPY
		odir='/Volumes/AMISR_004/processed_data/2008/IPY05'; exp='20080404.004'; 
		calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20080404.004-ipy05/IPY05-calibration-LP-10.08.2008.txt'
		tpAll=['ac_15min-cal','lp_15min-cal']
#		tpAll=['ac_15min-64157-cal', 'ac_15min-cal', 'lp_15min-64157-cal', 'lp_15min-cal']
		st='IPY05-'
		useScaler=0 # scaler from plasma line
		useCalData=1 # beam-dependent factor
		"""

		"""
		# IPY
		odir='/Volumes/AMISR_004/processed_data/2007/IPY01'; exp='20070801.003'; 
		tpAll=['ac_15min-64157-cal', 'lp_15min-64157-cal']
		st='IPY01-'
		useScaler=0 # scaler from plasma line
		useCalData=0 # beam-dependent factor

		# Mis
		odir='/Volumes/AMISR_004/processed_data/2007/LTCS01'; exp='20071209.006'; 
		calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/20071209.006/LTCS01-calibration-LP-10.10.2008.txt'		
		tpAll=['lp_2min-cal', 'ac_5min-cal', 'lp_15min-cal', 'ac_15min-cal']
		st=''
		useScaler=0 # scaler from plasma line
		useCalData=1 # beam-dependent factor
		"""

		"""
		# Lyons
		odir='/Volumes/AMISR_004/processed_data/Lyons03'; exp='20080618.001'; 
		tpAll=['lp_1min', 'lp_3min', 'ac_2min-rng2', 'ac_6min']
		st=''
		useScaler=0 # scaler from plasma line
		useCalData=0 # beam-dependent factor
		"""
	
		if useCalData:
			calData=scipy.io.read_array(calFname)
		
		for ii in range(len(tpAll)):
			tp=tpAll[ii]
				
			datName=os.path.join(odir,exp,st+exp +'_' + tp +'.h5'); oName=os.path.join(odir,exp,st+exp + '_' + tp+ '-cal' + '.h5');  plotdir=os.path.join(odir,exp,'plots_' + tp)

			# read datfile
			h5dat=readafile(datName)
			mUnixTime=scipy.mean(h5dat['/Time']['UnixTime'],1)
			NeFit=h5dat['/FittedParams']['Ne']; (Nrecs,Nbeams,Nhts)=NeFit.shape
			dNeFit=h5dat['/FittedParams']['dNe']
			Ne_NoTr=h5dat['/NeFromPower']['Ne_NoTr']; (x1,x2,Nhts2)=Ne_NoTr.shape
			Ne_Mod=h5dat['/NeFromPower']['Ne_Mod']
			Fits=h5dat['/FittedParams']['Fits']
			BeamCodes=h5dat['/']['BeamCodes']
			Nbeams=Ne_Mod.shape[1]
			
			# scaler (plasma line)
			scaler=scipy.ones((Nbeams,2),dtype='float32')
			if useScaler:
				if os.path.exists(os.path.join(odir,exp,subDir,'CalFile-'+tp+'.txt')):
					scaler=scipy.io.read_array(os.path.join(odir,exp,subDir,'CalFile-'+tp+'.txt'))
					print 'Using scaler'
				else:
					print 'Not using scaler'
					xxxxx
				
			# beam-dependent
			if useCalData:
				tCalData=calData.copy()
				tCalData[:,4]=tCalData[:,4]*scaler[:,1]
			
				corrNeFit=NeFit.copy(); corrdNeFit=dNeFit.copy();
				corrNe_NoTr=Ne_NoTr.copy(); corrNe_Mod=Ne_Mod.copy();
				for ibm in range(Ne_Mod.shape[1]):
					ibm2do=scipy.where(tCalData[:,0]==BeamCodes[ibm,0])[0]
					corrNeFit[:,ibm,:]=corrNeFit[:,ibm,:]/tCalData[ibm2do,4]
					corrdNeFit[:,ibm,:]=corrdNeFit[:,ibm,:]/tCalData[ibm2do,4]
					corrNe_NoTr[:,ibm,:]=corrNe_NoTr[:,ibm,:]/tCalData[ibm2do,4]
					corrNe_Mod[:,ibm,:]=corrNe_Mod[:,ibm,:]/tCalData[ibm2do,4]	
				
				#
				h5dat['/FittedParams']['Ne']=corrNeFit
				h5dat['/FittedParams']['dNe']=corrdNeFit
				h5dat['/NeFromPower']['Ne_NoTr']=corrNe_NoTr
				h5dat['/NeFromPower']['Ne_Mod']=corrNe_Mod
			
			# chirp correction
			if corrChirp:
				corrFits=Fits.copy()
				corrFits[:,:,:,:,-1]=corrFits[:,:,:,:,-1]+chirpCorr
				h5dat['/FittedParams']['Fits']=corrFits
			
			t=datetime.date.today()
			datestr=t.strftime("%Y-%m-%d")			
			
			# write datfile
			print 'Writing output to' + oName
			outh5file=tables.openFile(oName, mode = "w", title = "Fit File")
			for key in h5dat.keys():
				write_outputfile(outh5file,h5dat[key],h5dat[key].keys(),groupname=key[1:])
			if useCalData:
				write_outputfile(outh5file,calData,groupname='Calibration',name='CalDataBeam')
				write_outputfile(outh5file,datestr,groupname='Calibration',name='CalDateBeam')
			if useScaler:
				write_outputfile(outh5file,scaler,groupname='Calibration',name='AdditionalScaler')
				write_outputfile(outh5file,os.path.basename(calFname),groupname='Calibration',name='CalFileBeam')
			if corrChirp:
				write_outputfile(outh5file,chirpCorr,groupname='Calibration',name='ChirpCorrection')
			outh5file.close()
			
			if replot:
				plot_utils.replot_pcolor_all(oName,saveplots=1,opath=plotdir,clims=[[10,12],[0,1500],[0,3000],[0,4],[-500,500]],ylim=[],tlim=[])
		
