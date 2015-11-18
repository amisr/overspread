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
#matplotlib.use('Agg')
import pylab

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

		# long pulse
		odir='/Volumes/AMISR_004/processed_data/2007/IPY01/20070703.002'
		datName=os.path.join(odir,'IPY01-20070703.002_lp_15min-64157-cal.h5'); oName=os.path.join(odir,'IPY01-20070703.002_lp_15min-64157-cal2.h5'); plotdir=os.path.join(odir,'plots_lp_2min')
		datName=os.path.join(odir,'IPY01-20070703.002_ac_15min-64157-cal.h5'); oName=os.path.join(odir,'IPY01-20070703.002_ac_15min-64157-cal2.h5'); plotdir=os.path.join(odir,'plots_lp_2min')

		replot=0;
		
#		odir='/Volumes/AMISR_004/processed_data/Erickson02/20080614.004'
#		datName=os.path.join(odir,'20080614.004_lp_2min.h5'); oName=os.path.join(odir,'20080614.004_lp_2min2.h5'); plotdir=os.path.join(odir,'plots_lp_2min')
#		datName=os.path.join(odir,'20080614.004_ac_5min.h5'); oName=os.path.join(odir,'20080614.004_ac_5min.h5'); plotdir=os.path.join(odir,'plots_ac_5min')


		# read datfile
		h5dat=readafile(datName)
		mUnixTime=scipy.mean(h5dat['/Time']['UnixTime'],1)
		NeFit=h5dat['/FittedParams']['Ne']; (Nrecs,Nbeams,Nhts)=NeFit.shape
		dNeFit=h5dat['/FittedParams']['dNe']
		Ne_NoTr=h5dat['/NeFromPower']['Ne_NoTr']; (x1,x2,Nhts2)=Ne_NoTr.shape
		Ne_Mod=h5dat['/NeFromPower']['Ne_Mod']
		Fits=h5dat['/FittedParams']['Fits']
		Errs=h5dat['/FittedParams']['Errors']
		AeuRx=h5dat['/ProcessingParams']['AeuRx']
		AeuTx=h5dat['/ProcessingParams']['AeuTx']
		try: AeuTotal=h5dat['/ProcessingParams']['AeuTotal']
		except: AeuTotal=4096
		TxPower=h5dat['/ProcessingParams']['TxPower']
		
		usePower=1; Pmax=1.1e6; Psc=2.0; doFits=1; Psc=scipy.nan;
#		usePower=0; useAeu=1; Psc=scipy.nan; doFits=1
		
		if usePower:
			TxPower=scipy.signal.medfilt(TxPower,1)
			I=scipy.where(TxPower<Pmax)[0];

			corrNeFit=NeFit.copy(); corrNeFit[I]=corrNeFit[I]/Psc
			corrdNeFit=dNeFit.copy(); corrdNeFit[I]=corrdNeFit[I]/Psc
			corrNe_NoTr=Ne_NoTr.copy(); corrNe_NoTr[I]=corrNe_NoTr[I]/Psc
			corrNe_Mod=Ne_Mod.copy(); corrNe_Mod[I]=corrNe_Mod[I]/Psc

			corrFits=Fits.copy();
			corrErrs=Errs.copy();			
			if doFits:
				corrFits[I]=corrFits[I]/Psc
				corrErrs[I]=corrErrs[I]/Psc

		elif useAeu:
			AeuRx=scipy.signal.medfilt(AeuRx/AeuTotal,3)
			AeuTx=scipy.signal.medfilt(AeuTx/AeuTotal,3)
			I=scipy.where((AeuRx<0.1) & (AeuTx<0.1))[0]

			corrNeFit=NeFit.copy(); corrNeFit[I]=corrNeFit[I]/Psc
			corrdNeFit=dNeFit.copy(); corrdNeFit[I]=corrdNeFit[I]/Psc
			corrNe_NoTr=Ne_NoTr.copy(); corrNe_NoTr[I]=corrNe_NoTr[I]/Psc
			corrNe_Mod=Ne_Mod.copy(); corrNe_Mod[I]=corrNe_Mod[I]/Psc
			corrFits=Fits.copy(); corrFits[I]=corrFits[I]/Psc
			corrErrs=Errs.copy(); corrErrs[I]=corrErrs[I]/Psc
		else:
			dsfsdfdfdsfsfsdfsdf
		
		#
		h5dat['/FittedParams']['Fits']=corrFits
		h5dat['/FittedParams']['Errors']=corrErrs
		h5dat['/FittedParams']['Ne']=corrNeFit
		h5dat['/FittedParams']['dNe']=corrdNeFit
		h5dat['/NeFromPower']['Ne_NoTr']=corrNe_NoTr
		h5dat['/NeFromPower']['Ne_Mod']=corrNe_Mod
		
		# write datfile
		print 'Writing output to' + oName
		outh5file=tables.openFile(oName, mode = "w", title = "Fit File")
		for key in h5dat.keys():
			write_outputfile(outh5file,h5dat[key],h5dat[key].keys(),groupname=key[1:])
		outh5file.close()
		
		if replot:
			plot_utils.replot_pcolor_all(oName,saveplots=1,opath=plotdir,clims=[[10,12],[0,1500],[0,3000],[0,4],[-500,500]],ylim=[],tlim=[])
		
