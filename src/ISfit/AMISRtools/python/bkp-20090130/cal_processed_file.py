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
import scipy.interpolate
import scipy.stats
import scipy.io
import glob
import shutil
import datetime


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
		calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20071201.001-ipy04/ipy04_cal_Long Pulse.txt'
		odir='/Volumes/AMISR_004/processed_data/2007/LTCS01/20071209.006'
		datName=os.path.join(odir,'20071209.006_lp_2min.h5'); oName=datName[:-3] + '-cal.h5'

		"""
		"""
		# alt code
		calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20071201.001-ipy04/ipy04_cal_Alt Code.txt'
		odir='/Volumes/AMISR_004/processed_data/2007/LTCS01/20071209.006'
		datName=os.path.join(odir,'20071209.006_ac_5min.h5'); oName=datName[:-3] + '-cal.h5'
				
		# read calfile
		calData=scipy.io.read_array(calFname)
		calData=scipy.concatenate((scipy.array([[0.0,scipy.stats.stats.nanmedian(calData[0:10,1])]]),calData))		
		calData=scipy.concatenate((calData,scipy.array([[2.0e9,scipy.stats.stats.nanmedian(calData[-10:,1])]])))		
		
		# read datfile
		h5dat=readafile(datName)
		mUnixTime=scipy.mean(h5dat['/Time']['UnixTime'],1)
		NeFit=h5dat['/FittedParams']['Ne']; (Nrecs,Nbeams,Nhts)=NeFit.shape
		dNeFit=h5dat['/FittedParams']['dNe']
		Ne_NoTr=h5dat['/NeFromPower']['Ne_NoTr']; (x1,x2,Nhts2)=Ne_NoTr.shape
		Ne_Mod=h5dat['/NeFromPower']['Ne_Mod']
		
		# interpolate Ksyscorr
		KsysCorr=scipy.interpolate.interp1d(calData[:,0],calData[:,1],bounds_error=False)(mUnixTime)
#		for aa in range(Idiff.shape[0]):
#			I=scipy.where((mUnixTime>=ppData[Idiff[aa]]) & (mUnixTime<=ppData[Idiff[aa+1]]))
#			print I
#			xxxx
		
		corrNeFit=NeFit/scipy.repeat(scipy.repeat(KsysCorr[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts,axis=2)
		corrdNeFit=dNeFit/scipy.repeat(scipy.repeat(KsysCorr[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts,axis=2)
		corrNe_NoTr=Ne_NoTr/scipy.repeat(scipy.repeat(KsysCorr[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts2,axis=2)
		corrNe_Mod=Ne_Mod/scipy.repeat(scipy.repeat(KsysCorr[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts2,axis=2)
		
		#
		h5dat['/FittedParams']['Ne']=corrNeFit
		h5dat['/FittedParams']['dNe']=corrdNeFit
		h5dat['/NeFromPower']['Ne_NoTr']=corrNe_NoTr
		h5dat['/NeFromPower']['Ne_Mod']=corrNe_Mod
		
		t=datetime.date.today()
		datestr=t.strftime("%Y-%m-%d")
		
		# write datfile
		print 'Writing output to' + oName
		outh5file=tables.openFile(oName, mode = "w", title = "Fit File")
		for key in h5dat.keys():
			write_outputfile(outh5file,h5dat[key],h5dat[key].keys(),groupname=key[1:])
		write_outputfile(outh5file,calData,groupname='Calibration',name='CalDataAll')
		write_outputfile(outh5file,datestr,groupname='Calibration',name='CalDateAll')
		write_outputfile(outh5file,calFname,groupname='Calibration',name='CalFileAll')		
		outh5file.close()
