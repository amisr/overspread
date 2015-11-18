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

		ipyCompatDir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/IPY01/20071109/IPY-compatible/'

		# long pulse
		datName='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/IPY01/20071109/IPY01-20071109_lp_15min.h5'
		fileWC='*-IPY-compatible_lp_15min.h5'
		oname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/IPY01/20071109/IPY01-combined-20071109_lp_15min.h5'
		
		# alt code
#		calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_012008/ipy01_cal_Alt Code.txt'
#		datName='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/IPY01/20071109/IPY01-20071109_ac_15min.h5'
#		oName='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/IPY01/20071109/IPY01-cal-20071109_ac_15min.h5'
		
		files=glob.glob(os.path.join(ipyCompatDir,fileWC)) 
		Nfiles=len(files)

		# read datfile
		h5dat=readafile(datName)		
		UnixTime=h5dat['/Time']['UnixTime']; Nrecs=UnixTime.shape[0]

		for ifile in range(Nfiles):
		
			ih5dat=readafile(files[ifile])
			iUnixTime=ih5dat['/Time']['UnixTime']; iNrecs=iUnixTime.shape[0]
			
			for irec in range(iNrecs):
				I=scipy.where(iUnixTime[irec,0]>h5dat['/Time']['UnixTime'][:,1])[0]; I=I[-1]
				# /Time
				for key in h5dat['/Time'].keys():
					h5dat['/Time'][key]=scipy.concatenate((h5dat['/Time'][key][:I+1],ih5dat['/Time'][key][irec][scipy.newaxis],h5dat['/Time'][key][I+1:]))
				# /FittedParams
				for key in h5dat['/FittedParams'].keys():
					if h5dat['/FittedParams'][key].shape[0]==Nrecs:
						h5dat['/FittedParams'][key]=scipy.concatenate((h5dat['/FittedParams'][key][:I+1],ih5dat['/FittedParams'][key][irec][scipy.newaxis],h5dat['/FittedParams'][key][I+1:]))
				# /NeFromPower
				for key in h5dat['/NeFromPower'].keys():
					if h5dat['/NeFromPower'][key].shape[0]==Nrecs:
						h5dat['/NeFromPower'][key]=scipy.concatenate((h5dat['/NeFromPower'][key][:I+1],ih5dat['/NeFromPower'][key][irec][scipy.newaxis],h5dat['/NeFromPower'][key][I+1:]))
				xxx


		xxxx
		
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
		
		# write datfile
		print 'Writing output to' + oName
		outh5file=tables.openFile(oName, mode = "w", title = "Fit File")
		for key in h5dat.keys():
			write_outputfile(outh5file,h5dat[key],h5dat[key].keys(),groupname=key[1:])
		outh5file.close()
