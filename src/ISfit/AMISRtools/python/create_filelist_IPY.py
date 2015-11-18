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
import scipy.io
import glob
import shutil



##############################

if __name__ == '__main__':

	DISK_PATH='/Volumes/AMISR_008/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')  # where the experiments are
	FITTER_PATH='/Volumes/AMISR_004/processed_data/2009/'
	DIR_WC='200901*' # wild card to select exps from

	MODE='IPY15'
	CODE='AC'

	# setup for IPY05/IPY15
	if MODE=='IPY15':
		if CODE=='LP':
			# Long pulse setup
			IPY_COMPAT_MODES=[MODE,'IPY16','PLCal13','Erickson12','WorldDay16''Mathews11','Lyons13','ACES10','MSWinds23','MSWinds22','A16flTest10','Hedden10'];	
			IPY_FILES_WC=['*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_LP.txt')
		elif CODE=='AC':
			# Alt code setup
			IPY_COMPAT_MODES=[MODE,'IPY16','PLCal13','Erickson12','WorldDay16','Mathews11','Lyons13','ACES10','MSWinds23','MSWinds22','A16flTest10','Hedden10'];	
			IPY_FILES_WC=['*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt1.h5','*.dt1.h5','*.dt0.h5','*.dt0.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_AC.txt')
		elif CODE=='PL1':
			# plasma line channel 1 setup
			IPY_COMPAT_MODES=[MODE,'IPY16','PLCal13','Erickson12','WorldDay16','Mathews11','Lyons13','ACES10'];	
			IPY_FILES_WC=['*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL1.txt')
		elif CODE=='PL2':
			# plasma line channel 2 setup
			IPY_COMPAT_MODES=[MODE,'IPY16','PLCal13','Erickson12','WorldDay16','Mathews11','Lyons13','ACES10'];	
			IPY_FILES_WC=['*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL2.txt')


	# setup for IPY04
	if MODE=='IPY04':
		IPY_COMPAT_MODES=[MODE,'Lyons02','Lyons03','WorldDay03','WorldDay04','Watkins02','Watkins03','LTCS01','Hysell01','Erickson01','Erickson02','PLCal03','IPY05'];	
		if CODE=='LP':
			# Long pulse setup
			IPY_FILES_WC=['*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5','*.dt3.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_LP.txt')
		elif CODE=='AC':
			# Alt code setup
			IPY_FILES_WC=['*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5','*.dt0.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_AC.txt')
		elif CODE=='PL1':
			# plasma line channel 1 setup
			IPY_FILES_WC=['*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5','*.dt1.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL1.txt')
		elif CODE=='PL2':
			# plasma line channel 2 setup
			IPY_FILES_WC=['*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5','*.dt2.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL2.txt')


	# setup for IPY01
	if MODE=='IPY01':
		IPY_COMPAT_MODES=[MODE,'IPY02','ipy01','Lyons01','WorldDay02','Thayer01','Watkins01','Themis01','Joule2pl05'];	
		if CODE=='LP':
			# Long pulse setup
			IPY_FILES_WC=['*.Dt3.h5','*.Dt3.h5','*.Dt1.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_LP.txt')
		elif CODE=='AC':
			# Alt code setup
			IPY_FILES_WC=['*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_AC.txt')
		elif CODE=='PL1':
			# plasma line channel 1 setup
			IPY_FILES_WC=['*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL1.txt')
		elif CODE=='PL2':
			# plasma line channel 2 setup
			IPY_FILES_WC=['*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL2.txt')

	# setup for IPY02
	if MODE=='IPY02':
		IPY_COMPAT_MODES=[MODE,'Lyons01','WorldDay02','Thayer01','Watkins01','Themis01'];	
		if CODE=='LP':
			# Long pulse setup
			IPY_COMPAT_MODES=[MODE];	
			IPY_FILES_WC=['*.Dt3.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5','*.Dt3.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_LP.txt')
		elif CODE=='AC':
			# Alt code setup
			IPY_COMPAT_MODES=[MODE]; 
			IPY_FILES_WC=['*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5','*.Dt0.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_AC.txt')
		elif CODE=='PL1':
			# plasma line channel 1 setup
			IPY_FILES_WC=['*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5','*.Dt1.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL1.txt')
		elif CODE=='PL2':
			# plasma line channel 2 setup
			IPY_FILES_WC=['*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5','*.Dt2.h5']; 
			OUTFILE=os.path.join(FITTER_PATH,MODE+'/'+MODE+'-filelist_PL2.txt')

	
	dirs=glob.glob(os.path.join(FILE_PATH,DIR_WC)) 
	Ndirs=len(dirs)
	dirs.sort()

	ALL_FILES=[]
	for aaa in range(Ndirs):
		
		# get the setup file
		exp_file = glob.glob(os.path.join(dirs[aaa],'Setup/*.exp'))
		if len(exp_file)==1:
			exp_file=exp_file[0]
			tmp1,tmp2,exp_file=exp_file.partition(os.path.join(dirs[aaa],'Setup/'))
			exp_file,tmp1,tmp2=exp_file.partition('.exp')
			
			if IPY_COMPAT_MODES.__contains__(exp_file): # check if this is an IPY compatible mode
				print 'Doing dir ' + dirs[aaa] + ', EXP: ' + exp_file
				
				dhead,dtail=os.path.split(dirs[aaa])
				
				I=IPY_COMPAT_MODES.index(exp_file)

				"""
				files=glob.glob(os.path.join(FILE_PATH,dtail,IPY_FILES_WC[I]))
				files.sort()
				"""
				
				ALL_FILES.extend([os.path.join(FILE_PATH,dtail,IPY_FILES_WC[I])])

				
	FILE = open(OUTFILE,"w")
	for aa in range(len(ALL_FILES)):
		FILE.write(ALL_FILES[aa]+'\n')
	FILE.close()
	
	