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

	DISK_PATH='/Volumes/ISR_Data-1/'
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')
	DIR_WC='2007*'
	FILE_WC='*.[dD]t*.h5'
	MODE='IPY04'
	EXP_FILE='Setup/'+MODE+'.exp'
	#OUTPATH_DIRS='/Volumes/PFISR IPY/IPY/dirs'
	OUTPATH_DIRS=os.path.join(DISK_PATH,'IPY',MODE,'dirs')
	#OUTPATH_FILES='/Volumes/PFISR IPY/IPY/dat_files'
	OUTPATH_FILES=os.path.join(DISK_PATH,'IPY',MODE,'dat_files')

	dirs=glob.glob(os.path.join(FILE_PATH,DIR_WC)) 
	Ndirs=len(dirs)
	dirs.sort()
	
	for aaa in range(Ndirs):

		if os.path.exists(os.path.join(dirs[aaa],EXP_FILE)):
			
			dhead,dtail=os.path.split(dirs[aaa])
										
			if not os.path.exists(os.path.join(OUTPATH_DIRS,dtail)):
				print 'Doing dir ' + dirs[aaa]
			
				shutil.copytree(dirs[aaa],os.path.join(OUTPATH_DIRS,dtail))
		
				files=glob.glob(os.path.join(OUTPATH_DIRS,dtail,FILE_WC))
				Nfiles=len(files)
				
				for bbb in range(Nfiles):
					fhead,ftail=os.path.split(files[bbb])
					shutil.move(files[bbb],os.path.join(OUTPATH_FILES,ftail))
	
			else:
				print 'Directory %s already exists, skipping' % (dtail)
	
	
	

							
	
	
	