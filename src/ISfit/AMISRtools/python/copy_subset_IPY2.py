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

	DISK_PATH='/Volumes/AMISR_006/'
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')
	DIR_WC='200*'
	FILE_WC='*.[dD]t*.h5'
	MODES=['IPY01','IPY02','ipy01','IPY03','IPY04','IPY05','Joule2pl05','Lyons01','Lyons02','Lyons03','WorldDay02','WorldDay03','WorldDay04','WorldDayTEC05','Thayer01','Watkins01','Themis01','Erickson01','Erickson02','Erickson12','LTCS01','PLCal01','PLCal02','PLCal03','Watkins01','Watkins02','Hysell01']
	OUTPATH='/Volumes/My Book/'

	dirs=glob.glob(os.path.join(FILE_PATH,DIR_WC)) 
	Ndirs=len(dirs)
	dirs.sort()
	
	for aaa in range(Ndirs):

		# get the setup file
		exp_file = glob.glob(os.path.join(dirs[aaa],'Setup/*.exp'))
		if len(exp_file)==1:
			exp_file=exp_file[0]
			tmp1,tmp2,exp_file=exp_file.partition(os.path.join(dirs[aaa],'Setup/'))
			exp_file,tmp1,tmp2=exp_file.partition('.exp')
		
			if MODES.__contains__(exp_file): # check if this is an IPY compatible mode
				dhead,dtail=os.path.split(dirs[aaa])
				if not os.path.exists(os.path.join(OUTPATH,dtail)):
					print 'Doing dir ' + dirs[aaa] + ', EXP: ' + exp_file
					shutil.copytree(dirs[aaa],os.path.join(OUTPATH,dtail))
			
"""
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
"""
	
	
	

							
	
	
	