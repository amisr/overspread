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

	DISK_PATH='/Volumes/Sondre_002/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'')  # where the experiments are
	DIR_WC='200909*' # wild card to select expss from
	OUTFILE='/Volumes/Sondre_001/ExperimentLists/ExpList_Sondre002_'+DIR_WC[:-1]+'.csv'
		
	dirs=glob.glob(os.path.join(FILE_PATH,DIR_WC)) 
	Ndirs=len(dirs)
	dirs.sort()

	ALLEXPS=[]
	for aaa in range(Ndirs):
		
		# get the setup file
		exp_file = glob.glob(os.path.join(dirs[aaa],'Setup/*.exp'))
		if len(exp_file)==1:		
			exp_file=exp_file[0]
			tmp1,tmp2,exp_file=exp_file.partition(os.path.join(dirs[aaa],'Setup/'))
			exp_file,tmp1,tmp2=exp_file.partition('.exp')
							
			dhead,dtail=os.path.split(dirs[aaa])
			
			strout="'" + dtail + "'" + ', ' + "'" + exp_file + "'"
			
			print strout
			
			ALLEXPS.append(strout)
				
	FILE = open(OUTFILE,"w")
	for aa in range(len(ALLEXPS)):
		FILE.write(ALLEXPS[aa]+'\n')
	FILE.close()
	
	