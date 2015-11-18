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

	"""
	DISK_PATH='/Volumes/AMISR_005/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')  # where the experiments are
	DIR_WC='200*' # wild card to select exps from
	OUTFILE='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/EXPLIST_Amisr005.csv'
	"""

	"""
	DISK_PATH='/Volumes/AMISR_006/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')  # where the experiments are
	DIR_WC='200805*' # wild card to select expss from
	OUTFILE='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/EXPLIST_Amisr006.csv'
	"""

	"""
	DISK_PATH='/Volumes/AMISR_007/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')  # where the experiments are
	DIR_WC='200808*' # wild card to select expss from
	OUTFILE='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/EXPLIST_Amisr007.csv'
	
	"""
	DISK_PATH='/Volumes/AMISR_008/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')  # where the experiments are
	DIR_WC='200809*' # wild card to select expss from
	OUTFILE='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/EXPLIST_Amisr008.csv'
		
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
	
	