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

	DISK_PATH='/Volumes/AMISR_005/' # volume where data is
	FILE_PATH=os.path.join(DISK_PATH,'Data AMISR Poker')  # where the experiments are
	DIR_WC='200708*' # wild card to select exps from
	
#	NAME='Faraday'; MODE=['Faraday01']; CODE='LP'; WC='*.Dt3.h5';
#	NAME='Sporadic'; MODE=['Sporadic01','Sporadic02']; CODE='LP'; WC='*.Dt2.h5';
#	NAME='ipy01'; MODE=['ipy01']; CODE='LP'; WC='*.Dt3.h5'; 
#	NAME='ipy01'; MODE=['ipy01']; CODE='AC'; WC='*.Dt0.h5';
#	NAME='Hplus'; MODE=['Hplus01','Hplus02','Hplus03']; CODE='LP'; WC='*.Dt0.h5'; 
#	NAME='Bhatt'; MODE=['Bhatt01']; CODE='LP'; WC='*.Dt0.h5';
#	NAME='Watkins'; MODE=['Watkins01']; CODE='LP'; WC='*.Dt3.h5';
#	NAME='Watkins'; MODE=['Watkins01']; CODE='AC'; WC='*.Dt0.h5';
#	NAME='Themis'; MODE=['Themis01']; CODE='LP'; WC='*.Dt3.h5';
#	NAME='Themis'; MODE=['Themis01']; CODE='AC'; WC='*.Dt0.h5';
	NAME='IPY01'; MODE=['IPY01']; CODE='LP'; WC='*.Dt3.h5'; 
	NAME='IPY01'; MODE=['IPY01']; CODE='AC'; WC='*.Dt0.h5'; 
#	NAME='IPY02'; MODE=['IPY02']; CODE='LP'; WC='*.Dt3.h5'; 
#	NAME='IPY02'; MODE=['IPY02']; CODE='AC'; WC='*.Dt0.h5'; 
#	NAME='IPY04'; MODE=['IPY04']; CODE='LP'; WC='*.dt3.h5'; 
#	NAME='IPY04'; MODE=['IPY04']; CODE='AC'; WC='*.dt0.h5'; 
#	NAME='IPY05'; MODE=['IPY05','IPY15']; CODE='LP'; WC='*.dt3.h5'; 
#	NAME='IPY05'; MODE=['IPY05','IPY15']; CODE='AC'; WC='*.dt0.h5'; 
	OUTDIR=os.path.join('/Volumes/AMISR_004/processed_data/2007',NAME)

	
	OUTFILE=os.path.join(OUTDIR,NAME+'-filelist_'+CODE+'.txt')

	
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
			
			if MODE.__contains__(exp_file): # check if this is an IPY compatible mode
				print 'Doing dir ' + dirs[aaa] + ', EXP: ' + exp_file
				
				dhead,dtail=os.path.split(dirs[aaa])
				
#				files=glob.glob(os.path.join(dtail,WC))
#				files.sort()
				
				ALL_FILES.append(os.path.join(dirs[aaa],WC))

				
	FILE = open(OUTFILE,"w")
	for aa in range(len(ALL_FILES)):
		FILE.write(ALL_FILES[aa]+'\n')
	FILE.close()
	
	