#!/opt/madrigal2/bin/python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys, getopt
import os.path
import tables
import glob
import shutil
import copy


import os
sondre_tools_path = os.environ['AMISR_FITTER_PATH'].split('AMISR_fitter_py')[0]
sondre_tools_path = os.path.join(sondre_tools_path,'SondreTools/python')
sys.path.append(sondre_tools_path)

#sys.path.append('/Users/mnicolls/Documents/Work/ISfit/SondreTools/python')

import madrigal_sondre

##############################

def usage():
	print "usage: ", sys.argv[0]
	print "\t EXPERIMENT: experiment to process (REQUIRED INPUT) - file Madrigal.ini must exist"

	sys.exit(2)

def main():
	
	try:
		expName=sys.argv[1]
	except:
		usage()
	try:
		upload=sys.argv[2]
		if upload=='upload':
			upload=1
		else:
			usage()
	except:
		upload=0
	madName = os.path.join(expName,'Madrigal.ini')
	if os.path.exists(madName):
		print "Processing experiment ", madName
	else:
		raise IOError, madName + " does not exist"
		
	dpath=os.path.join(expName,'Madrigal')
	if not os.path.exists(dpath):
		try:
			os.mkdir(dpath)
		except:
			raise IOError, "Unable to make dir " + dpath

	madBatch = madrigal_sondre.BatchExperiment()
	if upload:
		print "Uploading to Madrigal"
		madBatch.uploadExperiment(madName)
	else:
		madBatch.createNewExperimentFromIni(madName)

	print "I'm done, Mary."

if __name__ == '__main__':

	main()	
