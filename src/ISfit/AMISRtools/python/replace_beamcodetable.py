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



##############################

if __name__ == '__main__':

	FILE_PATH='/Volumes/AMISR_008/Data AMISR Poker/'
	DIR_WC='20080912*'
	FILE_WC='*.dt*.h5'
	caldate='2008-11-05'
	version='/opt/amisr/share/phasetables/pokerflat/20070930-Panels128/bcotable007.txt'
	newbmtable='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/dat/bcotable7.txt'

	raw_input('Doing all files in directories' + FILE_PATH + ' with wild cards ' + DIR_WC + ' and files ' + FILE_WC + '. ok? hit enter')	
	
	fid=open(newbmtable)
	bmtable=scipy.io.read_array(fid)
	fid.close()

	print bmtable
	raw_input('OK? hit enter to go')

	dirs=glob.glob(os.path.join(FILE_PATH,DIR_WC)) 
	Ndirs=len(dirs)
	dirs.sort()
	
	for aaa in range(Ndirs):
		
		print 'Doing dir ' + dirs[aaa]
		files=glob.glob(os.path.join(dirs[aaa],FILE_WC))
		Nfiles=len(files)
	
		for bbb in range(Nfiles):
			print '\t File: ' + files[bbb]

			h5file=tables.openFile(files[bbb],"r+")
			try:			
				setup=h5file.getNode('/Setup')
				h5file.moveNode('/Setup',name='BeamcodeMap',newname='BeamcodeMap2',overwrite=True)
				bmnode=h5file.getNode('/Setup/BeamcodeMap2')
				bmnode2=h5file.createArray(setup, 'BeamcodeMap', bmtable, "Static array")
				
				attrs=bmnode.attrs._g_listAttr()
				for aa in range(len(attrs)):
					bmnode2.setAttr(attrs[aa],bmnode.getAttr(attrs[aa]))
				
				bmnode2.setAttr('CalibrationDate',caldate)
				bmnode2.setAttr('version',version)
				
				bmnode.remove()
				h5file.close()
				
			except tables.NoSuchNodeError:
				print '\t\t Beamcode map does not exist, doing nothing'
			
			h5file.close()			
	
	