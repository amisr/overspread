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
import copy

DATAPATH='/data/ISR_Data/processed_data/2009'
templateDefault = """[DEFAULT]\nDataPath:   %s\nExperimentType:   %s\nExperimentName:   %s\n\n"""
templateExperiment = """[Experiment]\ntitle:   %(ExperimentType)s - %(ExperimentTitle1)s\ninstrument:  61\nlogFile:  %(DataPath)s/%(ExperimentType)s/%(ExperimentName)s/MadrigalLog.txt\nexpID: %(ExperimentName2)s\npi: Craig Heinselman\nmodexp: %(ExperimentTitle2)s\ncmodexp: %(ExperimentDescription)s\n\n"""
templateFile = """\n[File%(filenum)s]\nhdf5Filename:   %(DataPath)s/%(ExperimentType)s/%(ExperimentName)s/%(h5file)s\nkindat:   %(kindat)s\ncreateRecPlots:   True\ntype: %(type)s\nckindat: %(ckindat)s\n"""
templateImg = """imageTitle%(imgnum)s:   %(imgtitle)s\nimage%(imgnum)s:   %(DataPath)s/%(ExperimentType)s/%(ExperimentName)s/%(imgname)s\n"""

##############################

if __name__ == '__main__':

	DISK_PATH='/Volumes/AMISR_004/processed_data/2009' # volume where data is
	EXP_TYPE='Lyons13'
	EXP_NAME_WC='200903*.*[0-9]' # experiment wild card
	OverwriteExisting = 1	
	Plots2do_Fits = ['*_Ne.png','*_nuin.png', '*_OpFrac.png', '*_Te.png', '*_Ti.png', '*_Tr.png', '*_Vlos.png']#,'*geoplot.png']
	PlotsTitles_Fits = ['Electron Density','Ion-Neutral Collision Frequency','O+ Ion Fraction','Electron Temperature','Ion Temperature','Te/Ti Ratio','LOS Velocity']#,'Geometry Plot']
	Plots2do_Pwr=['*_NePower_NoTr.png', '*_NePower_TeTimod.png']#,'*geoplot.png']
	PlotsTitles_Pwr=['Electron density - No Te/Ti Correction','Electron density - Te/Ti from Model']#,'Geometry Plot']
	Plots2do1 = ['*geoplot.png']; PlotsTitles1 = ['Geometry Plot']
#	Plots2do1 = []; PlotsTitles1 = []
	ST=''
	DontDoDone=1
	
	FILE_PATH=os.path.join(DISK_PATH,EXP_TYPE)  # where the experiments are
	
	dirs=glob.glob(os.path.join(FILE_PATH,EXP_NAME_WC)); 
	if DontDoDone:
		dirsdone=glob.glob(os.path.join(FILE_PATH,'*.done')); dirs=list(set(dirs)-set(dirsdone))
	Ndirs=len(dirs)
	dirs.sort()
	
	expDescFile=os.path.join(FILE_PATH,EXP_TYPE+'ExperimentDescription.txt')
	if not os.path.exists(expDescFile):
		print '%s does not exist' % (expDescFile)
		sys.exit(-1)
	else:
		fid=open(expDescFile); EXP_TITLE1=fid.readline(); EXP_TITLE2=fid.readline(); EXP_DESC=fid.readline(); fid.close()
		if EXP_TITLE1[-1] == '\n': EXP_TITLE1=EXP_TITLE1[:-1]
		if EXP_TITLE2[-1] == '\n': EXP_TITLE2=EXP_TITLE2[:-1]
		if EXP_DESC[-1] == '\n': EXP_DESC=EXP_DESC[:-1]
	
	for aaa in range(Ndirs):
		
		expName=os.path.basename(dirs[aaa])
		madname=os.path.join(dirs[aaa],'Madrigal.ini')
		
		tmp=expName.split('.'); expName2 = tmp[0] + '.61.' + tmp[1]
		
		if (not os.path.exists(madname)) or OverwriteExisting:

			print 'doing %s' % (madname)

			madfile = open(madname, 'w')

			# write default and experiment section
			madfile.write(templateDefault % (DATAPATH,EXP_TYPE,expName))
			madfile.write(templateExperiment % {'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentTitle1': EXP_TITLE2, 'ExperimentName': expName, 'ExperimentName2': expName2, 'ExperimentTitle2': EXP_TITLE1, 'ExperimentDescription': EXP_DESC})
			
			files = list(set(glob.glob(os.path.join(dirs[aaa],'plots_*'))) - set(glob.glob(os.path.join(dirs[aaa],'*Lag0*'))))
			Nfiles=len(files)
			Nfiles2=0
			if os.path.exists(os.path.join(dirs[aaa],'derivedParams')):
				files2 = glob.glob(os.path.join(dirs[aaa],'derivedParams','*.h5'))
				Nfiles2=len(files2)
			
			fileNum=1
			for bbb in range(Nfiles):
				tmp1,tmp2,procName=files[bbb].partition('plots')
				h5file='%s%s%s.h5' % (ST,expName,procName)
				h5filecal='%s%s%s-cal.h5' % (ST,expName,procName)
				if os.path.exists(os.path.join(dirs[aaa],h5filecal)):
					h5file=h5filecal
				
				# uncorrected Ne
				if os.path.exists(os.path.join(dirs[aaa],h5file)):
					if procName.startswith('_lp'):
						tkindat=5961
						type='uncorrected_ne_only'
						ckindat='Long Pulse Uncorrected Ne'
						rngLims=[100.0,1000.0]
					elif procName.startswith('_ac'):
						tkindat=5962
						type='uncorrected_ne_only'
						ckindat='Alternating Code Uncorrected Ne'
						rngLims=[80.0,1000.0]
					elif procName.startswith('_bc'):
						tkindat=5963
						type='uncorrected_ne_only'
						ckindat='Barker Code Uncorrected Ne'
						rngLims=[50.0,1000.0]
					else:
						tkindat=0; ckindat=''
						print 'unknown kindat, %s' % (procName)
					madfile.write(templateFile % {'filenum': fileNum, 'h5file': h5file, 'kindat': tkindat, 'type': type, 'ckindat': ckindat, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})						
					madfile.write('lowerRange:   %.1f\nupperRange:   %.1f\n' % (rngLims[0],rngLims[1]))
						
					imgNum=1
					for ccc in range(len(Plots2do_Pwr)):
						imgs=glob.glob(os.path.join(files[bbb],Plots2do_Pwr[ccc]))
						if len(imgs)>0:
							imgs=os.path.basename(imgs[0])
							title = procName + ' ' + PlotsTitles_Pwr[ccc]
							madfile.write(templateImg % {'imgnum': imgNum, 'imgtitle': title, 'imgname': 'plots'+procName+'/'+imgs, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})
							imgNum=imgNum+1
					if fileNum==1:
						for ccc in range(len(Plots2do1)):
							imgs=glob.glob(os.path.join(files[bbb],Plots2do1[ccc]))
							if len(imgs)>0:
								imgs=os.path.basename(imgs[0])
								madfile.write(templateImg % {'imgnum': imgNum, 'imgtitle': PlotsTitles1[ccc], 'imgname': 'plots'+procName+'/'+imgs, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})
								imgNum=imgNum+1	
								
					fileNum += 1
								

				# Fit file
				if os.path.exists(os.path.join(dirs[aaa],h5file)) and (procName.find('Ne')==-1):
					if procName.startswith('_lp'):
						tkindat=5950
						type='standard'
						ckindat='Long Pulse'
					elif procName.startswith('_ac'):
						tkindat=5951
						type='standard'
						ckindat='Alternating Code'
					else:
						tkindat=0; ckindat=''
						print 'unknown kindat, %s' % (procName)
					madfile.write(templateFile % {'filenum': fileNum, 'h5file': h5file, 'kindat': tkindat, 'type': type, 'ckindat': ckindat, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})

					imgNum=1
					for ccc in range(len(Plots2do_Fits)):
						imgs=glob.glob(os.path.join(files[bbb],Plots2do_Fits[ccc]))
						if len(imgs)>0:
							imgs=os.path.basename(imgs[0])
							title = procName + ' ' + PlotsTitles_Fits[ccc]
							madfile.write(templateImg % {'imgnum': imgNum, 'imgtitle': title, 'imgname': 'plots'+procName+'/'+imgs, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})
							imgNum=imgNum+1
					if fileNum==1:
						for ccc in range(len(Plots2do1)):
							imgs=glob.glob(os.path.join(files[bbb],Plots2do1[ccc]))
							if len(imgs)>0:
								imgs=os.path.basename(imgs[0])
								madfile.write(templateImg % {'imgnum': imgNum, 'imgtitle': PlotsTitles1[ccc], 'imgname': 'plots'+procName+'/'+imgs, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})
								imgNum=imgNum+1						

					fileNum += 1

			for bbb in range(Nfiles2):
				h5file=files2[bbb]
				baseh5=os.path.basename(h5file)
				dbaseh5=baseh5[:-3]
				if baseh5.rfind('vels') != -1:
					tkindat=5960
					type='velocity'
					ckindat='Vector velocities'
					tt='Vector Velocities'
				else:
					tkindat=0; ckindat=''
					print 'unknown kindat'
					
				if tkindat>0:
					madfile.write(templateFile % {'filenum': fileNum, 'h5file': 'derivedParams/'+baseh5, 'kindat': tkindat, 'type': type, 'ckindat': ckindat, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})				
				
					imgNum=1
					imgName='derivedParams/' + dbaseh5 + '.png'
					if os.path.exists(os.path.join(dirs[aaa],imgName)):
						title=dbaseh5 + ' ' + tt
						madfile.write(templateImg % {'imgnum': imgNum, 'imgtitle': title, 'imgname': imgName, 'DataPath': DATAPATH, 'ExperimentType': EXP_TYPE, 'ExperimentName': expName})

					fileNum += 1
			
					
			madfile.close()
		else:
			print '%s exists already, skipping' % (madname)


	