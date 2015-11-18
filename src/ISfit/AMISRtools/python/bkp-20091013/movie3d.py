#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import os
import sys
import math
import scipy
import ctypes 
import scipy.fftpack
import scipy.interpolate
import scipy.optimize
import scipy.io

import struct
import datetime
import array
import glob
import tables
import pylab
import matplotlib

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import vvels
import plot_utils

#fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20061212/20061212_bc_2min_Ne.h5'
##fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20061212/20061212_bc_30sec_Ne.h5'
#opath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20061212/movie_bc_2min_Ne/'

fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/20070611.001_bc_2min_Ne.h5'
opath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/movie_bc_2min/'

h5file=tables.openFile(fname)
output={}
for array in h5file.listNodes('/',classname = 'Array'):
	output[array.name]=array.read()	
for group in h5file.walkGroups("/"):
	output[group._v_pathname]={}
	for array in h5file.listNodes(group, classname = 'Array'):						
		output[group._v_pathname][array.name]=array.read()		
h5file.close()

az=output['/']['BeamCodes'][:,1]
el=output['/']['BeamCodes'][:,2]
Nbeams=az.size

dne=output['/NeFromPower']['Ne_NoTr']
alt=output['/NeFromPower']['Altitude']/1000.0

time=output['/Time']['dtime']
unixtime=output['/Time']['UnixTime']

htmin=82.0
htmax=87.0

Ialt=[]
for aa in range(Nbeams):
	I=scipy.where((alt[aa,:]>=htmin) & (alt[aa,:]<=htmax))[0]
	Ialt.append(I)

figg=pylab.figure(1, figsize=(5,5), facecolor='w')
	
for Itm in range(time.shape[0]):

	figg.clf()
	colors=scipy.zeros(Nbeams,dtype='Float64')
	for aa in range(Nbeams):
		tne=scipy.mean(dne[Itm,aa,Ialt[aa]])
		colors[aa]=scipy.log10(tne)
#		colors[aa]=tne

	tmp1=datetime.datetime.utcfromtimestamp(unixtime[Itm,0])
	tmp2=datetime.datetime.utcfromtimestamp(unixtime[Itm,1])

	tt='%.2d:%.2d:%.2d-%.2d:%.2d:%.2d UT' % (tmp1.hour,tmp1.minute,tmp1.second,tmp2.hour,tmp2.minute,tmp2.second)
	print colors[0]

	labsize=12
	textsize=12

	axesBG  = '#f6f6f6'

	# polar plot
	r = (90.0-el)*math.pi/180
	theta = (-az+90)*math.pi/180
	area = 200
	#colors = r
	ax = pylab.subplot(111, polar=True,axisbg=axesBG)
	c = pylab.scatter(theta, r, c=colors, s=area)
	c.set_alpha(0.75)

	ring_angles = [ x * (math.pi/180.) for x in range(0,50,10)]
	ring_labels = [ str(x) for x in range(50,100,10)]
	ring_labels.reverse()
	pylab.rgrids (ring_angles,ring_labels, angle=-90+22.5,fontsize=textsize)
	pylab.thetagrids( range(0,360,45), ('E', 'NE', 'N','NW', 'W', 'SW', 'S', 'SE'), fontsize=textsize )

	xl='log10 (Ne), %d-%d km' % (htmin,htmax)
	ax=pylab.gca()
	ax.set_title(xl, fontsize=labsize, horizontalalignment='center')
	ax.text(-math.pi/2,55*math.pi/180,tt, fontsize=labsize, horizontalalignment='center')
	pylab.clim([10.0,11])
#	pylab.clim([0.0,1e12])
	pylab.colorbar()
	
	oname='ne_az_%.4d.png' % (Itm)
	figg.savefig(os.path.join(opath,oname))

	pylab.show()
	
command = ('mencoder',
           'mf://*.png',
           '-mf',
           'type=png:w=800:h=600:fps=25',
           '-ovc',
           'lavc',
           '-lavcopts',
           'vcodec=mpeg4',
           '-oac',
           'copy',
           '-o',
           'output.avi')

os.spawnvp(os.P_WAIT, 'mencoder', command)
	
