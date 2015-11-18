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
import matplotlib.axes3d

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')

import vvels
import plot_utils

#fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20061212/20061212_bc_2min_Ne.h5'
##fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20061212/20061212_bc_30sec_Ne.h5'
#opath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20061212/movie_bc_2min_Ne/'

fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/20070611.001_bc_1min_Ne.h5'
opath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/movie_bc_1min_3d/'

#fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/20070611.001_bc_30sec_Ne.h5'
#opath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/movie_bc_30sec_3d/'

#fname='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/20070611.001_bc_1min_Ne.h5'
#opath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/runfiles/Sporadic02/20070611.001/movie_bc_1min_3d/'

h5file=tables.openFile(fname)
output={}
for array in h5file.listNodes('/',classname = 'Array'):
	output[array.name]=array.read()	
for group in h5file.walkGroups("/"):
	output[group._v_pathname]={}
	for array in h5file.listNodes(group, classname = 'Array'):						
		output[group._v_pathname][array.name]=array.read()		
h5file.close()

Beamcodes=output['/']['BeamCodes']

az=Beamcodes[:,1]*math.pi/180.0
el=Beamcodes[:,2]*math.pi/180.0
Nbeams=az.size

kbeams=scipy.array([scipy.cos(el)*scipy.cos(az),scipy.cos(el)*scipy.sin(az),scipy.sin(el)],'Float64') # unit vector in k direction (geographic)
kn=scipy.sort(kbeams[0,:])
Ibeam=[]
for rr in range(Nbeams):
	Ibeam.append(int(scipy.where(kbeams[0,:]==kn[rr])[0]))


dne=scipy.real(output['/NeFromPower']['Ne_NoTr'])
refl=dne*1e-28/2.0
snr=output['/NeFromPower']['SNR'] 
alt=output['/NeFromPower']['Altitude']/1000.0
rng=scipy.squeeze(output['/NeFromPower']['Range'])/1000.0

clim=[-18.0,-16.5]
#refl[scipy.where(refl<=0.0)[0]]=10.0**clim[0]
#refl[scipy.where(scipy.isnan(refl))[0]]=10.0**clim[0]

time=output['/Time']['dtime']
unixtime=output['/Time']['UnixTime']

htmin=82.0
htmax=87.0

Ialt=[]
for aa in range(Nbeams):
	I=scipy.where((alt[aa,:]>=htmin) & (alt[aa,:]<=htmax))[0]
	Ialt.append(I)

figg=pylab.figure(1, figsize=(5,5), facecolor='w')
	
labsize=10
textsize=8
matplotlib.rcParams['xtick.labelsize']=textsize
	
#for Itm in range(1): 
for Itm in range(time.shape[0]):

	figg.clf()

	tmp1=datetime.datetime.utcfromtimestamp(unixtime[Itm,0])
	tmp2=datetime.datetime.utcfromtimestamp(unixtime[Itm,1])

	tt='%.2d:%.2d:%.2d-%.2d:%.2d:%.2d UT' % (tmp1.hour,tmp1.minute,tmp1.second,tmp2.hour,tmp2.minute,tmp2.second)

	ax=matplotlib.axes3d.Axes3D(figg)
	
	xl='log10 (Refl), %d-%d km, %s' % (htmin,htmax,tt)
#	xl='SNR (dB), %d-%d km, %s' % (htmin,htmax,tt)
	
	for aa in range(Nbeams):
		Ibm=Ibeam[aa]
		tmp=refl[Itm,Ibm,Ialt[Ibm]]
		tmp[scipy.where(tmp<0.0)[0]]=10.0**clim[0]
		tmp[scipy.where(scipy.isnan(tmp))[0]]=10.0**clim[0]
		colors=scipy.log10(tmp)
#		colors=10.0*scipy.log10(snr[Itm,Ibm,Ialt[Ibm]])
		area=scipy.linspace(250,100,len(Ialt[Ibm]))
		xs=rng[Ialt[Ibm]]*kbeams[0,Ibm]
		ys=rng[Ialt[Ibm]]*kbeams[1,Ibm]
		zs=rng[Ialt[Ibm]]*kbeams[2,Ibm]
		ax.scatter3D(xs,ys,zs,c=colors, s=area, vmin=clim[0], vmax=clim[1])

#	ax.set_xlim([-21.0,70.0])
	ax.set_zlim([81.5,88])
	
#	zlim=[80,90]
#	ax.set_zlim(zlim)
#	labels=ax
	ax.view_init(72,0)
#	ax.set_ylabel('E-W (km)', fontsize=labsize, horizontalalignment='center', rotation=90)
	ax.text(-185,0,'Zonal Distance (km)', fontsize=labsize, horizontalalignment='center', verticalalignment='bottom', rotation=0.0)
	ax.text(-250,-140,'Meridional Distance (km)', fontsize=labsize, horizontalalignment='center', verticalalignment='bottom', rotation=90.0)
	ax.text(-430,-150,'Altitude (km)', fontsize=labsize, horizontalalignment='center', verticalalignment='bottom', rotation=97.0)
#	ax.set_xlabel('N-S (km)', fontsize=labsize, horizontalalignment='center')
#	ax.set_zlabel('Alt. (km)', fontsize=labsize, horizontalalignment='center')
	ax.text(-160,0,xl, fontsize=labsize, horizontalalignment='center', verticalalignment='bottom')


#	ax.clim([10.0,11])
##	pylab.clim([0.0,1e12])
#	pylab.colorbar()

	pylab.show()

#	xxxxxxx
	
	"""
	return
	xxxx




	

	# polar plot
	r = (90.0-el)*math.pi/180
	theta = (-az+90)*math.pi/180
	#colors = r
	ax = pylab.subplot(111, polar=True,axisbg=axesBG)
	c = pylab.scatter(theta, r, c=colors, s=area)
	c.set_alpha(0.75)

	xxxx

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

"""	
	oname='ne_3d_%.4d.png' % (Itm)
	figg.savefig(os.path.join(opath,oname))

	pylab.show()

"""
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
"""
	
