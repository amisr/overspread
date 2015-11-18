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

FITTER_PATH='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/'
GRAT_PATH='dat/grating_lobes.txt'
BMCODE_PATH='dat/beamcodes.txt'

fgrat=open(os.path.join(FITTER_PATH,GRAT_PATH)) 
grat_lobes=scipy.io.read_array(fgrat)

fbm=open(os.path.join(FITTER_PATH,BMCODE_PATH)) 
bmcodes=scipy.io.read_array(fbm)

az=bmcodes[:,1]
el=bmcodes[:,2]

Nbeams=az.size

labsize=12
textsize=12

axesBG  = '#f6f6f6'
figg=pylab.figure(1, figsize=(5,5), facecolor='w')

# polar plot
r = (90.0-el)*math.pi/180
theta = (-az+90)*math.pi/180
area = 15
colors = r
ax = pylab.subplot(111, polar=True,axisbg=axesBG)
c = pylab.scatter(theta, r, c=colors, s=area)
c.set_alpha(0.75)

area = 10
pylab.scatter((grat_lobes[:,0]+2*22.5+15)*math.pi/180.,(90-grat_lobes[:,1])*math.pi/180., s=area)


ring_angles = [ x * (math.pi/180.) for x in range(0,70,10)]
ring_labels = [ str(x) for x in range(30,100,10)]
ring_labels.reverse()
pylab.rgrids (ring_angles,ring_labels, angle=-90+22.5,fontsize=textsize)
pylab.thetagrids( range(0,360,45), ('E', 'NE', 'N','NW', 'W', 'SW', 'S', 'SE'), fontsize=textsize )


pylab.show()
