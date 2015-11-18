import datetime
import os
import numpy
import scipy
import matplotlib.pyplot as plt
import tables
from scipy.optimize import leastsq
import scipy.io as sio


def get_BS_angle(az,el):
    az_bs = 26.0*scipy.pi/180.0
    el_bs = 55.0*scipy.pi/180.0
    k = numpy.array([[scipy.cos(el)*scipy.cos(az)],
                     [scipy.cos(el)*scipy.sin(az)],
                     [scipy.sin(el)]])
    
    tk = rotmat(k,3,az_bs)
    tk2 = rotmat(tk,2,scipy.pi/2.0-el_bs)
    
    alphaBS=90.0-scipy.arcsin(tk2[2])*180.0/scipy.pi
    
    return alphaBS

def rotmat(input, dir, angle):
    if dir == 1:
        rotmat = numpy.array([ [1,0,0],
                               [0, scipy.cos(angle), scipy.sin(angle)],
                               [0, -scipy.sin(angle), scipy.cos(angle)]])
    if dir == 2:
        rotmat = numpy.array([ [scipy.cos(angle), 0, -scipy.sin(angle)],
                               [0, 1, 0],
                               [scipy.sin(angle), 0, scipy.cos(angle)]])
    if dir == 3:
        rotmat = numpy.array([ [scipy.cos(angle), scipy.sin(angle), 0],
                               [-scipy.sin(angle), scipy.cos(angle), 0],
                               [0, 0, 1]])
    
    return scipy.dot(rotmat,input)
    
if __name__ == '__main__':

    now = datetime.datetime.now()
    date = now.strftime("%m.%d.%Y")
    
    #change experiment month here
    exp = 'cal-201207'
    
    #
    dat = sio.loadmat('cal-201207-filelist_lp.txt_-0.01_2.02-12.12.2012.mat')
    x = dat['x'][0]
    
    BeamcodeMap = numpy.loadtxt('WorldDay64m-calibration-ksys-10.04.2012.txt')
    
    Nbeams = numpy.shape(BeamcodeMap)[0]
    
    fid = open('%s-calibration-scalar-%s.txt' %(exp,date),'w')
    fid2 = open('%s-calibration-ksys-%s.txt' %(exp,date),'w')
    
    for ibm in range(Nbeams):
        
        tbm = BeamcodeMap[ibm][:]
        az = BeamcodeMap[ibm][1]*scipy.pi/180.0
        el = BeamcodeMap[ibm][2]*scipy.pi/180.0
        
        kold = BeamcodeMap[ibm][3]
        
        alphaBS = get_BS_angle(az,el)
        

        ksys = (x[0]*alphaBS[0]+x[1]) * 1e-21
        print ksys,kold
        ksysCorr = ksys/kold
        
        print tbm[0],tbm[1],tbm[2],ksys,ksysCorr
        fid.write('%d %2.2f %2.2f %2.2e %3.5f\n' %(tbm[0],tbm[1],tbm[2],ksys,ksysCorr))
        fid2.write('%d %2.2f %2.2f %2.2e\n'%(tbm[0],tbm[1],tbm[2],ksys))
        
    
    fid.close()
    fid2.close()
