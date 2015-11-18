import datetime
import os
import numpy
import scipy
import matplotlib.pyplot as plt
import tables
from scipy.optimize import leastsq
import scipy.io as sio


def get_BS_angle(az,el):
    az_bs = 15.0*scipy.pi/180.0
    el_bs = 74.0*scipy.pi/180.0
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
    exp = 'cal-201303'
    
    #
    dat = sio.loadmat('cal-201303-filelist_lp.txt_2.03_9.43-03.21.2013.mat')
    x = dat['x'][0]
    
    read_from_file = True
    
    
    if read_from_file == True:
        # From processed file
        #fname = '/Volumes/ISR_DATA_02/processed_data/PFISR/2010/04/Lyons30/20100406.001/20100406.001_ac_2min.h5'
        
        # From raw file
        filelist = []
        curdir = os.getcwd().split('/')
        n = len(curdir)
        filepath = '/Volumes/AMISR_017/Data AMISR Poker/' + curdir[n-1] + '/'
        dirList = os.listdir(filepath)
        for fname in dirList:
            if fname.endswith('.dt0.h5'):
                filelist.append(fname)
                
        h5file=tables.openFile(filepath + filelist[0])
        output={}
        for array in h5file.listNodes('/',classname = 'Array'):
            output[array.name]=array.read() 
        for group in h5file.walkGroups("/"):
            output[group._v_pathname]={}
            for array in h5file.listNodes(group, classname = 'Array'):                      
                output[group._v_pathname][array.name]=array.read()      
        h5file.close()
        
        try:
            BeamcodeMap = numpy.array(output['BeamCodes'])
        except:
            BeamcodeMap = numpy.array(output['/Setup']['BeamcodeMap'])
                  
    else:
        BeamcodeMap = numpy.loadtxt('BeamCodeMap.txt')
    
    Nbeams = numpy.shape(BeamcodeMap)[0]
    
    fid = open('%s-calibration-scalar-%s.txt' %(exp,date),'w')
    fid2 = open('%s-calibration-ksys-%s.txt' %(exp,date),'w')
    
    for ibm in range(Nbeams):
        
        tbm = BeamcodeMap[ibm][:]
        az = BeamcodeMap[ibm][1]*scipy.pi/180.0
        el = BeamcodeMap[ibm][2]*scipy.pi/180.0
        
        kold = BeamcodeMap[ibm][3]
        
        alphaBS = get_BS_angle(az,el)
        

        ksys = x[0]*scipy.power(scipy.cos(alphaBS[0]*scipy.pi/180.0+x[2]),x[1])*1e-19
        ksysCorr = ksys/kold
        
        print tbm[0],tbm[1],tbm[2],ksys,ksysCorr
        fid.write('%d %2.2f %2.2f %2.2e %3.5f\n' %(tbm[0],tbm[1],tbm[2],ksys,ksysCorr))
        fid2.write('%d %2.2f %2.2f %2.2e\n'%(tbm[0],tbm[1],tbm[2],ksys))
        
    
    fid.close()
    fid2.close()
