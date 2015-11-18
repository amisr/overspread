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
    
    
def func(x,a,b,c):
    return a*scipy.power(scipy.cos(x*scipy.pi/180.0+c), b)
    
def residual(p, y, x):
    a,b,c = p
    return y - func(x,a,b,c)

    
if __name__ == '__main__':
    
    now = datetime.datetime.now()
    date = now.strftime("%m.%d.%Y")
    
    #change experiment month here
    exp = 'cal-201205'

    GL = numpy.loadtxt('GratingLimits.txt')
    
    BM = numpy.loadtxt('bcotable3.txt')
    
    ###Change filelist here###
    #fname = ['filelist_ac.txt']
    fname = ['filelist_lp.txt']
    
    #get filelist
    FILES = []
    
    for files in fname:
        f = open(files)
        FPATH = f.readline().strip()
        FILES = f.readlines()
        FILES = [line.strip() for line in FILES]
        FILES = [FPATH + line for line in FILES]
    
    thbs = scipy.linspace(0,40,100)
    A = 0.98395
    B = 3.8781
    
    oldksys = A * scipy.power(scipy.cos(thbs*scipy.pi/180.0),B)
    
    h5file = tables.openFile('bm_orig.h5', mode = 'r')
    
    output={}
    for array in h5file.listNodes('/',classname = 'Array'):
        output[array.name]=array.read() 
    for group in h5file.walkGroups("/"):
        output[group._v_pathname]={}
        for array in h5file.listNodes(group, classname = 'Array'):                      
            output[group._v_pathname][array.name]=array.read()      
    h5file.close() 
    
    BeamcodeMap = output['/']['BeamcodeMap']
    BeamcodeMap = numpy.array(BeamcodeMap, dtype='float')
    
    ksysbco= BeamcodeMap[:,0]
    ksystab= BeamcodeMap[:,3]*1e19
    
    fig = plt.figure()
    
    
    alphaBS = scipy.zeros(len(FILES))
    aGL = scipy.zeros(len(FILES))
    KSYScorr = scipy.zeros(len(FILES))
    KSYS = scipy.zeros(len(FILES))
    eKSYS = scipy.zeros(len(FILES))
    
    for aa in range(len(FILES)):
        f = open(FILES[aa])
        TLINE1 = numpy.array([value for value in f.readline().split()],dtype='float')
        TLINE2 = numpy.array([value for value in f.readline().split()],dtype='float')
        
        az = TLINE1[1]*scipy.pi/180.0
        el = TLINE1[2]*scipy.pi/180.0
        
        alphaBS[aa] = get_BS_angle(az,el)
        
        a = numpy.min(scipy.absolute(az*180.0/scipy.pi-GL[:,0]))
        I = scipy.absolute(az*180.0/scipy.pi-GL[:,0]).argmin()

        aGL[aa] = GL[I,2]-alphaBS[aa]
        
        KSYScorr[aa] = TLINE2[0]
        
        I = scipy.where(TLINE1[0] == ksysbco)
        
        if not I:
            xxx
            KSYS[aa] = KSYScorr[aa]*A*scipy.power(cos(alphaBS[aa]*scipy.pi/180.0), B)
        else:
            KSYS[aa] = ksystab[I]*KSYScorr[aa]
        
        print 'Beam: %d, %f, %f, %f, %f\n' % (TLINE1[0], az*180/scipy.pi, el*180/scipy.pi,alphaBS[aa], KSYS[aa])
        
        eKSYS[aa] = TLINE2[1]*KSYS[aa]

        plt.plot(alphaBS[aa],KSYS[aa],'k.', hold=True)
        plt.plot([alphaBS[aa], alphaBS[aa]], [KSYS[aa]-eKSYS[aa], KSYS[aa]+eKSYS[aa]], 'b', hold=True)
        
    
    for i in range(len(ksysbco)):
        I = scipy.where(BM[:,0] == ksysbco[i])
        az = scipy.float64(BM[I,1]*scipy.pi/180.0)
        el = scipy.float64(BM[I,2]*scipy.pi/180.0)

        if el>0:
            tbs = get_BS_angle(az,el)
            
            tksys = ksystab[i]
            plt.plot(tbs,tksys,'rx',hold=True)
    
    plt.plot(thbs,oldksys,'r-', hold=True)
    
    plt.xlabel('Angle off Boresight')
    plt.ylabel('Ksys')
    
    sc = scipy.ones(len(alphaBS))

    
    num = numpy.unique(alphaBS)
    
    y = scipy.zeros(len(num))
    for x in range(len(num)):
        I = scipy.where(num[x] == alphaBS)
        y[x] = scipy.median(KSYS[I])

    #initial guess
    [a,b,c],flag = leastsq(residual,[0,0,0],args=(y,num))
    
    yn = a*scipy.power(scipy.cos(thbs*scipy.pi/180.0+c), b)
    
    plt.plot(thbs,yn,'k')
    plt.plot(thbs,yn*1.1, '--k')
    plt.plot(thbs,yn*.9,'--k')
    
    #load AC
    try:
        dat = sio.loadmat('cal-201205-filelist_ac.txt_2.60_6.84-09.27.2012.mat')
        acx = dat['x'][0]
        acCal = acx[0]*scipy.power(scipy.cos(thbs*scipy.pi/180.0+acx[2]),acx[1])
        plt.plot(thbs,acCal,'g')
    except:
        pass
        
    #load previous month
    try:
        dat = sio.loadmat('cal-201204-filelist_lp.txt_2.48_6.48-09.24.2012.mat')
        acx = dat['x'][0]
        acCal = acx[0]*scipy.power(scipy.cos(thbs*scipy.pi/180.0+acx[2]),acx[1])
        plt.plot(thbs,acCal,'m')
    except:
        pass

    plt.title('x=%2.3f %2.3f %2.3f' % (a,b,c))
    plt.xlim((numpy.min(thbs)-1, numpy.max(thbs)-1))
    oname = '%s-%s_%2.2f_%2.2f-%s' % (exp,fname[0], a,b,date)

    
    sio.savemat(oname +'.mat',{'x':[a,b,c]},oned_as='row')
    
    plt.savefig(oname + '.png', dpi=200)
    plt.show()
    
        