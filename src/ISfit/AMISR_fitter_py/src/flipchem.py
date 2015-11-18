#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import scipy
import ctypes

def chemion(ct_flipchem,YYYYDDD,ALT,LAT,LON,AP,F107,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE,UTSEC,LTHRS,SZAD):
    #
    #
    #     returns O+, O2+, NO+, N2+, N+, NO, and N(2D) densities
    #
    #
    #
    
    chemi=ct_flipchem.chemion_

    OXPLUS=ctypes.c_float(0.0); O2PLUS=ctypes.c_float(0.0); NOPLUS=ctypes.c_float(0.0)
    N2PLUS=ctypes.c_float(0.0); NPLUS=ctypes.c_float(0.0); NNO=ctypes.c_float(0.0); N2D=ctypes.c_float(0.0); 
    INEWT=ctypes.c_int(0)
    
    ap=(ctypes.c_float*7)()
    for i in range(7):
        ap[i]=AP[i]
    
    chemi(ctypes.byref(ctypes.c_int(0)),ctypes.byref(ctypes.c_int(YYYYDDD)),ctypes.byref(ctypes.c_float(ALT)),ctypes.byref(ctypes.c_float(LAT)),ctypes.byref(ctypes.c_float(LON)),
        ctypes.byref(ap),ctypes.byref(ctypes.c_float(F107)),ctypes.byref(ctypes.c_float(F107A)),ctypes.byref(ctypes.c_float(TE)),ctypes.byref(ctypes.c_float(TI)),ctypes.byref(ctypes.c_float(TN)),
        ctypes.byref(ctypes.c_float(OXN)),ctypes.byref(ctypes.c_float(O2N)),ctypes.byref(ctypes.c_float(N2N)),ctypes.byref(ctypes.c_float(HEN)),ctypes.byref(ctypes.c_float(-1.0)),
        ctypes.byref(ctypes.c_float(N4S)),ctypes.byref(ctypes.c_float(NE)),ctypes.byref(ctypes.c_float(-1.0)),ctypes.byref(ctypes.c_float(UTSEC)),ctypes.byref(ctypes.c_float(LTHRS)),
        ctypes.byref(ctypes.c_float(SZAD)),ctypes.byref(OXPLUS),ctypes.byref(O2PLUS),ctypes.byref(NOPLUS),ctypes.byref(N2PLUS),ctypes.byref(NPLUS),ctypes.byref(NNO),ctypes.byref(N2D),ctypes.byref(INEWT))
        
    return OXPLUS.value,O2PLUS.value,NOPLUS.value,N2PLUS.value,NPLUS.value,NNO.value,N2D.value,INEWT.value

def getltsza(ct_flipchem,YYYYDDD,UTSEC,LAT,LON):
    #
    #
    #
   
    func=ct_flipchem.getltsza_
    SAT=ctypes.c_float(0.0); SZA=ctypes.c_float(0.0); DEC=ctypes.c_float(0.0)
    
    func(ctypes.byref(ctypes.c_int(YYYYDDD)),ctypes.byref(ctypes.c_float(UTSEC)),ctypes.byref(ctypes.c_float(LAT*scipy.pi/180.0)),
        ctypes.byref(ctypes.c_float(LON)),ctypes.byref(SAT),ctypes.byref(SZA),ctypes.byref(DEC))

    return SAT.value,SZA.value*180.0/scipy.pi,DEC.value*180.0/scipy.pi

def call_flip(ct_flipchem,year,doy,HrUt,AltKm,Lat,Lon,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE,MINVAL=0.001,MAXVAL=1.0,ALTOP=300.0):

    YYYYDDD = "%.4d%.3d" %(year,doy); YYYYDDD = int(YYYYDDD)

    UTSEC=HrUt*3600.0
    LTHRS,SZAD,DEC=getltsza(ct_flipchem,YYYYDDD,UTSEC,Lat,Lon)

    OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT=chemion(ct_flipchem,YYYYDDD,AltKm,Lat,Lon,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE,UTSEC,LTHRS,SZAD)

    # compute fractions
    if AltKm>ALTOP:
        OXPLUS=1.0; O2PLUS=0.0; NOPLUS=0.0; N2PLUS=0.0; NPLUS=0.0
    else:
        OXPLUS=OXPLUS/NE; O2PLUS=O2PLUS/NE; NOPLUS=NOPLUS/NE; N2PLUS=N2PLUS/NE; NPLUS=NPLUS/NE
    
    # check ranges
    if OXPLUS<MINVAL:	OXPLUS=0.0
    elif OXPLUS>MAXVAL:	OXPLUS=1.0
    if O2PLUS<MINVAL:	O2PLUS=0.0
    elif O2PLUS>MAXVAL:	O2PLUS=1.0
    if NOPLUS<MINVAL:	NOPLUS=0.0
    elif NOPLUS>MAXVAL:	NOPLUS=1.0
    if N2PLUS<MINVAL:	N2PLUS=0.0
    elif N2PLUS>MAXVAL:	N2PLUS=1.0
    if NPLUS<MINVAL:	NPLUS=0.0
    elif NPLUS>MAXVAL:	NPLUS=1.0
        
    return LTHRS,SZAD,DEC,OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT

def test_flip():
    import model_utils

    geophys_dir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/dat/geophys_params'
    pathath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/flip-chem/libflip.dylib'
    ct=ctypes.CDLL(pathath) # spectra library

    # input structure
    YYYY=2007
    DDD=75
    TIME=22.0 # UT hours
    
    ALT=420.0; LAT=65.0; LON=213.0
    TE=400.0; TI=400.0; TN=400.0
    OXN=4.0e7
    O2N=6.0e4
    N2N=1.5e6
    HEN=0.0
    N4S=3.0e5
    NE=9.0e5
    UTSEC=TIME*3600.0
    
    F107D, F107A, AP=model_utils.read_geophys(YYYY,DDD,TIME,geophys_dir)

    for i in range(2):
        res=call_flip(ct,YYYY,DDD,TIME,ALT,LAT,LON,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE)
        print YYYY,DDD, ALT,LAT,LON,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE
        print res
        
    return res
    
def test_flip2():
    import model_utils

    geophys_dir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/dat/geophys_params'
    
    pathath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/flip-chem/libflip.dylib'
    ct=ctypes.CDLL(pathath) # spectra library
    ct_msis=ctypes.CDLL('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/nrlmsise00/nrlmsis00_c_version/libnrlmsise-00.dylib')

    # input structure
    YYYY=2009
    DDD=285
    TIME=22.0 # UT hours
    
    ALT=170.0; LAT=66.99; LON=-50.95
    NE=4.0e5

    F107D, F107A, AP=model_utils.read_geophys(YYYY,DDD,TIME,geophys_dir)

    HEN,OXN,N2N,O2N,ARN,M,HN,NN,aON,Texo,TN,nui,nue=model_utils.call_MSIS(ct_msis,DDD,TIME,LAT,LON,YYYY,ALT,AP,F107A,F107D)
    N4S=0.5*NN
    TE=TN
    TI=TN
    
    res=call_flip(ct,YYYY,DDD,TIME,ALT,LAT,LON,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE)
    #print YYYY,DDD, ALT,LAT,LON,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE
    #print res
                
    return res

def test_flip3():
    import model_utils
    import scipy.io

    geophys_dir='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/dat/geophys_params'
    
    pathath='/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/flip-chem/libflip.dylib'
    ct=ctypes.CDLL(pathath) # spectra library
    ct_msis=ctypes.CDLL('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/nrlmsise00/nrlmsis00_c_version/libnrlmsise-00.dylib')

    # input structure
    YYYY=2009
    DDD=285
    AllTime=scipy.arange(18.0,24.0,0.05)
        
    ALT=180.0; LAT=66.99; LON=-50.95
#	ALT=180.0; LAT=65.13; LON=213.0

    NE=3.0e5
    
    OpFrac=scipy.zeros(AllTime.shape)
    SZA=scipy.zeros(AllTime.shape)

    for i in range(len(AllTime)):
    
        TIME=AllTime[i]

        F107D, F107A, AP=model_utils.read_geophys(YYYY,DDD,TIME,geophys_dir)

        HEN,OXN,N2N,O2N,ARN,M,HN,NN,aON,Texo,TN,nui,nue=model_utils.call_MSIS(ct_msis,DDD,TIME,LAT,LON,YYYY,ALT,AP,F107A,F107D)
        N4S=0.5*NN
        TE=TN
        TI=TN
    
        LTHRS,SZAD,DEC,OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT=call_flip(ct,YYYY,DDD,TIME,ALT,LAT,LON,AP,F107D,F107A,TE,TI,TN,OXN,O2N,N2N,HEN,N4S,NE)
        print "%2.2f,%2.2f,%2.2f,%2.2f,%d" % (TIME,LTHRS,SZAD,OXPLUS,INEWT)
        
        OpFrac[i]=OXPLUS
        SZA[i]=SZAD
    
    mdict={}
    mdict['OpFrac']=OpFrac
    mdict['SZA']=SZA
    mdict['Time']=AllTime
    
    scipy.io.savemat('flipchem.mat', mdict, appendmat=True)
            
    return OpFrac,SZA,AllTime

    