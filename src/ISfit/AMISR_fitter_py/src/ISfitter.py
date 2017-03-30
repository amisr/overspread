#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys, ctypes 
import scipy, scipy.fftpack, scipy.interpolate, scipy.optimize, scipy.signal

import io_utils
from constants import *

from matplotlib import pyplot

DEBUG=0 # turn on debugging
if DEBUG==1:
    from matplotlib import pyplot


def print_timing(func):
    def wrapper(*arg):
        import time
        
        t1 = time.time()
        res = func(*arg)
        t2 = time.time()
        print '%s took %0.3f ms' % (func.func_name, (t2-t1)*1000.0)
        return res
    return wrapper

def fit_fun_FP(parameter,data,var,ht,ht_knots,lags,dtau,drng,Wtt,Psc,pldfvvr,pldfvvi,ct_spec,Ifit,freq,ni,ti,mi,psi,vi,k_radar0,p_N0=1.0e11,p_T0=1000.0,p_M0=16,scat_fac=1,mode=0):
    # INPUTS:
    #   x
    # OUTPUTS:
    #   x

    sc=1 # always scaled parameters for fitting
    Nknots=ht_knots.shape[0]
    dk=ht_knots[2]-ht_knots[1]
    (Nalts,Nlags)=data.shape

    tni=ni.copy()
    tti=ti.copy()
    tmi=mi.copy()
    tpsi=psi.copy()
    tvi=vi.copy()
    
    parameter=scipy.reshape(parameter,(parameter.shape[0]/Nknots,Nknots))
    
    tne=parameter[0,:]*p_N0
	
    ii=1
    # are we fitting for  fraction?
    I=scipy.where(Ifit[:,0]==1)[0]
    if I.size != 0: 
        tni[I]=parameter[ii:ii+I.size,:]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,0]==-1)[0]
        tni[I1,:]=1.0-tni[I[0],:]
    # are we fitting for temperature?
    I=scipy.where(Ifit[:,1]==1)[0]
    if I.size != 0: 
        tti[I,:]=parameter[ii:ii+I.size,:]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,1]==-1)[0]
        tti[I1,:]=tti[I[0],:]
    # are we fitting for collision frequency?
    I=scipy.where(Ifit[:,2]==1)[0]
    if I.size != 0: 
        tpsi[I,:]=parameter[ii:ii+I.size,:]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,2]==-1)[0]
        tpsi[I1,:]=tpsi[I[0],:]     
    # are we fitting for velocity?
    I=scipy.where(Ifit[:,3]==1)[0]
    if I.size != 0: 
        tvi[I,:]=parameter[ii:ii+I.size,:]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,3]==-1)[0]
        tvi[I1,:]=tvi[I[0],:]		

    tni=tni/p_N0

    # evaluate model on the grid
    Rgrid=scipy.arange(ht_knots[0],ht_knots[-1],1.0e3) # one kilometer grid
    Nranges=Rgrid.shape[0]
    Nfreq=freq.shape[0]
    smrModel=scipy.zeros((Nranges,dtau.shape[0]))
    # regrid params 
    ttne=scipy.signal.bsplines.cspline1d_eval(tne, Rgrid, dx=dk, x0=ht_knots[0])
    ttti=scipy.zeros((Ifit.shape[0],Nranges))
    ttvi=scipy.zeros((Ifit.shape[0],Nranges))
    ttni=scipy.zeros((Ifit.shape[0],Nranges))
    ttpsi=scipy.zeros((Ifit.shape[0],Nranges))
    for ii in range(Ifit.shape[0]):
        ttni[ii,:]=scipy.interpolate.interp1d(ht_knots,tni[ii,:],bounds_error=0)(Rgrid)
        ttti[ii,:]=scipy.signal.bsplines.cspline1d_eval(tti[ii,:], Rgrid, dx=dk, x0=ht_knots[0])
        ttpsi[ii,:]=scipy.interpolate.interp1d(ht_knots,tpsi[ii,:],bounds_error=0)(Rgrid)
        ttvi[ii,:]=scipy.signal.bsplines.cspline1d_eval(tvi[ii,:], Rgrid, dx=dk, x0=ht_knots[0])
    # run model
    for iht in range(Nranges):
        
        # compute the acf
        s=compute_spec(ct_spec,pldfvvr,pldfvvi,freq,ttne[iht],ttni[:,iht],ttti[:,iht],mi,ttpsi[:,iht],ttvi[:,iht],k_radar0,sc,p_N0,p_T0,p_M0)
        (tau,acf)=spec2acf(freq,s)

        # interpolate acf
        m2=scipy.zeros(dtau.size,dtype='Complex64');
        m2.real=scipy.interpolate.interp1d(tau,acf.real,bounds_error=0)(dtau) # linear interpolation
        m2.imag=scipy.interpolate.interp1d(tau,acf.imag,bounds_error=0)(dtau) # linear interpolation
    
        smrModel[iht,:]=m2
    
    # smear model in range and lag    
    Model=scipy.zeros(data.shape)
    for ilag in range(Nlags):
        ilag=0
        
        tlag=lags[ilag]
        tWtt=Wtt[ilag]

        # center of gravity of 2d ambiguity function
        I=scipy.where(scipy.absolute(dtau-tlag)==scipy.absolute(dtau-tlag).min())[0]
        scipy.absolute(scipy.cumsum(tWtt[I,:])-scipy.sum(tWtt[I,:])/2.0)
        I=scipy.where(scipy.absolute(scipy.cumsum(tWtt[I,:])-scipy.sum(tWtt[I,:])/2.0)==scipy.absolute(scipy.cumsum(tWtt[I,:])-scipy.sum(tWtt[I,:])/2.0).min())[0]
        rngs=drng[I]
        
        for irng in range(Nalts):
            tRng=drng+ht[irng]-rngs # actually, height
            tsmr=scipy.zeros((tWtt.shape),dtype='Complex64')
            tsmr.real=scipy.interpolate.interp1d(Rgrid,scipy.transpose(smrModel.real),bounds_error=0,fill_value=0.0)(tRng)
            tsmr.imag=scipy.interpolate.interp1d(Rgrid,scipy.transpose(smrModel.imag),bounds_error=0,fill_value=0.0)(tRng)

            tmp=scipy.sum(tsmr*tWtt,axis=0) # sum in lag
            tPsc=scipy.interpolate.interp1d(ht_knots,Psc,bounds_error=0,fill_value=0.0)(tRng)

            Model[irng,ilag]=scipy.sum(tmp*tPsc) # sum in range
      
    y=scipy.concatenate((scipy.reshape(data.real-Model.real,(Nlags*Nalts)),scipy.reshape(data.imag-Model.imag,(Nlags*Nalts))))
    y=y.astype('float64')

    return y


def fit_fun(parameter,data,var,dtau,Wl,Psc,pldfvvr,pldfvvi,ct_spec,Ifit,freq,ni,ti,mi,psi,vi,k_radar0,p_N0=1.0e11,p_T0=1000.0,p_M0=16,fitSpectra=0,tn=0.0,L=[0.0,0.0],scat_fac=1,mode=0):
    # INPUTS:
    #   x
    # OUTPUTS:
    #   x

    sc=1 # always scaled parameters for fitting

    ne=parameter[0]*p_N0 # first fitting parameter is always Ne
    
    tni=ni.copy()
    tti=ti.copy()
    tmi=mi.copy()
    tpsi=psi.copy()
    tvi=vi.copy()
        
    ii=1
    # are we fitting for  fraction?
    I=scipy.where(Ifit[:,0]==1)[0]
    if I.size != 0: 
        tni[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,0]==-1)[0]
        tni[I1]=1.0-tni[I[0]]
    # are we fitting for temperature?
    I=scipy.where(Ifit[:,1]==1)[0]
    if I.size != 0: 
        tti[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,1]==-1)[0]
        tti[I1]=tti[I[0]]
    # are we fitting for collision frequency?
    I=scipy.where(Ifit[:,2]==1)[0]
    if I.size != 0: 
        tpsi[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,2]==-1)[0]
        tpsi[I1]=tpsi[I[0]]
        if Ifit[-1,2]==-1:
            tpsi[-1]=tpsi[-1]*0.35714
    # are we fitting for velocity?
    I=scipy.where(Ifit[:,3]==1)[0]
    if I.size != 0: 
        tvi[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,3]==-1)[0]
        tvi[I1]=tvi[I[0]]
    
    tni=tni/p_N0

    if DEBUG:
        print ne
        print tni*p_N0
        print tpsi
        print tvi
        print tti
    
    s=compute_spec(ct_spec,pldfvvr,pldfvvi,freq,ne,tni,tti,mi,tpsi,tvi,k_radar0,sc,p_N0,p_T0,p_M0)

    # compute acf
    (tau,acf)=spec2acf(freq,s)

    # interpolate acf
    m2=scipy.zeros(dtau.size,dtype='Complex64');
    m2.real=scipy.interpolate.interp1d(tau,acf.real,bounds_error=0)(dtau) # linear interpolation
    m2.imag=scipy.interpolate.interp1d(tau,acf.imag,bounds_error=0)(dtau) # linear interpolation
#   t=scipy.interpolate.splrep(tau,acf.real) 
#   m2.real=scipy.interpolate.splev(dtau,t) # cubic spline interpolation
#   t=scipy.interpolate.splrep(tau,acf.imag)
#   m2.imag=scipy.interpolate.splev(dtau,t) # cubic spline interpolation
        
    # lag ambiguity function - weighted average
    m=scipy.zeros(Wl.shape[1],dtype='Complex64')
    for i in range(Wl.shape[1]):  
        #print Wl[:,i]
        m[i]=scipy.sum(Wl[:,i]*m2)
#        tmp=scipy.sum(Wl[:,i])
#        tmp=(15.0-i)/16.0
#        m[i]=tmp*scipy.interpolate.interp1d(dtau,m2.real,bounds_error=0)(20.0e-6*(i+1))[0] + tmp*scipy.interpolate.interp1d(dtau,m2.imag,bounds_error=0)(20.0e-6*(i+1))[0]*1.0j

    # scaling factor
    m=m*Psc
    
    if mode==1:
        return m,m2[0],tni,tti,tpsi,tvi
		        
    # if we want to fit spectra, transform
    if fitSpectra==1:
        tmp=scipy.concatenate((m,scipy.conjugate(m[:0:-1])),axis=0) # hermitian extension
        m=scipy.fftpack.fftshift(scipy.fftpack.fft(tmp,axis=0),axes=[0]) # compute spectra
        y=(data-m)/scipy.sqrt(var)
    else:
        y=scipy.concatenate(((data.real-m.real)/scipy.sqrt(var),(data.imag-m.imag)/scipy.sqrt(var)))

    y=scipy.concatenate((y,[scipy.sqrt(L[0]*scipy.exp(-min([0.0,tti[-1]-tn]))),scipy.sqrt(L[1]*scipy.exp(-min([0.0,tti[0]-tn])))]))
    y=y.astype('float64')
    
    if DEBUG:
    
        print scipy.sum(scipy.power(y,2.0))
    
        pylab.figure(10)
        pylab.clf()
        pylab.plot(freq,s)  
        #pylab.show()

        pylab.figure(20)
        pylab.clf()
        pylab.errorbar(range(data.size),data.real,scipy.sqrt(var))
        pylab.hold(1)
        pylab.plot(range(data.size),m,'k')
        #pylab.show()

        #pylab.figure(30)
        #pylab.clf()
        #pylab.plot(range(data.size),y,'k')
        #pylab.show()

#   pylab.figure()
#   pylab.errorbar(scipy.arange(data.size,dtype='float64')*30e-6,data.imag,scipy.sqrt(var))
#   pylab.hold(1)
#   pylab.plot(scipy.arange(data.size,dtype='float64')*30e-6,m.imag,'k.')
#   pylab.plot(dtau,m2.imag,'r')
#   pylab.show()

    y=y[scipy.where(scipy.isfinite(y))]

    return y


def fit_fun_with_noise(parameter,data,var,dtau,Wl,Psc,pldfvvr,pldfvvi,ct_spec,Ifit,freq,ni,ti,mi,psi,vi,k_radar0,pertubation_noise_acf,noise_var,p_N0=1.0e11,p_T0=1000.0,p_M0=16,fitSpectra=0,tn=0.0,L=[0.0,0.0],scat_fac=1,mode=0):
    # INPUTS:
    #   x
    # OUTPUTS:
    #   x

    sc=1 # always scaled parameters for fitting

    noise_power = parameter[0]  # first fitting parameter is always perturbation noise power
    ne=parameter[1]*p_N0        # second fitting parameter is always Ne

    # Molecular model ion parameters    
    tni=ni.copy()
    tti=ti.copy()
    tmi=mi.copy()
    tpsi=psi.copy()
    tvi=vi.copy()
        
    ii=2
    # are we fitting for  fraction?
    I=scipy.where(Ifit[:,0]==1)[0]
    if I.size != 0: 
        tni[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,0]==-1)[0]
        tni[I1]=1.0-tni[I[0]]
    # are we fitting for temperature?
    I=scipy.where(Ifit[:,1]==1)[0]
    if I.size != 0: 
        tti[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,1]==-1)[0]
        tti[I1]=tti[I[0]]
    # are we fitting for collision frequency?
    I=scipy.where(Ifit[:,2]==1)[0]
    if I.size != 0: 
        tpsi[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,2]==-1)[0]
        tpsi[I1]=tpsi[I[0]]
        if Ifit[-1,2]==-1:
            tpsi[-1]=tpsi[-1]*0.35714
    # are we fitting for velocity?
    I=scipy.where(Ifit[:,3]==1)[0]
    if I.size != 0: 
        tvi[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=scipy.where(Ifit[:,3]==-1)[0]
        tvi[I1]=tvi[I[0]]
    
    tni=tni/p_N0
    

    # Compute the model spectrum and compute acf from the model spectrum
    s=compute_spec(ct_spec,pldfvvr,pldfvvi,freq,ne,tni,tti,mi,tpsi,tvi,k_radar0,sc,p_N0,p_T0,p_M0)
    (tau,acf)=spec2acf(freq,s)

    # Interpolate the modeled Acf
    m2=scipy.zeros(dtau.size,dtype='Complex64');
    m2.real=scipy.interpolate.interp1d(tau,acf.real,bounds_error=0)(dtau) # linear interpolation
    m2.imag=scipy.interpolate.interp1d(tau,acf.imag,bounds_error=0)(dtau) # linear interpolation
        
    # Apply the lag ambiguity function - weighted average to the modeled Acf
    m=scipy.zeros(Wl.shape[1],dtype='Complex64')
    for i in range(Wl.shape[1]):  
        m[i]=scipy.sum(Wl[:,i]*m2)

    # scaling factor
    m=m*Psc
    scaled_pertubation_noise_acf = pertubation_noise_acf * noise_power

    if mode==1:
        # fig = pyplot.figure()
        # ax = fig.add_subplot(311)
        # ax.plot(data.real,'b--')
        # ax.plot(data.imag,'g--')
        # ax.plot(m.real+scaled_pertubation_noise_acf,'r')
        # ax.plot(m.imag,'k')

        # # fig = pyplot.figure()
        # ax = fig.add_subplot(312)
        # ax.plot(dtau,m2.real,'r')
        # ax.plot(dtau,m2.imag,'k')

        # ax = fig.add_subplot(313)
        # ax.plot(scipy.sum(Wl,axis=0),'r')
        # ax.plot(pertubation_noise_acf,'k')
        # pyplot.show()

        m.real = m.real + scaled_pertubation_noise_acf
        return m,m2[0],tni,tti,tpsi,tvi
                
    # if we want to fit spectra, transform
    if fitSpectra==1:
        tmp=scipy.concatenate((m,scipy.conjugate(m[:0:-1])),axis=0) # hermitian extension
        m=scipy.fftpack.fftshift(scipy.fftpack.fft(tmp,axis=0),axes=[0]) # compute spectra
        y=(data-m)/scipy.sqrt(var)
    else:
        real_diff = (data.real-m.real-scaled_pertubation_noise_acf)/scipy.sqrt(var)
        imag_diff = (data.imag-m.imag)/scipy.sqrt(var)
        y=scipy.concatenate((real_diff, imag_diff))

    # Add constraint for ion/electron temps > neutral temps
    y = scipy.concatenate((y,[scipy.sqrt(L[0]*scipy.exp(-min([0.0,tti[-1]-tn]))),scipy.sqrt(L[1]*scipy.exp(-min([0.0,tti[0]-tn])))]))

    # Density can't be negative...
    y = scipy.concatenate((y,[scipy.sqrt(L[2]*scipy.exp(-max([0.0,((ne-1e8)/1e9)])))]))

    # Intended to remove NaNs caused by variance = 0 (such as for the imaginary component of lag0)
    y = y.astype('float64')
    y = y[scipy.where(scipy.isfinite(y))]

    return y

    
# loads the ctype library
def load_ct_spec(path):
    # the C source to compute the spectrum
    ct_spec=ctypes.CDLL(path)
    
    return ct_spec

# loads the plasma dispersion table from a file
def load_disp_table(path):
    
    # the plasma dispersion table
    pldfvv=scipy.fromfile(path,dtype=scipy.float32)
    if sys.byteorder=='little':
        pldfvv=pldfvv.byteswap()
    pldfvvr=pldfvv[0:pldfvv.size/2]
    pldfvvr=pldfvvr.astype('float64')
    pldfvvi=pldfvv[pldfvv.size/2:]
    pldfvvi=pldfvvi.astype('float64')

    return pldfvvr,pldfvvi

# spec2acf
#@print_timing
def spec2acf(f,s):
    # converts the spectra to an acf

    Nspec=s.size

    zsize=Nspec*3
    if scipy.mod(Nspec,2.0)==0.0:
        zsize=Nspec/2.0

    spec=scipy.concatenate((scipy.zeros((zsize),dtype='float64'),s,scipy.zeros((zsize),dtype='float64')),axis=0) # zero pad the spectra
    
    NFFT=spec.size
    df=f[1]-f[0]
    tau=scipy.linspace(-1.0/df/2.0,1.0/df/2.0,NFFT)
    dtau=tau[1]-tau[0]

    m=scipy.fftpack.fftshift(scipy.fftpack.ifft(scipy.fftpack.ifftshift(spec)))/dtau
            
    return tau, m

# computes a spectrum
def compute_spec(ct_spec,pldfvvr,pldfvvi,freq,ne,ni,ti,mi,psi,vi,k_radar0,sc=0,p_N0=1.0e11,p_T0=1000.0,p_M0=16,scat_fac=1):
    # INPUTS:
    #   ct_spec: ctype library
    #   pldfvvr, pldfvvi: real and imaginary plasma dispersion function table
    #   freq: frequency array
    #   ne: electron density
    #   ni: vector of ion fractions (last is electron)
    #   ti: vector of ion temps (last is electron)
    #   mi: vector of ion masses in amu (last is electron)
    #   psi: vector of ion collision frequencies (last is electron)
    #   vi: vector of ion velocities (last is electron)
    #   k_radar0:
    #   sc: 
    #   p_N0:
    #   p_T0:
    #   p_M0:
    #   scat_fac:
    # OUTPUTS:
    #   res: spectrum estimate, in SI units (m^-3xs)
    
    p_om0=k_radar0*scipy.sqrt(2.0*v_Boltzmann*p_T0/(p_M0*v_amu))
    p_D0=scipy.sqrt(v_epsilon0*v_Boltzmann*p_T0/(p_N0*v_elemcharge*v_elemcharge))
    
    NION=ni.size-1 
    NOM=freq.size
    
    if sc:
        NIN0=ni*ne
        #te=ti[-1]*p_T0
        TIT0=ti
        MIM0=mi/p_M0
        PSI=psi/(scipy.sqrt(TIT0/MIM0)*scat_fac)
        VI=-1.0*vi      
    else:
        NIN0=ni*ne/p_N0
        #te=ti[-1]
        TIT0=ti/p_T0
        MIM0=mi/p_M0
        PSI=psi/(p_om0*scipy.sqrt(TIT0/MIM0)*scat_fac)
        VI=-1.0*vi/(p_om0/k_radar0)
    
    kd2=(k_radar0*scat_fac)*p_D0
    kd2=kd2*kd2
    
    OM=2*pi*freq/p_om0

    scr=scipy.zeros((NION+2)*(3+4*NOM),dtype='double')
    res=scipy.zeros(freq.size,dtype='double')

    ct_spec.specCalc(pldfvvr.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),pldfvvi.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        NIN0.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),TIT0.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),ctypes.c_long(NION),
        MIM0.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),PSI.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        VI.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),ctypes.c_double(kd2),scr.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
        ctypes.c_long(NOM),OM.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),res.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),ctypes.c_long(0))
        
    res=res*scipy.sqrt((p_M0/30.5)*(300.0/p_T0))*(p_N0/1.0e11);
    res=res*v_lightspeed*5.1823/(k_radar0*scat_fac);
    
    return res

#
def test_fit():

    ct_spec=load_ct_spec('libspec.dylib')
    (pldfvvr,pldfvvi)=load_disp_table('pldfvv.dat')

    # frequency array
    NFFT=2048*2;
    f=scipy.linspace(-100e3,100e3,NFFT+1)

    # generate some data
    sc=1
    ne=0.9e11
    p_N0=1.0e11
    p_T0=1000.00
    p_M0=16
    k_radar0=4*pi*450.0e6/v_lightspeed
    p_om0=k_radar0*scipy.sqrt(2.0*v_Boltzmann*p_T0/(p_M0*v_amu))
    ni=scipy.array([1.0,1.0])/p_N0
    NION=ni.size-1
    ti=scipy.array([1200.0,2000.0])/p_T0
    mi=scipy.array([16.0,v_electronmass/v_amu])
    psi=scipy.array([0.0,0.0])/p_om0
    vi=scipy.array([100,100])/(p_om0/k_radar0)
    res2=compute_spec(ct_spec,pldfvvr,pldfvvi,f,ne,ni,ti,mi,psi,vi,k_radar0,sc,p_N0,p_T0,p_M0)
    (tau2,acf2)=spec2acf(f,res2)
    tauf=scipy.arange(0.0,16.0*30.0e-6,30.0e-6)
    acfdata=scipy.interpolate.interp1d(tau2,acf2,bounds_error=0)(tauf)
    acfvar=0.01*acfdata.real[0]
    acfvar=acfvar*acfvar
    
    ni=ni*p_N0
    
    Ifit=scipy.zeros([NION+1,4])
    Ifit[0,1]=1 # fit for Ti
    Ifit[-1,1]=1 # fit for Te
    Ifit[0,3]=1 # fit for Vi
    Ifit[-1,3]=-1 # force Ve=Vi
    
    dtau=tauf
    Wl= scipy.diag([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) # tau x lags
    Psc=1
    
    params0=scipy.array([1.0,1.0,1.0,0.01]);
    (x, cov_x, infodict, mesg, ier) = scipy.optimize.leastsq(fit_fun,params0,(acfdata,acfvar,dtau,Wl,Psc,pldfvvr,pldfvvi,ct_spec,Ifit,f,ni,ti,mi,psi,vi,k_radar0,p_N0,p_T0,p_M0),full_output=1,epsfcn=1.0e-6)
    
    print x[0]*p_N0
    print x[1]*p_T0
    print x[2]*p_T0
    print x[3]*(p_om0/k_radar0)
    print scipy.sqrt(cov_x)

    pylab.figure()
    pylab.errorbar(tauf,acfdata,scipy.sqrt(acfvar))
    pylab.show()
    
    return

# just computes a spectrum for testing
def test_spec():

    ct_spec=load_ct_spec('../lib/spec_worker/libspec.dylib')
    (pldfvvr,pldfvvi)=load_disp_table('../dat/pldfvv.dat')

    # frequency array
    NFFT=1024/2
    f1=scipy.linspace(-30e3,30e3,NFFT+1)
    f2=scipy.linspace(-30e3,30e3,NFFT*2+1)
    
    # unscaled example
    ne=1.0e11
    ni=scipy.array([0.5,0.5,1.0])
    ti=scipy.array([1000.0,1000.0,1000.0])/4.0
    mi=scipy.array([16.0,30.5,v_electronmass/v_amu])
    psi=scipy.array([0.0,0.0,0.0])
    vi=scipy.array([0.0,0.0,0.0])+500.0
    k_radar0=4.0*pi*450.0e6/v_lightspeed
    res=compute_spec(ct_spec,pldfvvr,pldfvvi,f1,ne,ni,ti,mi,psi,vi,k_radar0)
    
    # scaled example
    sc=1
    ne=1.0e11
    p_N0=1.0e11
    p_T0=1000.00
    p_M0=16.0
    p_om0=k_radar0*scipy.sqrt(2.0*v_Boltzmann*p_T0/(p_M0*v_amu))
    ni=scipy.array([0.5,0.5,1.0])/p_N0
    ti=scipy.array([1000.0,1000.0,1000.0])/p_T0/4.0
    mi=scipy.array([16.0,30.5,v_electronmass/v_amu])
    psi=scipy.array([0.0,0.0,0.0])/p_om0
    vi=(scipy.array([0.0,0.0,0.0])+500.0)/(p_om0/k_radar0)
    res2=compute_spec(ct_spec,pldfvvr,pldfvvi,f2,ne,ni,ti,mi,psi,vi,k_radar0,sc,p_N0,p_T0,p_M0)
    
    # compute the acfs
    (tau1,acf1)=spec2acf(f1,res)
    (tau2,acf2)=spec2acf(f2,res2)
    
    # they should be equal -fractional error
#   print scipy.sum((res-res2)*(res-res2))/scipy.sum((res)*(res))
    
    # plot
    pylab.figure()
    pylab.plot(f1/1000.0,res)
    pylab.hold(1)
    pylab.plot(f2/1000.0,res2,'r')
    pylab.show()
    
    pylab.figure()
    pylab.plot(tau1/30e-6,acf1.real,'b')
    pylab.hold(1)
    pylab.plot(tau2/30e-6,acf2.real,'r')
    pylab.show()

    pylab.figure()
    pylab.plot(tau1/30e-6,acf1.imag,'b')
    pylab.hold(1)
    pylab.plot(tau2/30e-6,acf2.imag,'r')
    pylab.show()
        
# just computes a spectrum for testing
def test_acoustic():

    ct_spec=load_ct_spec('../lib/spec_worker/libspec.dylib')
    (pldfvvr,pldfvvi)=load_disp_table('../dat/pldfvv.dat')

    # frequency array
    NFFT=1024;
    f=scipy.linspace(-30e3,30e3,NFFT+1)

    # unscaled example
    ne=1.0e11
    ni=scipy.array([0.0,1.0,1.0])
    mi=scipy.array([16.0,30.5,v_electronmass/v_amu])
    psi=scipy.array([1.0e4,1.0e4,1.0e4*0.35714])
    vi=scipy.array([0.0,0.0,0.0])
    k_radar0=4.0*pi*450.0e6/v_lightspeed

    ti=scipy.array([1000.0,1000.0,1000.0])
    ac=ti[0]*3/2+ti[-1]
    print ac
    print ti
    res=compute_spec(ct_spec,pldfvvr,pldfvvi,f,ne,ni,ti,mi,psi,vi,k_radar0)
    
    ti=scipy.array([800.0,800.0,1000.0])    
    ti[-1]=ac-ti[0]*3/2
    ac=ti[0]*3/2+ti[-1]
    print ti[0]*3/2+ti[-1]
    print ti
    res2=compute_spec(ct_spec,pldfvvr,pldfvvi,f,ne,ni,ti,mi,psi,vi,k_radar0)
    
    # compute the acfs
    (tau1,acf1)=spec2acf(f,res)
    (tau2,acf2)=spec2acf(f,res2)
        
    pylab.figure()
    pylab.plot(tau1,acf1.real/acf1[acf1.size/2].real,'b')
    pylab.hold(1)
    pylab.plot(tau2,acf2.real/acf2[acf2.size/2].real,'r')
    pylab.show()    
    
    # plot
    pylab.figure()
    pylab.plot(f/1000.0,res)
    pylab.hold(1)
    pylab.plot(f/1000.0,res2,'r')
    pylab.show()
    
    return
	
# just computes a spectrum for testing
def dreg():

    ct_spec=load_ct_spec('../lib/spec_worker/libspec.dylib')
    (pldfvvr,pldfvvi)=load_disp_table('../dat/pldfvv.dat')

    # frequency array
    NFFT=1024*200;
    f=scipy.linspace(-1e3,1e3,NFFT+1); df=f[1]-f[0]

    neall=[1.0e8,1.0e9,1.0e10,1.0e8,1.0e9,1.0e10]; nuisc=1.0e9;
    nuiall=[nuisc,nuisc,nuisc,nuisc,nuisc,nuisc]; nueall=[0.0,0.0,0.0,nuisc,nuisc,nuisc];
    ni=scipy.array([0.0,1.0,1.0])
    mi=scipy.array([16.0,30.5,v_electronmass/v_amu])
    vi=scipy.array([0.0,0.0,0.0])
    k_radar0=4.0*pi*450.0e6/v_lightspeed
    ti=scipy.array([200.0,200.0,200.0])

    pylab.figure()

    for aa in range(len(neall)):
        # unscaled
        ne=neall[aa]; nui=nuiall[aa]; nue=nueall[aa]
        psi=scipy.array([nui,nui,nue])
    
        res=compute_spec(ct_spec,pldfvvr,pldfvvi,f,ne,ni,ti,mi,psi,vi,k_radar0,sc=0,p_N0=1.0e10,p_T0=300.0,p_M0=30.5,scat_fac=1)
        maxres=res.max()
        print maxres
    
        tPow=scipy.trapz(res,dx=df)
        print tPow/ne
        
        # plot
        pylab.semilogy(f/1000.0,res)
        pylab.show()
    
    return
