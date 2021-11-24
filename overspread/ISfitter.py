#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""


import sys
import ctypes
import numpy as np
import scipy, scipy.fftpack, scipy.interpolate, scipy.optimize, scipy.signal

from . import io_utils
from .constants import *

DEBUG=0 # turn on debugging


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
        print(ne)
        print(tni*p_N0)
        print(tpsi)
        print(tvi)
        print(tti)
    
    s=compute_spec(ct_spec,pldfvvr,pldfvvi,freq,ne,tni,tti,mi,tpsi,tvi,k_radar0,sc,p_N0,p_T0,p_M0)

    # compute acf
    (tau,acf)=spec2acf(freq,s)

    # interpolate acf
    m2=scipy.zeros(dtau.size,dtype=complex);
    m2.real=scipy.interpolate.interp1d(tau,acf.real,bounds_error=0)(dtau) # linear interpolation
    m2.imag=scipy.interpolate.interp1d(tau,acf.imag,bounds_error=0)(dtau) # linear interpolation

    # lag ambiguity function - weighted average
    m=scipy.zeros(Wl.shape[1],dtype=complex)
    for i in range(Wl.shape[1]):  
        m[i]=scipy.nansum(Wl[:,i]*m2)

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
    y=y.astype(float)
    
    y=y[scipy.where(scipy.isfinite(y))]

    return y


def fit_fun_with_noise(parameter,data,var,dtau,Wl,Psc,pldfvvr,pldfvvi,ct_spec,Ifit,freq,ni,ti,mi,psi,vi,k_radar0,pertubation_noise_acf,noise_var,p_N0=1.0e11,p_T0=1000.0,p_M0=16,fitSpectra=0,tn=0.0,L=[0.0,0.0],scat_fac=1,mode=0):
    # INPUTS:
    #   x
    # OUTPUTS:
    #   x

    sc=1 # always scaled parameters for fitting

    # Molecular model ion parameters    
    tni=ni.copy()
    tti=ti.copy()
    tmi=mi.copy()
    tpsi=psi.copy()
    tvi=vi.copy()
        
    if np.isfinite(pertubation_noise_acf[0]):
        noise_power = parameter[0]  # first fitting parameter is always perturbation noise power
        ne=parameter[1]*p_N0        # second fitting parameter is always Ne
        ii=2
    else:
        noise_power = np.nan     # first fitting parameter is always perturbation noise power
        ne=parameter[0]*p_N0        # second fitting parameter is always Ne
        ii=1

    # are we fitting for  fraction?
    I=np.where(Ifit[:,0]==1)[0]
    if I.size != 0: 
        tni[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=np.where(Ifit[:,0]==-1)[0]
        tni[I1]=1.0-tni[I[0]]
    # are we fitting for temperature?
    I=np.where(Ifit[:,1]==1)[0]
    if I.size != 0: 
        tti[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=np.where(Ifit[:,1]==-1)[0]
        tti[I1]=tti[I[0]]
    # are we fitting for collision frequency?
    I=np.where(Ifit[:,2]==1)[0]
    if I.size != 0: 
        tpsi[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=np.where(Ifit[:,2]==-1)[0]
        tpsi[I1]=tpsi[I[0]]
        if Ifit[-1,2]==-1:
            tpsi[-1]=tpsi[-1]*0.35714
    # are we fitting for velocity?
    I=np.where(Ifit[:,3]==1)[0]
    if I.size != 0: 
        tvi[I]=parameter[ii:ii+I.size]
        ii=ii+I.size
        I1=np.where(Ifit[:,3]==-1)[0]
        tvi[I1]=tvi[I[0]]
    
    tni=tni/p_N0
    

    # Compute the model spectrum and compute acf from the model spectrum
    s=compute_spec(ct_spec,pldfvvr,pldfvvi,freq,ne,tni,tti,mi,tpsi,tvi,k_radar0,sc,p_N0,p_T0,p_M0)
    (tau,acf)=spec2acf(freq,s)

    # Interpolate the modeled Acf
    m2=np.zeros(dtau.size,dtype=complex);
    m2.real=scipy.interpolate.interp1d(tau,acf.real,bounds_error=0)(dtau) # linear interpolation
    m2.imag=scipy.interpolate.interp1d(tau,acf.imag,bounds_error=0)(dtau) # linear interpolation
        
    # Apply the lag ambiguity function - weighted average to the modeled Acf
    m=np.zeros(Wl.shape[1],dtype=complex)
    for i in range(Wl.shape[1]):  
        m[i]=np.nansum(Wl[:,i]*m2)

    # scaling factor
    m=m*Psc
    scaled_pertubation_noise_acf = pertubation_noise_acf * noise_power

    if mode==1:
        # If we didn't want perturbation noise fitting, the perturbation_noise_acf will be all nans
        if np.isfinite(noise_power):
            m.real = m.real + scaled_pertubation_noise_acf

        return m,m2[0],tni,tti,tpsi,tvi
                
    # if we want to fit spectra, transform
    if fitSpectra==1:
        tmp=np.concatenate((m,np.conjugate(m[:0:-1])),axis=0) # hermitian extension
        m=np.fft.fftshift(np.fft.fft(tmp,axis=0),axes=[0]) # compute spectra
        y=(data-m)/np.sqrt(var)
    else:
        if np.isfinite(noise_power):
            real_diff = (data.real-m.real-scaled_pertubation_noise_acf)/np.sqrt(var)
        else:
            real_diff = (data.real-m.real)/np.sqrt(var)
        imag_diff = (data.imag-m.imag)/np.sqrt(var)
        y=np.concatenate((real_diff, imag_diff))

    # Add constraint for ion/electron temps > neutral temps
    y = np.concatenate((y,[np.sqrt(L[0]*np.exp(-min([0.0,tti[-1]-tn]))),np.sqrt(L[1]*np.exp(-min([0.0,tti[0]-tn])))]))

    # Density can't be negative...
    y = np.concatenate((y,[np.sqrt(L[2]*np.exp(-max([0.0,((ne-1e8)/1e9)])))]))

    y = y.astype(float)
    # Intended to remove NaNs caused by variance = 0 (such as for the imaginary component of lag0)
    y[np.where(~np.isfinite(y))] = 1e6

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
    pldfvvr=pldfvv[0:int(pldfvv.size/2)]
    pldfvvr=pldfvvr.astype(float)
    pldfvvi=pldfvv[int(pldfvv.size/2):]
    pldfvvi=pldfvvi.astype(float)

    return pldfvvr,pldfvvi

# spec2acf
def spec2acf(f,s):
    # converts the spectra to an acf

    Nspec=s.size

    zsize=Nspec*3
    if np.mod(Nspec,2.0)==0.0:
        zsize=Nspec/2.0

    spec=np.concatenate((np.zeros((zsize),dtype=float),s,np.zeros((zsize),dtype=float)),axis=0) # zero pad the spectra
    
    NFFT=spec.size
    df=f[1]-f[0]
    tau=np.linspace(-1.0/df/2.0,1.0/df/2.0,NFFT)
    dtau=tau[1]-tau[0]

    m=np.fft.fftshift(np.fft.ifft(np.fft.ifftshift(spec)))/dtau
            
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

    # res = _call_specCalc(pldfvvr,pldfvvi,NIN0,TIT0,NION,MIM0,PSI,VI,kd2,scr,NOM,OM,res,0)

    res = res * scipy.sqrt((p_M0 / 30.5) * (300.0 / p_T0)) * (p_N0 / 1.0e11);
    res = res * v_lightspeed * 5.1823 / (k_radar0 * scat_fac);
    
    return res

def _call_specCalc(pldfvvr,pldfvvi,NIN0,TIT0,NION,MIM0,PSI,VI,kd2,scr,NOM,OM,res,ifref):
    # set up pointer arrays needed for specCalc
    _pldfvvr = _create_and_populate(pldfvvr)
    _pldfvvi = _create_and_populate(pldfvvi)
    _NIN0 = _create_and_populate(NIN0)
    _TIT0 = _create_and_populate(TIT0)
    _MIM0 = _create_and_populate(MIM0)
    _PSI = _create_and_populate(PSI)
    _VI = _create_and_populate(VI)
    _scr = _create_and_populate(scr)
    _OM = _create_and_populate(OM)
    _res = _create_and_populate(res)

    # call specCalc
    _specworker.specCalc(_pldfvvr,_pldfvvi,_NIN0,_TIT0,NION,_MIM0,_PSI,_VI,kd2,_scr,NOM,_OM,_res,ifref)

    # save the output res array
    for i in range(len(res)):
        res[i] = _specworker.doubleArray___getitem__(_res,i)

    # destroy the pointer arrays (no one likes memory leaks)
    _specworker.delete_doubleArray(_pldfvvr)
    _specworker.delete_doubleArray(_pldfvvi)
    _specworker.delete_doubleArray(_NIN0)
    _specworker.delete_doubleArray(_TIT0)
    _specworker.delete_doubleArray(_MIM0)
    _specworker.delete_doubleArray(_PSI)
    _specworker.delete_doubleArray(_VI)
    _specworker.delete_doubleArray(_scr)
    _specworker.delete_doubleArray(_OM)
    _specworker.delete_doubleArray(_res)

    return res


def _create_and_populate(input_array):
    size = len(input_array)

    array = _specworker.new_doubleArray(size)
    for i,value in enumerate(input_array):
        _specworker.doubleArray___setitem__(array,i,value)

    return array