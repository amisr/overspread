#! /usr/bin/env python

"""

"""

import scipy, scipy.fftpack

from amisr_py.constants.constants import *


# gordeyev
def gordeyev_cz(theta,alpha,phi,psin,psic,kmax=10.0,Nk=100,tol=1e-3,maxLoops=30,Nmax=1e4):
    '''
        Computes Gordeyev integral for either electrons or ions
        Based on Chirp-z transform method of Li et al., IEEE Trans. Ant. Prop., 1991
    
        Inputs:
            theta - normalized frequency, (w - kV)/(k*Vth)
            alpha - magnetic field aspect angle, radians
            phi - normalized gyro frquency, O/(k*Vth)
            psin - normalized neutral collision frquency, nu/(k*Vth)
            psic - normalized coulomb collision frquency, nu/(k*Vth)
        Optional Inputs:
            kmax - max k value (t value) for integration
            Nk - number of points in initial integrand
            tol - tolerance of Gordeyev integral convergence
            maxLoops - maximum number of loops for gordeyev integrations
            Nmax - Maximum number of points in integrand (will cause overflow if too high)
    '''
    

    N = theta.shape[0]
    x0 = theta[0]
    dx = (theta[1]-theta[0]).__abs__()

    if N>Nk:
        Nk=N

    done = 0
    dk = kmax/Nk
    iloop=0

    alp = -1.0j
 
    SnPrev = 1.0e10
    while not done:
    
        n = scipy.arange(0.0,Nk+1,1.0);    
        t = dk*n
        p = dx*kmax/2.0/pi
        m = scipy.arange(0.0,N,1.0)
        Wn = scipy.exp(alp*2.0*p*pi/Nk)
                    
        f=funcgv(t,alpha,phi,psin,psic)
        
        g = f[:-1]
        g[0]=g[0]/2
    
        Xn = g*scipy.exp(alp*t[:-1]*x0)*scipy.power(Wn,n[:-1]**2.0/2.0)
        Yn = scipy.power(Wn,-n**2.0/2.0)
        xn = scipy.zeros((2*Nk),dtype=Xn.dtype)
        yn = scipy.zeros((2*Nk),dtype=Yn.dtype)
        xn[:Nk] = Xn
        yn[:Nk+1] = Yn; yn[Nk+1:] = scipy.flipud(Yn[1:-1])
            
        tm = scipy.ifft(scipy.fft(xn)*scipy.fft(yn))[:N]
                  
        R = dk*f[-1]/2.0*scipy.exp(alp*kmax*x0)*scipy.exp(alp*2.0*m*p*pi)
        Sn = dk*scipy.power(Wn,m**2.0/2.0)*tm + R
        
        # check convergence
        ds = scipy.absolute(Sn-SnPrev)/scipy.absolute(SnPrev)
        if ds.max()<tol or iloop>maxLoops:
            done=1            
        else:
            SnPrev=Sn
            kmax=kmax*2
            Nk=Nk*2
            if Nk>Nmax:
                Nk=Nmax
            iloop +=1
    
    J = Sn/(1.0-psin*Sn)
           
    return J,iloop

def funcgv(t,alpha,phi,psin,psic):
    # magnetic field, coulomb collisions, neutral collisions
    
    K0=scipy.exp(-t*psin)
    if psic>0:
        alpha=pi/2.0-alpha
        gamma=scipy.arctan(psic/phi)
        K1=scipy.exp(-(psic*t-1.0+scipy.exp(-psic*t))/(2.0*psic*psic*scipy.power(scipy.sin(alpha),-2)))
        K2=scipy.exp(-(scipy.cos(2.0*gamma)+psic*t-scipy.exp(-psic*t)*scipy.cos(phi*t-2.0*gamma))/(2.0*(psic*psic+phi*phi)*scipy.power(scipy.cos(alpha),-2)))
    else:
        K1=scipy.exp(-scipy.power(scipy.sin(alpha),2.0)*scipy.power(scipy.sin(0.5*phi*t),2.0)/(phi*phi)-0.25*t*t*scipy.cos(alpha)*scipy.cos(alpha))
        K2=1.0

    val=K0*K1*K2 
    
    return val  


# gordeyev
def gordeyev(theta,alpha,phi,psin,psic,tol=1e-2,bma=100.0,aend=1500.0,x=[],w=[]):
    #
    # Computes Gordeyev integral for either electrons or ions
    #
    # Inputs:
 

    if len(x)==0 or len(w)==0:
        x,w=xw(bma,aend)
    
    Jprev=1.0e10
    done=0
    while done==0:
    
        fx=gv3(x,alpha,phi,theta,psin,psic)		
        J=scipy.sum(fx*w)
        
        J = J/(1.0-psin*J)
        
        if scipy.absolute(J-Jprev)<tol:
            done=1
        else:
            Jprev=J.copy()
            x0=x
            w0=w
            bma0=bma
            aend0=aend
            bma=scipy.ceil(bma*0.95)
            aend=scipy.ceil(aend*1.05)
            x,w=xw(bma,aend)

    return x0,w0,bma0,aend0,J

# gv1	
def gv1(t,alpha,phi,theta):
    # magnetic field, no coulomb collisions

    K=scipy.exp(-scipy.power(scipy.sin(alpha),2.0)*scipy.power(scipy.sin(0.5*phi*t),2.0)/(phi*phi)-0.25*t*t*scipy.cos(alpha)*scipy.cos(alpha))
    val=scipy.exp(-1.0j*theta*t)*K
        
    return val
    
# gv2
def gv2(t,alphaa,phi,theta,psi):
    # magnetic field, with coulomb collisions
    
    alpha=pi/2.0-alphaa

    gamma=scipy.arctan(psi/phi)

    K1=scipy.exp(-(psi*t-1.0+scipy.exp(-psi*t))/(2.0*psi*psi*scipy.power(scipy.sin(alpha),-2)))
    K2=scipy.exp(-(scipy.cos(2.0*gamma)+psi*t-scipy.exp(-psi*t)*scipy.cos(phi*t-2.0*gamma))/(2.0*(psi*psi+phi*phi)*scipy.power(scipy.cos(alpha),-2)))

    val=scipy.exp(-1.0j*theta*t)*K1*K2;     
    
    return val
    
# gv3
def gv3(t,alpha,phi,theta,psin,psic):
    # magnetic field, coulomb collisions, neutral collisions
    
    K0=scipy.exp(-t*psin)
    
    if psic>0:
        alpha=pi/2.0-alpha
        gamma=scipy.arctan(psic/phi)
        K1=scipy.exp(-(psic*t-1.0+scipy.exp(-psic*t))/(2.0*psic*psic*scipy.power(scipy.sin(alpha),-2)))
        K2=scipy.exp(-(scipy.cos(2.0*gamma)+psic*t-scipy.exp(-psic*t)*scipy.cos(phi*t-2.0*gamma))/(2.0*(psic*psic+phi*phi)*scipy.power(scipy.cos(alpha),-2)))
    else:
        K1=scipy.exp(-scipy.power(scipy.sin(alpha),2.0)*scipy.power(scipy.sin(0.5*phi*t),2.0)/(phi*phi)-0.25*t*t*scipy.cos(alpha)*scipy.cos(alpha))
        K2=1.0

    val=scipy.exp(-1.0j*theta*t)*K0*K1*K2 
    
    return val    
    
def xw(bma,aend):
    a=scipy.arange(0,aend,bma)
    b=a+bma
    N=scipy.power(2.0,6.0)
    x=scipy.zeros((a.size,N),dtype='float64')
    w=scipy.zeros((a.size,N),dtype='float64')
    for ii in range(a.size):
        (x[ii,:],w[ii,:])=fclencurt(N,a[ii],b[ii])	

    return x,w

def compute_spec(Je,Ji,thetae,thetai,Ce,Ci,ni,mui,he,k,w):
    #
    # Computes spectra, user must supply Gordevey integrals
    #
    # Inputs:
    #	Je - 
    #	Ji - 
    #	thetae -
    #	Ce - 
    #	Ci - 
    #	ni -
    #	mui
    #	he -
    #	k - 
    #	w - 
    
    ye=1.0j+thetae*Je # electron admittance
    yi=1.0j+thetai*Ji # ion admittance
    yis=scipy.sum(scipy.repeat(scipy.reshape(mui*ni,(1,ni.size)),thetae.size,axis=0)*yi,axis=1)

    tmpe=Je.real/(Ce*k*scipy.sqrt(2.0))
    tmpi=scipy.sum(scipy.repeat(scipy.reshape(ni/Ci,(1,ni.size)),thetae.size,axis=0)*Ji.real,axis=1)/k/scipy.sqrt(2.0)

    den=scipy.power(scipy.absolute(ye+yis+1.0j*he*he*k*k),2.0)
    num1=scipy.power(scipy.absolute(ye),2.0)*tmpi
    num2=scipy.power(scipy.absolute(yis+1.0j*he*he*k*k),2.0)*tmpe
    spec=(num1+num2)/den
    
    return spec	
    
# spec2acf
"""
def spec2acf(f,s):
    #
    # converts a one-sided spectra to an acf, zero padding to increase resolution
    #
    
    Nspec=s.size
    spec=scipy.concatenate((s,scipy.zeros((Nspec),dtype='float64')),axis=0)
    spec2=scipy.concatenate((spec[-1:0:-1],spec),axis=0)
    NFFT=spec2.size

    df=f[1]-f[0]
#	dtau=1.0/(f[-1]*2.0)
#	dtau=dtau/2.0
#	print dtau
    tau=scipy.linspace(-1.0/df/2.0,1.0/df/2.0,NFFT)
    dtau=tau[1]-tau[0]
#	print dtau
#	tau=scipy.arange(-((NFFT-1)/2.0)*dtau,((NFFT-1)/2.0+1)*dtau,dtau)

    m=scipy.fftpack.fftshift(scipy.fftpack.ifft(scipy.fftpack.ifftshift(spec2)))/dtau*4.0
            
    return tau, m	
"""
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

# fclencurt
def fclencurt(N1,a,b):

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #%
    #% fclencurt.m - Fast Clenshaw Curtis Quadrature
    #%
    #% Compute the N nodes and weights for Clenshaw-Curtis
    #% Quadrature on the interval [a,b]. Unlike Gauss 
    #% quadratures, Clenshaw-Curtis is only exact for 
    #% polynomials up to order N, however, using the FFT
    #% algorithm, the weights and nodes are computed in linear
    #% time. This script will calculate for N=2^20+1 (1048577
    #% points) in about 5 seconds on a normal laptop computer.
    #%
    #% Written by: Greg von Winckel - 02/12/2005
    #% Contact: gregvw(at)chtm(dot)unm(dot)edu
    #%
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    N=N1-1
    bma=b-a
    c=scipy.zeros((N1,2),dtype='float64')
    c[0,0]=2.0
    c[2:N1:2,0]=(2.0/(1-scipy.power(scipy.arange(2,N,2,dtype='float64'),2)))
    c[1,1]=1
    tmp=scipy.concatenate((c,c[N-1:0:-1,:]),axis=0)
    f=scipy.real(scipy.ifft(tmp,axis=0))
    w=scipy.zeros(N1,dtype='float64')
    w[0]=f[0,0]
    w[1:N]=2*f[1:N,0]
    w[-1]=f[N1-1,0]
    w=w*bma/2.0
    x=0.5*((b+a)+N*bma*f[0:N1,1])

    return x,w
    