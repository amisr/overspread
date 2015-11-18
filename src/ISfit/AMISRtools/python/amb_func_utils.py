#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys
import os.path
import tables
import scipy, scipy.fftpack
import scipy.io
import glob
import pylab
import string

##############################

v_lightspeed=299792458

def conv2d(a, b):
    x = scipy.zeros((a.shape[0], a.shape[1]+b.shape[1]-1),dtype='float64')
    a = scipy.fliplr(a)
 
    for ii in range(a.shape[0]):
        x[ii,:] = scipy.convolve(a[ii,:], b[ii,:])

    return x

def baudexpand(x, baud):

    if baud.size==1:
        px = scipy.reshape(scipy.repeat(x[:,scipy.newaxis], baud, 0), (1,x.size*baud))
    else:
        px = scipy.reshape(baud*scipy.transpose(x), (1,x.size*baud.size))
        xxxx
        
    return px

def a4():

    codeset=[[1,1,1,1],
        [1,-1,1,1],
        [1,1,-1,1],
        [1,-1,-1,1],
        [1,1,1,-1],
        [1,-1,1,-1],
        [1,1,-1,-1],
        [1,-1,-1,-1]]
        
    return scipy.array(codeset)
	
def code88():
	codec=[1,-2,1,-5,3,-1,2,-2,1,-1,2,-3,1,-2,2,-1,1,-1,4,-1,1,-1,1,-1,3,-6,1,-2,1,-1,1,-6,1,-1,2,-2,3,-3,1,-1,2,-2,3,-3,1,-1]
	
	code=[]
	for ii in range(len(codec)):
		for jj in range(scipy.absolute(codec[ii])):
			code.append(scipy.sign(codec[ii]))
	
	return scipy.array(code)


def a2():
    # Calculates randomized set of A16 codes, strong condition
    #
    # Octal
    aoct = ['000','000','001']
    a=[]
    for ii in range(len(aoct)):
        a.append(string.atoi(aoct[ii],8))
    
    a2code=scipy.zeros(2,dtype='float64')
    signs=scipy.zeros((2,3),dtype='float64')
    for jpul in range(2):
        codetmp = 0
        signstmp = []
        for ibaud in range(len(a)):
            if walsh(a[ibaud],jpul)==1:
                signstmp.append(1)
                codetmp = codetmp + scipy.power(2,(len(aoct)-1-ibaud))
            else:
                signstmp.append(-1)
        a2code[jpul] = codetmp
        print signstmp
        signs[jpul,:] = signstmp

    return a2code,signs

def a16rand():
    # Calculates randomized set of A16 codes, strong condition
    #
    # Octal
    aoct = ['000','001','002','004','010','020','017','037','021','014','031','035','024','006','015','032']
    a=[]
    for ii in range(len(aoct)):
        a.append(string.atoi(aoct[ii],8))

    randomizer = [1,1,1,1,-1,-1,-1,1,-1,-1,1,-1,-1,1,-1,1]
    
    a16code=scipy.zeros(32,dtype='float64')
    signs=scipy.zeros((32,16),dtype='float64')
    for jpul in range(32):
        codetmp = 0
        signstmp = []
        for ibaud in range(16):
            if randomizer[ibaud]*walsh(a[ibaud],jpul)==1:
                signstmp.append(1)
                codetmp = codetmp + scipy.power(2,(15-ibaud))
            else:
                signstmp.append(-1)
        a16code[jpul] = codetmp
        signs[jpul,:] = signstmp

    return a16code,signs

# walsh
def walsh(i,j):
    # calculates elements of a walsh sign matrix for alternating codes
    bita = scipy.bitwise_and(i,j)
    s = 1
    for ibit in range(16):
        if scipy.bitwise_and(bita,scipy.power(2,ibit))>0:
            s = -s
    return s

# generate_barkercode
def generate_barkercode(leng,opt=0):
    # returns barker code pulse sequence

    if leng==1:
        c=[1]
    elif (leng==2) and (opt==0):
        c=[1,-1]
    elif (leng==2) and (opt==1):
        c=[1,1]
    elif leng==3:
        c=[1,1,-1]
    elif (leng==4) and (opt==0):
        c=[1,-1,1,1]
    elif leng==4 and (opt==1):
        c=[1,-1,-1,-1]
    elif leng==5:
        c=[1,1,1,-1,1]
    elif leng==7:
        c=[1,1,1,-1,-1,1,-1]
    elif leng==11:
        c=[1,1,1,-1,-1,-1,1,-1,-1,1,-1]
    elif leng==13:
        c=[1,1,1,1,1,-1,-1,1,1,-1,1,-1,1]
    elif leng==28:
        c=[1,1,-1,1,1,-1,1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,1,1,1,1,-1,-1,-1]

    return c

def multipulse(gaps, pattern=[]):

    mp=[]
    
    if len(pattern)==0:
        mp.append(1.0)
        for igap in range(len(gaps)):
            gap=gaps[igap]
            mp.extend(scipy.zeros(gap).tolist())
            mp.append(1.0)
        
    else: # pattern is repeated without extra breaks
        mp.extend(pattern)
        leng = len(pattern)
        for igap in range(len(gaps)):
            gap=gaps[igap]
            mp.extend(scipy.zeros(gap).tolist())
            mp.extend(pattern)
   
    return scipy.array(mp)

# mpulse
def dpulse(codeset, clen, baud, os, h, lags):
    #
    # computes a barker code range-lag ambiguity function
    #
    # codeset - coding of pulse
    # baud - baud length (microseconds)
    # os - oversampling factor
    # h - impulse response function

    baud=scipy.array(baud)
    
    print codeset
    xxxxx
    
    hbc=scipy.squeeze(scipy.fliplr(baudexpand(codeset, baud))) # this is the total impulse response of a Barker decoder with delta function impulse response
    env=scipy.squeeze(baudexpand(multipulse(pattern,codeset),baud)) # pulse transmission envelope 
    
    # sampling of barker code
    sc=scipy.zeros(hbc.shape,hbc.dtype)
    sc[::baud/os]=1
    hbc=hbc*sc

    # delta function
    hdelt=scipy.zeros(h.shape)
    hdelt[scipy.floor(hdelt.shape[0]/2.0)]=1.0
        
    # to get the total impulse response, convolve filter impulse response with decoder
    h=scipy.convolve(h,hbc)
    hDelt=scipy.convolve(hdelt,hbc)
    
    wta=scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
    wtaDelt=scipy.zeros((hDelt.shape[0],env.shape[0]+h.shape[0]),dtype='float32')

    for tau in range(h.shape[0]):
        wta[tau,tau:tau+env.shape[0]]=h[tau]*env
        wtaDelt[tau,tau:tau+env.shape[0]]=hDelt[tau]*env

    mlag=scipy.array(lags).max()

    wtt=scipy.zeros((baud*mlag+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
    wlag=scipy.zeros((len(lags),baud*mlag+2*h.shape[0]),dtype='float32')
    wrng=scipy.zeros((len(lags),env.shape[0]+h.shape[0]),dtype='float32')
    try:
        wttall=scipy.zeros((len(lags),baud*mlag+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
        comp_all=1
    except:
        print 'Unable to allocate memory for full, 3D ambiguity function'
        comp_all=0
        wttall=[]
        
    offs = scipy.arange(2*h.shape[0]-1)

    for ii in range(len(lags)):
        lgind=lags[ii]
        lag = baud*lgind
        print 'Lag-%d' % (lag)
        if lgind==0:
            tmp=scipy.transpose(conv2d(scipy.transpose(wta[:,lag:]), scipy.transpose(wta[:,0:])))
        else:
            tmp=scipy.transpose(conv2d(scipy.transpose(wta[:,lag:]), scipy.transpose(wta[:,0:-lag])))
        if comp_all:
            wttall[ii,lag+offs,lag:]=tmp
        else:
            wlag[ii,lag+offs]=wlag[ii,lag+offs]+scipy.sum(tmp,axis=1)
            wrng[ii,lag:]=wrng[ii,lag:]+scipy.sum(tmp,axis=0)
        tmp2=wtt[lag+offs,lag:]
        I=scipy.where(tmp>tmp2)
        tmp2[I]=tmp[I]
        wtt[lag+offs,lag:]=tmp2

    wttDelt=scipy.transpose(conv2d(scipy.transpose(wtaDelt), scipy.transpose(wtaDelt)))
    fact=scipy.sum(scipy.sum(wttDelt))
    
    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')

    wtt=wtt/fact #/(leng*leng*baud*os*os)
    wttall=wttall/fact #/(leng*leng*baud*os*os)

    # lag ambiguity function
    wlag=scipy.sum(wttall,axis=2)
    wlagsum=scipy.sum(wlag,axis=1)
    
    # range ambiguity function
    wrng=scipy.sum(wttall,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)

    comp_all=1

    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all

# mpulse
def mpulse(codeset, pattern, baud, os, h, lags):
    #
    # computes a barker code range-lag ambiguity function
    #
    # codeset - coding of pulse
    # pattern - multipulse pattern
    # baud - baud length (microseconds)
    # os - oversampling factor
    # h - impulse response function

    baud=scipy.array(baud)
    
    print multipulse(pattern,codeset)
    print os
    
    hbc=scipy.squeeze(scipy.fliplr(baudexpand(codeset, baud))) # this is the total impulse response of a Barker decoder with delta function impulse response
    env=scipy.squeeze(baudexpand(multipulse(pattern,codeset),baud)) # pulse transmission envelope 
    
    # sampling of barker code
    sc=scipy.zeros(hbc.shape,hbc.dtype)
    sc[::baud/os]=1
    hbc=hbc*sc

    # delta function
    hdelt=scipy.zeros(h.shape)
    hdelt[scipy.floor(hdelt.shape[0]/2.0)]=1.0
        
    # to get the total impulse response, convolve filter impulse response with decoder
    h=scipy.convolve(h,hbc)
    hDelt=scipy.convolve(hdelt,hbc)
    
    wta=scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
    wtaDelt=scipy.zeros((hDelt.shape[0],env.shape[0]+h.shape[0]),dtype='float32')

    for tau in range(h.shape[0]):
        wta[tau,tau:tau+env.shape[0]]=h[tau]*env
        wtaDelt[tau,tau:tau+env.shape[0]]=hDelt[tau]*env

    mlag=scipy.array(lags).max()

    wtt=scipy.zeros((baud*mlag+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
    wlag=scipy.zeros((len(lags),baud*mlag+2*h.shape[0]),dtype='float32')
    wrng=scipy.zeros((len(lags),env.shape[0]+h.shape[0]),dtype='float32')
    try:
        wttall=scipy.zeros((len(lags),baud*mlag+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
        comp_all=1
    except:
        print 'Unable to allocate memory for full, 3D ambiguity function'
        comp_all=0
        wttall=[]
        
    offs = scipy.arange(2*h.shape[0]-1)

    for ii in range(len(lags)):
        lgind=lags[ii]
        lag = baud*lgind
        print 'Lag-%d' % (lag)
        if lgind==0:
            tmp=scipy.transpose(conv2d(scipy.transpose(wta[:,lag:]), scipy.transpose(wta[:,0:])))
        else:
            tmp=scipy.transpose(conv2d(scipy.transpose(wta[:,lag:]), scipy.transpose(wta[:,0:-lag])))
        if comp_all:
            wttall[ii,lag+offs,lag:]=tmp
        else:
            wlag[ii,lag+offs]=wlag[ii,lag+offs]+scipy.sum(tmp,axis=1)
            wrng[ii,lag:]=wrng[ii,lag:]+scipy.sum(tmp,axis=0)
        tmp2=wtt[lag+offs,lag:]
        I=scipy.where(tmp>tmp2)
        tmp2[I]=tmp[I]
        wtt[lag+offs,lag:]=tmp2

    wttDelt=scipy.transpose(conv2d(scipy.transpose(wtaDelt), scipy.transpose(wtaDelt)))
    fact=scipy.sum(scipy.sum(wttDelt))
    
    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')

    wtt=wtt/fact #/(leng*leng*baud*os*os)
    wttall=wttall/fact #/(leng*leng*baud*os*os)

    # lag ambiguity function
    wlag=scipy.sum(wttall,axis=2)
    wlagsum=scipy.sum(wlag,axis=1)
    
    # range ambiguity function
    wrng=scipy.sum(wttall,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)

    comp_all=1

    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all


# barker_code
def coh_code(codeset, baud, os, h):
    #
    # computes a barker code range-lag ambiguity function
    #
    # leng - barker code lenggth
    # baud - baud length (microseconds)
    # os - oversampling factor
    # h - impulse response function

    baud=scipy.array(baud)

#    codeset=scipy.array(generate_barkercode(leng,opt=type)) # generate barker code
    
    hbc=scipy.squeeze(scipy.fliplr(baudexpand(codeset, baud))) # this is the total impulse response of a Barker decoder with delta function impulse response
    env=scipy.flipud(hbc) # pulse transmission envelope 
    
    # sampling of barker code
    sc=scipy.zeros(hbc.shape,hbc.dtype)
    sc[::baud/os]=1
    hbc=hbc*sc

    # delta function
    hdelt=scipy.zeros(h.shape)
    hdelt[scipy.floor(hdelt.shape[0]/2.0)]=1.0
        
    # to get the total impulse response, convolve filter impulse response with decoder
    h=scipy.convolve(h,hbc)
    hDelt=scipy.convolve(hdelt,hbc)
    
    wta=scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float64')
    wtaDelt=scipy.zeros((hDelt.shape[0],env.shape[0]+h.shape[0]),dtype='float64')

    for tau in range(h.shape[0]):
        wta[tau,tau:tau+env.shape[0]]=h[tau]*env
        wtaDelt[tau,tau:tau+env.shape[0]]=hDelt[tau]*env
    
    wtt=scipy.transpose(conv2d(scipy.transpose(wta), scipy.transpose(wta)))
    wttall=wtt[scipy.newaxis,:,:]

    wttDelt=scipy.transpose(conv2d(scipy.transpose(wtaDelt), scipy.transpose(wtaDelt)))
    fact=scipy.sum(scipy.sum(wttDelt))

    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')

    wtt=wtt/fact #/(leng*leng*baud*os*os)
    wttall=wttall/fact #/(leng*leng*baud*os*os)

    # lag ambiguity function
    wlag=scipy.sum(wttall,axis=2)
    wlagsum=scipy.sum(wlag,axis=1)
    
    # range ambiguity function
    wrng=scipy.sum(wttall,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)

    comp_all=1

    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all


# barker_code
def barker_code(leng, baud, os, h, Fs, type=0):
    #
    # computes a barker code range-lag ambiguity function
    #
    # leng - barker code lenggth
    # baud - baud length (microseconds)
    # os - oversampling factor
    # h - impulse response function
    # Fs - nyquist
	# type - in cases where there are two BCs

    baud=scipy.array(baud)

    codeset=scipy.array(generate_barkercode(leng,opt=type)) # generate barker code
    
    hbc=scipy.squeeze(scipy.fliplr(baudexpand(codeset, baud))) # this is the total impulse response of a Barker decoder with delta function impulse response
    env=scipy.flipud(hbc) # pulse transmission envelope 

    """
    # messed up pulse (additional 30us on end of transmit pulse)
    tcodeset=scipy.concatenate((codeset,scipy.ones(3)))  
    env=scipy.squeeze(baudexpand(tcodeset, baud))
    """
    
    # sampling of barker code
    sc=scipy.zeros(hbc.shape,hbc.dtype)
    sc[::baud/os]=1
    hbc=hbc*sc/10.0
    print scipy.sum(hbc)

    # delta function
    hdelt=scipy.zeros(h.shape)
    hdelt[scipy.floor(hdelt.shape[0]/2.0)]=1.0
    
    BWfilt=Fs*scipy.sum(h**2.0)/scipy.sum(h)**2.0
    print BWfilt
    
    tpp=scipy.absolute(scipy.fftpack.fftshift(scipy.fftpack.fft(h)))**2.0
    tf=scipy.linspace(-Fs,Fs,tpp.shape[0])
    df=scipy.median(scipy.diff(tf))
    
    BWfilt=0.5*df*scipy.sum(tpp)**2.0/scipy.sum(tpp**2.0)
    print BWfilt
    
#    pylab.plot(tf,10.0*scipy.log10(tpp/tpp.max()))        
                  
    # to get the total impulse response, convolve filter impulse response with decoder
    h=scipy.convolve(h,hbc)
    hDelt=scipy.convolve(hdelt,hbc)
    
    tp=scipy.absolute(scipy.fftpack.fftshift(scipy.fftpack.fft(h)))**2.0
    tf=scipy.linspace(-Fs,Fs,tp.shape[0])
    df=scipy.median(scipy.diff(tf))
    
 #   pylab.plot(tf,10.0*scipy.log10(tp/tp.max()))        
    
    BW=Fs*scipy.sum(h**2.0)/scipy.sum(h)**2.0
    print BW

    BWfilt=0.5*df*scipy.sum(tp)**2.0/scipy.sum(tp**2.0)
    print BWfilt

    xxxx
    
    wta=scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float64')
    wtaDelt=scipy.zeros((hDelt.shape[0],env.shape[0]+h.shape[0]),dtype='float64')

    for tau in range(h.shape[0]):
        wta[tau,tau:tau+env.shape[0]]=h[tau]*env
        wtaDelt[tau,tau:tau+env.shape[0]]=hDelt[tau]*env
    
    wtt=scipy.transpose(conv2d(scipy.transpose(wta), scipy.transpose(wta)))
    wttall=wtt[scipy.newaxis,:,:]

    wttDelt=scipy.transpose(conv2d(scipy.transpose(wtaDelt), scipy.transpose(wtaDelt)))
    fact=scipy.sum(scipy.sum(wttDelt))

    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')

    wtt=wtt/fact #/(leng*leng*baud*os*os)
    wttall=wttall/fact #/(leng*leng*baud*os*os)

    # lag ambiguity function
    wlag=scipy.sum(wttall,axis=2)
    wlagsum=scipy.sum(wlag,axis=1)
    
    # range ambiguity function
    wrng=scipy.sum(wttall,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)

    comp_all=1

    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all, BWfilt, BW

# alt_code_phaseerr
def alt_code_phaserr(codeset, baud, h, lags):
    #
    # computes an alternating code range-lag ambiguity function for the case of a coding error
    #
    # codeset - sign sequence for the alternating code (should be nbaud x nscans size)
    # baud - baud length (microseconds)
    # h - impulse response function
    # lags - lags that will be computed, in units of baud

    [nbaud, scancount] = codeset.shape
    baud=scipy.array(baud)

    cwta=[]
    for ii in range(scancount):
        env=baudexpand(codeset[:,ii], baud)
        
        # introduce the errors
        env2=env.copy()
        III=scipy.where(codeset[:,ii]==-1)[0]
#       print III
        for aa in range(len(III)):
            II=III[aa]
            env2[0,II*baud+2./3.*baud:(II+1)*baud]=1
#       pylab.plot(scipy.squeeze(env2),'-r')
#       pylab.hold(1)
#       pylab.plot(scipy.squeeze(env),'-k')
#       xxx
        env=scipy.squeeze(scipy.fliplr(env2))
        
        cwta.append(scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float64'))
    
        for tau in range(h.shape[0]):
            cwta[ii][tau,tau:tau+env.shape[0]] = h[tau]*env

    wtt=scipy.zeros((baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float64')
    wttall=scipy.zeros((len(lags),baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0],nbaud),dtype='float32')

    offs = scipy.arange(2*h.shape[0]-1)

    for iii in range(len(lags)):
        lgind=lags[iii]
        cwtt=[]
        for ii in range(scancount):
            lag = baud*lgind;
            print 'Lag-%d' % (lag)
            if lgind==0:
                tmp=scipy.transpose(conv2d(scipy.transpose(cwta[ii][:,lag:]), scipy.transpose(cwta[ii][:,0:])))
                wtt[offs,:]+=tmp
                wttall[iii,offs,:,0] = wtt[offs,:]+tmp
            else:
                tmp=scipy.transpose(conv2d(scipy.transpose(cwta[ii][:,lag:]), scipy.transpose(cwta[ii][:,0:-lag])))
            cwtt.append(tmp)

        if lgind != 0:
            # decode alt. codes
            for jj in range(nbaud-lgind):
                decode = codeset[jj,:]*codeset[jj+lgind,:]
                wtmp = scipy.zeros(cwtt[0].shape)
                for ii in range(scancount):
                    wtmp+=decode[ii]*cwtt[ii]

                # wtt contains the maximum value
                wttall[iii,lag+offs,lag:,jj]=wtmp.copy()
                tmp2=wtt[lag+offs,lag:]
                I=scipy.where(wtmp>tmp2)
                tmp2[I]=wtmp[I]
                wtt[lag+offs,lag:] = tmp2           

    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')
    
    wtt=wtt/(nbaud*scancount*baud)
    wttall=wttall/(nbaud*scancount*baud)
    
    # lag ambiguity function
    wlag=scipy.sum(scipy.sum(wttall,axis=3),axis=2)
    wlagsum=scipy.sum(wlag,axis=1)
        
    tmp=scipy.zeros((wttall.shape[0],wttall.shape[1],wttall.shape[2]+baud*nbaud),dtype='float32')
    for ii in range(wttall.shape[3]):
        tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],ii*baud),dtype='float32'),wttall[:,:,:,ii],scipy.zeros((wttall.shape[0],wttall.shape[1],baud*nbaud-ii*baud),dtype='float32')),axis=2)  
#       if ii==0:
#           tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*nbaud),dtype='float32'),wttall[:,:,:,ii]),axis=2)
#       else:
#           tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:-baud*ii,ii]),axis=2)
    wttall=tmp
    
    wtt=scipy.concatenate((wtt,scipy.zeros((wtt.shape[0],baud*nbaud),dtype='float32')),axis=1)
    r=scipy.arange(wtt.shape[1],dtype='float64')

#   tmp=scipy.zeros((wttall.shape[0],wttall.shape[1],wttall.shape[2]),dtype='float32')
#   for ii in range(wttall.shape[3]):
#       if ii==0:
#           tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:,ii]),axis=2)
#       else:
#           tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:-baud*ii,ii]),axis=2)
#   wttall=tmp

    # range ambiguity function
    wrng=scipy.sum(wttall,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)
    
    comp_all=1
    
    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all

# alt_code_fraclag
def alt_code_fraclag(codeset, baud, fraction, h, lags):
    #
    # computes an alternating code range-lag ambiguity function
    #
    # codeset - sign sequence for the alternating code (should be nbaud x nscans size)
    # baud - baud length (microseconds)
    # fraction - fractional lag
    # h - impulse response function
    # lags - lags that will be computed, in units of baud

    [nbaud, scancount] = codeset.shape

    newcodeset=scipy.zeros((codeset.shape[0]*fraction,codeset.shape[1]))
    for aa in range(codeset.shape[0]):
        newcodeset[aa*fraction:aa*fraction+fraction,:]=codeset[aa,:]

    nx=2*fraction-1
    decodeset=scipy.zeros(((nbaud-1)*nx,(nbaud-1)*nx+1,newcodeset.shape[1]))
    lagmat=scipy.zeros((nbaud-1)*nx+1)
    for aa in range(decodeset.shape[2]):
        for bb in range(1,codeset.shape[0]):
            sc=codeset[bb:,aa]*codeset[:-bb,aa]
            sc=scipy.repeat(sc[:,scipy.newaxis],nx,axis=1)
            decodeset[range(0,newcodeset.shape[0]-bb*fraction,fraction),1+(bb-1)*nx:1+(bb)*nx,aa]=sc
            lagmat[1+(bb-1)*nx:1+(bb)*nx]=scipy.arange((bb-1)*fraction+(fraction-(fraction-1)),(bb-1)*fraction+(fraction+(fraction+1))-1)
    
    codeset=newcodeset
    baud=baud/fraction
    
    [nbaud, scancount] = codeset.shape
    baud=scipy.array(baud)
    
    cwta=[]
    for ii in range(scancount):
        env=scipy.squeeze(scipy.fliplr(baudexpand(codeset[:,ii], baud)))
        cwta.append(scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float64'))
    
        for tau in range(h.shape[0]):
            cwta[ii][tau,tau:tau+env.shape[0]] = h[tau]*env

    wtt=scipy.zeros((baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float64')
    wlag=scipy.zeros((len(lags),baud*len(lags)+2*h.shape[0]),dtype='float32')
    wrng=scipy.zeros((len(lags),env.shape[0]+h.shape[0]),dtype='float32')    
    try:
        wttall=scipy.zeros((len(lags),baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0],nbaud),dtype='float32')
        comp_all=1
    except:
        print 'Unable to allocate memory for full, 3D ambiguity function'
        comp_all=0
        wttall=[]	

    offs = scipy.arange(2*h.shape[0]-1)

    for iii in range(lagmat.shape[0]):
        lgind=int(lagmat[iii])
        cwtt=[]
        for ii in range(scancount):
            lag = baud*lgind;
            print 'Lag-%d' % (lag)
            if lgind==0:
                tmp=scipy.transpose(conv2d(scipy.transpose(cwta[ii][:,lag:]), scipy.transpose(cwta[ii][:,0:])))
                wtt[offs,:]+=tmp
                if comp_all:
                    wttall[lgind,offs,:,0] = wtt[offs,:]+tmp
                else:
                    wlag[lgind,lag+offs]=wlag[lgind,lag+offs]+scipy.sum(tmp,axis=1)
                    wrng[lgind,lag:]=wrng[lgind,lag:]+scipy.sum(tmp,axis=0)                    
            else:
                tmp=scipy.transpose(conv2d(scipy.transpose(cwta[ii][:,lag:]), scipy.transpose(cwta[ii][:,0:-lag])))
            cwtt.append(tmp)

        if lgind != 0:
            # decode alt. codes
            for jj in range(nbaud-lgind):
            
                decode=decodeset[jj,iii,:]
                            
                wtmp = scipy.zeros(cwtt[0].shape)
                for ii in range(scancount):
                    wtmp+=decode[ii]*cwtt[ii]

                # wtt contains the maximum value
                if comp_all:
                    wttall[lgind,lag+offs,lag:,jj]=wttall[lgind,lag+offs,lag:,jj]+wtmp.copy()
                else:
                    wlag[lgind,lag+offs]=wlag[lgind,lag+offs]+scipy.sum(wtmp,axis=1)
                    wrng[lgind,lag:]=wrng[lgind,lag:]+scipy.sum(wtmp,axis=0)
                tmp2=wtt[lag+offs,lag:]
                I=scipy.where(wtmp>tmp2)
                tmp2[I]=wtmp[I]
                wtt[lag+offs,lag:] = tmp2           
#           raw_input("Enter something: ")

    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')
    
    wtt=wtt/(nbaud*scancount*baud)
        
    if comp_all:
        wttall=wttall/(nbaud*scancount*baud)
        tmp=scipy.zeros((wttall.shape[0],wttall.shape[1],wttall.shape[2]),dtype='float32')
        for ii in range(wttall.shape[3]):
            if ii==0:
                tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:,ii]),axis=2)
            else:
                tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:-baud*ii,ii]),axis=2)
        wttall=tmp
    
    # range and lag ambiguity function
    if comp_all:
        wlag=scipy.sum(scipy.sum(wttall,axis=3),axis=2)
        wrng=scipy.sum(wttall,axis=1)
    else:
        wlag=wlag/(nbaud*scancount*baud)
        wrng=wrng/(nbaud*scancount*baud)
    wlagsum=scipy.sum(wlag,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)
    
    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all


# alt_code
def alt_code(codeset, baud, h, lags):
    #
    # computes an alternating code range-lag ambiguity function
    #
    # codeset - sign sequence for the alternating code (should be nbaud x nscans size)
    # baud - baud length (microseconds)
    # h - impulse response function
    # lags - lags that will be computed, in units of baud

    [nbaud, scancount] = codeset.shape
    baud=scipy.array(baud)

    cwta=[]
    for ii in range(scancount):
        env=scipy.squeeze(scipy.fliplr(baudexpand(codeset[:,ii], baud)))
        cwta.append(scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float64'))
    
        for tau in range(h.shape[0]):
            cwta[ii][tau,tau:tau+env.shape[0]] = h[tau]*env

    wtt=scipy.zeros((baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float64')
    wttall=scipy.zeros((len(lags),baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0],nbaud),dtype='float32')

    offs = scipy.arange(2*h.shape[0]-1)

    for iii in range(len(lags)):
        lgind=lags[iii]
        cwtt=[]
        for ii in range(scancount):
            lag = baud*lgind;
            print 'Lag-%d' % (lag)
            if lgind==0:
                tmp=scipy.transpose(conv2d(scipy.transpose(cwta[ii][:,lag:]), scipy.transpose(cwta[ii][:,0:])))
                wtt[offs,:]+=tmp
                wttall[iii,offs,:,0] = wtt[offs,:]+tmp
            else:
                tmp=scipy.transpose(conv2d(scipy.transpose(cwta[ii][:,lag:]), scipy.transpose(cwta[ii][:,0:-lag])))
            cwtt.append(tmp)

        if lgind != 0:
            # decode alt. codes
            for jj in range(nbaud-lgind):
                decode = codeset[jj,:]*codeset[jj+lgind,:]
                wtmp = scipy.zeros(cwtt[0].shape)
                for ii in range(scancount):
                    wtmp+=decode[ii]*cwtt[ii]

                # wtt contains the maximum value
                wttall[iii,lag+offs,lag:,jj]=wtmp.copy()
                tmp2=wtt[lag+offs,lag:]
                I=scipy.where(wtmp>tmp2)
                tmp2[I]=wtmp[I]
                wtt[lag+offs,lag:] = tmp2           

    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')

    wtt=wtt/(nbaud*scancount*baud)
    wttall=wttall/(nbaud*scancount*baud)

    # lag ambiguity function
    wlag=scipy.sum(scipy.sum(wttall,axis=3),axis=2)
    wlagsum=scipy.sum(wlag,axis=1)

    tmp=scipy.zeros((wttall.shape[0],wttall.shape[1],wttall.shape[2]),dtype='float32')
    for ii in range(wttall.shape[3]):
        if ii==0:
            tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:,ii]),axis=2)
        else:
            tmp+=scipy.concatenate((scipy.zeros((wttall.shape[0],wttall.shape[1],baud*ii),dtype='float32'),wttall[:,:,:-baud*ii,ii]),axis=2)
    wttall=tmp
    
    # range ambiguity function
    wrng=scipy.sum(wttall,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)
    
    comp_all=1

    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all

# long_pulse
def long_pulse(nbaud, baud, h, lags):
    #
    # computes a long pulse range-lag ambiguity function
    #
    # nbaud - number of bauds
    # baud - baud length (microseconds)
    # h - impulse response function
    # lags - lags that will be computed, in units of baud

    # pulse envelope
    env=scipy.ones((nbaud*baud),dtype='float32')

    wta=scipy.zeros((h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')

    for tau in range(h.shape[0]):
        wta[tau,tau:tau+env.shape[0]]=h[tau]*env
    
    wtt=scipy.zeros((baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
    wlag=scipy.zeros((len(lags),baud*len(lags)+2*h.shape[0]),dtype='float32')
    wrng=scipy.zeros((len(lags),env.shape[0]+h.shape[0]),dtype='float32')
    try:
        wttall=scipy.zeros((len(lags),baud*len(lags)+2*h.shape[0],env.shape[0]+h.shape[0]),dtype='float32')
        comp_all=1
    except:
        print 'Unable to allocate memory for full, 3D ambiguity function'
        comp_all=0
        wttall=[]

    offs = scipy.arange(2*h.shape[0]-1)

    for ii in range(len(lags)):
        lgind=lags[ii]
        lag = baud*lgind
        print 'Lag-%d' % (lag)
        if lgind==0:
            tmp=scipy.transpose(conv2d(scipy.transpose(wta[:,lag:]), scipy.transpose(wta[:,0:])))
        else:
            tmp=scipy.transpose(conv2d(scipy.transpose(wta[:,lag:]), scipy.transpose(wta[:,0:-lag])))
        if comp_all:
            wttall[ii,lag+offs,lag:]=tmp
        else:
            wlag[ii,lag+offs]=wlag[ii,lag+offs]+scipy.sum(tmp,axis=1)
            wrng[ii,lag:]=wrng[ii,lag:]+scipy.sum(tmp,axis=0)
        tmp2=wtt[lag+offs,lag:]
        I=scipy.where(tmp>tmp2)
        tmp2[I]=tmp[I]
        wtt[lag+offs,lag:]=tmp2

    tau=scipy.arange(wtt.shape[0],dtype='float64')-h.shape[0]+1
    r=scipy.arange(wtt.shape[1],dtype='float64')
    
    wtt=wtt/(nbaud*baud)
    if comp_all:
        wttall=wttall/(nbaud*baud)
        wlag=scipy.sum(wttall,axis=2)
        wrng=scipy.sum(wttall,axis=1)
    else:
        wlag=wlag/(nbaud*baud)
        wrng=wrng/(nbaud*baud)
    
    wlagsum=scipy.sum(wlag,axis=1)
    wrngsum=scipy.sum(wrng,axis=1)
    
    
    return tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all


# plot_lamb
def plot_lamb(x,y,wtt,wttall,wlag,wrng,plotall=1,plotson=0,plotdir='',lags=[]):

    if plotson:
        if not os.path.exists(plotdir):
            try: 
                os.mkdir(plotdir)
            except:
                plotson=0

    labsize=12

    figg=pylab.figure()
    pylab.plot(x,scipy.transpose(wlag))
    ax=pylab.gca()
    ax.set_ylabel('Scaling', fontsize=labsize)
    ax.set_xlabel(r"$\tau \ (\mu s)$", fontsize=labsize)
    ax.set_title('Lag Ambiguity Function', fontsize=labsize, horizontalalignment='center')
    if plotson:
        oname='wlag.png'
        figg.savefig(os.path.join(plotdir,oname))

    figg=pylab.figure()
    pylab.plot(y,scipy.transpose(wrng))
    ax=pylab.gca()
    ax.set_ylabel('Scaling', fontsize=labsize)
    ax.set_xlabel(r"$Range \ (\mu s)$", fontsize=labsize)
    ax.set_title('Range Ambiguity Function', fontsize=labsize, horizontalalignment='center')
    if plotson:
        oname='wrng.png'
        figg.savefig(os.path.join(plotdir,oname))
    
    figg=pylab.figure()
    pylab.imshow(scipy.transpose(wtt),origin='lower',extent=[x.min(),x.max(),y.min(),y.max()])
    ax=pylab.gca()
    ax.set_ylabel(r"$Range \ (\mu s)$", fontsize=labsize)
    ax.set_xlabel(r"$\tau \ (\mu s)$", fontsize=labsize)
    ax.set_title('Full 2d Ambiguity Function', fontsize=labsize, horizontalalignment='center')
    pylab.colorbar()
    if plotson:
        oname='wtt.png'
        figg.savefig(os.path.join(plotdir,oname))
    
    pylab.show()
    
    if plotall:
        for aa in range(len(lags)):
            if aa==0:
                figg=pylab.figure()
            else:
                pylab.clf()
            if len(lags)==0:
                lag=aa
            else:
                lag=lags[aa]
            pylab.imshow(scipy.transpose(wttall[aa,:,:]),origin='lower',extent=[x.min(),x.max(),y.min(),y.max()])
            ax=pylab.gca()
            ax.set_ylabel(r"$Range \ (\mu s)$", fontsize=labsize)
            ax.set_xlabel(r"$\tau \ (\mu s)$", fontsize=labsize)
            ax.set_title('2d Ambiguity Function, Lag %d' % (lag), fontsize=labsize, horizontalalignment='center')
            pylab.colorbar()
            if plotson:
                oname='wtt-lag%d.png' % (lag)
                figg.savefig(os.path.join(plotdir,oname))
            pylab.show()
    
    return
    
def write_outputfile(fhandle,dict2do,keys2do=[],groupname='',name=''):

    if groupname == '':
        group=fhandle.root
    else:
        if fhandle.groups.__contains__('/'+groupname):
            group='/'+groupname
        else:
            group=fhandle.createGroup(fhandle.root, groupname, 'Dataset')

    if len(keys2do)==0:
        try:
            fhandle.removeNode(group,name)
        except:
            ''
        fhandle.createArray(group,name, dict2do, "Dataset")
    else:
        for key in keys2do:
            fhandle.createArray(group, key, dict2do[key], "Dataset")
    
    return      
    
# compute_lamb
def compute_lamb(type,hfile,MCIC,in1,in2,outdir,lags=[]):
    #
    # desc
    #
    # type - pulse type (0=Long Pulse, 1=Alternating Code, 2=Alternating Code with phase errs, 3=Barker Code, 4=Fractional Lag Alternating Code)
    # hfile - file name containing the impulse response function (time spacing should be one microsecond)
    # lags - lags that will be computed, in units of baud (default is all)
    # MCIC - length 2 list containing the decimation factors
    #
    # type-0:
    #   in1 - number of samples per pulse
    #   in2 - [sample time (microseconds)]
    # type-1:
    #   in1 - codeset
    #   in2 - [baud length (microseconds)]
    # type-2:
    #   in1 - codeset
    #   in2 - [baud length (microseconds)]
    # type-3:
    #   in1 - code length
    #   in2 - [baud length (microseconds) oversampling factor]
    # type-4:
    #   in1 - code length
    #   in2 - [baud length(microseconds) fraction]
                
    # get impulse response function
    file=open(hfile,'r')
    h=scipy.io.read_array(file)
    file.close()
    
    if len(MCIC)!=2:
        print 'MCIC must be length 2'
        sys.exit(-1)
    else:
        dt=float(MCIC[0])*float(MCIC[1])/50.0

    out1=[]
    # Long Pulse
    if type==0:
        name1='nSamples'
        name2=['SampleTime']
        typedesc='Long Pulse'
        if len(lags)==0:
            lags=range(in1)
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all = long_pulse(in1, int(float(in2[0])/dt), h, lags)
        PulseLength=float(in1)*float(in2[0])
        LagSp=int(float(in2[0])/dt)
    # Alternating Code
    elif type==1:
        name1='Codeset'
        name2=['BaudLength']
        typedesc='Alternating Code'
        if len(lags)==0:
            lags=range(in1.shape[0])
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum,comp_all = alt_code(in1, int(float(in2[0])/dt), h, lags)
        PulseLength=float(in1.shape[0])*float(in2[0])
        LagSp=int(float(in2[0])/dt)
    # Alternating Code with Phase Errors
    elif type==2:
        name1='Codeset'
        name2=['BaudLength']
        typedesc='Alternating Code with Coding Error'
        if len(lags)==0:
            lags=range(in1.shape[0])
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all = alt_code_phaserr(in1, int(float(in2[0])/dt), h, lags)
        PulseLength=float(in1.shape[0])*float(in2[0])
        LagSp=int(float(in2[0])/dt)
    # Barker Code
    elif type==3:
        name1='Length'
        if len(in2)==2:
            name2=['BaudLength', 'OversamplingFactor']
            ctype=0
        elif len(in2)==3:
            name2=['BaudLength', 'OversamplingFactor', 'CodeType']
            ctype=in2[2]
        typedesc='Barker Code'
        lags=[0]
        Fs=0.5/dt*1.0e6
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all, BWfilt, BW = barker_code(in1, int(float(in2[0])/dt), int(in2[1]), h, Fs, type=ctype)
        PulseLength=float(in1)*float(in2[0])
        LagSp=int(float(in2[0])/dt)
        out1=[BWfilt,BW]
        nameOut1=['FilterBandwidth', 'Bandwidth']
    # Fractional Lag Alternating Code
    elif type==4:
        name1='Codeset'
        name2=['BaudLength','Fraction']
        typedesc='Fractional Lag Alternating Code'
        if len(lags)==0:
            lags=range(in1.shape[0]*in2[1])
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum,comp_all = alt_code_fraclag(in1, int(float(in2[0])/dt), in2[1], h, lags)
        PulseLength=float(in1.shape[0])*float(in2[0])
        LagSp=int(float(in2[0])/dt)
    # Coherent Code
    elif type==5:    
        name1='Codeset'
        if len(in2)==2:
            name2=['BaudLength', 'OversamplingFactor']
            ctype=0
        elif len(in2)==3:
            name2=['BaudLength', 'OversamplingFactor', 'CodeType']
            ctype=in2[2]
        typedesc='Barker Code'
        lags=0
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all = coh_code(in1, int(float(in2[0])/dt), int(in2[1]), h)
        PulseLength=float(in1.shape[0])*float(in2[0])
        LagSp=int(float(in2[0])/dt)
    # Multi pulse
    elif type==6:
        name1='Codeset'
        name2=['Baudlength', 'OversamplingFactor', 'Pattern']
        typedesc='MultiPulse'
        if len(lags)==0:
            lags=range(in1.shape[0])    
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all = mpulse(in1, in2[2], int(float(in2[0])/dt), int(in2[1]), h, lags)
        PulseLength=float(in1.shape[0])*float(in2[0])
        LagSp=int(float(in2[0]))
        print LagSp
    # Double pulse
    elif type==7:
        name1='Codeset'
        name2=['Baudlength', 'OversamplingFactor', 'CodeLength']
        typedesc='MultiPulse'
        if len(lags)==0:
            lags=range(in1.shape[0])    
        tau, r, wttall, wtt, wlag, wlagsum, wrng, wrngsum, comp_all = dpulse(in1, in2[2], int(float(in2[0])/dt), int(in2[1]), h, lags)
        PulseLength=float(in1.shape[0])*float(in2[0])
        LagSp=int(float(in2[0]))
        print LagSp		
    
    in2[0]=in2[0]*1e-6

    tau=tau*dt
    r=r*dt
            
    # write output files
    outname=os.path.join(outdir,'AmbFuncFull.h5')
    outh5file=tables.openFile(outname, mode = "w", title = "Ambiguity Function File")
    write_outputfile(outh5file,tau.astype('float64')*1e-6,groupname='',name='Delay')
    write_outputfile(outh5file,r.astype('float64')*v_lightspeed/2.0*1e-6,groupname='',name='Range')
    if comp_all:
        write_outputfile(outh5file,wttall,groupname='',name='WttAll')
    write_outputfile(outh5file,wtt,groupname='',name='Wtt')
    write_outputfile(outh5file,wlag,groupname='',name='Wlag')
    write_outputfile(outh5file,wlagsum,groupname='',name='WlagSum') 
    write_outputfile(outh5file,wrng,groupname='',name='Wrange')
    write_outputfile(outh5file,wrngsum,groupname='',name='WrangeSum')   
    write_outputfile(outh5file,in1,groupname='',name=name1)
    for aa in range(len(in2)):
        write_outputfile(outh5file,in2[aa],groupname='',name=name2[aa])
    for aa in range(len(out1)):
        write_outputfile(outh5file,out1[aa],groupname='',name=nameOut1[aa])
    write_outputfile(outh5file,type,groupname='',name='Type')
    write_outputfile(outh5file,typedesc,groupname='',name='TypeDesc')
    write_outputfile(outh5file,hfile,groupname='',name='FilterFile')
    write_outputfile(outh5file,scipy.asarray(MCIC),groupname='',name='MCIC')
    write_outputfile(outh5file,dt*1e-6,groupname='',name='dt')
    write_outputfile(outh5file,scipy.array(lags).astype('float64')*float(LagSp)*1e-6,groupname='',name='Lags')
    write_outputfile(outh5file,PulseLength*1e-6,groupname='',name='PulseLength')
    outh5file.close()
    outname=os.path.join(outdir,'AmbFunc.h5')
    outh5file=tables.openFile(outname, mode = "w", title = "Ambiguity Function File")
    write_outputfile(outh5file,tau.astype('float64')*1e-6,groupname='',name='Delay')
    write_outputfile(outh5file,r.astype('float64')*v_lightspeed/2.0*1e-6,groupname='',name='Range')
    write_outputfile(outh5file,wlag,groupname='',name='Wlag')
    write_outputfile(outh5file,wlagsum,groupname='',name='WlagSum') 
    write_outputfile(outh5file,wrng,groupname='',name='Wrange')
    write_outputfile(outh5file,wrngsum,groupname='',name='WrangeSum')   
#    write_outputfile(outh5file,in1,groupname='',name=name1)
#    for aa in range(len(in2)):
#        write_outputfile(outh5file,in2[aa],groupname='',name=name2[aa])
    for aa in range(len(out1)):
        write_outputfile(outh5file,out1[aa],groupname='',name=nameOut1[aa])
#    write_outputfile(outh5file,type,groupname='',name='Type')
#    write_outputfile(outh5file,typedesc,groupname='',name='TypeDesc')
#    write_outputfile(outh5file,hfile,groupname='',name='FilterFile')
#    write_outputfile(outh5file,MCIC,groupname='',name='MCIC')
#    write_outputfile(outh5file,dt*1e-6,groupname='',name='dt')
    write_outputfile(outh5file,scipy.array(lags).astype('float64')*float(LagSp)*1e-6,groupname='',name='Lags')
#    write_outputfile(outh5file,PulseLength*1e-6,groupname='',name='PulseLength')
    outh5file.close()

    
    # make plots    
    plot_lamb(tau,r,wtt,wttall,wlag,wrng,plotson=1,plotdir=os.path.join(outdir,'plots'),plotall=comp_all,lags=lags)
    
    return tau,r,wttall,wtt,wlag,wrng
