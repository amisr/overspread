#! /usr/bin/env python

"""

"""

import sys
import scipy
import time

sys.path.append('/Users/mnicolls/Documents/Work/ISfit')
from amisr_py.constants.constants import *
import spec_utils

def testspec():

    import matplotlib
    matplotlib.use('TkAgg'); matplotlib.interactive(True)
    import pylab

    input={}
    input['Nion'] = 2
    input['mi'] = [1.0,16.0]
    input['B']=50000e-9 # B field (T)
    input['f0']=450.0e6 # Frequency (Hz)
    input['te']=1000.0	# Electron temperature (K)
    input['alpha']=0.0 # aspect angle (degrees)
    input['ne']=1.0e10	# Ne (m^-3)
    input['ti']=[1000.0,1000.0] # Ti
    input['ni']=[0.0,1.0] # ion fraction
    input['nuin']=[0.0,0.0] # ion-neutral collision frequency
    input['nuen']=0.0
    input['vi']=[0.0,1000.0]
    input['ve']=1000.0
    k=2.0*pi/v_lightspeed*(2.0*input['f0'])
    ld = 2.0*pi/k 

    iss = ISspec(input,emode=[1,0,1],imode=[0,0,1],fmax=2e4,Nfreq=100)
    
    x=time.time()
    (f1,spec1,tau1,acf1) = iss.something()
    print time.time()-x

    iss = ISspec(input,emode=[1,0,1],imode=[0,0,1])

    x=time.time()
    (f1,spec1,tau1,acf1) = iss.something()
    print time.time()-x
    
    pylab.figure()
    pylab.plot(f1,spec1,'b')
    pylab.show()
    
    pylab.figure()
    pylab.plot(tau1,acf1,'b')
    pylab.show()

    return iss
    
    
class ISspec:        

    Amu2Ion = {'1':'H+','4':'He+','12':'C+','14':'N+','16':'O+','29':'CO+','28':'N2+','30':'NO+','32':'O2+','44':'CO2+'}

    def __init__(self,input,emode=[0,0,0],imode=[0,0,0],fmax=-1.0,Nfreq=100,normalize=0,override=0):
        
        """
        
        Inputs:
            input: A dictionary with the following entries
                input['Nion'] - # of ions
                input['mi'] - list of ion masses in amu, e.g. [1.0,16.0,4.0], length Nion
                input['B'] - background magnetic field values in Tesla
                input['f0'] - Frequency in Hz
                input['te'] - Electron temperature in K
                input['alpha'] - aspect angle in degrees (90 for perpendicular to B)
                input['ne'] - electron density in m^-3
                input['ti'] - list of Ti values, e.g. [1000.0,1000.0,1000.0], length Nion
                input['ni'] - list of ion fractions, e.g. [0.0, 1.0, 0.0], length Nion, must add to 1
                input['ve'] - electron speed, m/s
                input['vi'] - list of ion speeds, m/s, e.g. [0.0, 0.0, 0.0], length Nion
                input['nuen'] - electron-neutral collision frequency, s^-1
                input['nuin'] - list of ion neutral collision frequencies, s^-1, e.g. [0.0, 0.0, 0.0], length Nion
            emode, imode: flags for Gordeyev integral, 0=off, 1=on
                [0] - Bfield
                [1] - Coulomb collisions
                [2] - Ion-neutral collisions (BGK)
            fmax: max frequency in Hz (will calculate if not provided)
            Nfreq: determines frequency resolution
            normalize: whether to normalize ACF
            
        """
        
        # check input vars and copy to class vars
        self.override=override

        self.prepareAndCheck(input)
        self.emode=emode
        self.imode=imode
    
        self.fmax=fmax
        self.Nfreq=Nfreq
        self.normalize=normalize
        
    
        return
                
    def adjustParams(self,newParms):
        
        """
        adjustParams: Changes parameters for IS model evaluation
        
        Inputs:
            newParms: A dictionary with entries corresponding to the elements to be changed

        """

        pkeys = newParms.keys()
                
        for key in pkeys:
            try: 
                self.Params[key]
            except:
                raise KeyError, "invalid parameter %s" % key                        
          
            if key in ('ni','ti','vi','nuin'):
                self.Params[key] = scipy.array(newParms[key])
            else:
                self.Params[key] = newParms[key]
                         
        if not self.override:
            self.checkParams()
          
        return
        
    def prepareAndCheck(self,input):
    
        self.Params = {}
    
        try:
            self.Nion = input['Nion']
        except:
            raise KeyError, 'input dict must include Nion'
            
        try: 
            self.mi_amu = scipy.array(input['mi'])
            self.mi=self.mi_amu*v_amu
        except:
            raise KeyError, 'input dict must include mi'
                                
        try: 
            self.Params['f0'] = input['f0']
        except:
            raise KeyError, 'input dict must include f0'

        try: 
            self.Params['B'] = input['B']
        except:
            self.Params['B'] = 0.0
            
        try: 
            self.Params['te'] = input['te']
        except:
            self.Params['te'] = 0.0
            
        try: 
            self.Params['alpha'] = input['alpha']
        except:
            self.Params['alpha'] = 0.0
            
        try: 
            self.Params['ne'] = input['ne']
        except:
            self.Params['ne'] = 0.0
            
        try: 
            self.Params['ti'] = scipy.array(input['ti'])
        except:
            self.Params['ti'] = scipy.zeros(self.mi.shape)
            
        try: 
            self.Params['ni'] = scipy.array(input['ni'])
        except:
            self.Params['ni'] = scipy.zeros(self.mi.shape)
            self.Params['ni'][0] = 1.0

        try: 
            self.Params['ve'] = input['ve']
        except:
            self.Params['ve'] = 0.0

        try: 
            self.Params['vi'] = scipy.array(input['vi'])
        except:
            self.Params['vi'] = scipy.zeros(self.mi.shape)

        try: 
            self.Params['nuen'] = input['nuen']            
        except:
            self.Params['nuen'] = 0.0

        try: 
            self.Params['nuin'] = scipy.array(input['nuin'])
        except:
            self.Params['nuin'] = scipy.zeros(self.mi.shape)            

        if not self.override:
            self.checkParams()
        
        return

    def checkParams(self):
        
        try:
            
            if not (self.Params['ti'].shape[0] == self.Nion and self.Params['ni'].shape[0] == self.Nion \
                and self.Params['vi'].shape[0] == self.Nion and self.Params['nuin'].shape[0] == self.Nion \
                and self.mi.shape[0] == self.Nion):
                raise ValueError, 'mui, ni, mi, vi, nuin must have length Nion'

            if scipy.absolute(1.0 - scipy.sum(self.Params['ni']))>1.0e-6:
                raise ValueError, "ni must add to 1.0, currentlty adds to %2.7f" % scipy.sum(self.Params['ni'])
        
            for ion in self.mi_amu:
                if not self.Amu2Ion.has_key(str(int(ion))):
                    raise ValueError, "ion masses must be one of %s" %self.Amu2Ion.keys()
                
        except ValueError:
            raise

        except:
            print "Unexpected error:", sys.exc_info()[0]
            raise
            
        return
        
    def computeSpec(self):
    
        #### compute parameters
        alpha = self.Params['alpha']*pi/180.0
        
        # if no Bfield, set alpha to 0
        if not self.emode[0]:
            alphae = 0.0
        else:
            alphae = alpha
        if not self.imode[0]:
            alphai = 0.0
        else:
            alphai = alpha
        
        # k vector and wavelength
        k=2.0*pi/v_lightspeed*(2.0*self.Params['f0']) 
        lambd=2.0*pi/k 
                    
        # electron and ion thermal speeds
        Ce=scipy.sqrt(v_Boltzmann*self.Params['te']/v_electronmass) 
        Ci=scipy.sqrt(v_Boltzmann*self.Params['ti']/self.mi)
        
        # electron and ion gyro frequencies
        Oe=v_elemcharge*self.Params['B']/v_electronmass 
        Oi=v_elemcharge*self.Params['B']/self.mi 
        
        # electron and ion plasma frequency
        wpe=scipy.sqrt(self.Params['ne']*v_elemcharge*v_elemcharge/v_electronmass/v_epsilon0) 
        wpi=scipy.sqrt(self.Params['ni']*self.Params['ne']*v_elemcharge*v_elemcharge/self.mi/v_epsilon0)
        
        # electron, ion, and plasma debye length
        he=Ce/wpe 
        
        # coulomb collisions
        if self.emode[1]:
            nu0e = (1.0+1.0/scipy.sqrt(2))*54.5*self.Params['ne']*1.0e-6/self.Params['te']**1.5
            aC=lambd*nu0e/Ce # from Woodman [2004]
            nuec=nu0e*15.8209*(1.0-scipy.exp(-(scipy.sin(pi/2.0-alpha)/aC+0.1705)/2.0785)) # fsr extrapolation
            print 'nue/nu0e=%f, nue=%f, nu0e=%f' % (nuec/nu0e,nuec,nu0e)
            psiec = nuec/(k*Ce*scipy.sqrt(2))            
        else:
            psiec = 0.0
        if self.imode[1]:
            psiic = scipy.zeros(self.mi.shape)        
            for i1 in range(self.Nion):
                for i2 in range(self.Nion):
                    if self.Params['ni'][i2]>1.0e-6:
                        tb = spec_utils.getBst(self.mi_amu[i1],self.mi_amu[i2])
                        psiic[i1] = psiic[i1] + tb*self.Params['ni'][i2]*self.Params['ne']*1.0e-6/self.Params['ti'][i2]**1.5
            psiic = psiic/(k*Ci*scipy.sqrt(2.0))
        else:
            psiic = scipy.zeros(self.mi.shape)
        
        # neutral collisions
        if self.emode[2]:
            psien = self.Params['nuen']/(k*Ce*scipy.sqrt(2))
        else:
            psien = 0.0
        if self.imode[2]:
            psiin = self.Params['nuin']/(k*Ci*scipy.sqrt(2))
        else:
            psiin = scipy.zeros(self.mi.shape)
        
        print psi
        
        ####
 
        # frequency        
        if self.fmax>0.0:
            fmax = self.fmax
        else:
            fmax=scipy.sum(self.Params['ni']*k*Ci*scipy.sqrt(2.0))/2.0+scipy.absolute(self.Params['vi']).max()/lambd
        fmin=-fmax
        ff=scipy.linspace(fmin,fmax,self.Nfreq);
        ww=2.0*pi*ff

        # special normalized frequencies
        thetae = (ww + k*self.Params['ve'])/(k*Ce*scipy.sqrt(2.0)) 
        thetai=(scipy.repeat(ww[:,scipy.newaxis],self.Nion,axis=1) + k*self.Params['vi'])/(k*Ci*scipy.sqrt(2.0))         
        phie=Oe/(k*Ce*scipy.sqrt(2))
        phii=Oi/(k*Ci*scipy.sqrt(2))

        # compute Gordeyev integrals
        Ji=scipy.zeros(thetai.shape,dtype='complex64')
        Je=scipy.zeros(thetae.shape,dtype='complex64')        
        Je,Nl=spec_utils.gordeyev_cz(thetae,alphae,phie,psien,psiec)
        print Nl
        for ithi in range(self.Nion):
            if self.Params['ni'][ithi]>1.0e-6:
                Ji[:,ithi],Nl=spec_utils.gordeyev_cz(thetai[:,ithi],alphai,phii[ithi],psiin[ithi],psiic[ithi])        
                
        spec=spec_utils.compute_spec(Je,Ji,thetae,thetai,Ce,Ci,self.Params['ni'],self.Params['te']/self.Params['ti'],he,k,ww)  # spec with everything
        spec = 2.0*spec*self.Params['ne']        
        
        tau,acf=spec_utils.spec2acf(ff,spec)
       
        if self.normalize==1:
            acf=acf.real
            acf=acf/acf[int(scipy.floor(tau.size/2.0))]
    
        return ff,spec,tau,acf
               