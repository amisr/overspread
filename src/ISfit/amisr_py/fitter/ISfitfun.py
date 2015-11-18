#! /usr/bin/env python

"""

"""

import scipy

DEBUG=0 # turn on debugging
if DEBUG==1:
    import matplotlib
    matplotlib.use('TkAgg')
    import pylab

def print_timing(func):
    def wrapper(*arg):
        import time
        
        t1 = time.time()
        res = func(*arg)
        t2 = time.time()
        print '%s took %0.3f ms' % (func.func_name, (t2-t1)*1000.0)
        return res
    return wrapper

# fitfun
class fitfun:

    mapFcns =  {'ni': lambda x: 1.0-x, 'ti': lambda x: x, 'vi': lambda x:x, 'psi': lambda x:x}

    # init
    def __init__(self,cspect,Ifit,nfit,tau,amb,mi):
        """
        """
    
        self.cspect = cspect   
        self.Ifit = Ifit
        self.tau = tau
        self.amb = amb
        self.nlag = amb.shape[1]
        self.nfit = nfit

        self.ambfin = [scipy.where(amb[:,i]/max(amb[:,i])>1e-3)[0] for i in range(self.nlag)]

        self.Parms={'mi':mi}
        
        n = Ifit.shape
        m=['ni','ti','psi','vi']
        self.map=[(m[i1],i2,scipy.where(Ifit[:,i1]==-1)[0]) if i2<n[0]-1 else (m[i1],i2,[]) for i1 in range(n[1]) for i2 in range(n[0]) if Ifit[i2,i1]==1]
        self.map.insert(0,('ne',0,[]))
                
        return
        
    def setErrArray(self,dparams):
            
        dfit = scipy.zeros(self.Ifit.shape)
        self.map=[dparams if Ifit[i2,i1]==1 if i2<n[0]-1 else (m[i1],i2,[]) for i1 in range(n[1]) for i2 in range(n[0]) ]
    
        return
        
    def setArrays(self,ne,ni,ti,psi,vi):

        self.Parms['ne']=ne
        self.Parms['ni']=ni
        self.Parms['ti']=ti
        self.Parms['psi']=psi
        self.Parms['vi']=vi
        
        self.updateSpect()
        
        return
        
    def updateArrays(self,params):
    
        if params.shape[0] != self.nfit:
            raise ValueError, "huh"
            
        self.Parms[self.map[0][0]]=params[0]
        for i in range(1,self.nfit):
            tmap = self.map[i]
            self.Parms[tmap[0]][tmap[1]] = params[i]
            self.Parms[tmap[0]][tmap[2]] = self.mapFcns[tmap[0]](params[i])

        self.updateSpect()
                    
        return

    def updateSpect(self):
        self.cspect.adjustParams({'ne':self.Parms['ne'],'vi':self.Parms['vi'][:-1],'ve':self.Parms['vi'][-1],\
            'ti':self.Parms['ti'][:-1],'te':self.Parms['ti'][-1],'ni':self.Parms['ni'][:-1],\
            'nuin':self.Parms['psi'][:-1],'nuen':self.Parms['psi'][-1]}) 
        
        return

    def getSpec(self):
        # compute theoretical spectra
        ff,spec,tau,acf = self.cspect.computeSpec()
        
        # interpolate acf
        m2=scipy.zeros(self.tau.size,dtype='Complex64');
        m2.real=scipy.interpolate.interp1d(tau,acf.real,bounds_error=0)(self.tau) # linear interpolation
        m2.imag=scipy.interpolate.interp1d(tau,acf.imag,bounds_error=0)(self.tau) # linear interpolation
        
        # apply ambiguity function
        m = scipy.array([scipy.sum(self.amb[:,i][self.ambfin[i]]*m2[self.ambfin[i]]) for i in range(self.nlag)])

        return ff,spec,tau,acf,self.tau,m2,m

# fitf
def fitf(params,finst,data,var,Psc,tn=0.0,L=[0.0,0.0]):

    """
    
    """

    # update parameter arrays
    finst.updateArrays(params)
    
    # get spec and acf
    ff,spec,tau,acf,tau2,m2,m = finst.getSpec()
    m=m*Psc
    
    # DEBUG: make some plots
    if DEBUG:        
        pylab.figure(10)
        pylab.clf()
        pylab.plot(range(finst.nlag),m.real)        
        pylab.errorbar(range(finst.nlag),data.real,scipy.sqrt(var))
        pylab.plot(range(finst.nlag),m.imag)        
        pylab.errorbar(range(finst.nlag),data.imag,scipy.sqrt(var))
        pylab.show()

    # model data differences
    y=scipy.concatenate(((data.real-m.real)/scipy.sqrt(var),(data.imag-m.imag)/scipy.sqrt(var)))
    y=scipy.concatenate((y,[scipy.sqrt(L[0]*scipy.exp(-min([0.0,finst.Parms['ti'][-1]-tn]))),scipy.sqrt(L[1]*scipy.exp(-min([0.0,finst.Parms['ti'][0]-tn])))]))
    y=y.astype('float64')
    y=y[scipy.where(scipy.isfinite(y))]

    return y

    
