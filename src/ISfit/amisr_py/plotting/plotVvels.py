#!/usr/bin/env python

"""

"""

from amisr_py.constants.constants import *

import scipy
import matplotlib
matplotlib.use('Agg')
matplotlib.rcParams.update({'figure.max_open_warning': 0})
import pylab

import plot_utils
from matplotlib.colors import LogNorm

CMAP='RdBu'

class tratePlot:
    
    def __init__(self):
        """ initialization function """
        return
    
    def makePlot(self,time,alt,JdotE,dJdotE,JdotEp,dJdotEp,SpE2,dSpE2,intJdotE,dintJdotE,intJdotEp,dintJdotEp,intSpE2,dintSpE2,\
        title='Energy Transfer Rates',units='W/m^3',cax=[-0.25,0.25],caxInt=[-20.0,20.0],label='Altitude (km)',sc=1e6):
    
        textsize = 8        # size for axes text
        labsize = 10    

        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (10,10)
    
        ncols=3; nrows=2
        dx, dy= 0.015, 0.05	
        POS=[0.1,0.7,1.0/(3.6)-dx*2,1.0/(3.2)-dy*1.5]	
            
        figg=pylab.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))
            bb+=1
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))        
        bb=0; aa+=1;
        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]*2+dx,POS[3]]
        ax.append(pylab.axes(rect, axisbg=axesBG))

        ii=0
        x,dat=plot_utils.timegaps(time,JdotE*sc)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title(r"$q={\bf J} \cdot {\bf E}$", fontsize=labsize)
        ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')

        ii=3
        cl=pylab.colorbar(pc,ax[ii])
        cl.set_label(str(sc) + ' x ' + units,fontsize=labsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize) 

        ii=4
        x,dat=plot_utils.timegaps(time,scipy.absolute(dJdotE/JdotE))
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        
        ii=7
        cl=pylab.colorbar(pc,ax[ii])
        cl.set_label('Fractional Error',fontsize=labsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)         

        ii=1
        x,dat=plot_utils.timegaps(time,JdotEp*sc)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        ax[ii].set_title(r"$q_j = {\bf J} \cdot {\bf E^\prime}$", fontsize=labsize)

        ii=5
        x,dat=plot_utils.timegaps(time,scipy.absolute(dJdotEp/JdotEp))
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
          
        ii=2
        x,dat=plot_utils.timegaps(time,SpE2*sc)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        ax[ii].set_title(r"$q_j^E = \sigma_P |E|^2$", fontsize=labsize)
            
        ii=6
        x,dat=plot_utils.timegaps(time,scipy.absolute(dSpE2/SpE2))
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
                                                                    
        ii=8
        mtime = matplotlib.dates.epoch2num(scipy.mean(time,axis=1))
        ax[ii].errorbar(mtime,intJdotE*1e3,yerr=dintJdotE*1e3,fmt='-k.',label=r"$Q=\int{{\bf J} \cdot {\bf E}}$")        
        ax[ii].errorbar(mtime,intJdotEp*1e3,yerr=dintJdotEp*1e3,fmt='-b.',label=r"$Q_j=\int{{\bf J} \cdot {\bf E^\prime}}$")
        ax[ii].errorbar(mtime,intSpE2*1e3,yerr=dintSpE2*1e3,fmt='-r.',label=r"$Q_j^E=\int{\sigma_P |E|^2}$")
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(caxInt)                
        ax[ii].legend(loc = 'upper right', bbox_to_anchor = (1.5,1.0))
        labels = pylab.getp(ax[ii].get_legend(), 'texts')
        pylab.setp(labels, fontsize=textsize)        
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)               
        ax[ii].set_ylabel('Energy Transfer Rate (mW/m^2)', fontsize=labsize)
        ax[ii].set_xlabel('Time (UT Hours)', fontsize=labsize)
        
        dx=(time[-1,-1]-time[0,0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(scipy.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(scipy.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)        
        
        self.figg=figg
        return


class condPlot:
    
    def __init__(self):
        """ initialization function """
        return
    
    def makePlot(self,time,alt,sp,dsp,sh,dsh,intSp,dintSp,intSh,dintSh,\
        title='Conductivity',units='mho/m',cax=[0,0.5],caxInt=[0.0,20.0],label='Altitude (km)',sc=1e3):
    
        textsize = 8        # size for axes text
        labsize = 10    

        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (10,10)
    
        ncols=2; nrows=2
        dx, dy= 0.015, 0.05	
        POS=[0.1,0.7,1.0/(3.6)-dx*2,1.0/(3.2)-dy*1.5]	
            
        figg=pylab.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))
            bb+=1
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))        
        bb=0; aa+=1;
        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]*2+dx,POS[3]]
        ax.append(pylab.axes(rect, axisbg=axesBG))

        ii=0
        x,dat=plot_utils.timegaps(time,sp*sc)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        #pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',norm=LogNorm(vmin=cax[0], vmax=cax[1]))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('Pedersen Conductivity', fontsize=labsize)

        ii+=1
        x,dat=plot_utils.timegaps(time,sh*sc)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        #pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',norm=LogNorm(vmin=cax[0], vmax=cax[1]))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        ax[ii].set_title('Hall Conductivity', fontsize=labsize)
          
        ii+=1
        cl=pylab.colorbar(pc,ax[ii])
        cl.set_label(str(sc) + ' x ' + units,fontsize=labsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize) 

        ii+=1
        x,dat=plot_utils.timegaps(time,dsp/sp)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)

        ii+=1
        x,dat=plot_utils.timegaps(time,dsh/sh)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(alt),scipy.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,scipy.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
          
        ii+=1
        cl=pylab.colorbar(pc,ax[ii])
        cl.set_label('Fractional Error',fontsize=labsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)     
                                
        ii+=1
        mtime = matplotlib.dates.epoch2num(scipy.mean(time,axis=1))
        ax[ii].semilogy(mtime,intSp,'-k.',label='Ped. Cond.')        
        ax[ii].semilogy(mtime,intSh,'-b.',label='Hall Cond.')        
        #ax[ii].errorbar(mtime,intSp,yerr=dintSp,fmt='-k.',label='Ped. Cond.')        
        #ax[ii].errorbar(mtime,intSh,yerr=dintSh,fmt='-b.',label='Hall Cond.')
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(caxInt)                
        ax[ii].legend(loc=0)
        labels = pylab.getp(ax[ii].get_legend(), 'texts')
        pylab.setp(labels, fontsize=textsize)        
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)               
        ax[ii].set_ylabel('Conductance', fontsize=labsize)
        ax[ii].set_xlabel('Time (UT Hours)', fontsize=labsize)
        
        dx=(time[-1,-1]-time[0,0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(scipy.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(scipy.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)        
        
        self.figg=figg
        return

class efieldPlot:
    
    def __init__(self):
        """ initialization function """
        return
    
    def makePlot(self,time,ex,ey,dex,dey,title='Electric Field',units='mV/m',ylims=[-100.0,100.0]):
    
        textsize = 8        # size for axes text
        labsize = 10    

        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (7,6)
    
        ncols=1; nrows=2
        dx, dy= 0.015, 0.05	
        POS=[0.1,0.5,1.0/(ncols+0.6)-dx,1.0/(nrows+0.2)-dy*1.5]	
            
        figg=pylab.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))
        ax.append(ax[-1].twinx())    
            
        mtime = matplotlib.dates.epoch2num(scipy.mean(time,axis=1))
        xlims=[mtime[0],mtime[-1]]

        Emag = scipy.sqrt(ex**2.0+ey**2.0)
        dEmag = scipy.sqrt( scipy.power(dex,2.0)*scipy.power(ex/Emag,2.0) + scipy.power(dey,2.0)*scipy.power(ey/Emag,2.0) ).real
        
        Edir = 180.0/pi*scipy.arctan2(ex,ey).real
        dEdir=180.0/pi*((1.0/scipy.absolute(ex))*(1.0/(1.0+scipy.power(ey/ex,2.0)))*scipy.sqrt(scipy.power(dey,2.0)+scipy.power(ey/ex*dex,2.0))).real

        yy1=scipy.array([ex.min(),ey.min()]).min()*1e3-10.0
        yy2=scipy.array([ex.max(),ey.max()]).max()*1e3+10.0
        ylims=[yy1,yy2]
        ylims2=[0.0,Emag.max()*1e3+10.0]

        iax=0
        ax[iax].errorbar(mtime,scipy.squeeze(ex*1e3),scipy.squeeze(dex*1e3),fmt='-k.',label='Perp-East')        
        ax[iax].errorbar(mtime,scipy.squeeze(ey*1e3),scipy.squeeze(dey*1e3),fmt='-b.',label='Perp-North')        
        ax[iax].set_xlim(xlims)        
        ax[iax].set_ylim(ylims)        
        ax[iax].set_ylabel('Electric Field (mV/m)', fontsize=labsize);
        ax[iax].legend(loc=0)
        labels = pylab.getp(ax[iax].get_legend(), 'texts')
        pylab.setp(labels, fontsize=textsize)        
        labels = pylab.getp(ax[iax], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[iax], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)
        ax[iax].text(xlims[0],(ylims[1]-ylims[0])*0.05+ylims[1],title,fontsize=labsize, horizontalalignment='left')

        iax+=1
        ax[iax].errorbar(mtime,scipy.squeeze(Emag*1e3),yerr=scipy.squeeze(dEmag*1e3),fmt='-k.',label='|E|')        
        ax[iax].set_ylim(ylims2)
        ax[iax].set_xlim(xlims)        
        ax[iax].set_ylabel('Magnitude (mV/m)', fontsize=labsize);
        ax[iax].set_xlabel('Time (UT Hours)', fontsize=labsize);
        labels = pylab.getp(ax[iax], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[iax], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)

        iax+=1
        ax[iax].errorbar(mtime,scipy.squeeze(Edir),yerr=scipy.squeeze(dEdir),fmt='-b.',label='Edir')
        ax[iax].set_ylim([-180.0,180.0])        
        ax[iax].set_xlim(xlims)                
        ax[iax].set_ylabel('Direction (deg)', fontsize=labsize, color='blue');
        labels = pylab.getp(ax[iax], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[iax], 'yticklabels')
        pylab.setp(labels, fontsize=textsize, color='blue')

        dx=(time[-1,-1]-time[0,0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(scipy.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval= int(scipy.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)
                                       
        self.figg=figg
        
        return

class vvelsPlot:
    
    def __init__(self):
        """ initialization function """
        return
        
    def close(self):
        pylab.close(self.figg)
        return
    
    def makePlot(self,time,MLTtime,lat,vx,vy,dvx,dvy,title='Vector Vels',units='m/s',parm='V',\
        p=[200.0,.25,4000,500.0],sc=15.0,cax=[-1000,1000],label='Mag. Lat. (degrees)',\
        ncols=2,vz=[],dvz=[],vzsc=1.0,nrows=4,geo=0,doQuiv=1,textsize=8,labsize=10):

        vx=vx.copy()
        vy=vy.copy()
        dvx=dvx.copy()
        dvy=dvy.copy()
        if ncols==3:
            vz=vz.copy()*vzsc
            dvz=dvz.copy()*vzsc
        
        if lat.ndim==2:
            lat2=scipy.nanmean(lat,axis=1)
            lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
        else:
            lat2=lat
        
        time2=scipy.mean(time,axis=1)

        textsize = 8        # size for axes text
        labsize = 10

        pylab.ioff()
        
        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (7,9)
        if ncols==3:
            figsz = (11,12)

        dx, dy= 0.015, 0.05	
        POS=[0.1,0.75,1.0/(ncols+0.6)-dx,1.0/(nrows)-dy*1.5]	
        if ncols==3:
            POS=[0.075,0.75,1.0/(ncols+0.6)-dx,1.0/(nrows)-dy*1.5]	
            
        figg=pylab.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows-1-doQuiv):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))
        
        ii=0
        x,dat=plot_utils.timegaps(time,vx)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(lat),scipy.nanmax(lat)]
        pc=ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        if geo==1:
            ax[ii].set_title(parm+' east '+units, fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title(parm+' perp east '+units, fontsize=labsize, horizontalalignment='center')
        ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')
        if ncols==1:
            cl=pylab.colorbar(pc)
            cl.set_label(units,fontsize=labsize)
                    
        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,vy)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        #	pylab.colorbar()
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        if geo==1:
            ax[ii].set_title(parm+' north '+units, fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title(parm+' perp north '+units, fontsize=labsize, horizontalalignment='center')

        ii=ii+1

        if ncols==3:
            x,dat=plot_utils.timegaps(time,vz)
            dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
            x=matplotlib.dates.epoch2num(x)
            xlim=[x[0],x[-1]]	
            ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1],cmap=pylab.get_cmap(CMAP))
            ax[ii].set_xlim(xlim)
            ax[ii].set_ylim(ylim)
            #	pylab.colorbar()
            labels = pylab.getp(ax[ii], 'xticklabels')
            pylab.setp(labels, fontsize=textsize)
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)	
            pylab.setp(ax[ii], yticklabels=[])
            if geo==1:
                ax[ii].set_title('%s up (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')	
            else:
                ax[ii].set_title('%s anti par (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')	
            
            ii=ii+1
        
        x,dat=plot_utils.timegaps(time,dvx)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc2=ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=0,vmax=cax[1]/5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        if geo==1:
            ax[ii].set_title('err %s east %s' % (parm,units), fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('err %s perp east %s' % (parm,units), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,dvy)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=0,vmax=cax[1]/5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        pylab.setp(ax[ii], yticklabels=[])
        if geo==1:
            ax[ii].set_title('err %s north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('err %s perp north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
                
        ii=ii+1

        if ncols==3:
            x,dat=plot_utils.timegaps(time,dvz)
            dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
            x=matplotlib.dates.epoch2num(x)
            xlim=[x[0],x[-1]]	
            ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=0,vmax=cax[1]/5)
            ax[ii].set_xlim(xlim)
            ax[ii].set_ylim(ylim)
            ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
            labels = pylab.getp(ax[ii], 'xticklabels')
            pylab.setp(labels, fontsize=textsize)
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)	
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)	
            pylab.setp(ax[ii], yticklabels=[])
            if geo==1:
                ax[ii].set_title('err %s up (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')					
            else:
                ax[ii].set_title('err %s anti par (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')
            
            ii=ii+1

        # quiver plot
        if doQuiv:
            aa=aa+1
            bb=0
            ss=2
            sss=1
            if ncols==33:
                ss=3
                sss=0.8

            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa-dy/1*sss-POS[3]*(sss-1),POS[2]*ss+dx,POS[3]*sss]
            ax.append(pylab.axes(rect, axisbg=axesBG))
            
            I=scipy.where(scipy.isnan(vx))
            vx[I]=0
            vy[I]=0
            I=scipy.where(scipy.isnan(vy))
            vx[I]=0
            vy[I]=0
            
            I=scipy.where(scipy.absolute(dvx)/(scipy.absolute(vx)+p[0])>p[1])
            vx[I]=0
            vy[I]=0
            I=scipy.where(scipy.absolute(dvy)/(scipy.absolute(vy)+p[0])>p[1])
            vx[I]=0
            vy[I]=0
            
            # Quick hack for maximum S.C 06/03/15 
            #p[2] = 3000
            I=scipy.where(scipy.sqrt(vx*vx+vy*vy)>p[2])
            vx[I]=0
            vy[I]=0
        
            I=scipy.where((scipy.absolute(dvx)>p[3]) | (scipy.absolute(dvy)>p[3]))
            vx[I]=0
            vy[I]=0
            
            
            C=scipy.ones(scipy.transpose(vx).shape)-scipy.sign(scipy.transpose(vx))
            x=matplotlib.dates.epoch2num(time2)
            [X,Y]=scipy.meshgrid(x,lat2)
            Q=ax[ii].quiver(X,Y,scipy.transpose(vx),scipy.transpose(vy),C,scale=1000.0*sc,width=rect[2]*0.005)
            ax[ii].quiverkey(Q, 1.1, 1, cax[1], str(cax[1]) + ' ' + units,fontproperties={'size' : labsize},labelpos='S')
            ax[ii].xaxis.tick_bottom()
            ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
            labels = pylab.getp(ax[ii], 'xticklabels')
            pylab.setp(labels, fontsize=textsize)
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)	
            ax[ii].set_ylabel(label, fontsize=labsize)
            pylab.hold(1)
            pylab.xlim((x[0],x[-1]))
            pylab.ylim((scipy.nanmin(lat2),scipy.nanmax(lat2)))	
            
            ax22 = figg.add_axes(ax[ii].get_position(), sharey=ax[ii], frameon=False)
            pylab.plot(MLTtime,MLTtime,'k')
            ax22.set_xlim([MLTtime[0],MLTtime[-1]])
            ax22.xaxis.tick_top()
            ax22.set_xlabel('Magnetic Local Time', fontsize=labsize)
            ax22.xaxis.set_label_position('top')
            labels = pylab.getp(ax22, 'xticklabels')
            pylab.setp(labels, fontsize=textsize)
            labels = pylab.getp(ax22, 'yticklabels')
            pylab.setp(labels, fontsize=textsize)
            pylab.ylim((scipy.nanmin(lat2),scipy.nanmax(lat2)))	

            ii=ii+1
        
        if ncols==1:
            npls=ii-1		
            bb=1
            for aa in range(npls):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))	
                cl=pylab.colorbar(pc,ax[ii])
                cl.set_label(units,fontsize=labsize)
                labels = pylab.getp(ax[ii], 'yticklabels')
                pylab.setp(labels, fontsize=textsize)
                ii+=1
            
        else:
            aa=aa-2
            bb=2
            if ncols==3:
                bb=3
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))
            
            cl=pylab.colorbar(pc,ax[ii])
            cl.set_label(units,fontsize=labsize)
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)

            ii=ii+1
            
            aa=aa+1
            bb=2
            if ncols==3:
                bb=3
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(pylab.axes(rect, axisbg=axesBG))
            
            cl=pylab.colorbar(pc2,ax[ii])
            cl.set_label(units,fontsize=labsize)
            labels = pylab.getp(ax[ii], 'yticklabels')
            pylab.setp(labels, fontsize=textsize)
            
        locator = matplotlib.dates.HourLocator(interval=1)
    #	locator = matplotlib.dates.MinuteLocator(interval=5)
        formatter = matplotlib.dates.DateFormatter("%H:%M")
        
        dx=(time2[-1]-time2[0])/3600.0
        dx2=dx/7.0
        
        ss=5
        if ncols==3:
            ss=7
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(scipy.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(scipy.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)
        
        """
        rr=rr+1
        locator = matplotlib.dates.MinuteLocator(interval=30)
        formatter = matplotlib.dates.DateFormatter("%H:%M")
        ax[rr].xaxis.set_major_locator(locator)
        ax[rr].xaxis.set_major_formatter(formatter)
        """
        
        pylab.show()
        
        self.figg=figg
        
        return
  
class vvelsMagPlot:
    
    def __init__(self):
        """ initialization function """
        return
        
    def close(self):
        pylab.close(self.figg)
        return        
                    
    def makePlot(self,time,MLTtime,lat,vx,vy,dvx,dvy,title='Vector Vels',units='m/s',parm='V',cax1=[-1000,1000],cax2=[-180,180],label='Mag. Lat. (degrees)'):

        vx=vx.copy()
        vy=vy.copy()
        dvx=dvx.copy()
        dvy=dvy.copy()
        
        if lat.ndim==2:
            lat2=scipy.nanmean(lat,axis=1)
            lat=scipy.concatenate((lat[:,0],lat[[-1],1]))
        else:
            lat2=lat
        
        time2=scipy.mean(time,axis=1)

        textsize = 8        # size for axes text
        labsize = 10

        pylab.ioff()
        
        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (7,9)

        dx, dy= 0.015, 0.05	
        nrows=4; ncols=1
        POS=[0.07,0.75,0.8-dx,1.0/(nrows)-dy*1.5]	
            
        figg=pylab.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))
        
        pc=[]
        
        ii=0
        x,dat=plot_utils.timegaps(time,vx)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[scipy.nanmin(lat),scipy.nanmax(lat)]
        pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=cax1[0],vmax=cax1[1]))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('%s mag (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')
                    
        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,dvx)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=0.0,vmax=cax1[1]/5))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        #	pylab.colorbar()
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('err %s mag (%s)' %(parm,units), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,vy)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=cax2[0],vmax=cax2[1],cmap=pylab.get_cmap('hsv')))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('%s dir (%s)' % (parm,'degrees'), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,dvy)
        dat=scipy.ma.masked_where(scipy.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc.append(ax[ii].pcolor(x,lat,scipy.transpose(dat),edgecolors='none',vmin=0,vmax=cax2[1]/5))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        labels = pylab.getp(ax[ii], 'xticklabels')
        pylab.setp(labels, fontsize=textsize)
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        labels = pylab.getp(ax[ii], 'yticklabels')
        pylab.setp(labels, fontsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('err %s dir (%s)' % (parm,'degrees'), fontsize=labsize, horizontalalignment='center')
                
        ii=ii+1
        
        if ncols==1:
            bb=1
            for aa in range(len(pc)):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/20,POS[3]]
                ax.append(pylab.axes(rect, axisbg=axesBG))	
                cl=pylab.colorbar(pc[aa],ax[ii])
                if aa<2:
                    cl.set_label(units,fontsize=labsize)
                else:
                    cl.set_label('degrees',fontsize=labsize)			
                pylab.setp(pylab.getp(ax[ii], 'yticklabels'), fontsize=textsize)
                ii+=1
                    
        locator = matplotlib.dates.HourLocator(interval=1)
    #	locator = matplotlib.dates.MinuteLocator(interval=5)
        formatter = matplotlib.dates.DateFormatter("%H:%M")
        
        dx=(time2[-1]-time2[0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(scipy.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(scipy.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)
        
        pylab.show()
        
        self.figg=figg
        
        return figg
