#!/usr/bin/env python

"""

"""

from amisr_py.constants.constants import *

import numpy as np
import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot

import plot_utils
from matplotlib.colors import LogNorm

CMAP='RdBu'
rCMAP='RdBu_r'

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
            
        figg=pyplot.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(figg.add_axes(rect, facecolor=axesBG))
            bb+=1
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(figg.add_axes(rect, facecolor=axesBG))        
        bb=0; aa+=1;
        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]*2+dx,POS[3]]
        ax.append(figg.add_axes(rect, facecolor=axesBG))

        ii=0
        x,dat=plot_utils.timegaps(time,JdotE*sc)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title(r"$q={\bf J} \cdot {\bf E}$", fontsize=labsize)
        ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')

        ii=3
        cl=pyplot.colorbar(pc,ax[ii])
        cl.set_label(str(sc) + ' x ' + units,fontsize=labsize)
        labels = ax[ii].get_yticklabels()
        ax[ii].set_yticklabels(labels, fontsize=textsize)

        ii=4
        x,dat=plot_utils.timegaps(time,np.absolute(dJdotE/JdotE))
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel(label, fontsize=labsize)
        
        ii=7
        cl=pyplot.colorbar(pc,ax[ii])
        cl.set_label('Fractional Error',fontsize=labsize)
        labels = ax[ii].get_yticklabels()
        ax[ii].set_yticklabels(labels, fontsize=textsize)       

        ii=1
        x,dat=plot_utils.timegaps(time,JdotEp*sc)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([])
        ax[ii].set_title(r"$q_j = {\bf J} \cdot {\bf E^\prime}$", fontsize=labsize)

        ii=5
        x,dat=plot_utils.timegaps(time,np.absolute(dJdotEp/JdotEp))
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([])
          
        ii=2
        x,dat=plot_utils.timegaps(time,SpE2*sc)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([])
        ax[ii].set_title(r"$q_j^E = \sigma_P |E|^2$", fontsize=labsize)
            
        ii=6
        x,dat=plot_utils.timegaps(time,np.absolute(dSpE2/SpE2))
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([])
                                                                    
        ii=8
        mtime = matplotlib.dates.epoch2num(np.mean(time,axis=1))
        ax[ii].errorbar(mtime,intJdotE*1e3,yerr=dintJdotE*1e3,fmt='-k.',label=r"$Q=\int{{\bf J} \cdot {\bf E}}$")        
        ax[ii].errorbar(mtime,intJdotEp*1e3,yerr=dintJdotEp*1e3,fmt='-b.',label=r"$Q_j=\int{{\bf J} \cdot {\bf E^\prime}}$")
        ax[ii].errorbar(mtime,intSpE2*1e3,yerr=dintSpE2*1e3,fmt='-r.',label=r"$Q_j^E=\int{\sigma_P |E|^2}$")
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(caxInt)                
        ax[ii].legend(loc = 'upper right', bbox_to_anchor = (1.5,1.0),fontsize=textsize)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel('Energy Transfer Rate (mW/m^2)', fontsize=labsize)
        ax[ii].set_xlabel('Time (UT Hours)', fontsize=labsize)
        
        dx=(time[-1,-1]-time[0,0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(np.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(np.ceil(dx*60.0/7.0))
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
            
        figg=pyplot.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(figg.add_axes(rect, facecolor=axesBG))
            bb+=1
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/10,POS[3]]
            ax.append(figg.add_axes(rect, facecolor=axesBG))        
        bb=0; aa+=1;
        rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]*2+dx,POS[3]]
        ax.append(figg.add_axes(rect, facecolor=axesBG))

        ii=0
        x,dat=plot_utils.timegaps(time,sp*sc)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        #pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',norm=LogNorm(vmin=cax[0], vmax=cax[1]))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('Pedersen Conductivity', fontsize=labsize)

        ii+=1
        x,dat=plot_utils.timegaps(time,sh*sc)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        #pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1])
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',norm=LogNorm(vmin=cax[0], vmax=cax[1]))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([], fontsize=textsize)
        ax[ii].set_title('Hall Conductivity', fontsize=labsize)
          
        ii+=1
        cl=pyplot.colorbar(pc,ax[ii])
        cl.set_label(str(sc) + ' x ' + units,fontsize=labsize)
        labels = ax[ii].get_yticklabels()
        ax[ii].set_yticklabels(labels, fontsize=textsize)

        ii+=1
        x,dat=plot_utils.timegaps(time,dsp/sp)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)

        ii+=1
        x,dat=plot_utils.timegaps(time,dsh/sh)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(alt),np.nanmax(alt)]
        pc=ax[ii].pcolor(x,alt,np.transpose(dat),edgecolors='none',vmin=0.0,vmax=0.5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([], fontsize=textsize)
          
        ii+=1
        cl=pyplot.colorbar(pc,ax[ii])
        cl.set_label('Fractional Error',fontsize=labsize)
        labels = ax[ii].get_yticklabels()
        ax[ii].set_yticklabels(labels, fontsize=textsize)   
                                
        ii+=1
        mtime = matplotlib.dates.epoch2num(np.mean(time,axis=1))
        ax[ii].semilogy(mtime,intSp,'-k.',label='Ped. Cond.')        
        ax[ii].semilogy(mtime,intSh,'-b.',label='Hall Cond.')        
        #ax[ii].errorbar(mtime,intSp,yerr=dintSp,fmt='-k.',label='Ped. Cond.')        
        #ax[ii].errorbar(mtime,intSh,yerr=dintSh,fmt='-b.',label='Hall Cond.')
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(caxInt)                
        ax[ii].legend(loc=0., fontsize=textsize)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)              
        ax[ii].set_ylabel('Conductance', fontsize=labsize)
        ax[ii].set_xlabel('Time (UT Hours)', fontsize=labsize)
        
        dx=(time[-1,-1]-time[0,0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(np.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(np.ceil(dx*60.0/7.0))
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
        # POS=[0.1,0.5,1.0/(ncols+0.2)-dx,1.0/(nrows+0.2)-dy*1.5]
        POS=[0.1,0.5, 0.8,1.0/(2 + 0.2)-dy*1.5]
            
        figg=pyplot.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(figg.add_axes(rect, facecolor=axesBG))
        ax.append(ax[-1].twinx())    
            
        mtime = matplotlib.dates.epoch2num(np.mean(time,axis=1))
        xlims=[mtime[0],mtime[-1]]

        Emag = np.sqrt(ex**2.0+ey**2.0)
        dEmag = np.sqrt( np.power(dex,2.0)*np.power(ex/Emag,2.0) + np.power(dey,2.0)*np.power(ey/Emag,2.0) ).real
        
        Edir = 180.0/pi*np.arctan2(ex,ey).real
        dEdir=180.0/pi*((1.0/np.absolute(ex))*(1.0/(1.0+np.power(ey/ex,2.0)))*np.sqrt(np.power(dey,2.0)+np.power(ey/ex*dex,2.0))).real

        yy1=np.array([ex.min(),ey.min()]).min()*1e3-10.0
        yy2=np.array([ex.max(),ey.max()]).max()*1e3+10.0
        ylims=[yy1,yy2]
        ylims2=[0.0,Emag.max()*1e3+10.0]

        iax=0
        ax[iax].errorbar(mtime,np.squeeze(ex*1e3),np.squeeze(dex*1e3),fmt='-k.',label='Perp-East')        
        ax[iax].errorbar(mtime,np.squeeze(ey*1e3),np.squeeze(dey*1e3),fmt='-b.',label='Perp-North')        
        ax[iax].set_xlim(xlims)        
        ax[iax].set_ylim(ylims)        
        ax[iax].set_ylabel('Electric Field (mV/m)', fontsize=labsize);
        ax[iax].legend(loc=0,fontsize=textsize)
        # labels = ax[iax].get_xticklabels()
        # ax[iax].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[iax].get_yticklabels()
        # ax[iax].set_yticklabels(labels, fontsize=textsize)
        ax[iax].tick_params(axis='both',labelsize=textsize)
        ax[iax].text(xlims[0],(ylims[1]-ylims[0])*0.05+ylims[1],title,fontsize=labsize, horizontalalignment='left')

        iax+=1
        ax[iax].errorbar(mtime,np.squeeze(Emag*1e3),yerr=np.squeeze(dEmag*1e3),fmt='-k.',label='|E|')        
        ax[iax].set_ylim(ylims2)
        ax[iax].set_xlim(xlims)        
        ax[iax].set_ylabel('Magnitude (mV/m)', fontsize=labsize);
        ax[iax].set_xlabel('Time (UT Hours)', fontsize=labsize);
        # labels = ax[iax].get_xticklabels()
        # ax[iax].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[iax].get_yticklabels()
        # ax[iax].set_yticklabels(labels, fontsize=textsize)
        ax[iax].tick_params(axis='both',labelsize=textsize)

        iax+=1
        ax[iax].errorbar(mtime,np.squeeze(Edir),yerr=np.squeeze(dEdir),fmt='-b.',label='Edir')
        ax[iax].set_ylim([-180.0,180.0])        
        ax[iax].set_xlim(xlims)                
        ax[iax].set_ylabel('Direction (deg)', fontsize=labsize, color='blue');
        # labels = ax[iax].get_xticklabels()
        # ax[iax].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[iax].get_yticklabels()
        # ax[iax].set_yticklabels(labels, fontsize=textsize, color='blue')
        ax[iax].tick_params(axis='both',labelsize=textsize)
        ax[iax].tick_params(axis='y',color='blue')

        dx=(time[-1,-1]-time[0,0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(np.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval= int(np.ceil(dx*60.0/7.0))
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
        pyplot.close(self.figg)
        return
    
    def makePlot(self,time,MLTtime,lat,vx,vy,dvx,dvy,title='Vector Vels',units='m/s',parm='V',\
        p=[200.0,.25,4000,500.0],sc=15.0,cax=[-1000,1000],label='Mag. Lat. (degrees)',\
        ncols=2,vz=[],dvz=[],vzsc=1.0,nrows=4,geo=0,doQuiv=1,textsize=8,labsize=10):

        ncols=3
        nrows=3

        vx=vx.copy()
        vy=vy.copy()
        dvx=dvx.copy()
        dvy=dvy.copy()

        vz=vz.copy()*vzsc
        dvz=dvz.copy()*vzsc
        
        if lat.ndim==2:
            lat2=np.nanmean(lat,axis=1)
            lat=np.concatenate((lat[:,0],lat[[-1],1]))
        else:
            lat2=lat
        
        time2=np.mean(time,axis=1)

        textsize = 8        # size for axes text
        labsize = 10
        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (10,9)


        figg=pyplot.figure(figsize=figsz, facecolor=figBG)
        ax = list()

        pos11 = [0.1  ,0.685,0.255,0.225]
        pos21 = [0.37 ,0.685,0.255,0.225]
        pos31 = [0.64 ,0.685,0.255,0.225]

        pos12 = [0.1  ,0.395,0.255,0.225]
        pos22 = [0.37 ,0.395,0.255,0.225]
        pos32 = [0.64 ,0.395,0.255,0.225]

        pos13 = [0.1  ,0.07 ,0.795,0.225]

        pos_col1 = [0.91  ,0.685,0.02,0.225]
        pos_col2 = [0.91  ,0.395 ,0.02,0.225]
        pos_col3 = [0.91  ,0.07 ,0.02,0.225]

        ax.append(figg.add_axes(pos11, facecolor=axesBG))
        ax.append(figg.add_axes(pos21, facecolor=axesBG))
        ax.append(figg.add_axes(pos31, facecolor=axesBG))

        ax.append(figg.add_axes(pos12, facecolor=axesBG))
        ax.append(figg.add_axes(pos22, facecolor=axesBG))
        ax.append(figg.add_axes(pos32, facecolor=axesBG))

        ax.append(figg.add_axes(pos13, facecolor=axesBG))

        ax.append(figg.add_axes(pos_col1, facecolor=axesBG))
        ax.append(figg.add_axes(pos_col2, facecolor=axesBG))
        ax.append(figg.add_axes(pos_col3, facecolor=axesBG))


        ii=0
        x,dat=plot_utils.timegaps(time,vx)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(lat),np.nanmax(lat)]
        pc=ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1],cmap=pyplot.get_cmap(CMAP))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel(label, fontsize=labsize)
        if geo==1:
            ax[ii].set_title('%s east (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('%s perp east (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')
        if ncols==1:
            cl=pyplot.colorbar(pc)
            cl.set_label(units,fontsize=labsize)
        

        ii=ii+1
        x,dat=plot_utils.timegaps(time,vy)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1],cmap=pyplot.get_cmap(CMAP))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([], fontsize=textsize)
        if geo==1:
            ax[ii].set_title('%s north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('%s perp north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        x,dat=plot_utils.timegaps(time,vz)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=cax[0],vmax=cax[1],cmap=pyplot.get_cmap(CMAP))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([], fontsize=textsize)
        if geo==1:
            ax[ii].set_title('%s up (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')	
        else:
            ax[ii].set_title('%s anti par (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')	
        
        ii=ii+1
        x,dat=plot_utils.timegaps(time,dvx)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc2=ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=0,vmax=cax[1]/5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel(label, fontsize=labsize)
        if geo==1:
            ax[ii].set_title('err %s east (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('err %s perp east (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        x,dat=plot_utils.timegaps(time,dvy)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=0,vmax=cax[1]/5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([], fontsize=textsize)
        if geo==1:
            ax[ii].set_title('err %s north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        else:
            ax[ii].set_title('err %s perp north (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
                
        ii=ii+1
        x,dat=plot_utils.timegaps(time,dvz)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=0,vmax=cax[1]/5)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_yticklabels([], fontsize=textsize)
        if geo==1:
            ax[ii].set_title('err %s up (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')					
        else:
            ax[ii].set_title('err %s anti par (%s) x %d' % (parm,units,vzsc), fontsize=labsize, horizontalalignment='center')
        
        # quiver plot
        ii=ii+1
        I=np.where(np.isnan(vx))
        vx[I]=0
        vy[I]=0
        I=np.where(np.isnan(vy))
        vx[I]=0
        vy[I]=0
        
        I=np.where(np.absolute(dvx)/(np.absolute(vx)+p[0])>p[1])
        vx[I]=0
        vy[I]=0
        I=np.where(np.absolute(dvy)/(np.absolute(vy)+p[0])>p[1])
        vx[I]=0
        vy[I]=0
        

        I=np.where(np.sqrt(vx*vx+vy*vy)>p[2])
        vx[I]=0
        vy[I]=0
    
        I=np.where((np.absolute(dvx)>p[3]) | (np.absolute(dvy)>p[3]))
        vx[I]=0
        vy[I]=0
        
        C=np.ones(np.transpose(vx).shape)-np.sign(np.transpose(vx))
        x=matplotlib.dates.epoch2num(time2)
        [X,Y]=np.meshgrid(x,lat2)
        Q=ax[ii].quiver(X,Y,np.transpose(vx),np.transpose(vy),C,clim=[0,2],scale=900.0*sc,width=0.3*0.005,cmap=pyplot.get_cmap(rCMAP))
        ax[ii].quiverkey(Q, 1.06, 1.15, cax[1], str(cax[1]) + ' ' + units,fontproperties={'size' : labsize},labelpos='S')
        ax[ii].xaxis.tick_bottom()
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        labels = ax[ii].get_xticklabels()
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_xlim((x[0],x[-1]))
        ax[ii].set_ylim((np.nanmin(lat2),np.nanmax(lat2)))	
        
        ax22 = figg.add_axes(ax[ii].get_position(), sharey=ax[ii], frameon=False)
        ax22.plot(MLTtime,np.nan*np.array(MLTtime),'k')
        ax22.set_xlim([MLTtime[0],MLTtime[-1]])
        ax22.xaxis.tick_top()
        ax22.set_xlabel('Magnetic Local Time', fontsize=labsize)
        ax22.xaxis.set_label_position('top')
        ax22.tick_params(axis='both',labelsize=textsize)
        ax22.set_ylim((np.nanmin(lat2),np.nanmax(lat2)))

        # Colorbar top
        ii=ii+1
        cl=pyplot.colorbar(pc,ax[ii])
        cl.set_label(units,fontsize=labsize)
        labels = ax[ii].get_yticklabels()
        ax[ii].set_yticklabels(labels, fontsize=textsize)

        # Colorbar middle
        ii=ii+1
        cl=pyplot.colorbar(pc2,ax[ii])
        cl.set_label(units,fontsize=labsize)
        labels = ax[ii].get_yticklabels()
        ax[ii].set_yticklabels(labels, fontsize=textsize)

        # Colorbar bottom
        ii=ii+1
        cl=pyplot.colorbar(Q,ax[ii])
        ax[ii].invert_yaxis()
        cl.set_ticks(np.linspace(0,2, 5))
        cl.set_ticklabels(['','East', '', 'West',''])

        # Set time formatting
        locator = matplotlib.dates.HourLocator(interval=1)
        formatter = matplotlib.dates.DateFormatter("%H:%M")
        
        dx=(time2[-1]-time2[0])/3600.0
        dx2=dx/7.0
        

        ss=7
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(np.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(np.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)

        self.figg=figg
        
        return
  
class vvelsMagPlot:
    
    def __init__(self):
        """ initialization function """
        return
        
    def close(self):
        pyplot.close(self.figg)
        return        
                    
    def makePlot(self,time,MLTtime,lat,vx,vy,dvx,dvy,title='Vector Vels',units='m/s',parm='V',cax1=[-1000,1000],cax2=[-180,180],label='Mag. Lat. (degrees)'):

        vx=vx.copy()
        vy=vy.copy()
        dvx=dvx.copy()
        dvy=dvy.copy()
        
        if lat.ndim==2:
            lat2=np.nanmean(lat,axis=1)
            lat=np.concatenate((lat[:,0],lat[[-1],1]))
        else:
            lat2=lat
        
        time2=np.mean(time,axis=1)

        textsize = 8        # size for axes text
        labsize = 10
       
        figBG   = 'w'        # the figure background color
        axesBG  = '#f6f6f6'  # the axies background color
        figsz = (7,9)

        dx, dy= 0.015, 0.05	
        nrows=4; ncols=1
        POS=[0.07,0.75,0.8-dx,1.0/(nrows)-dy*1.5]
        POS=[0.1,0.75,0.77-dx,1.0/(nrows)-dy*1.5]
            
        figg=pyplot.figure(figsize=figsz, facecolor=figBG)
        
        ax=[]
        for aa in range(nrows):
            for bb in range(ncols):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
                ax.append(figg.add_axes(rect, facecolor=axesBG))
        
        pc=[]
        
        ii=0
        x,dat=plot_utils.timegaps(time,vx)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        ylim=[np.nanmin(lat),np.nanmax(lat)]
        pc.append(ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=cax1[0],vmax=cax1[1]))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('%s mag (%s)' % (parm,units), fontsize=labsize, horizontalalignment='center')
        ax[ii].text(xlim[0],(ylim[1]-ylim[0])*0.15+ylim[1],title,fontsize=labsize, horizontalalignment='left')
                    
        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,dvx)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc.append(ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=0.0,vmax=cax1[1]/5))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('err %s mag (%s)' %(parm,units), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,vy)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc.append(ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=cax2[0],vmax=cax2[1],cmap=pyplot.get_cmap('hsv')))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('%s dir (%s)' % (parm,'degrees'), fontsize=labsize, horizontalalignment='center')

        ii=ii+1
        
        x,dat=plot_utils.timegaps(time,dvy)
        dat=np.ma.masked_where(np.isnan(dat),dat)
        x=matplotlib.dates.epoch2num(x)
        xlim=[x[0],x[-1]]	
        pc.append(ax[ii].pcolor(x,lat,np.transpose(dat),edgecolors='none',vmin=0,vmax=cax2[1]/5))
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlabel('Time (UT)', fontsize=labsize)
        # labels = ax[ii].get_xticklabels()
        # ax[ii].set_xticklabels(labels, fontsize=textsize)
        # labels = ax[ii].get_yticklabels()
        # ax[ii].set_yticklabels(labels, fontsize=textsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)	
        ax[ii].set_ylabel(label, fontsize=labsize)
        ax[ii].set_title('err %s dir (%s)' % (parm,'degrees'), fontsize=labsize, horizontalalignment='center')
                
        ii=ii+1
        
        if ncols==1:
            bb=1
            for aa in range(len(pc)):
                rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2]/20,POS[3]]
                ax.append(figg.add_axes(rect, facecolor=axesBG))	
                cl=pyplot.colorbar(pc[aa],ax[ii])
                if aa<2:
                    cl.set_label(units,fontsize=labsize)
                else:
                    cl.set_label('degrees',fontsize=labsize)
                labels = ax[ii].get_yticklabels()
                ax[ii].set_yticklabels(labels, fontsize=textsize)
                ii+=1
                    
        locator = matplotlib.dates.HourLocator(interval=1)
    #	locator = matplotlib.dates.MinuteLocator(interval=5)
        formatter = matplotlib.dates.DateFormatter("%H:%M")
        
        dx=(time2[-1]-time2[0])/3600.0
        dx2=dx/7.0
        
        for rr in range(len(ax)):
        
            if dx2>0.5:
                interval=int(np.ceil(dx/7.0))
                locator = matplotlib.dates.HourLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
            elif dx2<0.5:
                interval=int(np.ceil(dx*60.0/7.0))
                locator = matplotlib.dates.MinuteLocator(interval=interval)
                formatter = matplotlib.dates.DateFormatter("%H:%M")
                
            ax[rr].xaxis.set_major_locator(locator)
            ax[rr].xaxis.set_major_formatter(formatter)
        
        # pyplot.show()
        
        self.figg=figg
        
        return figg
