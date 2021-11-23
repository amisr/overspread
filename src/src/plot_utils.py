#! /usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import os
import numpy as np
import matplotlib
matplotlib.use('Agg')
matplotlib.interactive(False)
from matplotlib import pyplot
import matplotlib.dates

from constants import *


def geoplot(az,el,rng,ht,plat,plong,dip,dec,Ibeam=[]):

    labsize=10
    textsize=8

    I=np.where(el>0)[0]
    az=az[I]
    el=el[I]
    plat=plat[I]
    plong=plong[I]
    dip=dip[I]
    dec=dec[I]

    Nbeams=az.size

    elm=int(np.floor(el.min()/10.0)*10.0)

    axesBG  = '#f6f6f6'
    figg=pyplot.figure(1, figsize=(5,8), facecolor='w')

    # polar plot
    r = (90.0-el)*pi/180
    theta = (-az+90)*pi/180
    area = 30
    colors = r
    ax = figg.add_subplot(211, polar=True,facecolor=axesBG)
    c = ax.scatter(theta, r, s=area)
    c.set_alpha(0.75)

    ring_angles = [ x * (pi/180.) for x in range(0,100-elm,10)]
    ring_labels = [ str(x) for x in range(elm,100,10)]
    ring_labels.reverse()
    if ring_angles[0]==0.0:
        ring_angles[0]=ring_angles[0]+0.01
    pyplot.rgrids(ring_angles,ring_labels, angle=-90+22.5,fontsize=textsize)
    pyplot.thetagrids( range(0,360,45), ('E', 'NE', 'N','NW', 'W', 'SW', 'S', 'SE'), fontsize=textsize )

    if len(Ibeam)==0:
        Ibeam=range(Nbeams)

    for ii in range(az.size):
#       t=r'$%d: \ %2.1f^o,\ %2.1f^o$' % (ii,az[ii],el[ii])
        ri=np.where(np.array(Ibeam)==ii)[0]
        t=r'$%d$' % (ri+1)
        #print(ri)

        ax.text(theta[ii],r[ii],t,fontsize=labsize, horizontalalignment='right')


    #

    ax = figg.add_subplot(212,facecolor=axesBG)
    for ii in range(Nbeams):
        ri=np.where(np.array(Ibeam)==ii)[0]
        t=r'$%d$' % (ri+1)
        tlat=plat[ii,:].copy()
        tlon=plong[ii,:].copy()
        I=np.where(np.absolute(np.diff(tlon))>300.0)[0]
        if len(I)>0:
            tlon[I[0]+1:]+=360.0
        ax.plot(tlat,tlon,label=str(ii),linewidth=2)
        ax.text(tlat[-1],tlon[-1],t,fontsize=labsize, horizontalalignment='right')

    ax.tick_params(axis='both',labelsize=textsize)

    ax.set_ylabel('Mag. Long. (degrees)', fontsize=labsize)
    ax.set_xlabel('Mag. Lat. (degrees)', fontsize=labsize)


    return figg

def multi_axes(nrows,ncols,dx=0.02,dy=0.05,figsz=(14,10)):

    figBG   = 'w'        # the figure background color
    axesBG  = '#f6f6f6'  # the axies background color

    if ncols==1 and nrows==1:
        POS=[0.1,0.1,1.0/(ncols+3.0)-dx,1.0/(nrows)-dy*5]
    elif ncols==1:
        POS=[0.1,0.65,1.0/(ncols+3.0)-dx,1.0/(nrows)-dy*2]
    elif nrows==1:
        POS=[0.1,0.1,1.0/(ncols+1)-dx,1.0/(nrows)-dy*5]
    elif ncols==-1 and nrows==-1:
        ncols=1; nrows=2; dx=0.1
        POS=[0.1,0.1,1.0-dx,1.0/(nrows)-dy*5]
    else:
        fy=1.0/(nrows)-dy*1.5
        POS=[0.1,1.0-fy-dy*1.5,1.0/(ncols+1)-dx,fy]

    figg=pyplot.figure(figsize=figsz, facecolor=figBG)
    ax=[]
    for aa in range(nrows):
        for bb in range(ncols):
            rect=[POS[0]+(POS[2]+dx)*bb,POS[1]-(POS[3]+dy)*aa,POS[2],POS[3]]
            ax.append(figg.add_axes(rect, facecolor=axesBG))

    return figg,ax


def plot_array(ax,nrows,ncols,*args):

    # arr should be

    textsize = 8       # size for axes text
    labsize = 10

    xlims=args[-5]
    xlabels=args[-4]
    ylabels=args[-3]
    title=args[-2]
    bmcodes=args[-1]

    ii=0
    for aa in range(nrows):
        I=np.where(np.isfinite(args[aa*3+1]))

        ymin=args[aa*3+1][I].min()-15
        ymax=args[aa*3+1][I].max()+15

        for bb in range(ncols):

            ax[ii].clear()
            for cc in range(args[aa*3].shape[2]):
                tval=args[aa*3][bb,:,cc]
                tax=args[aa*3+1][bb,:,cc]
                terr=np.repeat(args[aa*3+2][bb,:,cc][np.newaxis,:],2,axis=0)

                if aa==0:
                    I=np.where(tval<=0.0)[0]
                    tval[I]=np.nan
                    terr[:,I]=np.nan
                    I=np.where(terr[0,:]>=tval)[0]
                    terr[0,I]=tval[I]-10.0**-10
                I=np.where((tval<xlims[aa][0]) | (tval>xlims[aa][1]))[0]
                tval[I]=np.nan; terr[:,I]=np.nan
                I=np.where((tval-terr[0,:])<xlims[aa][0])[0]; terr[0,I]=tval[I]-xlims[aa][0]-1.0e-10
                I=np.where((tval+terr[1,:])>xlims[aa][1])[0]; terr[1,I]=xlims[aa][1]-tval[I]-1.0e-10
                ax[ii].errorbar(tval,tax,xerr=terr,fmt='-k.')
            if aa==0:
                ax[ii].set_xscale("log")

            if aa==0 and bb==0:
                ax[ii].text(1,(ymax-ymin)*0.15+ymax,title,fontsize=labsize, horizontalalignment='left')
            if bb>0:
                ax[ii].yaxis.set_ticklabels([])
            else:
                ax[ii].set_ylabel(ylabels[aa], fontsize=labsize)
                ax[ii].tick_params(axis='y',labelsize=textsize)

            ax[ii].set_xlabel(xlabels[aa], fontsize=labsize)
            ax[ii].set_xlim(xlims[aa])
            ax[ii].set_ylim((ymin,ymax))

            ax[ii].tick_params(axis='x',labelsize=textsize)
            if aa==0:
                tt=r"$(%.1f^o \rm{az} , \ %.1f^o \rm{el})$" % (bmcodes[ii,1],bmcodes[ii,2])
                ax[ii].set_title(tt, fontsize=labsize, horizontalalignment='center')
            ii=ii+1

    return


def pcolor_plot_dual(x,y,y2,data,cax,xlim,ylim,xl,yl,yl2,title,text,bmcodes,log=0,ax=[],figg=-543,xxx=0.0,yyy=0.0,sc=1.0,im=0):

    textsize = 8        # size for axes text
    labsize = 12

    Nbeams=data.shape[1]
    Nplots=Nbeams+1
    if Nbeams==1:
        nrows=1
        ncols=2
    else:
        nrows=int(np.ceil(np.sqrt(Nplots)))
        ncols=nrows
        if nrows*(nrows-1)>=Nplots:
            ncols=nrows-1

    dx=(x[-1]-x[0])/3600.0
    dx2=dx/12.0
    if dx2>0.5:
        interval=int(np.ceil(dx/12.0))
        locator = matplotlib.dates.HourLocator(interval=interval)
        formatter = matplotlib.dates.DateFormatter("%H")
    elif dx2<0.5:
        interval=int(np.ceil(dx*60.0/3.0/sc))
        locator = matplotlib.dates.MinuteLocator(interval=interval)
        formatter = matplotlib.dates.DateFormatter("%H:%M")

    x=matplotlib.dates.epoch2num(x)
    xlim=[x[0],x[-1]]

    if len(ax)==0 or pyplot.gcf()!=figg:
        (figg,ax)=multi_axes(nrows,ncols)

    for ii in range(Nbeams):
        ax[ii].clear()
        pc=ax[ii].pcolor(x,y[ii,:],np.transpose(data[:,ii,:]),shading='flat',vmin=cax[0],vmax=cax[1])
        ax[ii].xaxis.set_major_locator(locator)
        ax[ii].xaxis.set_major_formatter(formatter)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        if np.mod(ii,ncols)==0:
            ax[ii].set_ylabel(yl, fontsize=labsize)
        ax[ii].tick_params(axis='both',labelsize=textsize)
#       if ii>=(nrows*(ncols-1)):
        if np.mod(ii,nrows)==0:
            ax[ii].set_xlabel(xl, fontsize=labsize)
        tt=r"$(%.1f^o \ \rm{az} , \ %.1f^o \ \rm{el})$" % (bmcodes[ii,1],bmcodes[ii,2])
        ax[ii].set_title(tt, fontsize=labsize, horizontalalignment='center')
        if ii==0:
            ax[ii].text(xlim[0]+(xlim[1]-xlim[0])/2+xxx,(ylim[1]-ylim[0])*0.05*nrows+ylim[1]+yyy,title,fontsize=labsize, horizontalalignment='left')

    if len(ax)==Nplots:
        ii=ii+1
        try:
            tmp=ax[ii].get_position()
            gp=np.array([tmp.xmin,tmp.ymin,tmp.width,tmp.height])
        except:
            gp=ax[ii].get_position()
    #   gp[1]=gp[1]*2.
        gp[3]=gp[3]/6.
        ax[ii].set_position(gp)
        if log:
            cl=pyplot.colorbar(pc,ax[ii],orientation='horizontal',format=pyplot.FormatStrFormatter('$10^{%.1f}$'))
        else:
            cl=pyplot.colorbar(pc,ax[ii],orientation='horizontal')
        ax[ii].yaxis.set_ticklabels([])
        ax[ii].tick_params(axis='x',labelsize=textsize)
    #   labels=np.linspace(cax[0],cax[1],len(labels)).tolist()
        cl.set_label(text,fontsize=labsize)
    else:
        if log:
            cl=figg.colorbar(pc,orientation='vertical',format=pyplot.FormatStrFormatter('$10^{%.1f}$'),pad=0.15)
        else:
            cl=figg.colorbar(pc,orientation='vertical',pad=0.15)
        cl.set_label(text,fontsize=labsize*1.25)

    for jj in range(len(ax)):
        ax2=pyplot.twinx(ax[ii])
        ax[ii].tick_params(axis='x',labelsize=textsize)
 #       ax2.plot([x[0],x[-1]],[y2[0],y2[-1]],'.',markersize=1e-10)
        ax2.set_xlim(xlim)
        ax2.set_ylim([y2[0],y2[-1]])
        ax[ii].tick_params(axis='y',labelsize=textsize)
        ax2.set_ylabel(yl2, fontsize=labsize)

    for jj in range(ii+1,len(ax)):
        ax[jj].set_visible(False)

    return figg,ax


def pcolor_plot(x,y,data,cax,xlim,ylim,xl,yl,title,text,bmcodes,log=0,ax=[],figg=-543,xxx=0.0,yyy=0.0,sc=1.0,im=0,textsize=8,labsize=12,show=1,Ibeam=[],horz=2,xtime=1,doResCol=0,maxbeams=30,cmap='jet',ncols=-1,nrows=-1):

    Nbeams=data.shape[1]

    if Nbeams>maxbeams:
        Ibeam=range(maxbeams)
        Nbeams=maxbeams

    if (nrows==-1) and (ncols==-1):
        Nplots=Nbeams+1
        if Nbeams==1:
            nrows=1
            ncols=2
        else:
            nrows=int(np.ceil(np.sqrt(Nplots)))
            ncols=nrows
            if nrows*(nrows-1)>=Nplots:
                ncols=nrows-1

    if xtime==1:
        #dx=(x[-1]-x[0])/3600.0
        dx=(xlim[-1]-xlim[0])/3600.0
        dx2=dx/12.0
        if dx2>0.5:
            interval=int(np.ceil(dx/12.0))
            locator = matplotlib.dates.HourLocator(interval=interval)
            formatter = matplotlib.dates.DateFormatter("%H")
        elif dx2<0.5:
            interval=int(np.ceil(dx*60.0/5.0/sc))
            locator = matplotlib.dates.MinuteLocator(interval=interval)
            formatter = matplotlib.dates.DateFormatter("%H:%M")

        x=matplotlib.dates.epoch2num(x)
        if len(xlim)!=0:
            xlim=[matplotlib.dates.epoch2num(xlim[0]),matplotlib.dates.epoch2num(xlim[-1])]
        else:
            xlim=[x[0],x[-1]]

    ax0=ax
    if len(ax)==0 or pyplot.gcf()!=figg:
        (figg,ax)=multi_axes(nrows,ncols)

    if len(Ibeam)==0:
        Ibeam=range(Nbeams)

    for ii in range(Nbeams):
        iiB=Ibeam[ii]
        ialt = np.where(np.isfinite(y[iiB,:]))[0]
        print(data.shape[0]*data.shape[2])
        ax[ii].clear()
        if (data.shape[0]*data.shape[2])>2.5e4:
            pc=ax[ii].imshow(np.transpose(data[:,iiB,ialt]),vmin=cax[0],vmax=cax[1],origin='lower',extent=(x.min(), x.max(), np.nanmin(y[iiB,:]), np.nanmax(y[iiB,:])),aspect='auto')
        else:
            pc=ax[ii].pcolor(x,np.squeeze(y[iiB,ialt]),np.transpose(data[:,iiB,ialt]),shading='interp',vmin=cax[0],vmax=cax[1],cmap=cmap)
        if xtime==1:
            ax[ii].xaxis.set_major_locator(locator)
            ax[ii].xaxis.set_major_formatter(formatter)
        ax[ii].set_xlim(xlim)
        ax[ii].set_ylim(ylim)
        if np.mod(ii,ncols)==0:
            ax[ii].set_ylabel(yl, fontsize=labsize)

        ax[ii].tick_params(axis='both',labelsize=textsize)

        if ii>=(Nbeams-(ncols*nrows-Nbeams)):
#       if np.mod(ii,ncols)==0:
            ax[ii].set_xlabel(xl, fontsize=labsize)
        tt=r"$%d \ (%.1f^o \ \rm{az} , \ %.1f^o \ \rm{el})$" % (ii+1,bmcodes[iiB,1],bmcodes[iiB,2])
        ax[ii].set_title(tt, fontsize=labsize, horizontalalignment='center')
        if ii==0:
            ax[ii].text(xlim[0]+(xlim[1]-xlim[0])/2+xxx,(ylim[1]-ylim[0])*0.05*nrows+ylim[1]+yyy,title,fontsize=labsize, horizontalalignment='left')

    if horz>0:
        ii=ii+1
        if (len(ax0)==0) or (doResCol==1):
            try:
                tmp=ax[ii].get_position()
                gp=np.array([tmp.xmin,tmp.ymin,tmp.width,tmp.height])
            except:
                gp=ax[ii].get_position()
        #   gp[1]=gp[1]*2.
            gp[1]=gp[1]+gp[3]
            gp[3]=gp[3]/6.
            gp[1]=gp[1]-gp[3]
            ax[ii].set_position(gp)
        if log:
            cl=pyplot.colorbar(pc,ax[ii],orientation='horizontal',format=pyplot.FormatStrFormatter('$10^{%.1f}$'))
        else:
            cl=pyplot.colorbar(pc,ax[ii],orientation='horizontal')
        ax[ii].yaxis.set_ticklabels([])
        ax[ii].tick_params(axis='y',labelsize=textsize)
        if horz>1:
            #pyplot.setp(labels,rotation='vertical')
            t = ax[ii].get_xticklabels()
            ax[ii].set_xticklabels(t,rotation=90)
    #   labels=np.linspace(cax[0],cax[1],len(labels)).tolist()
        cl.set_label(text,fontsize=labsize*1.25)
    else:
        if log:
            cl=figg.colorbar(pc,orientation='vertical',format=pyplot.FormatStrFormatter('$10^{%.1f}$'))
        else:
            cl=figg.colorbar(pc,orientation='vertical')
        cl.set_label(text,fontsize=labsize*1.25)

    for jj in range(ii+1,len(ax)):
        ax[jj].set_visible(False)

    figg.show()
    xxx
    return figg,ax


def spc_pcolor_plot(meas,mod,fsc,alt,bmcodes,title='',figg1=-542,ax1=[],figg2=-543,ax2=[],figg3=-544,ax3=[],ylim=[100.0,500.0],clim=[0.0,1.0]):

    # compute spectra
    tmp=np.concatenate((meas,np.conjugate(meas[:,:0:-1,:])),axis=1) # hermitian extension
    smeas=np.real(np.fft.fftshift(np.fft.fft(tmp,axis=1),axes=[1])) # compute spectra
    smeas=np.swapaxes(smeas,0,1)

    # compute spectra
    tmp=np.concatenate((mod,np.conjugate(mod[:,:0:-1,:])),axis=1) # hermitian extension
    smod=np.real(np.fft.fftshift(np.fft.fft(tmp,axis=1),axes=[1])) # compute spectra
    smod=np.swapaxes(smod,0,1)

    freqs=np.arange(-meas.shape[1]+1,meas.shape[1])*fsc/1000.0

    DRC=0
    if figg1 != -542:
        DRC=1

    txt=r'$\rm{Power \ Spectral \ Density \ - \ Measured}$'
    I=np.where(smeas<0)
    dat=np.real(np.log10(smeas)); dat[I]=np.nan
    dat=np.ma.masked_where(np.isnan(dat),dat)
    (figg1,ax1)=pcolor_plot(freqs,alt,dat,clim,[freqs[0],freqs[-1]],ylim,'Frequency (kHz)','Altitude (km)',title,txt,bmcodes,log=0,xtime=0,figg=figg1,ax=ax1,doResCol=DRC)

    txt=r'$\rm{Power \ Spectral \ Density \ - \ Modeled}$'
    I=np.where(smod<0)
    dat=np.real(np.log10(smod)); dat[I]=np.nan
    dat=np.ma.masked_where(np.isnan(dat),dat)
    (figg2,ax2)=pcolor_plot(freqs,alt,dat,clim,[freqs[0],freqs[-1]],ylim,'Frequency (kHz)','Altitude (km)',title,txt,bmcodes,log=0,xtime=0,figg=figg2,ax=ax2,doResCol=DRC)

    txt=r'$\rm{Power \ Spectral \ Density \ - \ Residual}$'
    I=np.where(smod<1e-30)
    dat=(smod-smeas)/np.repeat(np.sum(smod,axis=0)[np.newaxis],smod.shape[0],axis=0); dat[I]=np.nan
    dat=np.ma.masked_where(np.isnan(dat),dat)
    (figg3,ax3)=pcolor_plot(freqs,alt,dat,[-0.02,0.02],[freqs[0],freqs[-1]],ylim,'Frequency (kHz)','Altitude (km)',title,txt,bmcodes,log=0,xtime=0,figg=figg3,ax=ax3,doResCol=DRC)

    return figg1,ax1,figg2,ax2,figg3,ax3


def acf_plot(meas,errs,mod,alt,bmcodes,title='',ax=[],figg=-542,maxbeams=10,Ibeams=[],maxlag=-1):

    textsize = 8        # size for axes text
    labsize = 10

    (Nbeams,Nlags,Nhts)=meas.shape
    nrows=1
    if len(Ibeams)==0:
        ncols=Nbeams
        Ibeams=range(ncols)
    else:
        ncols=len(Ibeams)
    if ncols>maxbeams:
        ncols=maxbeams
        Ibeams=range(ncols)
    ii=np.where(np.isfinite(alt))
    ylim=[alt[ii].min()-25,alt[ii].max()+25]
    xl='Lag'
    yl='Altitude (km)'

    if maxlag==-1:
        maxlag=Nlags

    if len(ax)==0 or figg==-542:
        (figg,ax)=multi_axes(nrows,ncols)
    pyplot.figure(figg.number)

    for ii in range(ncols):
        rr=Ibeams[ii]
        ax[ii].clear()
        for jj in range(Nhts):
            try:
                dh=(alt[rr,jj+1]-alt[rr,jj])/2.0
            except:
                ''
            Iy=np.where(np.isfinite(mod[rr,:,jj]))[0]
            if jj==0:
                ml=len(Iy)
            elif len(Iy)>ml:
                ml=len(Iy)
            if len(Iy)>0:
                sc=1./np.nanmax(np.absolute(meas[rr,Iy,jj]))*dh
                ax[ii].errorbar(np.arange(len(Iy)),alt[rr,jj]+sc*meas[rr,Iy,jj].imag,yerr=sc*np.sqrt(errs[rr,Iy,jj]),fmt='r-')
                ax[ii].errorbar(np.arange(len(Iy)),alt[rr,jj]+sc*meas[rr,Iy,jj].real,yerr=sc*np.sqrt(errs[rr,Iy,jj]),fmt='b-')
                ax[ii].plot(range(len(Iy)),alt[rr,jj]+sc*mod[rr,Iy,jj].real,'k')
                ax[ii].plot(range(len(Iy)),alt[rr,jj]+sc*mod[rr,Iy,jj].imag,'k')
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlim([0,ml])
        ax[ii].set_xlabel(xl, fontsize=labsize)
        if ii==0:
            ax[ii].set_ylabel(yl, fontsize=labsize)
            ax[ii].text(1,(ylim[1]-ylim[0])*0.1+ylim[1],title,fontsize=labsize, horizontalalignment='left')
        ax[ii].tick_params(axis='both',labelsize=textsize)

        tt=r"$(%.1f^o \ \rm{az} , \ %.1f^o \ \rm{el})$" % (bmcodes[rr,1],bmcodes[rr,2])
        ax[ii].set_title(tt, fontsize=labsize, horizontalalignment='center')

    return figg,ax


def spc_plot(meas,errs,mod,alt,bmcodes,title='',ax=[],figg=-542,maxbeams=10,Ibeams=[],measSpc=0,fsc=1.0,Ialt=-1,ylim=-1,scfac=1.0,ploterrs=1,zeroline=1):

    textsize = 8        # size for axes text
    labsize = 10
    #print(alt)
    (Nbeams,Nlags,Nhts)=meas.shape
    nrows=1
    if len(Ibeams)==0:
        ncols=Nbeams
        Ibeams=range(ncols)
    else:
        ncols=len(Ibeams)
    if ncols>maxbeams:
        ncols=maxbeams
        Ibeams=range(ncols)
    ii=np.where(np.isfinite(alt))
    if ylim==-1:
        ylim=[alt[ii].min()-25,alt[ii].max()+25]
    xl='Frequency'
    yl='Altitude (km)'
    dh=(alt[Ibeams[0],1]-alt[Ibeams[0],0])/2.0

    if measSpc==0:
        Iy=np.where(np.absolute(np.nansum(mod[0,:,:],1))>0.0)[0]
        meas=meas[:,Iy,:]; mod=mod[:,Iy,:]; errs=errs[:,Iy,:]
        if np.all(np.isnan(np.absolute(meas[:,0,:]))):
            meas[:,0,:]=mod[:,0,:]

        # compute spectra
        tmp=np.concatenate((meas,np.conjugate(meas[:,:0:-1,:])),axis=1) # hermitian extension
        smeas=np.real(np.fft.fftshift(np.fft.fft(tmp,axis=1),axes=[1])) # compute spectra

        # compute spectra
        tmp=np.concatenate((mod,np.conjugate(mod[:,:0:-1,:])),axis=1) # hermitian extension
        smod=np.real(np.fft.fftshift(np.fft.fft(tmp,axis=1),axes=[1])) # compute spectra

        freqs=np.arange(-meas.shape[1]+1,meas.shape[1])*fsc

    else:
        smeas=meas; smod=mod
        freqs=np.arange(-np.ceil(meas.shape[1]/2.0)+1,np.ceil(meas.shape[1]/2.0))*fsc

    if len(ax)==0 or figg==-542:
        (figg,ax)=multi_axes(nrows,ncols)
    pyplot.figure(figg.number)

    for ii in range(ncols):
        rr=Ibeams[ii]
        ax[ii].clear()
        if Ialt==-1:
            Ialtt=range(Nhts)
        else:
            Ialtt=Ialt[ii]
        for jj in Ialtt:
            try:
                dh=(alt[rr,jj+1]-alt[rr,jj])/2.0
            except:
                ''
            sc=scfac/np.absolute(smeas[rr,:,jj]).max()*dh
            derr=np.sqrt(errs[rr,1,jj])/meas[rr,1,jj]
            if ploterrs:
                yerr=sc*smeas[rr,:,jj]*derr
                #print(yerr)
                ax[ii].errorbar(freqs,alt[rr,jj]+sc*smeas[rr,:,jj],yerr=sc*smeas[rr,:,jj]*derr,fmt='r-')
            else:
                ax[ii].plot(freqs,alt[rr,jj]+sc*smeas[rr,:,jj],'r-')
            ax[ii].plot(freqs,alt[rr,jj]+sc*smod[rr,:,jj],'k')
            ax[ii].plot(freqs,freqs/freqs*alt[rr,jj],'k--')
        ax[ii].set_ylim(ylim)
        ax[ii].set_xlim([freqs[0],freqs[-1]])
        ax[ii].set_xlabel(xl, fontsize=labsize)
        if ii==0:
            ax[ii].set_ylabel(yl, fontsize=labsize)
            ax[ii].text(1,(ylim[1]-ylim[0])*0.1+ylim[1],title,fontsize=labsize, horizontalalignment='left')
        ax[ii].tick_params(axis='y',labelsize=textsize)

        tt=r"$(%.1f^o \ \rm{az} , \ %.1f^o \ \rm{el})$" % (bmcodes[rr,1],bmcodes[rr,2])
        ax[ii].set_title(tt, fontsize=labsize, horizontalalignment='center')

    return figg,ax

def test_plot(RF,irec,ax=[],figg=-542,xlims=[(0.01,10),(0,3),(-1,1)],maxbeams=10,Ibeams=[],dofrac=0):

    nrows=3
    if dofrac:
        nrows=nrows+1

    if len(Ibeams)==0:
        ncols=RF.Nbeams
        Ibeams=range(ncols)
    else:
        ncols=len(Ibeams)
    nalts=RF.FITS['Altitude'].shape[1]

    if ncols>maxbeams:
        ncols=maxbeams
        Ibeams=range(ncols)

    #I=np.where(RF.FITS['FitInfo']['fitcode']<=0)

    # densities
    in1a=np.reshape(RF.FITS['Ne'][Ibeams,:],(ncols,nalts,1))/1.0e11
    in1b=np.reshape(RF.FITS['Altitude'][Ibeams,:],(ncols,nalts,1))/1000.0
    in1c=np.reshape(RF.FITS['dNe'][Ibeams,:],(ncols,nalts,1))/1.0e11

    # temperatures
    in2a=RF.FITS['Fits'][Ibeams,:,:,1]/1000.0
    in2b=np.repeat(np.reshape(RF.FITS['Altitude'][Ibeams,:],(ncols,nalts,1)),RF.FITS['Fits'].shape[2],axis=2)/1000.0
    in2c=RF.FITS['Errors'][Ibeams,:,:,1]/1000.0

    # velocities
#    in3a=np.reshape(RF.FITS['Fits'][Ibeams,:,0,3],(ncols,nalts,1))/100.0
#    in3b=np.reshape(RF.FITS['Altitude'][Ibeams,:],(ncols,nalts,1))/1000.0
#    in3c=np.reshape(RF.FITS['Errors'][Ibeams,:,0,3],(ncols,nalts,1))/100.0
    in3a=RF.FITS['Fits'][Ibeams,:,:,3]/100.0
    in3b=np.repeat(np.reshape(RF.FITS['Altitude'][Ibeams,:],(ncols,nalts,1)),RF.FITS['Fits'].shape[2],axis=2)/1000.0
    in3c=RF.FITS['Errors'][Ibeams,:,:,3]/100.0


    # fractions
    in4a=RF.FITS['Fits'][Ibeams,:,:,0]
    in4b=np.repeat(np.reshape(RF.FITS['Altitude'][Ibeams,:],(ncols,nalts,1)),RF.FITS['Fits'].shape[2],axis=2)/1000.0
    in4c=RF.FITS['Errors'][Ibeams,:,:,0]

    title= "%d-%d-%d %.3f-%.3f UT" % (RF.Time['Month'][0],RF.Time['Day'][0],RF.Time['Year'][0],RF.Time['dtime'][0],RF.Time['dtime'][1])
    xlabels=[r'$\rm{Ne} \ (10^{11} \ \rm{m}^{-3})$', r'$\rm{T} \ (10^3 \ \rm{K})$', r'$\rm{Vlos} \ (10^2 \ \rm{m/s})$']
    ylabels=['Altitude (km)','Altitude (km)','Altitude (km)','Altitude (km)']

    if dofrac:
        xlabels.append(r'$\rm{Fraction}$')
        ylabels.append('Altitude (km)')
        if len(xlims)==3:
            xlims.append((0,1))

        if len(ax)==0 or figg==-542:
            (figg,ax)=multi_axes(nrows,ncols,dx=0.015)
        #pyplot.figure(figg.number)
        plot_array(ax,nrows,ncols,in1a,in1b,in1c,in2a,in2b,in2c,in3a,in3b,in3c,in4a,in4b,in4c,xlims,xlabels,ylabels,title,RF.BMCODES[Ibeams,:])

    else:
        xlabels=[r'$\rm{Ne} \ (10^{11} \ \rm{m}^{-3})$', r'$\rm{T} \ (10^3 \ \rm{K})$', r'$\rm{Vlos} \ (10^2 \ \rm{m/s})$']
        ylabels=['Altitude (km)','Altitude (km)','Altitude (km)']
        title= "%d-%d-%d %.3f-%.3f UT" % (RF.Time['Month'][0],RF.Time['Day'][0],RF.Time['Year'][0],RF.Time['dtime'][0],RF.Time['dtime'][1])

        if len(ax)==0 or figg==-542:
            (figg,ax)=multi_axes(nrows,ncols,dx=0.015)
        #pyplot.figure(figg.number)
        plot_array(ax,nrows,ncols,in1a,in1b,in1c,in2a,in2b,in2c,in3a,in3b,in3c,xlims,xlabels,ylabels,title,RF.BMCODES[Ibeams,:])

    return figg,ax,title
