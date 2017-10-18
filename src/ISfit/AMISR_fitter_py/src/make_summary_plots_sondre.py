
import os
import scipy
import numpy as np
import tables
import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot
from matplotlib import cm
import matplotlib.dates


# TO DO, add the ability to plot all ions back in to the code.


def timegaps(time,data,rngOpt=[]):

    if len(rngOpt)>0:
        doRng=1
        rng2=[]

    time2=[]
    if scipy.ndim(data)==3:
        concnan=scipy.zeros((1,data.shape[1],data.shape[2]),dtype=data.dtype)*scipy.nan
    elif scipy.ndim(data)==2:
        concnan=scipy.zeros((1,data.shape[1]),dtype=data.dtype)*scipy.nan
    data2=data.copy()
    dt=scipy.median(scipy.diff(scipy.mean(time,axis=1)))
    for aa in range(time.shape[0]-1):
        time2.append(time[aa,0])
        if ( (time[aa+1,1]-time[aa,0]) > (dt*2.0) ):
            time2.append(time[aa,1])
            #print datetime.datetime.utcfromtimestamp(time[aa,1])
            if scipy.ndim(data)==3:
                data2=scipy.concatenate((data2[0:len(time2)-1,:,:],concnan,data2[len(time2)-1:,:,:]),axis=0)
            elif scipy.ndim(data)==2:
                data2=scipy.concatenate((data2[0:len(time2)-1,:],concnan,data2[len(time2)-1:,:]),axis=0)

    time2.append(time[-1,0])
    time2.append(time[-1,1])

    return scipy.array(time2), data2


def pcolor_plot_antenna(x,y,data,clim,xlim,ylim,xl,yl,title,text,az,el,trimmed_unix_time,save_fig_name=None,log=0):
    # Scale factor for number of x-tick markers when
    # more than 12 hours of data is being plotted.
    sc=1.0

    # Set some font sizes
    textsize=8
    labsize=12

    # Label alignment adjustment values
    xxx=0.0
    yyy=0.0

    # Colormap to use.
    cmap='jet'
    cmap_to_use = cm.get_cmap(cmap)
    cmap_to_use.set_bad('w',0)

    # Then set up some x-axis time formatting that is common to all of the
    # of the groups of beams we will plot.
    num_hours       = (xlim[-1] - xlim[0]) / 3600.0
    num_half_days   = num_hours / 12.0
    if num_half_days > 0.5:
        interval    = int(np.ceil(num_hours / 12.0))
        locator     = matplotlib.dates.HourLocator(interval=interval)
        formatter   = matplotlib.dates.DateFormatter("%H")
    elif num_half_days < 0.5:
        interval    = int(np.ceil(num_hours * 60.0 / 5.0 / sc))
        locator     = matplotlib.dates.MinuteLocator(interval=interval)
        formatter   = matplotlib.dates.DateFormatter("%H:%M")

    # Convert times to from epoch to numbers
    x = matplotlib.dates.epoch2num(x)
    xlim = [matplotlib.dates.epoch2num(xlim[0]),matplotlib.dates.epoch2num(xlim[-1])]

    # Now set up the figure and axes
    figBG   = 'w'        # the figure background color
    axesBG  = '#f6f6f6'  # the axies background color
    figsz = (14,10)

    fig = pyplot.figure(figsize=figsz, facecolor=figBG)
    ax1 = fig.add_axes([0.1,0.3,0.75,0.6], axis_bgcolor=axesBG)
    ax2 = fig.add_axes([0.1,0.1,0.75,0.15], axis_bgcolor=axesBG)
    cax = fig.add_axes([0.87,0.3,0.03,0.6])
    

    data_plot = data[:,0,:]

    num_x, num_y    = data_plot.shape
    temp_y          = y[0,:].reshape((num_y,))
    temp_y          = np.repeat(temp_y[np.newaxis,:],num_x,axis=0)
    temp_y_diff     = np.repeat(np.diff(temp_y[0,:])[np.newaxis,:],num_x,axis=0)
    y_diff          = np.zeros(temp_y.shape)
    y_diff[:,0:-1]  = temp_y_diff
    y_diff[:,-1]    = temp_y_diff[:,-1]

    # Construct the range array for plotting
    y_plot              = np.zeros((num_x+1,num_y+1))
    y_plot[0:-1,0:-1]   = temp_y - y_diff/2
    y_plot[0:-1,-1]     = temp_y[:,-1] + y_diff[:,-1]/2
    y_plot[-1,:]        = y_plot[-2,:]

    # Construct the time array for plotting
    x_plot = np.zeros((num_x+1,num_y+1))
    x_plot = np.repeat(x[:,np.newaxis],num_y+1,axis=1)


    # Use pcolormesh to plot the data.
    pc = ax1.pcolormesh(x_plot,y_plot,data_plot,vmin=clim[0],vmax=clim[1],cmap=cmap_to_use) #shading='interp',

    # Set axis formatting and labels
    ax1.xaxis.set_major_locator(locator)
    ax1.xaxis.set_major_formatter(formatter)
    ax1.set_xlim(xlim)
    ax1.set_ylim(ylim)
    ax1.set_ylabel(yl, fontsize=labsize)

    ax1.tick_params(axis='both',labelsize=textsize)

    # Determine if we are on the last row of beams to be plotted, if so
    # then label the x-axis appropriately.
    ax1.set_xlabel(xl, fontsize=labsize)

    # tt = ''
    # ax1.set_title(tt, fontsize=labsize, horizontalalignment='center')
    
    ax1.set_title(title,fontsize=labsize, horizontalalignment='center')


    # Now we need to do the colorbar
    # Scale the colorbar and change it's positions slightly.

    if log:
        cl=pyplot.colorbar(pc,cax,format=pyplot.FormatStrFormatter('$10^{%.1f}$'))
    else:
        cl=pyplot.colorbar(pc,cax)
    cax.xaxis.set_ticklabels([])
    cax.tick_params(axis='y',labelsize=textsize)

    # t = cax.get_xticklabels()
    # cax.set_yticklabels(t,rotation=90)
    cl.set_label(text,fontsize=labsize*1.25)

    x = matplotlib.dates.epoch2num(trimmed_unix_time)
    ax2.plot(x,az,'b')
    ax2.plot(x,el,'r')
    ax2.set_xlim(xlim)
    ax2.set_ylim([0,360])
    ax2.xaxis.set_major_locator(locator)
    ax2.xaxis.set_major_formatter(formatter)
    ax2.legend(['Azimuth','Elevation'])
    ax2.set_xlabel(xl, fontsize=labsize)


    # Finally, save the figure if a name was provided.
    fig.savefig(save_fig_name)

    # To help limit RAM usage, clear the figure from memory after
    # plotting and saving is done
    pyplot.close('all')


def pcolor_plot_antenna_all(plot_info, data):

    #RF,clims=[[10,12],[0,1500],[0,3000],[0,4],[-500,500]],ylim=[],ylim0=[],tlim=[],txMax=1.0e6,mi=[],nionPlot=0):

    # get required time information
    unix_time   = data['Time']['UnixTime']
    max_time    = plot_info['max_time']

    # get mean time for az/el plots
    mean_unix_time = np.array([(float(x[0]) + float(x[1])) / 2. for x in unix_time])

    # Determine how many time groups of plots we should make
    total_time = (unix_time[-1,-1] - unix_time[0,0]) / 3600.0
    num_time_groups = scipy.ceil(total_time / max_time)

    # Check if the output path exists
    print plot_info['plotsdir']
    if not os.path.exists(plot_info['plotsdir']):
        raise IOError("Specified path: ''%s'' does not exist!" % plot_info['plotsdir'])

    print("There will be %d time groups of plots made." % num_time_groups)

    # First make the plots of the NeFromPower parameters
    altitude        = data['NeFromPower']['alt'] / 1000.0
    inds            = np.where(~np.isfinite(altitude))     # set all non finite values nan
    altitude[inds]  = np.nan

    ne_notr = data['NeFromPower']['ne_notr']
    inds = np.where(ne_notr < 0)
    ne_notr[inds] = np.nan
    ne_notr = np.real(np.log10(ne_notr))

    snr = data['NeFromPower']['snr']
    snr[inds] = np.nan
    snr = 10.0*np.real(np.log10(snr))

    az = data['AvgAzimuth']
    el = data['AvgElevation']

    # Determine the y-axis limits for the nonfitted data
    if len(plot_info['nonfitted_ylim'])==0:
        nonfitted_ylim=[np.nanmin(altitude), np.nanmax(altitude)]
    else:
        nonfitted_ylim = plot_info['nonfitted_ylim']

    start_ind=0;
    for time_ind in range(int(num_time_groups)):
        end_ind = scipy.where(unix_time[:,-1] <= (unix_time[start_ind,0] + max_time * 3600.0))[0]
        end_ind = end_ind[-1]
        tlim = [start_ind,end_ind]
        start_ind = end_ind+1

        if num_time_groups>1:
            txtra='_day' + str(time_ind)
        else:
            txtra=''

        # Figure out the time text for the title of the plots
        title =  "%d-%d-%d "    % (data['Time']['Month'][tlim[0],0],data['Time']['Day'][tlim[0],0],data['Time']['Year'][tlim[0],0])
        title += "%.3f UT "     %  data['Time']['dtime'][tlim[0],0]
        title += "- %d-%d-%d "  % (data['Time']['Month'][tlim[-1],1],data['Time']['Day'][tlim[-1],1],data['Time']['Year'][tlim[-1],1])
        title += "%.3f UT"      %  data['Time']['dtime'][tlim[-1],1]

        # Set up the x-axis stuff
        xlim                = [unix_time[tlim[0],0], unix_time[tlim[1],1]]
        trimmed_unix_time   = unix_time[tlim[0]:(tlim[1]+1)]
        trimmed_mean_unix_time = mean_unix_time[tlim[0]:(tlim[1]+1)]
        trimmed_az = az[tlim[0]:(tlim[1]+1)]
        trimmed_el = el[tlim[0]:(tlim[1]+1)]

        # Plot the uncorrected density Ne_NoTr (Te/Ti)
        clim = plot_info['clims'][0]
        txt  = r'$\rm{Ne - no Tr} \ (\rm{m}^{-3})$'

        plot_times, plot_datas = timegaps(trimmed_unix_time,ne_notr[tlim[0]:tlim[1]+1])
        plot_datas = np.ma.masked_where(scipy.isnan(plot_datas),plot_datas)

        if plot_info['saveplots']==1:
            oname = title + '_NePower_NoTr' + txtra + '.png'
            output_fig_name = os.path.join(plot_info['plotsdir'],oname)
        else:
            output_fig_name = None

        pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,nonfitted_ylim,'Time (UT)','Altitude (km)',
                            title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=1)

        # Plot the SNR now.
        clim = [-20.0,10.0]
        txt  = r'$\rm{SNR} \ (\rm{dB})$'

        plot_times, plot_datas = timegaps(trimmed_unix_time,snr[tlim[0]:tlim[1]+1])
        plot_datas = np.ma.masked_where(scipy.isnan(plot_datas),plot_datas)

        if plot_info['saveplots']==1:
            oname = title + '_SNR' + txtra + '.png'
            output_fig_name = os.path.join(plot_info['plotsdir'],oname)
        else:
            output_fig_name = None

        pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,nonfitted_ylim,'Time (UT)','Altitude (km)',
                            title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)


    # Now plot the fitted data if we need to.   
    if plot_info['is_fitted']:
        # number of ions related information
        nionPlot = 0
        mi = data['FittedParams']['mi']
        ion = 0

        # First make the plots of the NeFromPower parameters
        altitude        = data['FittedParams']['alt'] / 1000.0
        inds            = np.where(~np.isfinite(altitude))     # set all non finite values nan
        altitude[inds]  = np.nan

        ne = data['FittedParams']['ne']
        inds = np.where(ne < 0)
        ne[inds] = np.nan
        ne = np.real(np.log10(ne))

        ti = data['FittedParams']['ti']
        ti[inds] = np.nan
        te = data['FittedParams']['te']
        te[inds] = np.nan
        tr = te/ti
        tr[inds] = np.nan
        vlos = data['FittedParams']['vlos']
        vlos[inds] = np.nan
        nu = data['FittedParams']['nuin']
        nu[inds] = np.nan
        frac = data['FittedParams']['frac']
        frac[inds] = np.nan

        # Determine the y-axis limits for the fitted data
        if len(plot_info['fitted_ylim'])==0:
            fitted_ylim=[np.nanmin(altitude), np.nanmax(altitude)]
        else:
            fitted_ylim = plot_info['fitted_ylim']


        start_ind=0;
        for time_ind in range(int(num_time_groups)):
            end_ind = scipy.where(unix_time[:,-1] <= (unix_time[start_ind,0] + max_time * 3600.0))[0]
            end_ind = end_ind[-1]
            tlim = [start_ind,end_ind]
            start_ind = end_ind+1

            if num_time_groups>1:
                txtra='_day' + str(time_ind)
            else:
                txtra=''

            # Figure out the time text for the title of the plots
            title =  "%d-%d-%d "    % (data['Time']['Month'][tlim[0],0],data['Time']['Day'][tlim[0],0],data['Time']['Year'][tlim[0],0])
            title += "%.3f UT "     %  data['Time']['dtime'][tlim[0],0]
            title += "- %d-%d-%d "  % (data['Time']['Month'][tlim[-1],1],data['Time']['Day'][tlim[-1],1],data['Time']['Year'][tlim[-1],1])
            title += "%.3f UT"      %  data['Time']['dtime'][tlim[-1],1]

            # Set up the x-axis stuff
            xlim                = [unix_time[tlim[0],0], unix_time[tlim[1],1]]
            trimmed_unix_time   = unix_time[tlim[0]:(tlim[1]+1)]
            trimmed_mean_unix_time = mean_unix_time[tlim[0]:(tlim[1]+1)]
            trimmed_az = az[tlim[0]:(tlim[1]+1)]
            trimmed_el = el[tlim[0]:(tlim[1]+1)]

            # plot density
            clim = plot_info['clims'][0]
            txt  = r'$\rm{Ne} \ (\rm{m}^{-3})$'

            plot_times,plot_datas=timegaps(trimmed_unix_time,ne[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)


            if plot_info['saveplots']==1:
                oname = title + '_Ne' + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=1)

            # plot ion temp
            clim = plot_info['clims'][1]
            txt  = r'$\rm{Ti} \ (\rm{K}) \ - %d \ {\rm amu}$' % (mi[ion])

            plot_times,plot_datas=timegaps(trimmed_unix_time,ti[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)

            if plot_info['saveplots']==1:
                oname = oname = title + '_Ti' + '-' + str(ion) + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)

            # plot elec temp
            clim = plot_info['clims'][2]
            txt = r'$\rm{Te} \ (\rm{K})$'

            plot_times,plot_datas=timegaps(trimmed_unix_time,te[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)

            if plot_info['saveplots']==1:
                oname = oname = title + '_Te' + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)

            # plot te/ti
            clim = plot_info['clims'][3]
            txt  = r'$\rm{Te/Ti} \ - %d \ {\rm amu}$' % (mi[ion])

            plot_times,plot_datas=timegaps(trimmed_unix_time,tr[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)

            if plot_info['saveplots']==1:
                oname = oname = title + '_Tr' + '-' + str(ion) + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)

            # plot M+
            if len(plot_info['clims']) > 5:
                clim = plot_info['clims'][5]
            else:
                clim = [0,1]
            txt = r'$\rm{Ion \ fraction} \ - %d \ {\rm amu}$' % (mi[ion])

            plot_times,plot_datas=timegaps(trimmed_unix_time,frac[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)

            if plot_info['saveplots']==1:
                oname = oname = title + '_IonFrac' + '-' + str(ion) + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)

            # plot nu_in
            clim = [-2,5]
            txt  = r'$\rm{\nu_{in}} \ \ (\rm{s^{-1}}) \ - %d \ {\rm amu}$' % (mi[0])

            plot_times,plot_datas=timegaps(trimmed_unix_time,nu[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)

            if plot_info['saveplots']==1:
                oname = oname = title + '_nuin' + '-' + str(ion) + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)


            # plot drift
            clim = plot_info['clims'][4]
            txt  = r'$\rm{Vlos} \ (\rm{m/s}) \ - %d \ {\rm amu}$' % (mi[ion])

            plot_times,plot_datas=timegaps(trimmed_unix_time,vlos[tlim[0]:(tlim[1]+1)])
            plot_datas = np.ma.masked_where(np.isnan(plot_datas),plot_datas)

            if plot_info['saveplots']==1:
                oname = oname = title + '_Vlos' + '-' + str(ion) + txtra + '.png'
                output_fig_name = os.path.join(plot_info['plotsdir'],oname)
            else:
                output_fig_name = None

            pcolor_plot_antenna(plot_times,altitude,plot_datas,clim,xlim,fitted_ylim,'Time (UT)','Altitude (km)',
                                title,txt,trimmed_az,trimmed_el,trimmed_mean_unix_time,save_fig_name=output_fig_name,log=0)



def has_fitted_params(fname):

    with tables.open_file(fname) as h5:
        try:
            h5.get_node('/FittedParams')
            fitted_params = True
        except tables.NoSuchNodeError:
            fitted_params = False

    return fitted_params


#max_time sets the maximum number of hours of data to plot in one plot
#max_beams sets the number of beams to plot in a plot

# This code is only designed to plot AMISR data

def replot_pcolor_antenna_all(fname,saveplots=0,opath='.',clims=[[10,12],[0,1500],[0,3000],[0,4],[-500,500]],nonfitted_ylim=[],fitted_ylim=[],max_time=24.0,max_beams=11):
    # Read the entire data file. Really we only need, Ne, Te, Ti, Tr, Vlos, Frac, SNR, nuin, NePower_NoTr
    # Also we want to plot the errors in eNe, eTe, eTi, eVlos

    # Initialize a plotting information dictionary and data dictionary
    plot_info = dict()
    data = dict()
    data['NeFromPower'] = dict()
    data['Time'] = dict()
    data['FittedParams'] = dict()

    # Copy the plotting configuration information
    plot_info['saveplots']      = saveplots
    plot_info['plotsdir']       = opath
    plot_info['clims']          = clims
    plot_info['nonfitted_ylim'] = nonfitted_ylim
    plot_info['fitted_ylim']    = fitted_ylim
    plot_info['max_time']       = max_time
    plot_info['max_beams']      = max_beams

    # If our fitted file has fitted parameters in it then we'll plot them
    if has_fitted_params(fname):
        plot_info['is_fitted'] = True
    else:
        plot_info['is_fitted'] = False

    # Now we'll read in the data that we need
    with tables.open_file(fname) as h5:
        # Fitted data if available
        if plot_info['is_fitted']:
            FITS                            = h5.root.FittedParams.Fits.read()
            data['FittedParams']['ne']      = h5.root.FittedParams.Ne.read()
            data['FittedParams']['te']      = FITS[:,:,:,-1,1]
            data['FittedParams']['ti']      = FITS[:,:,:,0,1]
            data['FittedParams']['vlos']    = FITS[:,:,:,0,-1]
            data['FittedParams']['frac']    = FITS[:,:,:,0,0]
            data['FittedParams']['nuin']    = FITS[:,:,:,0,2]
            data['FittedParams']['rng']     = h5.root.FittedParams.Range.read()
            data['FittedParams']['alt']     = h5.root.FittedParams.Altitude.read()

            ERRORS                          = h5.root.FittedParams.Fits.read()
            data['FittedParams']['ene']     = h5.root.FittedParams.dNe.read()
            data['FittedParams']['ete']     = ERRORS[:,:,:,-1,1]
            data['FittedParams']['eti']     = ERRORS[:,:,:,0,1]
            data['FittedParams']['evlos']   = ERRORS[:,:,:,0,-1]

            data['FittedParams']['mi']      = h5.root.FittedParams.IonMass.read()

        # Ne from Power data
        data['NeFromPower']['snr']      = h5.root.NeFromPower.SNR.read()
        data['NeFromPower']['ne_notr']  = h5.root.NeFromPower.Ne_NoTr.read()
        data['NeFromPower']['rng']      = h5.root.NeFromPower.Range.read()
        data['NeFromPower']['alt']      = h5.root.NeFromPower.Altitude.read()

        # Get the Beam Codes
        data['AvgAzimuth']          = h5.root.Antenna.AvgAzimuth.read()
        data['AvgElevation']        = h5.root.Antenna.AvgElevation.read()

        # Finally, grab the time and site information
        data['Time']['UnixTime']    = h5.root.Time.UnixTime.read()
        data['Time']['dtime']       = h5.root.Time.dtime.read()
        data['Time']['Day']         = h5.root.Time.Day.read()
        data['Time']['Month']       = h5.root.Time.Month.read()
        data['Time']['Year']        = h5.root.Time.Year.read()


    pcolor_plot_antenna_all(plot_info, data)

    return


def usage():
    print "usage: ", sys.argv[0]
    print "\t DATAFILE: hdf5 file of fitted data [REQUIRED]"
    print "\t PLOTDIR: directory to place plots in [OPTIONAL]"

    sys.exit(2)

if __name__ == "__main__":

    from datetime import datetime
    import sys

    # Parse input
    data_file = sys.argv[1]

    if len(sys.argv) < 2 or len(sys.argv) > 3:
        usage()
    elif len(sys.argv) == 2:
        plots_dir = './plots'
    else:
        plots_dir = sys.argv[2]

    # Check if the output plot directory exists
    if not os.path.exists(plots_dir):
        # If it doesn't, make it.
        try:
            os.mkdir(plots_dir)
        except OSError:
            print("Problem making the output plotting directory, exiting...")
            sys.exit(1)

    # Make the plots
    now = datetime.now()
    replot_pcolor_antenna_all(data_file,saveplots=1,opath=plots_dir) #,clims=clims)
    print('It took %d seconds to plot the data.' % (datetime.now()-now).total_seconds())
