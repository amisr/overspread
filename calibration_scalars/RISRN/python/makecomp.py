import os
import sys
import numpy
import calendar
import datetime
import shutil
import time
import glob
from scipy import stats
import numpy
import tables
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d


def find_nearest(array,value):
    idx = (numpy.abs(array-value)).argmin()
    return idx
    
    
def smooth(x,window_len=11,window='hanning'):
    """smooth the data using a window with requested size.
    
    This method is based on the convolution of a scaled window with the signal.
    The signal is prepared by introducing reflected copies of the signal 
    (with the window size) in both ends so that transient parts are minimized
    in the begining and end part of the output signal.
    
    input:
        x: the input signal 
        window_len: the dimension of the smoothing window; should be an odd integer
        window: the type of window from 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'
            flat window will produce a moving average smoothing.

    output:
        the smoothed signal
        
    example:

    t=linspace(-2,2,0.1)
    x=sin(t)+randn(len(t))*0.1
    y=smooth(x)
    
    see also: 
    
    numpy.hanning, numpy.hamming, numpy.bartlett, numpy.blackman, numpy.convolve
    scipy.signal.lfilter
 
    TODO: the window parameter could be the window itself if an array instead of a string
    NOTE: length(output) != length(input), to correct this: return y[(window_len/2-1):-(window_len/2)] instead of just y.
    """

    if x.ndim != 1:
        raise ValueError, "smooth only accepts 1 dimension arrays."

    if x.size < window_len:
        raise ValueError, "Input vector needs to be bigger than window size."


    if window_len<3:
        return x


    if not window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman']:
        raise ValueError, "Window is on of 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'"


    s=numpy.r_[x[window_len-1:0:-1],x,x[-1:-window_len:-1]]
    #print(len(s))
    if window == 'flat': #moving average
        w=numpy.ones(window_len,'d')
    else:
        w=eval('numpy.'+window+'(window_len)')

    y=numpy.convolve(w/w.sum(),s,mode='valid')
    return y
    
    
if __name__ == "__main__":

    # sys.argv[1] is path to processed file
    # sys.argv[2] is the file to compare to
    # sys.argv[3] (optional is the output directory)
    
    try:
        output_dir = sys.argv[3]
    except:
        print("No output directory specified... will output to local directory.")
        
    #try:
    minimum_elevation = 60.0
    
    print("Reading file: %s" %(sys.argv[1]))
    
    h5 = tables.openFile(sys.argv[1])
    
    Beamcodes = h5.root.BeamCodes.read()
    Ne = h5.root.FittedParams.Ne.read()
    Altitude = h5.root.FittedParams.Altitude.read()/1e3
    Time = h5.root.Time.UnixTime.read()
    h5.close()
    
    mtime = numpy.mean(Time,axis=1)

    Ibeams = numpy.where(Beamcodes[:,2] > minimum_elevation)
    Nbeams = len(Ibeams[0])
    Nrecs = len(mtime)

    Nmax = numpy.zeros((Nrecs,Nbeams))
    Hmax = numpy.zeros((Nrecs,Nbeams))
    
    for ibms in range(Nbeams):
        ibm = Ibeams[0][ibms]
        ihts = numpy.where((Altitude[ibm,:] > 170) & (Altitude[ibm,:] < 450))
        tNe = Ne[:,ibm,ihts[0]]
        tAlt = Altitude[ibm,ihts[0]]
        
        for itime in range(Nrecs):
            ttNe = numpy.squeeze(tNe[itime,:])
            ttNe[numpy.isnan(ttNe)] = 0
            max_value = max(ttNe)
            max_index = numpy.where(ttNe == max_value)
            
            try:
                P = numpy.polyfit(tAlt[max_index[0]-1:max_index[0]+2],ttNe[max_index[0]-1:max_index[0]+2],2)
            
                Hmax[itime,ibms] = -P[1]/2/P[0]
                Nmax[itime,ibms] = numpy.polyval(P,Hmax[itime,ibms])
            except:
                pass
        
    
    Nmax[Nmax < 0] = numpy.nan
    

    meanNmax = stats.nanmean(Nmax,axis=1)
    """
    meanNmax[meanNmax > stats.nanmean(meanNmax)*3] = numpy.nan
    meanNmax[meanNmax < stats.nanmean(meanNmax)*3] = numpy.nan
    """
    medNmax = stats.nanmedian(Nmax,axis=1)
    """
    medNmax[medNmax > stats.nanmean(medNmax) + stats.nanmean(medNmax)*3] = numpy.nan
    medNmax[medNmax < stats.nanmean(medNmax) - stats.nanmean(medNmax)*.5] = numpy.nan
    """
    stdNmax = stats.nanstd(Nmax,axis=1)
    """
    stdNmax[stdNmax > stats.nanmean(stdNmax)*3] = numpy.nan
    stdNmax[stdNmax < stats.nanmean(stdNmax)*3] = numpy.nan
    """
    meanHmax = stats.nanmean(Hmax,axis=1)
    medHmax = stats.nanmedian(Hmax,axis=1)
    stdHmax = stats.nanstd(Hmax,axis=1)
    
    try:
        # RISRN CADI STUFF

        RISRN_CADI = numpy.loadtxt(sys.argv[2], skiprows=1)    

        RISRN_foF2 = RISRN_CADI[:,6]
        RISRN_foF2[RISRN_foF2 == 999.999] =  numpy.nan
        RISRN_foF2 = RISRN_foF2 *1e6/8.98
        RISRN_foF2 = RISRN_foF2 * RISRN_foF2



        # Timestamps
        RISRN_timestamps = []
        for element in RISRN_CADI:
            RISRN_timestamps.append(calendar.timegm(    datetime.datetime(int(element[0]), 
                                                                        int(element[1]), 
                                                                        int(element[2]), 
                                                                        int(element[3]), 
                                                                        int(element[4]), 
                                                                        int(element[5])).timetuple()))
                                                                
        # END RISRN CADI

    except:
        # THULE CADI STUFF
        lines = [line.strip() for line in open(sys.argv[2])]
        RISRN_foF2 = []
        RISRN_timestamps =[]

        for element in lines:
            if element[0] == '#':
                pass
            else:
                tmp = element.split()
                RISRN_foF2.append(float(tmp[2]))

                RISRN_timestamps.append(calendar.timegm(datetime.datetime.strptime(tmp[0]+' '+tmp[1], "%Y-%m-%d %H:%M").timetuple()))

        RISRN_foF2 = numpy.array(RISRN_foF2)
        RISRN_foF2[RISRN_foF2 == 9999.0] =  numpy.nan
        RISRN_timestamps = numpy.array(RISRN_timestamps) #+104.52*60*5#+3600 #-719 
        
        RISRN_foF2 = RISRN_foF2 * 1e6/(8.98+.35) #offset from RISR CADI
        RISRN_foF2 = RISRN_foF2 * RISRN_foF2

        # END THULE CADI
    
        # Set up dates
    
        # Check for gaps in time...
    
        start_time = mtime[0]
        end_time = mtime[-1]
    
        # Find closest
    
        closest_start = find_nearest(start_time, RISRN_timestamps)
        closest_end = find_nearest(end_time,RISRN_timestamps)
    
        RISRN_timestamps[closest_start:closest_end]
    
        time_spacing = [30*60,15*60.0,10*60.0,5*60.0]
        # Iterate through to find gaps...
        for i in range(len(RISRN_timestamps[closest_start:closest_end])-1):
            if RISRN_timestamps[closest_start+i+1] - RISRN_timestamps[closest_start+i] not in time_spacing:
                RISRN_foF2[closest_start+i] = numpy.nan
    
    
    
        
    smooth_amount = 3
    cinterp = smooth(numpy.interp(mtime,RISRN_timestamps,RISRN_foF2),smooth_amount)[0:-smooth_amount+1]
    doff = medNmax/cinterp
    doff = doff[~numpy.isnan(doff)]
    doff = doff[doff >= 0.1]
    doff = doff[doff <= 1.9]

    f_offset = stats.nanmedian(doff)

    times = [datetime.datetime.utcfromtimestamp(t) for t in mtime]
    
    fig = plt.figure()
    fig.set_size_inches(18.5,10.5)
    ax1 = plt.subplot2grid((3,3),(0,0), colspan=2, rowspan=3)
    ax2 = plt.subplot2grid((3,3),(0,2), rowspan=3)
    
    try:
        plt.suptitle('%s' %(os.path.basename(sys.argv[3])))
    except:
        pass

    ax1.set_title('%s - %s' % (datetime.datetime.utcfromtimestamp(mtime[0]).strftime('%Y-%m-%d %H:%M:%S'),datetime.datetime.utcfromtimestamp(mtime[-1]).strftime('%Y-%m-%d %H:%M:%S')))
        
    ax1.set_ylabel(r'$N_{max} (m^{-3}$)')
    
    
    
    
    
    ax1.plot(times,medNmax, 'b',label='RISR-N')

    ax1.plot(times,cinterp, 'r',label='CADI')
    #ax1.plot(RISRN_timestamps,RISRN_foF2, 'r',label='CADI')

    handles, labels = ax1.get_legend_handles_labels()

    ax1.legend(handles, labels)
    
    #plt.show()
    
    
    
    ax2.set_title('Fractional offset ISR/CADI - %2.2f' %(f_offset))
    ax2.hist(doff, bins=50)
    ax2.set_xlim([0,2])
    
    try:
        if 'cal' in sys.argv[1]:
            plt.savefig('%s/%s-cal.eps'%(sys.argv[3],os.path.basename(sys.argv[3])) , format='eps', dpi=1000)
        else:
            plt.savefig('%s/%s-nocal.eps'%(sys.argv[3],os.path.basename(sys.argv[3])) , format='eps', dpi=1000)
            cal_file = glob.glob(os.path.join(os.path.split(sys.argv[1])[0],"*calibration*"))[0]
            new_cal_file = os.path.join(sys.argv[3],"%s-calibration_ksys.txt" %(time.strftime("%Y%m%d")))
            shutil.copyfile(cal_file, new_cal_file)
            f = open(new_cal_file)
            ksys = numpy.loadtxt(f)
            
            ratio_col = numpy.empty(numpy.shape(ksys)[0])
            ratio_col.fill(f_offset)
            ratio_col = ratio_col[:,numpy.newaxis]
            ksys = numpy.concatenate((ksys, ratio_col), axis=1)
            numpy.savetxt(new_cal_file, ksys, fmt="%5d %2.2f %2.2f %1.3e %1.3f")
    except:
        if 'cal' in sys.argv[1]:
            plt.savefig('cal.eps' , format='eps', dpi=1000)
        else:
            plt.savefig('nocal.eps' , format='eps', dpi=1000)
            
            cal_file = glob.glob(os.path.join(os.path.split(sys.argv[1])[0],"*calibration*"))[0]
            new_cal_file = "%s-calibration_ksys.txt" %(time.strftime("%Y%m%d"))
            shutil.copyfile(cal_file, new_cal_file)
            f = open(new_cal_file)
            ksys = numpy.loadtxt(f)
            
            ratio_col = numpy.empty(numpy.shape(ksys)[0])
            ratio_col.fill(f_offset)
            ratio_col = ratio_col[:,numpy.newaxis]
            ksys = numpy.concatenate((ksys, ratio_col), axis=1)
            numpy.savetxt(new_cal_file, ksys, fmt="%5d %2.2f %2.2f %1.3e %1.3f")
    
    
    
    
    
    
    
    