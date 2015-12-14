#!/usr/bin/env python

'''
xxxxx

~M. Nicolls
last revised: xx/xx/2007

'''

import sys
import os.path
import tables
import scipy
import numpy
import scipy.signal
import scipy.interpolate
import scipy.stats
import scipy.io
import scipy.signal as ss
import scipy.ndimage as I
import glob
import shutil
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
#plt.ion()
import matplotlib.image as IM
import pylab
import datetime

sys.path.append('/Users/fitter/Documents/amisr-src/src/ISfit/AMISR_fitter_py/src')
#sys.path.append('/Users/schen/Desktop/ISR_Project/AMISR_fitter_py/src')
#import plot_utils
import io_utils

from loggerinit.LoggerInit import *
from amisrplotting import amisrwrapper
##############################

def readafile(fname):

    h5file=tables.open_file(fname)
    output={}
    for group in h5file.walkGroups("/"):
        output[group._v_pathname]={}
        for array in h5file.listNodes(group, classname = 'Array'):
            output[group._v_pathname][array.name]=array.read()
    h5file.close()

    return output

if __name__ == '__main__':

        # Load logging module
        logger = LoggerInit(config=os.path.join('/Users/fitter/Documents/amisr-src/src/calibration/',
                            "config",
                            "log.ini") ).logger

        snrFilter=0;
        replot=0;
        if len(sys.argv) > 2:
            print("We have input arguments")
            #renaming input parameters
            dir = sys.argv[1]
            yyyy = dir.split("/")[5]
            print("yyyy = "+yyyy)
            mm = dir.split("/")[6]
            print("mm = "+mm)
            topdir = '/Volumes/ISR_DATA_02/processed_data/PFISR/%s/%s/' %(yyyy,mm)
            print("topdir =  "+topdir)
            keyword = dir.split("/")[7]
            print("keyword = %s" %(keyword))

            debug_flag = "n"
            list_flag = 0
            exp_WC = dir.split("/")[8]
            print("exp_WC = "+exp_WC)
            calFname = sys.argv[2]
            type_list = [sys.argv[3]]
        else:
            debug_flag = str(raw_input('Are you debugging? (enter "y" or "n")'))
            if debug_flag == 'n':
                keyword = str(raw_input('Enter keyword (like IPY27_Tracking_v01) : '))
                logger.debug(keyword)
                yyyy = str(raw_input('Enter year : '))
                mm = str(raw_input('Enter month : ')).zfill(2)
                topdir = '/Volumes/ISR_DATA_02/processed_data/PFISR/%s/%s/' %(yyyy,mm)
                #topdir = str(raw_input('Enter top directory (like /Volumes/ISR_DATA/processed_data/PFISR/2014/09/ **make sure you have the end slash**) : '))
                logger.debug(topdir)
                exp_WC = str(raw_input('Enter regular expression for matching (like 2014092*[0-9]) : '))
                logger.debug(exp_WC)
                if not exp_WC:
                    dir_list = str(raw_input('Enter list of directories for processing (like "dir1,dir2"): '))
                    dir_list = dir_list.split(',')
                    logger.debug(dir_list)
                    list_flag = 1
                else:
                    list_flag = 0
                type = str(raw_input('Enter LP or AC or BOTH: '))
                logger.debug(type)
                if type == 'BOTH':
                    type_list = ('LP','AC')
                elif type == 'LP':
                    type_list = ['LP']
                elif type == 'AC':
                    type_list = ['AC']
                logger.debug(type_list)
                exp_name = str(raw_input('Enter calibration folder name (like 20140921.001) : '))
                #calFname = str(raw_input('Enter experiment name (like 20140921.001/cal-201409-calibration-scalar-09.18.2015.txt : '))
                exp_fname = '/Volumes/ISR_DATA_02/calibration/AMISR/calibration_PFISR/%s%s/PLCal30/%s/' %(yyyy,mm,exp_name)
                logger.debug(exp_fname)
                calFname = glob.glob('%s*-calibration-scalar*.txt' %(exp_fname))
                calFname = calFname[0]
                logger.debug(calFname)
            else:
                topdir = '/Volumes/ISR_DATA_02/processed_data/PFISR/2014/09/' #'/Volumes/ISR_DATA/processed_data/PFISR/2015/01/'
                keyword = 'PLCal30'#'IPY27_Tracking_v01'
                list_flag = 0
                type_list = ['AC']#('LP','AC')
                #exp_WC = '20150109.001' goes with IPY27_Tracking_v01
                #exp_WC = '20150117.004'
                #exp_WC = '2015011*[0-9]'
                #exp_WC = '20150110.002'
                exp_WC = '20140921.001'#'20150113.002'
                exp_name = '20140921.001'#'20150108.003'
                #exp_name = '20150127.001'
                exp_fname = '/Volumes/ISR_DATA_02/calibration/AMISR/calibration_PFISR/201409/PLCal30/%s/' %(exp_name) #'/Volumes/ISR_DATA/calibration/AMISR/calibration_PFISR/201501/PLCal30/%s/' %(exp_name)
                calFname = glob.glob('%s*-calibration-scalar*.txt' %(exp_fname))
                calFname = calFname[0]

        logger.debug('now we have entered all the stuffs')

        for directory in glob.glob(topdir+keyword):

            for type_flag in type_list:

                logger.debug(type_flag)
                logger.debug(directory)
                # USE THIS ONE
                #odir='/Volumes/ISR_DATA/processed_data/PFISR/2014/02/Swarm*[0-9]/';
                odir = os.path.join(topdir + os.path.basename(directory))
                #exp_WC= '2014092*[0-9]'#'2014091*[6-9]'#'201312*[0-9]'; #20140226.014, 20140227.014
                if type_flag == 'LP':
                    tpAll=['lp_60min','lp_10sec','lp_20sec','lp_2min','lp_5min','lp_5min-dt0','lp_5min-dt1','lp_5min-dt2','lp_3min','lp_1min','lp_30sec','lp_9min','lp_15min']; snrMin=0.0;snrLim=0.8# #snrLim=0.4 #for RAXbg03
                elif type_flag == 'AC':
                    tpAll=['ac_5min-dt0','ac_5min-dt3','ac_10sec','ac_20sec','ac_30sec','ac_1min','ac_1.5min','ac_5min-dt1','ac_5min','ac_15min', 'ac_2min','ac_3min','ac_60min']; snrMin=5e-3; snrLim=0.2;#5e-3;  snrLim=0.2;
                elif type_flag == 'BC':
                    tpAll = ['bc_2min-Ne','bc_5min-Ne']; snrMin=0.0; snrLim=0.5;
                #tpAll=['lp_30sec','lp_30sec-dt0','lp_30sec-dt1','lp_30sec-dt2']; snrMin=0.0;  snrLim=0.8; #snrLim=0.4 #for RAXbg03
                #st='IPY17-'
                st=''
                # scaler from plasma line
                useScaler=0; subDir='cal-0'
                # beam-dependent factor
                useCalData=1
                #
                #calFname='/Volumes/ISR_DATA/calibration/AMISR/calibration_PFISR/201409/PLCal30/20140906.030/cal-201409-calibration-scalar-09.17.2015.txt'
                #calFname='/Volumes/ISR_DATA/calibration/AMISR/calibration_PFISR/201409/PLCal30/20140921.001/cal-201409-calibration-scalar-09.18.2015.txt'
                # time-dependent factor

                useCalTimeData=0
                caltimeFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20100701.001-ipy17/ipy17_cal_%s.txt'
                #caltimeFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20101001.002-ipy17/ipy17_cal_%s.txt'
                # replacement factor
                replaceVal=1; powLims=[2.0e6]; aeuLims=[700];Psc=[scipy.nan]
                #replaceVal=1; powLims=[2.0e6,1.4e6]; aeuLims=[700,5000];Psc=[scipy.nan,1.0/2.3407]
                # snr filter
                #snrFilter=1; NsnrAvg=20; snrSc=scipy.nan
                snrFilter=1; NsnrAvg=20; snrSc=scipy.nan
                # chirp
                corrChirp=1; chirpCorr=-20.0
                #
                replot=1;
                removeInter=1;

                # if user entered a list of directories
                if not list_flag:
                    logger.debug('user did not enter directory list')
                    dirs=glob.glob(os.path.join(odir,exp_WC))
                else:
                    dirs = []
                    logger.debug('LOOK HERE')
                    for d in dir_list:
                        logger.debug(d)
                        logger.debug(os.path.join(odir,d))
                        dirs.append(os.path.join(odir,d))
                    logger.debug(dirs)

                Ndirs=len(dirs)
                dirs.sort()

                for expp in dirs:

                    exp=os.path.basename(expp)

                    if useCalData:
                        calData=scipy.loadtxt(calFname)

                    for ii in range(len(tpAll)):
                        tp=tpAll[ii]

                        if useCalTimeData:
                            if tp.__contains__('ac'):
                                tcaltimeFname = caltimeFname % ('Alt Code')
                                caltimeData=scipy.loadtxt(tcaltimeFname)
                            elif tp.__contains__('lp'):
                                tcaltimeFname = caltimeFname % ('Long Pulse')
                                caltimeData=scipy.loadtxt(tcaltimeFname)
                            else:
                                raise ValueError, "Time - Not handled"
                            caltimeData=scipy.concatenate((scipy.array([[0.0,scipy.stats.stats.nanmedian(caltimeData[0:10,1])]]),caltimeData))
                            caltimeData=scipy.concatenate((caltimeData,scipy.array([[2.0e9,scipy.stats.stats.nanmedian(caltimeData[-10:,1])]])))

                        datName=os.path.join(odir,exp,st+exp +'_' + tp +'.h5')
                        oName=os.path.join(odir,exp,st+exp + '_' + tp+ '-cal' + '.h5')
                        plotdir=os.path.join(odir,exp,st+'plots_' + tp)
                        FitFile=1
                        if datName.__contains__('-Ne'):
                            FitFile=0

                        # read datfile
                        if os.path.exists(datName):
                            logger.debug('Cal file %s to %s' %(datName,oName))

                            h5file=tables.open_file(datName)
                            # Replaced this line August 22, 2013 because it was crashing
                            #h5file.copyFile(oName,overwrite=True)
                            shutil.copyfile(datName,oName)
                            h5file.close()

                            h5file=tables.open_file(oName,'r+')
                            mUnixTime=scipy.mean(h5file.get_node('/Time/UnixTime').read(),1)
                            if FitFile:
                                NeFit=h5file.get_node('/FittedParams/Ne').read()
                                (Nrecs,Nbeams,Nhts)=NeFit.shape
                                dNeFit=h5file.get_node('/FittedParams/dNe').read()
                                Fits=h5file.get_node('/FittedParams/Fits').read()
                            Ne_NoTr=h5file.get_node('/NeFromPower/Ne_NoTr').read()
                            (x1,x2,Nhts2)=Ne_NoTr.shape
                            Ne_Mod=h5file.get_node('/NeFromPower/Ne_Mod').read()
                            SNR=h5file.get_node('/NeFromPower/SNR').read()
                            BeamCodes=h5file.get_node('/BeamCodes').read()
                            if replaceVal:
                                AeuRx=h5file.get_node('/ProcessingParams/AeuRx').read()
                                AeuTx=h5file.get_node('/ProcessingParams/AeuTx').read()
                                try:
                                    AeuTotal=h5file.get_node('/ProcessingParams/AeuTotal').read()
                                except:
                                    AeuTotal=4096
                                TxPower=h5file.get_node('/ProcessingParams/TxPower').read()
                            Nbeams=Ne_Mod.shape[1]
                            Nrecs=Ne_Mod.shape[0]

                            # scaler (plasma line)
                            scaler=scipy.ones((Nbeams,2),dtype='float32')
                            if useScaler:
                                scalerName='CalFile-'+tp+'.txt'
                                if os.path.exists(os.path.join(odir,exp,subDir,scalerName)):
                                    scaler=scipy.loadtxt(os.path.join(odir,exp,subDir,scalerName))
                                    logger.debug('Using scaler')
                                else:
                                    logger.debug('Not using scaler')
                                    xxxxx

                            if useScaler or useCalData or replaceVal or useCalTimeData or corrChirp or snrFilter or removeInter:
                                if FitFile:
                                    corrNeFit=NeFit.copy()
                                    corrdNeFit=dNeFit.copy()
                                    corrFits=Fits.copy()
                                corrNe_NoTr=Ne_NoTr.copy()
                                corrNe_Mod=Ne_Mod.copy()

                            # beam-dependent cal - calData
                            if useCalData:
                                tCalData=calData.copy()
                                for ibm in range(Ne_Mod.shape[1]):
                                    ibm2do=scipy.where(tCalData[:,0]==BeamCodes[ibm,0])[0]
                                    logger.debug(tCalData[ibm2do,4])
                                    if FitFile:
                                        corrNeFit[:,ibm,:]=corrNeFit[:,ibm,:]/tCalData[ibm2do,4]
                                        corrdNeFit[:,ibm,:]=corrdNeFit[:,ibm,:]/tCalData[ibm2do,4]
                                    corrNe_NoTr[:,ibm,:]=corrNe_NoTr[:,ibm,:]/tCalData[ibm2do,4]
                                    corrNe_Mod[:,ibm,:]=corrNe_Mod[:,ibm,:]/tCalData[ibm2do,4]

                            # beam-dependent cal - scalar
                            if useScaler:
                                for ibm in range(Ne_Mod.shape[1]):
                                    ibm2do=scipy.where(scaler[:,0]==BeamCodes[ibm,0])[0]
                                    if FitFile:
                                        corrNeFit[:,ibm,:]=corrNeFit[:,ibm,:]/scaler[ibm2do,1]
                                        corrdNeFit[:,ibm,:]=corrdNeFit[:,ibm,:]/scaler[ibm2do,1]
                                    corrNe_NoTr[:,ibm,:]=corrNe_NoTr[:,ibm,:]/scaler[ibm2do,1]
                                    corrNe_Mod[:,ibm,:]=corrNe_Mod[:,ibm,:]/scaler[ibm2do,1]

                            # time-dependent cal - caltimeData
                            if useCalTimeData:
                                KsysCorrTime=scipy.interpolate.interp1d(caltimeData[:,0],caltimeData[:,1],bounds_error=False)(mUnixTime)
                                logger.debug(KsysCorrTime)
                                if FitFile:
                                    corrNeFit=corrNeFit/scipy.repeat(scipy.repeat(KsysCorrTime[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts,axis=2)
                                    corrdNeFit=corrdNeFit/scipy.repeat(scipy.repeat(KsysCorrTime[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts,axis=2)
                                corrNe_NoTr=corrNe_NoTr/scipy.repeat(scipy.repeat(KsysCorrTime[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts2,axis=2)
                                corrNe_Mod=corrNe_Mod/scipy.repeat(scipy.repeat(KsysCorrTime[:,scipy.newaxis,scipy.newaxis],Nbeams,axis=1),Nhts2,axis=2)
                            #xxx

                            # replacement
                            if replaceVal:
                                for irepl in range(len(powLims)):
                                    I=scipy.where((TxPower<powLims[irepl]) & (AeuTx<aeuLims[irepl]))
                                    if len(I)>0:
                                        I=I[0]
                                        #I=I[scipy.where(I>1052)]
                                        logger.debug(I)
                                        #xxxxxxxxx
                                        #I=range(I[0],Nrecs)
                                        corrNe_NoTr[I]/=Psc[irepl]
                                        corrNe_Mod[I]/=Psc[irepl]
                                        if FitFile:
                                            corrNeFit[I]/=Psc[irepl]
                                            corrdNeFit[I]/=Psc[irepl]
                                            if scipy.isnan(Psc[irepl]):
                                                corrFits[I]=scipy.nan

                            # snr filter
                            if snrFilter:
                                for ibm in range(Ne_Mod.shape[1]):
                                    snrm = scipy.absolute(scipy.median(SNR[:,ibm,-NsnrAvg:],axis=1))
                                    I=scipy.where((snrm>snrLim) | (snrm<snrMin))
                                    if len(I)>0:
                                        logger.debug(I)
                                        I=I[0]
                                        if FitFile:
                                            corrNeFit[I,ibm,:]=corrNeFit[I,ibm,:]/snrSc
                                            corrdNeFit[I,ibm,:]=corrdNeFit[I,ibm,:]/snrSc
                                            if scipy.isnan(snrSc):
                                                corrFits[I,ibm,:]=scipy.nan
                                        corrNe_NoTr[I,ibm,:]=corrNe_NoTr[I,ibm,:]/snrSc
                                        corrNe_Mod[I,ibm,:]=corrNe_Mod[I,ibm,:]/snrSc

                            # removeInter
                            # New awesome code 10/21/15 - made by Emma -TO DO
                            # Remove noisy interference values by setting ranges that exceed density threshold to nan
                            logger.debug(debug_flag)
                            logger.debug(removeInter)
                            if removeInter & (debug_flag == 'y'):

                                # copy data to "array1"
                                array1 = numpy.nansum(corrNeFit,axis=1) #corrNeFit[:,3,:]
                                # need to convert to scale [0,1]
                                array1 = array1/numpy.nanmax(array1)
                                # lets threshold
                                '''
                                low_values = array1<0.2
                                array1[low_values] = 0
                                high_values = array1>0.8
                                array1[high_values] = 1
                                '''

                                array1_shape = numpy.shape(array1)
                                x,y = numpy.meshgrid(numpy.linspace(0,array1_shape[0]-1,array1_shape[0]), numpy.linspace(0,array1_shape[1]-1,array1_shape[1]))
                                #plt.imshow(numpy.transpose(array1), aspect="auto", origin="lower")
                                x = numpy.transpose(x)
                                y = numpy.transpose(y)

                                #array1 = array1>0.5
                                '''
                                theta_res = 0.5
                                rho_res = 0.5
                                nC = len(numpy.transpose(array1))
                                nR = len(array1)
                                theta = numpy.linspace(-90.0,0.0,numpy.ceil(90.0/theta_res)+1.0)
                                theta = numpy.concatenate((theta,-theta[len(theta)-2::-1]))

                                D = numpy.sqrt((nR-1)**2+(nC-1)**2)
                                q = numpy.ceil(D/rho_res)
                                nrho = 2*q+1
                                rho = numpy.linspace(-q*rho_res,q*rho_res,nrho)
                                H = numpy.zeros((len(rho),len(theta)))
                                for rowIdx in range(nR):
                                    for colIdx in range(nC):
                                        if array1[rowIdx,colIdx]:
                                            for thIdx in range(len(theta)):
                                                rhoVal = colIdx*numpy.cos(theta[thIdx]*numpy.pi/180.0)+rowIdx*numpy.sin(theta[thIdx]*numpy.pi/180)
                                                rhoIdx = numpy.nonzero(numpy.abs(rho-rhoVal) == numpy.min(numpy.abs(rho-rhoVal)))[0]
                                                H[rhoIdx[0],thIdx]+= 1
                                figH = plt.imshow(H)
                                plt.show()
                                '''

                                # detect intersections
                                #Hmaxima = ss.argrelmax(H)

                                # convert to image space

                                # get rid of nans
                                array1_nans = numpy.isnan(array1)
                                array1_nonans = array1
                                array1_nonans[array1_nans] = 0

                                # zero out low altitudes
                                array_test = array1_nonans
                                array_test[:,:18] = 0
                                #fig,ax = plt.subplots(1)
                                #ax.pcolor(x,y,array_test)
                                #plt.show()

                                # this is the filter
                                data_col_len = array1.shape[1]
                                array2 = numpy.array([numpy.zeros(data_col_len),numpy.ones(data_col_len),numpy.zeros(data_col_len)])

                                # matched filtering
                                corr = ss.correlate(array_test,array2,mode='same')

                                # plotting things
                                #figCorr,axarray = plt.subplots(3)
                                #axarray[0].pcolor(x,y,array_test,vmin=0,vmax=1)
                                #axarray[1].pcolor(x,y,corr,vmin=0,vmax=10)

                                # turn correlation into vector of maxima
                                max_corr = numpy.nanmax(corr,axis=1)
                                saturated_idx = []
                                # checking for saturation columns
                                if numpy.nanmax(max_corr)/numpy.mean(max_corr) > 50:
                                    logger.debug('CHECK THIS!!!!!')
                                    logger.debug('THIS MEANS EMMA THINKS THERE IS SATURATION')
                                    logger.debug('SHE MIGHT BE WRONG')
                                    saturated_idx = scipy.where(abs(max_corr-numpy.nanmax(max_corr))<0.5)
                                    logger.debug(saturated_idx)
                                    max_corr[saturated_idx] = 0
                                    threshold = 0.2
                                else:
                                    threshold = 100.0
                                    #threshold = 1.0

                                #max_corr = max_corr/numpy.nanmax(max_corr)
                                logger.debug(numpy.mean(max_corr))
                                #axarray[2].plot(max_corr)
                                #plt.show()


                                # find peaks and eliminate those (interference)
                                # testing out threshold max_val>7.4
                                #print(max_corr)
                                idx = scipy.where(max_corr > threshold)
                                if 'saturated_idx' in locals():
                                    logger.debug('we have noticed that there is saturation')
                                    if not idx:
                                        idx = saturated_idx
                                    else:
                                        try:
                                            idx = numpy.concatenate((idx[0],saturated_idx[0]),axis=0)
                                        except:
                                            idx = idx[0];
                                            logger.debug('we hit the exception')
                                """
                                for beam in range(corrNeFit.shape[1]):
                                    corrNeFit[idx,beam,:] = scipy.nan
                                    corrdNeFit[idx,beam,:] = scipy.nan
                                    corrNe_NoTr[idx,beam,:] = scipy.nan
                                    corrNe_Mod[idx,beam,:] = scipy.nan
                                    corrFits[idx,beam,:,:,:] = scipy.nan
                                """


                                # testing by just eliminating the peak
                                '''
                                max_val = numpy.nanmax(max_corr)
                                max_ind = numpy.argmax(max_corr)
                                for beam in range(corrNeFit.shape[1]):
                                    corrNeFit[max_ind,beam,:] = scipy.nan
                                    corrdNeFit[max_ind,beam,:] = scipy.nan
                                    corrNe_NoTr[max_ind,beam,:] = scipy.nan
                                    corrNe_Mod[max_ind,beam,:] = scipy.nan
                                '''

                                """
                                array_flattened = corrNeFit.flatten(order='F')
                                sorted_ind = numpy.argsort(array_flattened)
                                sorted_array = numpy.sort(array_flattened)

                                threshold = 0.6*numpy.nanmax(scipy.squeeze(corrNeFit))
                                print(scipy.squeeze(corrNeFit).shape)
                                print(threshold)
                                """
                                '''
                                for beam in range(corrNeFit.shape[1]):
                                    #sum_corrNeFit = scipy.stats.nanmean(scipy.squeeze(corrNeFit[:,beam,:]), axis=1)
                                    sum_corrNeFit = numpy.nanmax(scipy.squeeze(corrNeFit[:,beam,:]), axis=1)
                                    threshold = 4e12
                                    idx = scipy.where(sum_corrNeFit > threshold)
                                    if FitFile:
                                        corrNeFit[idx,beam,:] = scipy.nan
                                        corrdNeFit[idx,beam,:] = scipy.nan
                                        corrNe_NoTr[idx,beam,:] = scipy.nan
                                        corrNe_Mod[idx,beam,:] = scipy.nan
                                '''
                            else:
                                logger.debug('we skipped the if statement')


                            # Recopies all modified data back into calibration file
                            if useScaler or useCalData or replaceVal or useCalTimeData or removeInter:
                                if FitFile:
                                    # Fitted Ne
                                    h5file.get_node('/FittedParams/Ne').rename('Ne2') # move node
                                    h5file.create_array('/FittedParams','Ne',corrNeFit) # new array
                                    h5file.get_node('/FittedParams/Ne2').attrs._f_copy(h5file.get_node('/FittedParams/Ne')) # attributes
                                    h5file.get_node('/FittedParams/Ne2').remove() # delete original
                                    # Fitted dNe
                                    h5file.get_node('/FittedParams/dNe').rename('dNe2') # move node
                                    h5file.create_array('/FittedParams','dNe',corrdNeFit) # new array
                                    h5file.get_node('/FittedParams/dNe2').attrs._f_copy(h5file.get_node('/FittedParams/dNe')) # attributes
                                    h5file.get_node('/FittedParams/dNe2').remove() # delete original
                                    # fitted
                                    h5file.get_node('/FittedParams/Fits').rename('Fits2') # move node
                                    h5file.create_array('/FittedParams','Fits',corrFits) # new array
                                    h5file.get_node('/FittedParams/Fits2').attrs._f_copy(h5file.get_node('/FittedParams/Fits')) # attributes
                                    h5file.get_node('/FittedParams/Fits2').remove() # delete original
                                # Ne_NoTr
                                h5file.get_node('/NeFromPower/Ne_NoTr').rename('Ne_NoTr2') # move node
                                h5file.create_array('/NeFromPower','Ne_NoTr',corrNe_NoTr) # new array
                                h5file.get_node('/NeFromPower/Ne_NoTr2').attrs._f_copy(h5file.get_node('/NeFromPower/Ne_NoTr')) # attributes
                                h5file.get_node('/NeFromPower/Ne_NoTr2').remove() # delete original
                                # Ne_Mod
                                h5file.get_node('/NeFromPower/Ne_Mod').rename('Ne_Mod2') # move node
                                h5file.create_array('/NeFromPower','Ne_Mod',corrNe_Mod) # new array
                                h5file.get_node('/NeFromPower/Ne_Mod2').attrs._f_copy(h5file.get_node('/NeFromPower/Ne_Mod')) # attributes
                                h5file.get_node('/NeFromPower/Ne_Mod2').remove() # delete original

                            # chirp correction
                            if corrChirp and FitFile:
                                corrFits[:,:,:,:,-1]=corrFits[:,:,:,:,-1]+chirpCorr
                                # vel
                                h5file.get_node('/FittedParams/Fits').rename('Fits2') # move node
                                h5file.create_array('/FittedParams','Fits',corrFits) # new array
                                h5file.get_node('/FittedParams/Fits2').attrs._f_copy(h5file.get_node('/FittedParams/Fits')) # attributes
                                h5file.get_node('/FittedParams/Fits2').remove() # delete original

                            t=datetime.date.today()
                            datestr=t.strftime("%Y-%m-%d")

                            io_utils.write_outputfile(h5file,datestr,groupname='Calibration',name='CalDate')
                            if useCalData:
                                io_utils.write_outputfile(h5file,calData,groupname='Calibration',name='CalDataBeam')
                                io_utils.write_outputfile(h5file,os.path.basename(calFname),groupname='Calibration',name='CalFileBeam')
                            if useCalTimeData:
                                io_utils.write_outputfile(h5file,caltimeData,groupname='Calibration',name='CalDataTime')
                                io_utils.write_outputfile(h5file,os.path.basename(os.path.dirname(tcaltimeFname))+'/'+os.path.basename(tcaltimeFname),groupname='Calibration',name='CalFileTime')
                            if useScaler:
                                io_utils.write_outputfile(h5file,scaler,groupname='Calibration',name='AdditionalScaler')
                                io_utils.write_outputfile(h5file,os.path.basename(scalerName),groupname='Calibration',name='CalFileAdditionalScaler')
                            if corrChirp and FitFile:
                                io_utils.write_outputfile(h5file,chirpCorr,groupname='Calibration',name='ChirpCorrection')

                            # close datfile
                            h5file.close()

                            if replot:
                                inst = amisrwrapper.amisrwrapper(configpath="/opt/amisr-plotting/amisrplotting/config/*.ini",logger=logger)
                                inst.pcolor_plot(oName,plotdir,replot=True)
