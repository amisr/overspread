#!/usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys
import os.path
import tables
import scipy
import scipy.signal
import scipy.interpolate
import scipy.stats
import scipy.io
import glob
import shutil
import matplotlib
matplotlib.use('Agg')
import pylab
import datetime

sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/src')
#sys.path.append('/Users/schen/Desktop/ISR_Project/AMISR_fitter_py/src')
import plot_utils
import io_utils

##############################

def readafile(fname):

    h5file=tables.openFile(fname)
    output={}
    for group in h5file.walkGroups("/"):
        output[group._v_pathname]={}
        for array in h5file.listNodes(group, classname = 'Array'):						
            output[group._v_pathname][array.name]=array.read()		
    h5file.close()
    
    return output

"""
def write_outputfile(fhandle,dict2do,keys2do=[],groupname='',name=''):

    if groupname == '':
        group=fhandle.root
    else:
        if fhandle.__contains__('/'+groupname):
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
"""

if __name__ == '__main__':


        snrFilter=0;
        replot=0;

        """
        # Erickson
        odir='/Volumes/AMISR_004/processed_data/PFISR/2010/Erickson30'; exp_WC='2010053*.*[0-9]'; 
        tpAll=['lp_3min','ac_5min']
        st=''
        # scaler from plasma line
        useScaler=1 
        subDir='cal-0'
        # beam-dependent factor
        useCalData=1 
        calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/Erickson02/Erickson30-calibration-06.16.2010.txt'
        # time-dependent factor
        useCalTimeData=0
        # replacement factor
        replaceVal=1; powLims=[1.2e6,2.0e6]; aeuLims=[2400,1500]; Psc=[1.2,scipy.nan]; replot=0;
        # chirp
        corrChirp=1; chirpCorr=-20.0
        """
        
        """
        # Joule
        odir='/Volumes/AMISR_004/processed_data/PFISR/2007/Joule2/'; exp_WC='200701*.*[0-9]'; 
        tpAll=['lp_2min','ac_5min-phaseerrs','ac_15min-phaseerrs']
        st=''
        # scaler from plasma line
        useScaler=0 
        subDir='cal-0'
        # beam-dependent factor
        useCalData=0 
        calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/Erickson02/Erickson30-calibration-01.14.2010.txt'
        # time-dependent factor
        useCalTimeData=0
        # replacement factor
        replaceVal=0;
        # chirp
        corrChirp=1; chirpCorr=-20.0		
        """

        """"""
        """
        # USE THIS ONE
        odir='/Volumes/ISR_DATA_02/processed_data/PFISR/2011/12/nodt3IPY17'; exp_WC='20111202.002';
        #tpAll=['lp_5min','lp_3min','lp_1min','lp_30sec','lp_9min','lp_15min']; snrMin=0.0;  snrLim=0.2; #snrLim=0.4 #for RAXbg03
        tpAll=['ac_5min-dt1','ac_5min','ac_3min','ac_15min']; snrMin=5e-3;  snrLim=0.2;
        #tpAll = ['bc_2min-Ne']; snrMin=0.0; snrLim=0.5;
        #st='IPY17-'
        st=''
        # scaler from plasma line
        useScaler=0; subDir='cal-0'
        # beam-dependent factor
        useCalData=1
        #calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20100701.001-ipy17/IPY17-calibration-LP-10.19.2010.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20101001.002-ipy17/IPY17-calibration-LP-07.12.2011.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201011/cal-201011-calibration-scalar-04.09.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201101/cal-201101-calibration-scalar-03.05.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201102/cal-201102-calibration-scalar-03.06.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201105/cal-201105-calibration-scalar-04.03.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201106/cal-201106-calibration-scalar-04.03.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201107/cal-201107-calibration-scalar-04.05.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201108/cal-201108-calibration-scalar-04.06.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201109/cal-201109-calibration-scalar-02.29.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201110/cal-201110-calibration-scalar-02.28.2012.txt'
        calFname='/Users/mnicolls/Dropbox/AMISR/calibration/201112/cal-201209-calibration-scalar-10.16.2012.txt'
        # time-dependent factor
        useCalTimeData=0
        caltimeFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20100701.001-ipy17/ipy17_cal_%s.txt'
        #caltimeFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20101001.002-ipy17/ipy17_cal_%s.txt'
        # replacement factor
        replaceVal=1; powLims=[2.0e6]; aeuLims=[700]; Psc=[scipy.nan]
        # snr filter
        snrFilter=1; NsnrAvg=20; snrSc=scipy.nan
        # chirp
        corrChirp=1; chirpCorr=-20.0
        #
        replot=1;
        
        #replaceVal=1; powLims=[2.0e6,1.8e6,1.5e6]; aeuLims=[700,5000,5000];Psc=[scipy.nan,1.0/1.56,1.0/.64]
        #replaceVal=1; powLims=[2.0e6,1.5e6]; aeuLims=[700,5000];Psc=[scipy.nan,1.0/.51]
        #replaceVal=1; powLims=[2.0e6,1.25e6,1.15e6,1.0e6,0.7e6]; aeuLims=[1000,5000,5000,5000,5000]; Psc=[scipy.nan,1.0/1.00,1.0/0.5941,1/3.5558,1/0.1467]
        #replaceVal=1; powLims=[2.0e6,1.35e6,1.0e6]; aeuLims=[700,5000,5000];Psc=[scipy.nan,1.0/.3,1.0/16]
        #replaceVal=1; powLims=[2.0e6,1.3e6]; aeuLims=[700,5000];Psc=[scipy.nan,1.0/2.6]
        #replaceVal=1; powLims=[2.0e6,1.33e6,1.10e6]; aeuLims=[700,5000,5000];Psc=[scipy.nan,1.0/.22,1.0/7.7]
        #replaceVal=1; powLims=[2.0e6,1.30e6,1.10e6,0.825e6]; aeuLims=[700,5000,5000,5000];Psc=[scipy.nan,1.0/.22,1.0/18,1.0/.66]
        #replaceVal=0
        #snrFilter=0
        """
        """"""
        #tpAll=['ac_5min-cal']
        #useCalData=0; snrFilter=0
        #replaceVal=1; powLims=[2.0e6,1.45e6]; aeuLims=[2000,5000]; Psc=[scipy.nan,1.4165]
        #replot=1;
        """"""
        """"""


        
        # RISR WorldDay
        odir='/Volumes/ISR_DATA_02/processed_data/RISR-N/2014/03/WorldDay66m/'; exp_WC='20140302.001'; 
        #tpAll=['ac_3min','ac_5min','lp_1min','lp_3min', 'lp_5min','lp_2min','lp_2min-Ne','bc_2min-Ne'];
        tpAll=['lp_1min','lp_5min']
        #tpAll=['ac_3min']
        #tpAll=['bc_5min-Ne','bc_1min-Ne']
        st=''
        subDir='cal-1'#'cal-1'
        useScaler=0 # scaler from plasma line
        useCalData=1 # beam-dependent factor
        calFname='/Volumes/ISR_DATA_02/calibration/AMISR/calibration_RISRN/digiComp/20140302.001/20141107-calibration_ksys.txt'        
        useCalTimeData=0
        corrChirp=0; chirpCorr=0.0
        replaceVal=1; powLims=[2.0e6,1.8e6]; aeuLims=[700,5000]; Psc=[scipy.nan,1.0/2.2]; replot=1; 
        #replaceVal=1; powLims=[2.0e6]; aeuLims=[1000]; Psc=[scipy.nan,1.0/.5]; replot=1; #done for 20120217.001
        #replaceVal=1; powLims=[2.0e6,1.1e6,.9e6]; aeuLims=[2000,5000,5000];Psc=[scipy.nan,1.0/1.3,1.0/1.5];replot=1; #done for 20120331.001
        #replaceVal=1; powLims=[2.0e6,1.4e6]; aeuLims=[2000,5000];Psc=[scipy.nan,1.0/1.8];replot=1; #done for 20120702.001
        
        

        """
        # LTCS
        odir='/Volumes/AMISR_004/processed_data/PFISR/2010/LTCS30'; exp_WC='20100119.001';
        tpAll=['lp_2min', 'ac_3min']
        st=''
        # scaler from plasma line
        useScaler=0
        # beam-dependent factor
        useCalData=1
        calFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20100101.002-ipy17/IPY17-calibration-LP-09.30.2010.txt'
        # time-dependent factor
        useCalTimeData=1
        caltimeFname='/Users/mnicolls/Documents/Work/AMISR/ipy/cal_20100101.002-ipy17/ipy17_cal_%s.txt'
        # replacement factor
        replaceVal=1; powLims=[1.3e6,1.3e6]; aeuLims=[2300,4000]; Psc=[scipy.nan,1.0/1.3]
        # chirp
        corrChirp=1; chirpCorr=-20.0
        """

        """
        # WorldDay03
        odir='/Volumes/AMISR_004/processed_data/PFISR/2009/WorldDay30'; exp_WC='2009*.*[0-9]'; 
        tpAll=['lp_3min','ac_5min']
        st=''
        # scaler from plasma line
        useScaler=1 
        subDir='cal-0'
        # beam-dependent factor
        useCalData=1 
        calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/WorldDay30/WorldDay30-calibration-01.14.2010.txt'
        # time-dependent factor
        useCalTimeData=0
        # replacement factor
        replaceVal=0;
        # chirp
        corrChirp=1; chirpCorr=-20.0
        """
        
        """
        # Lyons30
        odir='/Volumes/ISR_DATA_01/processed_data/PFISR/2011/01/PLCal30'; exp_WC='201101*[0-9]';
        tpAll=['lp_1min','ac_3min','lp_3min','ac_5min','lp_5min']
        st=''
        # scaler from plasma line
        useScaler=0 
        # beam-dependent factor
        useCalData=1 
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201109/cal-201109-calibration-scalar-02.29.2012.txt'
        #calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201110/cal-201110-calibration-scalar-02.28.2012.txt'
        calFname='/Users/mnicolls/Documents/Work/AMISR/calibration/201101/cal-201101-calibration-scalar-03.05.2012.txt'
        # time-dependent factor
        useCalTimeData=0
        # replacement factor
        replaceVal=1; powLims=[2.0e6]; aeuLims=[2000]; Psc=[scipy.nan]
        # chirp
        corrChirp=1; chirpCorr=-20.0
        """
        
        """
        # Sporadic
        odir='/Volumes/AMISR_004/processed_data/PFISR/2010/Grid11x11_11'; exp_WC='2010*[0-9]';
        tpAll=['lp_5min','bc_2min-Ne']
        st=''
        # scaler from plasma line
        useScaler=1; subDir='cal-0'
        # beam-dependent factor
        useCalData=0
        # time-dependent factor
        useCalTimeData=0
        # replacement factor
        replaceVal=0; 
        # chirp
        corrChirp=1; chirpCorr=-20.0		
        """		
        
        dirs=glob.glob(os.path.join(odir,exp_WC))
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
                        
                datName=os.path.join(odir,exp,st+exp +'_' + tp +'.h5'); oName=os.path.join(odir,exp,st+exp + '_' + tp+ '-cal' + '.h5');  plotdir=os.path.join(odir,exp,st+'plots_' + tp)
                FitFile=1
                if datName.__contains__('-Ne'):
                    FitFile=0

                # read datfile
                if os.path.exists(datName):
                    print 'Cal file ' + datName + ' to ' + oName
                
                    h5file=tables.openFile(datName)
                    h5file.copyFile(oName,overwrite=True)
                    h5file.close()

                    h5file=tables.openFile(oName,'r+')
                    mUnixTime=scipy.mean(h5file.getNode('/Time/UnixTime').read(),1)
                    if FitFile:
                        NeFit=h5file.getNode('/FittedParams/Ne').read(); (Nrecs,Nbeams,Nhts)=NeFit.shape
                        dNeFit=h5file.getNode('/FittedParams/dNe').read()
                        Fits=h5file.getNode('/FittedParams/Fits').read()
                    Ne_NoTr=h5file.getNode('/NeFromPower/Ne_NoTr').read(); (x1,x2,Nhts2)=Ne_NoTr.shape
                    Ne_Mod=h5file.getNode('/NeFromPower/Ne_Mod').read()
                    SNR=h5file.getNode('/NeFromPower/SNR').read()
                    BeamCodes=h5file.getNode('/BeamCodes').read()
                    if replaceVal:
                        AeuRx=h5file.getNode('/ProcessingParams/AeuRx').read()
                        AeuTx=h5file.getNode('/ProcessingParams/AeuTx').read()
                        try: AeuTotal=h5file.getNode('/ProcessingParams/AeuTotal').read()
                        except: AeuTotal=4096
                        TxPower=h5file.getNode('/ProcessingParams/TxPower').read()
                    Nbeams=Ne_Mod.shape[1]
                    Nrecs=Ne_Mod.shape[0]
                
                    # scaler (plasma line)
                    scaler=scipy.ones((Nbeams,2),dtype='float32')
                    if useScaler:
                        scalerName='CalFile-'+tp+'.txt'
                        if os.path.exists(os.path.join(odir,exp,subDir,scalerName)):
                            scaler=scipy.loadtxt(os.path.join(odir,exp,subDir,scalerName))
                            print 'Using scaler'
                        else:
                            print 'Not using scaler'
                            xxxxx
                
                    if useScaler or useCalData or replaceVal or useCalTimeData or corrChirp or snrFilter:					
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
                            if FitFile:
                                print tCalData[ibm2do,4]
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
                        print KsysCorrTime
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
                                print I
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
                                print I
                                I=I[0]
                                if FitFile:
                                    corrNeFit[I,ibm,:]=corrNeFit[I,ibm,:]/snrSc
                                    corrdNeFit[I,ibm,:]=corrdNeFit[I,ibm,:]/snrSc
                                    if scipy.isnan(snrSc):
                                        corrFits[I,ibm,:]=scipy.nan                                    
                                corrNe_NoTr[I,ibm,:]=corrNe_NoTr[I,ibm,:]/snrSc
                                corrNe_Mod[I,ibm,:]=corrNe_Mod[I,ibm,:]/snrSc                             
                                
                    #
                    if useScaler or useCalData or replaceVal or useCalTimeData:
                        if FitFile:
                            # Fitted Ne
                            h5file.getNode('/FittedParams/Ne').rename('Ne2') # move node
                            h5file.createArray('/FittedParams','Ne',corrNeFit) # new array
                            h5file.getNode('/FittedParams/Ne2').attrs._f_copy(h5file.getNode('/FittedParams/Ne')) # attributes
                            h5file.getNode('/FittedParams/Ne2').remove() # delete original
                            # Fitted dNe
                            h5file.getNode('/FittedParams/dNe').rename('dNe2') # move node
                            h5file.createArray('/FittedParams','dNe',corrdNeFit) # new array
                            h5file.getNode('/FittedParams/dNe2').attrs._f_copy(h5file.getNode('/FittedParams/dNe')) # attributes
                            h5file.getNode('/FittedParams/dNe2').remove() # delete original
                            # fitted
                            h5file.getNode('/FittedParams/Fits').rename('Fits2') # move node
                            h5file.createArray('/FittedParams','Fits',corrFits) # new array
                            h5file.getNode('/FittedParams/Fits2').attrs._f_copy(h5file.getNode('/FittedParams/Fits')) # attributes
                            h5file.getNode('/FittedParams/Fits2').remove() # delete original
                        # Ne_NoTr
                        h5file.getNode('/NeFromPower/Ne_NoTr').rename('Ne_NoTr2') # move node
                        h5file.createArray('/NeFromPower','Ne_NoTr',corrNe_NoTr) # new array
                        h5file.getNode('/NeFromPower/Ne_NoTr2').attrs._f_copy(h5file.getNode('/NeFromPower/Ne_NoTr')) # attributes
                        h5file.getNode('/NeFromPower/Ne_NoTr2').remove() # delete original
                        # Ne_Mod
                        h5file.getNode('/NeFromPower/Ne_Mod').rename('Ne_Mod2') # move node
                        h5file.createArray('/NeFromPower','Ne_Mod',corrNe_Mod) # new array
                        h5file.getNode('/NeFromPower/Ne_Mod2').attrs._f_copy(h5file.getNode('/NeFromPower/Ne_Mod')) # attributes
                        h5file.getNode('/NeFromPower/Ne_Mod2').remove() # delete original
                                            
                    # chirp correction
                    if corrChirp and FitFile:
                        corrFits[:,:,:,:,-1]=corrFits[:,:,:,:,-1]+chirpCorr
                        # vel
                        h5file.getNode('/FittedParams/Fits').rename('Fits2') # move node
                        h5file.createArray('/FittedParams','Fits',corrFits) # new array
                        h5file.getNode('/FittedParams/Fits2').attrs._f_copy(h5file.getNode('/FittedParams/Fits')) # attributes
                        h5file.getNode('/FittedParams/Fits2').remove() # delete original
                
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
                        if os.path.exists(plotdir):
                            oc = os.tempnam(os.path.dirname(plotdir),os.path.basename(plotdir)+'-')
                            os.renames(plotdir,oc)                    
                        os.makedirs(plotdir)
                        plot_utils.replot_pcolor_all(oName,saveplots=1,opath=plotdir,clims=[[10,12],[0,1500],[0,3000],[0,4],[-500,500]],ylim=[],tlim=[],txMax=24.0)
                        
