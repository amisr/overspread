#Add a "Calibration" record to the fitcal file

#Also need to add a method that can add a "CalibrationMethod" to existing -cal files.

import tables
import datetime
import scipy
import os
import sys

from . import io_utils

# Intention of this is that is will be added to the fitter code and used after the fitting
#has already been done. It will add information to the fitted h5 file about the calibration
#that was done in the fitting process.
cal_method = ['Plasma Line','Ionosonde','Swarm']

def add_calibration_info(fname,calFname,calMethodIndex):

    #Beam-dependent factor
    useCalData = True

    t = datetime.date.today()
    datestr = t.strftime("%Y-%m-%d")

    # Open the calibration file
    calData = scipy.loadtxt(calFname)

    # Determine the calibration Method
    method = cal_method[calMethodIndex]


    # Open the fitted h5 file
    with tables.open_file(fname,'r+') as h5file:
        # Add the current date to state when the calibration was done
        io_utils.write_outputfile(h5file,datestr,groupname='Calibration',name='CalDate')
        # Include the calibration info in the calibration file
        io_utils.write_outputfile(h5file,calData,groupname='Calibration',name='CalDataBeam')
        # Include the calibration filename
        io_utils.write_outputfile(h5file,os.path.basename(calFname),groupname='Calibration',name='CalFileBeam')
        # Specifiy the calibration method
        io_utils.write_outputfile(h5file,method,groupname='Calibration',name='CalibrationMethod')


# This is intended to be used to append the CalibrationMethod to files that have already been calibrated.
#Another function will need to be used to identify what the method used to calibrate the data was (or, this
#may have to be done manually).
def add_calibration_method(fname,calMethodIndex):
    # Determine the calibration Method
    method = cal_method[calMethodIndex]

	# Open the fitted h5 file
    with tables.open_file(fname,'r+') as h5file:
        # Specifiy the calibration method
        io_utils.write_outputfile(h5file,method,groupname='Calibration',name='CalibrationMethod')


# This function is indended to be used to filter a -fitcal file according to 
# the SNR and power limits hardcoded in the function. This is intended to do
# the same thing as cal_processed_file3.py did in this regard.
def filter_calibrated_data(fname,type_flag):

    # replacement factor
    # Use this to remove data that isn't consistent with the calibration
    # example: TX power drops due to an UDU dying, calibration for this
    # period of time wouldn't be valid
    replaceVal=1; powLims=[2.0e6]; aeuLims=[0]; Psc=[scipy.nan]

    # snr filter
    snrFilter=1; NsnrAvg=20; snrSc=scipy.nan

    if type_flag == 'LP':
        snrMin=0.0;snrLim=0.8# #snrLim=0.4 #for RAXbg03
    elif type_flag == 'AC':
        snrMin=5e-3; snrLim=0.2;#5e-3;  snrLim=0.2;
    elif type_flag == 'BC':
        snrMin=0.0; snrLim=0.5;

    # open the data file
    h5file=tables.open_file(fname,'r+')

    # Read in the arrays that we are going to filter
    mUnixTime=scipy.mean(h5file.get_node('/Time/UnixTime').read(),1)
    NeFit=h5file.get_node('/FittedParams/Ne').read()
    (Nrecs,Nbeams,Nhts)=NeFit.shape
    dNeFit=h5file.get_node('/FittedParams/dNe').read()
    Fits=h5file.get_node('/FittedParams/Fits').read()
    Ne_NoTr=h5file.get_node('/NeFromPower/Ne_NoTr').read()
    (x1,x2,Nhts2)=Ne_NoTr.shape
    Ne_Mod=h5file.get_node('/NeFromPower/Ne_Mod').read()
    SNR=h5file.get_node('/NeFromPower/SNR').read()
    BeamCodes=h5file.get_node('/BeamCodes').read()

    # If we are going to filter using power limits read in the
    # aeu and txpower values
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

    # Make copies of the arrays that we'll modify
    if replaceVal or snrFilter:
        corrNeFit=NeFit.copy()
        corrdNeFit=dNeFit.copy()
        corrFits=Fits.copy()
        corrNe_NoTr=Ne_NoTr.copy()
        corrNe_Mod=Ne_Mod.copy()

    # Filter based on txpower, aeu tx/rx
    if replaceVal:
        for irepl in range(len(powLims)):
            I=scipy.where((TxPower < powLims[irepl]) & (AeuTx < aeuLims[irepl]))
            if len(I)>0:
                I=I[0]
                corrNe_NoTr[I]/=Psc[irepl]  
                corrNe_Mod[I]/=Psc[irepl]    
                corrNeFit[I]/=Psc[irepl]
                corrdNeFit[I]/=Psc[irepl]
                if scipy.isnan(Psc[irepl]):
                    corrFits[I]=scipy.nan

    # SNR filter
    if snrFilter:
        for ibm in range(Ne_Mod.shape[1]):
            snrm = scipy.absolute(scipy.median(SNR[:,ibm,-NsnrAvg:],axis=1))
            I=scipy.where((snrm > snrLim) | (snrm < snrMin))
            if len(I)>0:
                I=I[0]
                corrNeFit[I,ibm,:]=corrNeFit[I,ibm,:]/snrSc
                corrdNeFit[I,ibm,:]=corrdNeFit[I,ibm,:]/snrSc
                if scipy.isnan(snrSc):
                    corrFits[I,ibm,:]=scipy.nan                                    
                corrNe_NoTr[I,ibm,:]=corrNe_NoTr[I,ibm,:]/snrSc
                corrNe_Mod[I,ibm,:]=corrNe_Mod[I,ibm,:]/snrSc                             

    # Recopies all modified data back into calibration file
    if replaceVal or snrFilter:
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

    # Close the file
    h5file.close()
