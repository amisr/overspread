#Add a "Calibration" record to the fitcal file

#Also need to add a method that can add a "CalibrationMethod" to existing -cal files.

import tables
import datetime
import scipy
import os
import sys

# Now we need to import io_utils. Normally it's located where the fitter source is, but
#it may also be in the same directory as this code is.
amisr_fitter_path = os.getenv("AMISR_FITTER_PATH")
# If no AMISR_FITTER_PATH, then assume io_utils is in current working directory
if amisr_fitter_path is not None:
    amisr_fitter_path = os.path.join(amisr_fitter_path, 'src')
    sys.path.append(amisr_fitter_path)

import io_utils

# Intention of this is that is will be added to the fitter code and used after the fitting
#has already been done. It will add information to the fitted h5 file about the calibration
#that was done in the fitting process.
cal_method = ['Plasma Line','Ionosonde','Swarm']

def add_calibration_info(fname,calFname,calMethodIndex):

    useCalData = True                #Beam-dependent factor
   # corrChirp=True; chirpCorr=-20.0  #Why -20? This is arbitrary?


    t=datetime.date.today()
    datestr=t.strftime("%Y-%m-%d")

    # Open the calibration file
    calData=scipy.loadtxt(calFname)

    # Determine the calibration Method
    method = cal_method[calMethodIndex]


    # Open the fitted h5 file
    with tables.openFile(fname,'r+') as h5file:
        # Add the current date to state when the calibration was done
        io_utils.write_outputfile(h5file,datestr,groupname='Calibration',name='CalDate')
        # Include the calibration info in the calibration file
        io_utils.write_outputfile(h5file,calData,groupname='Calibration',name='CalDataBeam')
        # Include the calibration filename
        io_utils.write_outputfile(h5file,os.path.basename(calFname),groupname='Calibration',name='CalFileBeam')
        # Specifiy the calibration method
        io_utils.write_outputfile(h5file,method,groupname='Calibration',name='CalibrationMethod')

        #IS THIS REALLY NECESSARY? NO, remove later
        #io_utils.write_outputfile(h5file,chirpCorr,groupname='Calibration',name='ChirpCorrection')



# This is intended to be used to append the CalibrationMethod to files that have already been calibrated.
#Another function will need to be used to identify what the method used to calibrate the data was (or, this
#may have to be done manually).
def add_calibration_method(fname,calMethodIndex):
    # Determine the calibration Method
    method = cal_method[calMethodIndex]

	# Open the fitted h5 file
    with tables.openFile(fname,'r+') as h5file:
        # Specifiy the calibration method
        io_utils.write_outputfile(h5file,method,groupname='Calibration',name='CalibrationMethod')