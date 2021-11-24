#!/usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2017

"""
import os
import sys

from .__init__ import __version__, __file__
version = __version__
current_dir = os.path.dirname(__file__)

import matplotlib
matplotlib.use('agg')

import numpy as np
import glob, datetime, time, copy
import optparse
import tables
import ctypes
import scipy
import scipy.interpolate
from matplotlib import pyplot

import configparser as ConfigParser

from . import io_utils, plot_utils, proc_utils, geomag, process_data
from .ISfitter import *
from .constants import *

# flipchem: https://github.com/amisr/flipchem
import flipchem
from flipchem import Flipchem, MSIS
from flipchem import compute_ion_neutral_collfreq, compute_electron_neutral_collfreq

import apexpy
import pymap3d
import pkg_resources

#For fitcal files (files that are calibrated and fitted at the same time)
#we need to add a Calibration record
from .add_calibration_record import *

from .make_summary_plots_sondre import replot_pcolor_antenna_all
from .make_summary_plots_amisr import replot_pcolor_all

MAXFEV_C = 20



##############################

class BadComposition(Exception):
    def __init__( self ):
        Exception.__init__(self, 'Composition not converging')

class Run_Fitter:

    # Initializes the class
    def __init__(self,options):
        #
        # initialize vars
        self.options = options
        self.OPTS = {}
        self.FITOPTS = {}
        self.DEFOPTS = {}
        self.AMB = {}
        self.AMB['Loaded'] = 0
        self.FITS = {}
        self.Time = {}
        self.Site = {}
        self.Params = {}
        self.Antenna = {}
        self.BMCODES = None

        self.ContinueFromLocked=1 # whether to allow fitter to continue from a locked file

        # output file definition
        self.h5Paths = {'Params'    :   ('/ProcessingParams','Experiment Parameters'),
                        'Geomag'    :   ('/Geomag','Geomagnetic Parameters'),
                        'RawPower'  :   ('/NeFromPower','Electron density From Power'),
                        'Site'      :   ('/Site','Site Parameters'),
                        'Time'      :   ('/Time','Time Information'),
                        'Fitted'    :   ('/FittedParams','Fitted Parameters'),
                        'FitInfo'   :   ('/FittedParams/FitInfo','Fitting Info'),
                        'Antenna'   :   ('/Antenna','Antenna Motion Parameters'),
                        'MSIS'      :   ('/MSIS', 'MSIS Output'),
                        'ACFs'      :   ('/FittedParams/ACFs','Autocorrelation Functions')}

        self.h5Attribs = {
            '/BeamCodes' : [('TITLE','BeamCodes'),('Description','Beamcode array'),('Size','Nbeams x 4 (Beamcode, Azimuth (degrees), Elevation (degrees), System constant (m^5/s)')],\
            '/Antenna/AvgAzimuth' : [('TITLE','Average Azimuth Angle'),('Description','Average azimuth angle over integration'),('Size','Nrecords'),('Unit','Degrees')],\
            '/Antenna/AvgElevation' : [('TITLE','Average Elevation Angle'),('Description','Average elevation angle over integration'),('Size','Nrecords'),('Unit','Degrees')],\
            '/Antenna/Azimuth' : [('TITLE','Azimuth Angle'),('Description','Azimuth angle range over integration'),('Size','Nrecords x 2'),('Unit','Degrees')],\
            '/Antenna/Elevation' : [('TITLE','Elevation Angle'),('Description','Elevation angle range over integration'),('Size','Nrecords x 2'),('Unit','Degrees')],\
            '/Antenna/Event' : [('TITLE','Event'),('Description','Antenna event over integration'),('Size','Nrecords')],\
            '/Antenna/Mode' : [('TITLE','Mode'),('Description','Antenna mode over integration'),('Size','Nrecords')],\
            '/FittedParams/Altitude' : [('TITLE','Altitude'),('Description','Mean altitude of bin'),('Unit','Meters')],\
            '/FittedParams/Errors' : [('TITLE','Errors'),('Description','Errors from fits'),('Size','Nrecords x Nbeams x Nranges x Nions+1 x 4 (fraction, temperature, collision frequency, LOS speed)'),('Unit','N/A, Kelvin, s^{-1}, m/s')],\
            '/FittedParams/Fits' : [('TITLE','Fits'),('Description','Fitted parameters'),('Size','Nrecords x Nbeams x Nranges x Nions+1 x 4 (fraction, temperature, collision frequency, LOS speed)'),('Unit','N/A, Kelvin, s^{-1}, m/s')],\
            '/FittedParams/Ne' : [('TITLE','Electron Density'),('Description', 'Fitted electron density'),('Size','Nrecords x Nbeams x Nranges'),('Unit','m^{-3}')],\
            '/FittedParams/Range' : [('TITLE','Range'),('Description','Mean range of bin'),('Unit','Meters')],\
            '/FittedParams/dNe' : [('TITLE','Error in Electron Density'),('Description', 'Error on fitted electron density'),('Size','Nrecords x Nbeams x Nranges'),('Unit','m^{-3}')],\
            '/FittedParams/IonMass' : [('TITLE','Ion Mass'),('Description', 'Mass of ions used in fitting'),('Unit','amu')],\
            '/FittedParams/FitInfo/chi2' : [('TITLE','Chi Squared'),('Description', 'Chi squared of fit')],\
            '/FittedParams/FitInfo/dof' : [('TITLE','Degrees of Freedom'),('Description', 'Degrees of freedom of fit')],\
            '/FittedParams/FitInfo/fitcode' : [('TITLE','Fit Code'),('Description', 'Output code of fitter: 1-4 denote a solution found, 0, <0 denote fundamental error, 5 denotes hit max function evals, >5 some other issues (see MINPACK lmdif man page)')],\
            '/FittedParams/FitInfo/nfev' : [('TITLE','Function Evals'),('Description', 'Number of function evaluations')],\
            '/NeFromPower/Altitude' : [('TITLE','Altitude'),('Unit','Meters')],\
            '/NeFromPower/Ne_Mod' : [('TITLE','Raw Electron Density'),('Description','Electron density from power with model Te/Ti'),('Unit','m^{-3}'),('Size','Nrecords x Nbeams x Nranges')],\
            '/NeFromPower/Ne_NoTr' : [('TITLE','Raw Electron Density'),('Description','Electron density from power with Te=Ti'),('Unit','m^{-3}'),('Size','Nrecords x Nbeams x Nranges')],\
            '/NeFromPower/Range' : [('TITLE','Range'),('Unit','Meters')],\
            '/NeFromPower/SNR' : [('TITLE','Signal to Noise Ratio'),('Description','SNR from power'),('Size','Nrecords x Nbeams x Nranges')],\
            '/NeFromPower/dNeFrac' : [('TITLE','Error in Raw Electron Density'),('Description','Fractional error in electron density'),('Size','Nrecords x Nbeams x Nranges')],\
            '/ProcessingParams/ProcessingTimeStamp' : [('TITLE','Processing Time Stamp')],\
            '/ProcessingParams/BaudLength' : [('TITLE','Baud Length'),('Unit','Seconds')],\
            '/ProcessingParams/PulseLength' : [('TITLE','Pulse Length'),('Unit','Seconds')],\
            '/ProcessingParams/RxFrequency' : [('TITLE','Rx Frequency'),('Description','Receive frequency'),('Unit','Hertz')],\
            '/ProcessingParams/TxFrequency' : [('TITLE','Tx Frequency'),('Description','Transmit frequency'),('Unit','Hertz')],\
            '/ProcessingParams/TxPower' : [('TITLE','Tx Power'),('Unit','Watts'),('Description','Average transmit power over integration'),('Size','Nrecords')],\
            '/ProcessingParams/AeuRx' : [('TITLE','Rx AEUs'),('Description','Number of AEUs on receive'),('Size','Nrecords')],\
            '/ProcessingParams/AeuTx' : [('TITLE','Tx AEUs'),('Description','Number of AEUs on transmit'),('Size','Nrecords')],\
            '/ProcessingParams/AeuTotal' : [('TITLE','Total AEUs'),('Description','Total number of system AEUs'),('Size','Nrecords')],\
            '/Site/Altitude' : [('TITLE','Altitude'),('Description','Altitude of site'),('Unit','Meters')],\
            '/Site/Code' : [('TITLE','Site Code')],\
            '/Site/Latitude' : [('TITLE','Latitude'),('Description','Latitude of site'),('Unit','Degrees North')],\
            '/Site/Longitude' : [('TITLE','Longitude'),('Description','Longitude of site'),('Unit','Degrees East')],\
            '/Site/MagneticLatitude' : [('TITLE','Magnetic Latitude'),('Description','Magnetic Latitude of site'),('Unit','Degrees North')],\
            '/Site/MagneticLongitude' : [('TITLE','Magnetic Longitude'),('Description','Magnetic Longitude of site'),('Unit','Degrees East')],\
            '/Site/MagneticLocalTimeMidnight' : [('TITLE','Magnetic Local Time Midnight'),('Unit','UT Hours')],\
            '/Site/Name' : [('TITLE','Name'),('Description','Site Name')],\
            '/Time/Day' : [('TITLE','Day of Month'),('Size','Nrecords x 2 (Start and end of integration')],\
            '/Time/Month' : [('TITLE','Month'),('Size','Nrecords x 2 (Start and end of integration')],\
            '/Time/Year' : [('TITLE','Year'),('Size','Nrecords x 2 (Start and end of integration')],\
            '/Time/doy' : [('TITLE','Day of Year'),('Size','Nrecords x 2 (Start and end of integration')],\
            '/Time/dtime' : [('TITLE','Decimal Time'),('Size','Nrecords x 2 (Start and end of integration'),('Unit','UT Hours')],\
            '/Time/UnixTime' : [('TITLE','Unix Time'),('Size','Nrecords x 2 (Start and end of integration'),('Unit','Seconds')],\
            '/Time/MagneticLocalTimeSite' : [('TITLE','Magnetic Local Time'),('Size','Nrecords x 2 (Start and end of integration'),('Unit','UT Hours')],\
            '/Geomag/Altitude' : [('TITLE','Altitude'),('Unit','Meters')],\
            '/Geomag/Range' : [('TITLE','Range'),('Unit','Meters')],\
            '/Geomag/B' : [('TITLE','Magnetic Field Vector'),('Unit','Tesla')],\
            '/Geomag/Babs' : [('TITLE','Absolute Value of Magnetic Field'),('Unit','Tesla')],\
            '/Geomag/Bx' : [('TITLE','X-Component of Magnetic Field'),('Unit','Tesla')],\
            '/Geomag/By' : [('TITLE','Y-Component of Magnetic Field'),('Unit','Tesla')],\
            '/Geomag/Bz' : [('TITLE','Z-Component of Magnetic Field'),('Unit','Tesla')],\
            '/Geomag/Declination' : [('TITLE','Declination Angle')],\
            '/Geomag/Dip' : [('TITLE','Dip Angle')],\
            '/Geomag/Latitude' : [('TITLE','Latitude')],\
            '/Geomag/Longitude' : [('TITLE','Longitude')],\
            '/Geomag/MagneticLatitude' : [('TITLE','Modified Apex Magnetic Latitude')],\
            '/Geomag/MagneticLongitude' : [('TITLE','Modified Apex Magnetic Longitude')],\
            '/Geomag/LshellRe' : [('TITLE','L Shell'),('Unit','Earth radii')],\
            '/Geomag/MLTMidnightUT' : [('TITLE','MLT Midnight'),('Unit','UT Hours')],\
            '/Geomag/kvec' : [('TITLE','k Vector - Geographic Flat Eart',),('Description','North, East, Up')],\
            '/Geomag/kgeo' : [('TITLE','k Vector - Geographic',),('Description','North, East, Up')],\
            '/Geomag/ke' : [('TITLE','k East',),('Description','Eastward component of radar k vector')],\
            '/Geomag/kn' : [('TITLE','k North',),('Description','Northward component of radar k vector')],\
            '/Geomag/kz' : [('TITLE','k Up',),('Description','Vertical component of radar k vector')],\
            '/Geomag/kgmag' : [('TITLE','LEGACY: k Vector - Geomagnetic Components',),('Description','LEGACY: Anti-Parallel, Perp-East, Perp-North. Derived from Modified Apex Coordinates where Anti-Parallel is -ke3 / |d3|, Perp-East is ke1 / |d1|, Perp-North is -ke2 / |d2|. See doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kpar' : [('TITLE','LEGACY: k Anti-Parallel - Geomagnetic Components',),('Description','LEGACY: Anti-Parallel to geomagnetic field component of radar k vector. Derived from Modified Apex Coordinates where Anti-Parallel is -ke3 / |d3|. See doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kpe' : [('TITLE','LEGACY: k Perp East - Geomagnetic Components',),('Description','LEGACY: Perpendicular to geomagnetic field and eastward component of radar k vector. Derived from Modified Apex Coordinates where Perp-East is ke1 / |d1|. See doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kpn' : [('TITLE','LEGACY: k Perp North - Geomagnetic Components',),('Description','LEGACY: Perpendicular to geomagnetic field and northward component of radar k vector. Derived from Modified Apex Coordinates where Perp-North is -ke2 / |d2|. See doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kgmagd' : [('TITLE','k Vector - Modified Apex "d" Components',),('Description','Modified Apex Base Vectors: Perp-East, Perp-Equatorward, Parallel to Magnetic field. See equation 61 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kd1' : [('TITLE','k East - Modified Apex "d" Components',),('Description','Modified Apex Base Vector Component Perp-East to Magnetic field. See equation 61 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kd2' : [('TITLE','k Equatorward - Modified Apex "d" Components',),('Description','Modified Apex Base Vector Component Perp-Equatorward to Magnetic field. See equation 61 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kd3' : [('TITLE','k Parallel - Modified Apex "d" Components',),('Description','Modified Apex Base Vector Component Parallel to Magnetic field. See equation 61 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/kgmage' : [('TITLE','k Vector - Modified Apex "e" Components',),('Description','Modified Apex Base Vectors: Approximately Perp-East, Approximately Perp-Equatorward-and-Downward, Parallel to Magnetic field. See equation 60 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/ke1' : [('TITLE','k East - Modified Apex "e" Components',),('Description','Modified Apex Base Vectors: Approximately Perp-East to Magnetic field. See equation 60 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/ke2' : [('TITLE','k Equatorward - Modified Apex "e" Components',),('Description','Modified Apex Base Vectors: Approximately Perp-Equatorward to Magnetic field. See equation 60 of doi: 10.1007/s11214-016-0275-y')],\
            '/Geomag/ke3' : [('TITLE','k Parallel - Modified Apex "e" Components',),('Description','Modified Apex Base Vectors: Parallel to Magnetic field. See equation 60 of doi: 10.1007/s11214-016-0275-y')],\
            '/MSIS/AP' : [('TITLE','AP Array',),('Description','Array elements: daily AP, 3-hr AP index cur time, for -3 hrs, for -6 hrs, for -9 hrs, Avg. for -12 to -33 hrs, Avg. for -36 to -57 hrs')],\
            '/MSIS/f107' : [('TITLE','F107 Index',),('Description','F107 index for previous day')],\
            '/MSIS/f107a' : [('TITLE','F107 Index',),('Description','81-day average F107 index')],\
            '/MSIS/LocalSolarTime' : [('TITLE','Local Solar Time',),('Unit','Hours')],\
            '/MSIS/SolarDec' : [('TITLE','Solar Declination Angle',),('Unit','Degrees')],\
            '/MSIS/SolarZen' : [('TITLE','Solar Zenith Angle',),('Unit','Degrees')],\
            '/MSIS/Texo' : [('TITLE','Exospheric Temperature',),('Unit','K')],\
            '/MSIS/Tn' : [('TITLE','Neutral Temperature',),('Unit','K')],\
            '/MSIS/nAr' : [('TITLE','Ar Density',),('Unit','m^{-3}')],\
            '/MSIS/nH' : [('TITLE','H Density',),('Unit','m^{-3}')],\
            '/MSIS/nHe' : [('TITLE','He Density',),('Unit','m^{-3}')],\
            '/MSIS/nN' : [('TITLE','N Density',),('Unit','m^{-3}')],\
            '/MSIS/nN2' : [('TITLE','N2 Density',),('Unit','m^{-3}')],\
            '/MSIS/nN2D' : [('TITLE','N(2D) Density',),('Unit','m^{-3}')],\
            '/MSIS/nNO' : [('TITLE','NO Density',),('Unit','m^{-3}')],\
            '/MSIS/nO' : [('TITLE','O Density',),('Unit','m^{-3}')],\
            '/MSIS/nO2' : [('TITLE','O2 Density',),('Unit','m^{-3}')],\
            '/MSIS/nOanom' : [('TITLE','Anomalous O Density',),('Unit','m^{-3}')],\
            '/MSIS/qOp' : [('TITLE','O+ Ion Fraction')],\
            '/MSIS/nMass' : [('TITLE','Neutral Mass Density',),('Unit','kg/m^{-3}')]}

        # parse the ini file
        self.ini_parse(options.conffile)

        # load libraries

        try:
            specworkerpath = os.path.join(current_dir,"_c_spec_worker*")
            candidate = glob.glob(specworkerpath)[0]
            self.ct_spec = ctypes.CDLL(candidate)       # spectra library
        except Exception as e:
            raise Exception('Problem loading libraries: %s' % (str(e)))

        # load ISR spectrum model data files
        (self.pldfvvr,self.pldfvvi) = load_disp_table(self.DAT_PLDFVV)

        # load the lag ambiguity function
        try:
            if type(self.OPTS['AMB_PATH']) != tuple:
                if os.path.exists(self.OPTS['AMB_PATH']):
                    self.AMB = io_utils.load_amb_func(self.OPTS['AMB_PATH'],full=0)
                    self.AMB['Loaded'] = 1
                    print('Read ambiguity function from external file - %s' % (self.OPTS['AMB_PATH']))
            else:
                if len(self.OPTS['AMB_PATH']) != 2:
                   print('Ambiguity Function as tuple must be length 2')
                else:
                    tmp      = io_utils.load_amb_func(self.OPTS['AMB_PATH'][0],full=0)
                    self.AMB = io_utils.load_amb_func(self.OPTS['AMB_PATH'][1],full=0)
                    self.AMB['Wlag'][0,:]    = scipy.interpolate.interp1d(tmp['Delay'],tmp['Wlag'],bounds_error=0,fill_value=0.0)(self.AMB['Delay']) # linear interpolation
                    self.AMB['Wrange'][0,:]  = scipy.interpolate.interp1d(tmp['Range'], tmp['Wrange'],bounds_error=0,fill_value=0.0)(self.AMB['Range']) # linear interpolation
                    self.AMB['WlagSum'][0]   = tmp['WlagSum'][0]
                    self.AMB['WrangeSum'][0] = tmp['WrangeSum'][0]
                    self.AMB['Loaded'] = 1
                    print('Read ambiguity function from external file - %s' % (self.OPTS['AMB_PATH'][0]))
                    print('Read ambiguity function from external file - %s' % (self.OPTS['AMB_PATH'][1]))
        except:
            raise Exception('Problem reading ambiguity function from file %s even though file exists' % (self.OPTS['AMB_PATH']))

        # set some other variables
        if self.FITOPTS['DO_FITS']:
            self.FITOPTS['NFIT'] = np.zeros(self.FITOPTS['Ngroup'])
            for i in range(self.FITOPTS['Ngroup']):
                self.FITOPTS['NFIT'][i] = np.where(self.FITOPTS['Ifit'][i,:,:] == 1)[0].shape[0]

        return


    def call_fitter(self,S,Noise,perturbation_noise_acf,sstr=''):

        ### Beams, Lags, Ranges
        Nbeams  = self.Nbeams  # number of beams
        Nlags   = self.Nlags   # number of lags
        Nranges = self.Nranges # number of ranges

        ### frequency array for computation of theoretical spectra
        NFFT = self.DEFOPTS['NFFT']
        f    = np.linspace(-self.DEFOPTS['FREQ_EVAL'],self.DEFOPTS['FREQ_EVAL'],NFFT+1)

        ### Output variables
        Ihtbm = np.zeros(Nbeams)
        HT = np.zeros((Nbeams,Nranges),dtype=float) * np.nan # Altitude
        RNG = np.zeros((Nbeams,Nranges),dtype=float) * np.nan # Range
        ne_out = np.zeros((Nbeams,Nranges,2),dtype=float) * np.nan # Fitted densities
        noise_out = np.zeros((Nbeams,Nranges,3),dtype=float) * np.nan # Fitted noise
        FITS_out = np.zeros((Nbeams,Nranges,self.FITOPTS['NION']+1,4),dtype=float) * np.nan # Fitted parameters
        ERRS_out = np.zeros((Nbeams,Nranges,self.FITOPTS['NION']+1,4),dtype=float) * np.nan # Errors from fits
        mod_ACF = np.zeros((Nbeams,Nlags,Nranges),dtype=complex) * np.nan # model ACFs
        meas_ACF = np.zeros((Nbeams,Nlags,Nranges),dtype=complex) * np.nan # measured ACFs
        errs_ACF = np.zeros((Nbeams,Nlags,Nranges),dtype=float) * np.nan # errors on the ACFs
        fitinfo = {} # dict containing information on fit
        fitinfo['fitcode'] = np.zeros((Nbeams,Nranges),dtype=int) # a fit code
        fitinfo['dof'] = np.zeros((Nbeams,Nranges),dtype=int) # degrees of freedom
        fitinfo['chi2'] = np.zeros((Nbeams,Nranges),dtype=float) # reduced chi2
        fitinfo['nfev'] = np.zeros((Nbeams,Nranges),dtype=int) # number of function evals
        models = {} # dict containing model params
        models['nHe'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nO'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nN2'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nO2'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nAr'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nMass'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nH'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nN'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nOanom'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['Texo'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['Tn'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['SolarZen'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['LocalSolarTime'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['SolarDec'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nNO'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['nN2D'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        models['qOp'] = np.zeros((Nbeams,Nranges),dtype=float) * np.nan
        try:
            gmag = self.Gmag
        except:
            gmag = geomag.blankGmag(Nbeams,Nranges)

        f107, f107a, ap = flipchem.read_geophys(self.Time['datetime'])
        models['f107'] = f107
        models['f107a'] = f107a
        models['AP'] = ap

        ### Debug
        if self.OPTS['plotson'] > 2:
            gf = pyplot.figure()

        if self.FITOPTS['molecularModel'] == 2:
            myfrac = np.loadtxt(self.FITOPTS['molmodFile'])

        ### Loop over beams
        for Ibm in range(Nbeams):
            print('\nBeam %s' % str(Ibm))

            AzAng = S['BMCODES'][Ibm,1]
            ElAng = S['BMCODES'][Ibm,2]

            IfitIndex = 0
            Ifit = np.squeeze(self.FITOPTS['Ifit'][IfitIndex,:,:])
            IfitMR = np.where(np.transpose(Ifit) == 1)
            NFIT = int(self.FITOPTS['NFIT'][IfitIndex]+1)
            SummationRule = self.FITOPTS['SUMMATION_RULE'][IfitIndex,:,:]
            NSUM = SummationRule[1,:]-SummationRule[0,:]+1

            if self.FITOPTS['BinByRange'] == 1:
                Ialt=np.where((S['Acf']['Range'][0,:] >= self.FITOPTS['rngmin']))[0]
                Nrs=self.FITOPTS['Nrngs']
                Altitude=np.mean(S['Acf']['Altitude'],axis=0)[np.newaxis]
            else:
                Ialt=np.where((S['Acf']['Altitude'][Ibm,:] >= self.FITOPTS['htmin']) & (S['Acf']['Altitude'][Ibm,:] <= self.FITOPTS['htmax']))[0]
                Nrs=1.0e6
                Altitude=S['Acf']['Altitude']

            htI = Ialt[0]
            Iht = -1
            ### Loop over altitudes
            while (htI + NSUM[0]) < Ialt[-1] and Iht < Nrs:
                Iht = Iht + 1

                ### Number of gates to sum as a function of lag
                htI = htI + NSUM[0]

                if Altitude[Ibm,int(htI)] >= self.FITOPTS['GroupHt'][int(IfitIndex)]:
                    if IfitIndex < (self.FITOPTS['Ngroup'] - 1):
                        IfitIndex = IfitIndex + 1
                        Ifit = np.squeeze(self.FITOPTS['Ifit'][IfitIndex,:,:])
                        IfitMR = np.where(np.transpose(Ifit) == 1)
                        NFIT = int(self.FITOPTS['NFIT'][IfitIndex] + 1)
                        SummationRule = self.FITOPTS['SUMMATION_RULE'][IfitIndex,:,:]
                        NSUM = SummationRule[1,:] - SummationRule[0,:] + 1

                RngAll = S['Acf']['Range'][0,int(htI + SummationRule[0,0]):int(htI + SummationRule[1,0] + 1)]
                RngMean = np.mean(RngAll)
                SumFactor = np.squeeze(RngMean**2.0 / RngAll**2.0)

                # sum using the summation rule
                pulses_integrated = np.sum(S['Acf']['PulsesIntegrated'][Ibm][:,int(htI + SummationRule[0,0]):int(htI + SummationRule[1,0] + 1)],axis=1)

                K    = pulses_integrated * S['Acf']['Kint']
                Psc  = np.zeros(Nlags,dtype=float)
                tAcf = np.zeros(Nlags,dtype=complex)
                tAcfVar = np.zeros(Nlags,dtype=float)
                for aa in range(Nlags):
                    # Acf
                    tAcf[aa] = np.mean(S['Acf']['Data'][Ibm,aa,int(htI + SummationRule[0,aa]):int(htI + SummationRule[1,aa] + 1)])
                    # scaling factor for power
                    Psc[aa] = np.mean(S['Acf']['Psc'][Ibm,aa,int(htI + SummationRule[0,aa]):int(htI + SummationRule[1,aa] + 1)])

                ### compute variance
                # Whether to use first or 0 lag
                if self.FITOPTS['uselag1'] == 1:
                    sig = np.real(tAcf[S['Acf']['Lag1Index']]) # first lag
                else:
                    sig = np.absolute(tAcf[0]) # 0 lag
                # Signal to noise ratio
                tSnr = np.mean(S['Power']['SNR'][Ibm,int(htI + SummationRule[0,0]):int(htI + SummationRule[1,0] + 1)] * SumFactor)

                # Variance
                tAcfVar = sig**2 * (1.0 + 1.0 / np.absolute(tSnr) + S['Acf']['iSCR'] * NSUM)**2.0 / (K.astype(float) * NSUM) # theoretical variances
                # Additional variance due to noise subtraction
                tAcfVar[0] = tAcfVar[0] + float(Noise['Power']['Data'][Ibm])**2 / Noise['Power']['PulsesIntegrated'][Ibm].astype(float)

                # Height and range
                HT[Ibm,Iht]=np.mean(Altitude[Ibm,int(htI+SummationRule[0,0]):int(htI+SummationRule[1,0]+1)])
                RNG[Ibm,Iht]=RngMean
                print(sstr + 'Alt ' + str(np.asarray(HT[Ibm,Iht]/1000.).round(decimals=2)) + ', Rng ' + str(np.asarray(RNG[Ibm,Iht]/1000.).round(decimals=2)))

                # using guess for Ne from density profile
                tNe = np.absolute(np.mean(S['Power']['Ne_Mod'][Ibm,int(htI+SummationRule[0,0]):int(htI+SummationRule[1,0]+1)]))

                if len(self.FITOPTS['Lags2fit'])==0:
                    Iy=range(Nlags)
                else:
                    b=np.where(HT[Ibm,Iht]>=self.FITOPTS['Lags2fit'][:,0])[0]
                    b=b[-1]
                    Iy=self.FITOPTS['Lags2fit'][b,:].tolist()

                if not self.FITOPTS['fit0lag'] and Iy.__contains__(0):
                    Iy=Iy[1:]

                ### Call MODELS

                # geomag
                if np.isnan(gmag['Latitude'][Ibm,Iht]):
                    tgmag=geomag.geomagTime(self.Time['Year'][0],np.array([AzAng]),np.array([ElAng]),self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0,rng=np.array([RNG[Ibm,Iht]/1000.0])) # run the geomag model
                    for key in list(gmag.keys()):
                        if gmag[key].shape == (Nbeams,Nranges):
                            gmag[key][Ibm,Iht]=tgmag[key]
                        else:
                            gmag[key][Ibm,:]=tgmag[key]

                # common params for msis, flipchem, collfreqs
                glat = gmag['Latitude'][Ibm,Iht]
                glon = gmag['Longitude'][Ibm,Iht]
                alt = HT[Ibm,Iht] / 1000.0

                # MSIS
                msis = MSIS(self.Time['datetime'])
                msis_outputs = msis.get_point(glat,glon,alt)
                H,He,N,O,N2,O2,Ar,Mass,AnomO,Texo,tn = msis_outputs

                models['nH'][Ibm,Iht]     = H
                models['nHe'][Ibm,Iht]    = He
                models['nN'][Ibm,Iht]     = N
                models['nO'][Ibm,Iht]     = O
                models['nN2'][Ibm,Iht]    = N2
                models['nO2'][Ibm,Iht]    = O2
                models['nAr'][Ibm,Iht]    = Ar
                models['nMass'][Ibm,Iht]  = Mass / 1000.0
                models['nOanom'][Ibm,Iht] = AnomO
                models['Texo'][Ibm,Iht]   = Texo
                models['Tn'][Ibm,Iht]     = tn
                models['qOp'][Ibm,Iht]    = np.nan

                # collision frequencies, initial guess, Te=Ti=Tn
                ion_masses = self.FITOPTS['mi'].tolist()
                nui = list()
                neutral_densities = (H,He,N,O,N2,O2,0) #H,He,N,O,N2,O2,Ar
                for mass in ion_masses:
                    nui.append(compute_ion_neutral_collfreq(neutral_densities, tn, mass, tn))
                nue = compute_electron_neutral_collfreq(neutral_densities, tn)
                nui.append(nue)
                nui = np.array(nui)

                # initial flip ion chemistry, with te=ti=tn and Ne = initial guess
                fc = Flipchem(self.Time['datetime'],altop=300.0)
                fc_outputs = fc.get_point(glat,glon,alt,tNe,tn,tn,msis_outputs=msis_outputs)
                LTHRS,SZAD,DEC,OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT = fc_outputs
                models['SolarZen'][Ibm,Iht] = SZAD
                models['LocalSolarTime'][Ibm,Iht] = LTHRS
                models['SolarDec'][Ibm,Iht] = DEC
                models['nNO'][Ibm,Iht] = NNO
                models['nN2D'][Ibm,Iht] = N2D

                # convert density to fractions
                OXPLUS /= tNe
                O2PLUS /= tNe
                NOPLUS /= tNe
                N2PLUS /= tNe
                NPLUS /= tNe

                ### Set up parameter arrays
                # Initialize
                ti = np.ones(self.FITOPTS['NION']+1,dtype=float)*tn*1.1; ti[-1]*=1.1
                mi = np.concatenate((self.FITOPTS['mi'],[v_electronmass/v_amu]))
                psi = np.zeros(self.FITOPTS['NION']+1,dtype=float)
                vi = np.zeros(self.FITOPTS['NION']+1,dtype=float)

                # set collision frequency
                I = np.where(Ifit[:,2] == -2)[0]
                psi[I] = nui[I]
                psi[-1] = psi[-1] * 0.35714

                terr = np.transpose(np.zeros((self.FITOPTS['NION']+1,4),dtype=float))*np.nan

                try:
                # if 1==1:
                    nloops=0
                    while 1==1:
                        nloops+=1

                        # scale arrays
                        ti=ti/self.FITOPTS['p_T0']
                        psi=psi/self.FITOPTS['p_om0']
                        vi=vi/self.FITOPTS['p_om0']*self.k_radar0

                        # set ion density
                        ni = np.ones(self.FITOPTS['NION']+1,dtype=float)
                        ni[np.where((Ifit[:-1,0]==0) | (Ifit[:-1,0]==1))]=0.0

                        if mi[1]==mi[0]:
                            if nloops == 1:
                                ni[1]=0.0
                                ti[1]=ti[0]*2.0
                                vi[1]=-200.0/self.FITOPTS['p_om0']*self.k_radar0
                                Ifit = np.array([[-1,1,-2,1],[0,0,-2,0],[0,1,-2,0]])
                                IfitMR=np.where(np.transpose(Ifit)==1)
                                NFIT = 4
                            elif nloops ==2:
                                ni[1]=0.1
                                ti[0] = tn*1.1/self.FITOPTS['p_T0']
                                if ti[-1]<tn:
                                    ti[-1] = tn*1.2/self.FITOPTS['p_T0']
                                vi[1]=vi[0]-200.0/self.FITOPTS['p_om0']*self.k_radar0
                                Ifit = np.array([[-1,1,-2,0],[1,1,-2,1],[0,1,-2,0]])
                                IfitMR=np.where(np.transpose(Ifit)==1)
                                NFIT = 6

                        I=np.where(Ifit[:,0]==-2)[0]
                        if I.size != 0:
                            for a in range(I.size):
                                if self.FITOPTS['molecularModel']==0:
                                    if mi[I[a]]>=28 and mi[I[a]]<=32: # its a molecular
                                        tfrac=1.0-models['qOp'][Ibm,Iht]
                                        ni[I[a]]=tfrac
                                elif self.FITOPTS['molecularModel']==2:
                                    ni[I[a]]=np.interp(HT[Ibm,Iht]/1000.0,myfrac[:,0],myfrac[:,1])
                                    mi[I[a]]=np.interp(HT[Ibm,Iht]/1000.0,myfrac[:,0],myfrac[:,2])
                                    print(HT[Ibm,Iht]/1000.0, ni[I[a]], mi[I[a]])
                                elif self.FITOPTS['molecularModel']==1:
                                    if mi[I[a]]==16.0: # O+
                                        ni[I[a]] = OXPLUS
                                    elif mi[I[a]]==32.0: # O2+
                                        ni[I[a]] = O2PLUS
                                    elif mi[I[a]]==30.0: # NO+
                                        ni[I[a]] = NOPLUS
                                    elif mi[I[a]]==28.0: # N2+
                                        ni[I[a]] = N2PLUS
                                    elif mi[I[a]]==14.0: # N+
                                        ni[I[a]] = NPLUS
                                    else:
                                        ni[I[a]]=0.0
                        I=np.where(Ifit[:,0]==-1)[0]
                        if I.size==1:
                            ni[I]=1.0-(np.sum(ni)-2.0)
                        elif I.size>1:
                            raise ValueError("Can't have more than one ion -1")

                        ### Initial guess
                        if Iht>0 and iparams0.size==NFIT and 1==0:
                            tmp=np.transpose(np.squeeze(FITS_out[Ibm,Iht-1,:,:]))
                            params0=np.zeros(NFIT,dtype=float)
                            if self.FITOPTS['PERTURBATION_NOISE']:
                                params0[1:]=tmp[IfitMR]
                            else:
                                params0=tmp[IfitMR]
                            params0=params0/scaler
                            I=np.where((params0>1.0*iparams0) | (params0<iparams0/1.0) | (params0<0))[0]
                            params0[I]=iparams0[I]
                            params0[0]=tNe
                            params0[0]=params0[0]/scaler[0]
                        else:
                            params0=np.zeros(NFIT,dtype=float)
                            scaler=np.zeros(NFIT,dtype=float)
                            params0[0]=tNe/self.FITOPTS['p_N0']
                            scaler[0]=self.FITOPTS['p_N0']
                            ii=1
                            I=np.where(Ifit[:,0]==1)[0]
                            if I.size != 0:
                                params0[ii:(ii+I.size)]=ni[I]
                                scaler[ii:(ii+I.size)]=1.0
                                ii=ii+I.size
                            I=np.where(Ifit[:,1]==1)[0]
                            if I.size != 0:
                                params0[ii:(ii+I.size)]=ti[I]
                                scaler[ii:(ii+I.size)]=self.FITOPTS['p_T0']
                                ii=ii+I.size
                            I=np.where(Ifit[:,2]==1)[0]
                            if I.size != 0:
                                params0[ii:(ii+I.size)]=nui[I]/self.FITOPTS['p_om0'] #????
                                scaler[ii:(ii+I.size)]=self.FITOPTS['p_om0']
                                ii=ii+I.size
                            I=np.where(Ifit[:,3]==1)[0]
                            if I.size != 0:
                                params0[ii:(ii+I.size)]=vi[I]
                                scaler[ii:(ii+I.size)]=self.FITOPTS['p_om0']/self.k_radar0
                                ii=ii+I.size
                        iparams0=params0.copy()


                        # get initial guess for additional noise as 1% of measured noise
                        # Then add it to the param0 and scaler arrays, but at the beginning.
                        if nloops == 1:
                            noise0 = float(Noise['Power']['Data'][Ibm]) * 0.01

                        if self.FITOPTS['PERTURBATION_NOISE']:
                            params0 = np.concatenate((np.array([noise0]),params0))
                            scaler = np.concatenate((np.array([1]),scaler))

                        # The variance of the measured noise will be used to weight the amount of allowed
                        # perturbation noise ACF. 
                        K_noise = Noise['Power']['PulsesIntegrated'][Ibm]
                        noise_var = float(Noise['Power']['Data'][Ibm]) / K_noise

                        # do the fit
                        if self.FITOPTS['fitSpectra']==1:
                            tmp=np.concatenate((tAcf,np.conjugate(tAcf[Iy[-1]:0:-1])),axis=0) # hermitian extension
                            tSpc=np.fft.fftshift(np.fft.fft(tmp,axis=0),axes=[0]) # compute spectra
                            tmp=np.concatenate((nAcf,np.conjugate(nAcf[Iy[-1]:0:-1])),axis=0)
                            nSpc=np.fft.fftshift(np.fft.fft(tmp,axis=0),axes=[0]) # compute spectra
                            tSn=tSpc/nSpc
                            #tSpcVar=scipy.power(scipy.sum(tSpc)*tSpc/tSpc*scipy.sqrt(tAcfVar[0])/tAcf[0],2.0); tSpcVar=tSpcVar.astype(float)
                            tSpcVar=np.power(tSpc,2.0)/Kmed*np.power(1.0+np.absolute(1.0/tSn),2.0)

                            (x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(fit_fun_with_noise,params0,(tSpc,tSpcVar,self.AMB['Delay'],np.transpose(self.AMB['Wlag'][Iy,:]),Psc[Iy],self.pldfvvr,self.pldfvvi,self.ct_spec,
                                Ifit,f,ni,ti,mi,psi,vi,self.k_radar0,perturbation_noise_acf,noise_var,self.FITOPTS['p_N0'],self.FITOPTS['p_T0'],self.FITOPTS['p_M0'],self.FITOPTS['fitSpectra'],0.75*tn/self.FITOPTS['p_T0'],self.FITOPTS['LagrangeParams']),
                                full_output=1,epsfcn=1.0e-5,ftol=1.0e-5,xtol=1.0e-5, gtol=0.0, maxfev=10*MAXFEV_C*params0.shape[0],factor=100,diag=None)
                        else:
                            (x,cov_x,infodict,mesg,ier)=scipy.optimize.leastsq(fit_fun_with_noise,params0,(tAcf[Iy],tAcfVar[Iy],self.AMB['Delay'],np.transpose(self.AMB['Wlag'][Iy,:]),Psc[Iy],self.pldfvvr,self.pldfvvi,self.ct_spec,
                                Ifit,f,ni,ti,mi,psi,vi,self.k_radar0,perturbation_noise_acf,noise_var,self.FITOPTS['p_N0'],self.FITOPTS['p_T0'],self.FITOPTS['p_M0'],self.FITOPTS['fitSpectra'],0.75*tn/self.FITOPTS['p_T0'],self.FITOPTS['LagrangeParams']),
                                full_output=1,epsfcn=1.0e-5,ftol=1.0e-5, xtol=1.0e-5, gtol=0.0, maxfev=10*MAXFEV_C*params0.shape[0],factor=100,diag=None)


                        # record termination parameter of fitter
                        fitinfo['fitcode'][Ibm,Iht]=ier
                        if cov_x is None:
                            try:
                                fitinfo['fitcode'][Ibm,Iht]=-fitcode[Ibm,Iht]
                            except:
                                fitinfo['fitcode'][Ibm,Iht]=-45
                        else:
                            cov_x=np.sqrt(np.diag(cov_x))*scaler
                            #print(np.shape(terr[IfitMR]),np.shape(cov_x))
                            if self.FITOPTS['PERTURBATION_NOISE']:
                                terr[IfitMR]=cov_x[2:]
                            else:
                                terr[IfitMR]=cov_x[1:]

                        infodict['fvec']=infodict['fvec'][:-len(self.FITOPTS['LagrangeParams'])]

                        fitinfo['nfev'][Ibm,Iht]=infodict['nfev']
                        fitinfo['dof'][Ibm,Iht]=infodict['fvec'].shape[0]-params0.shape[0]-1
                        fitinfo['chi2'][Ibm,Iht]=np.real(np.sum(np.power(np.real(infodict['fvec']),2.0))/fitinfo['dof'][Ibm,Iht])

                        # get model ACF and parameter arrays
                        (m,m0,ni,ti,psi,vi)=fit_fun_with_noise(x,tAcf,tAcfVar,self.AMB['Delay'],np.transpose(self.AMB['Wlag']),Psc,self.pldfvvr,self.pldfvvi,self.ct_spec,Ifit,
                            f,ni,ti,mi,psi,vi,self.k_radar0,perturbation_noise_acf,noise_var,self.FITOPTS['p_N0'],self.FITOPTS['p_T0'],self.FITOPTS['p_M0'],mode=1)
                        mod_ACF[Ibm,Iy,Iht]=m[Iy]
                        meas_ACF[Ibm,Iy,Iht]=tAcf[Iy]
                        errs_ACF[Ibm,Iy,Iht]=tAcfVar[Iy]
                        if not self.FITOPTS['fit0lag']:
                            mod_ACF[Ibm,0,Iht]=m[0]*Psc[Iy[0]]/Psc[0]

                        # scale output
                        x=x*scaler
                        ni=ni*self.FITOPTS['p_N0']
                        ti=ti*self.FITOPTS['p_T0']
                        psi=psi*self.FITOPTS['p_om0']
                        vi=vi*self.FITOPTS['p_om0']/self.k_radar0
                        if self.FITOPTS['PERTURBATION_NOISE']:
                            tNe = x[1]
                            noise0 = x[0]
                        else:
                            tNe = x[0]
                            noise0 = np.nan

                        # re-evaluate FLIP ion chemistry
                        if self.FITOPTS['molecularModel']==1:
                            ttn=tn # tn
                            tti=ti[0] # ti
                            if ttn>tti: tti=ttn
                            tte=ti[-1] # te
                            if tte<ttn:  tte=ttn
                            tOXPLUS = OXPLUS

                            fc = Flipchem(self.Time['datetime'],altop=300.0)
                            fc_outputs = fc.get_point(glat,glon,alt,tNe,tte,tti,msis_outputs=msis_outputs)
                            LTHRS,SZAD,DEC,OXPLUS,O2PLUS,NOPLUS,N2PLUS,NPLUS,NNO,N2D,INEWT = fc_outputs
                            
                            OXPLUS /= tNe
                            O2PLUS /= tNe
                            NOPLUS /= tNe
                            N2PLUS /= tNe
                            NPLUS /= tNe

                            # break loop or continue
                            if (np.absolute(OXPLUS-tOXPLUS) < 0.02): # break loop
                                break
                            elif nloops>=10:            # Increased from 5 to 10. Empirically found 
                                                        # that this helps prevent too many BadComposition
                                                        # errors when SNR is low.
                                raise BadComposition()  # throw an invalid fit error
                        elif mi[1]==mi[0] and nloops==1:
                            continue
                        else:
                            break

                    # store output
                    ne_out[Ibm,Iht,0]=tNe
                    noise_out[Ibm,Iht,0]=noise0
                    noise_out[Ibm,Iht,2]=float(Noise['Power']['Data'][Ibm])
                    FITS_out[Ibm,Iht,:,0]=ni
                    FITS_out[Ibm,Iht,:,1]=ti
                    FITS_out[Ibm,Iht,:,2]=psi
                    FITS_out[Ibm,Iht,:,3]=vi

                    # compute errors if the Jacobian was able to be inverted
                    if cov_x is not None:
                        if self.FITOPTS['PERTURBATION_NOISE']:
                            noise_out[Ibm,Iht,1]=cov_x[0]
                            ne_out[Ibm,Iht,1]=cov_x[1]
                        else:
                            noise_out[Ibm,Iht,1]=np.nan
                            ne_out[Ibm,Iht,1]=cov_x[0]
                        ERRS_out[Ibm,Iht,:,:]=np.transpose(terr)

                    # make some plots if we are supposed to
                    if self.OPTS['plotson']>2:
                        figgg = pyplot.figure()
                        axxx = figgg.add_subplot(111)
                        ax.errorbar(range(tAcf.size),tAcf.imag,np.sqrt(tAcfVar),np.sqrt(tAcfVar),'r')
                        ax.errorbar(range(tAcf.size),tAcf.real,np.sqrt(tAcfVar),np.sqrt(tAcfVar),'b')
                        ax.plot(range(tAcf.size),m.real,'k')
                        ax.plot(range(tAcf.size),m.imag,'k')
                        ax.set_title(str(HT[Ibm,Iht]/1000.)+' '+str(FITS_out[Ibm,Iht,-1,1]/FITS_out[Ibm,Iht,0,1]))
                        pyplot.show()

                except BadComposition as e:
                    fitinfo['fitcode'][Ibm,Iht]=-500
                    print(str(e))

                except Exception as e: # an unknown error in the fit
                    fitinfo['fitcode'][Ibm,Iht]=-100
                    print("Fit failed!! Unexpected error: " + str(e))

                # bad records
                if (fitinfo['fitcode'][Ibm,Iht]<1) or (fitinfo['fitcode'][Ibm,Iht]>4):
                    FITS_out[Ibm,Iht,:,:]=np.nan
                    ERRS_out[Ibm,Iht,:,:]=np.nan
                    ne_out[Ibm,Iht,:]=np.nan
                    noise_out[Ibm,Iht,:]=np.nan
                    fitinfo['chi2'][Ibm,Iht]=np.nan

                Ihtbm[Ibm]=Iht

        if self.FITOPTS['BinByRange']==1:
            Ihtbm=self.FITOPTS['Nrngs']-1
        else:
            ii=np.where(Ihtbm==Ihtbm.max())[0][0]
            Ihtbm=Ihtbm.max()

        Ihtbm = int(Ihtbm)
        HT=HT[:,0:(Ihtbm+1)]
        RNG=RNG[:,0:(Ihtbm+1)]
        ne_out=ne_out[:,0:(Ihtbm+1),:]
        noise_out=noise_out[:,0:(Ihtbm+1),:]
        FITS_out=FITS_out[:,0:(Ihtbm+1),:,:]
        ERRS_out=ERRS_out[:,0:(Ihtbm+1),:,:]
        mod_ACF=mod_ACF[:,:,0:(Ihtbm+1)]
        meas_ACF=meas_ACF[:,:,0:(Ihtbm+1)]
        errs_ACF=errs_ACF[:,:,0:(Ihtbm+1)]
        for key in list(fitinfo.keys()):
            fitinfo[key]= fitinfo[key][:,0:(Ihtbm+1)]
        for key in list(models.keys()):
            try:
                models[key]= models[key][:,0:(Ihtbm+1)]
            except:
                ''
        for key in list(gmag.keys()):
            try:
                gmag[key]= gmag[key][:,0:(Ihtbm+1)]
            except:
                ''

        if self.OPTS['plotson']>2:
            pyplot.close(gf)

        return RNG,HT,ne_out,noise_out,FITS_out,ERRS_out,mod_ACF,meas_ACF,errs_ACF,fitinfo,models,gmag

    # This function parses the configuration files
    def ini_parse(self,inifile):

        print("Using the following configuration files: %s" % (str(inifile.split(','))))
        
        # setup ConfigParser object
        config = ConfigParser.ConfigParser()
        config.read(inifile.split(','))

        # make sure all necessary sections exist
        if (not config.has_section('GENERAL')):
            raise Exception('Configuration files must contain at least section: GENERAL')
        if (not config.has_section('FIT_OPTIONS')):
            raise Exception('Configuration files must contain at least section: FIT_OPTIONS')
        if (not config.has_section('INPUT')):
            raise Exception('Configuration files must contain at least section: INPUT')
        if (not config.has_section('OUTPUT')):
            raise Exception('Configuration files must contain at least section: OUTPUT')

        # General section
        self.FITTER_PATH = io_utils.ini_tool(config,'DEFAULT','FITTER_PATH',required=1,defaultParm='')
        self.FITOPTS['MOTION_TYPE'] = eval(io_utils.ini_tool(config,'DEFAULT','MOTION_TYPE',required=0,defaultParm='0'))
        self.DAT_PLDFVV = io_utils.ini_tool(config,'GENERAL','DAT_PLDFVV',required=0,defaultParm=os.path.join(self.FITTER_PATH,'dat/pldfvv.dat'))
        self.GEOPHYS_PATH = io_utils.ini_tool(config,'GENERAL','GEOPHYS_PATH',required=0,defaultParm=os.path.join(self.FITTER_PATH,'dat/geophys_params'))

        # Default Options section
        self.DEFOPTS['BMCODEMAP_DEF'] = io_utils.ini_tool(config,'DEFAULT_OPTIONS','BMCODEMAP_DEF',required=0,defaultParm='')
        self.DEFOPTS['KSYS_DEF'] = float(eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','KSYS_DEF',required=0,defaultParm='0.0')))
        self.DEFOPTS['TX_POWER_DEF'] = float(eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','TX_POWER_DEF',required=0,defaultParm='0.0')))
        self.DEFOPTS['TX_FREQ_DEF'] = float(eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','TX_FREQ_DEF',required=0,defaultParm='0.0')))
        self.DEFOPTS['TXBAUD_DEF'] = float(eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','TXBAUD_DEF',required=0,defaultParm='0.0')))
        self.DEFOPTS['PW_DEF'] = float(eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','PW_DEF',required=0,defaultParm='0.0')))
        self.DEFOPTS['h5DataPaths_DEF'] = eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','h5DataPaths_DEF',required=1,defaultParm=''))
        self.DEFOPTS['NFFT'] = eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','NFFT',required=0,defaultParm='512'))
        self.DEFOPTS['FREQ_EVAL'] = eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','FREQ_EVAL',required=0,defaultParm='50.0e3'))
        self.DEFOPTS['CAL_TEMP_DEF'] = eval(io_utils.ini_tool(config,'DEFAULT_OPTIONS','CAL_TEMP_DEF',required=0,defaultParm='100.0'))

        # Input section
        # Get filelist or list of filelists
        self.OPTS['FILELIST'] = io_utils.ini_tool(config,'INPUT','FILELIST',required=1,defaultParm='')
        try: self.OPTS['FILELIST'] = eval(self.OPTS['FILELIST'])
        except: pass
        
        # Get input rawfile paths or list of them
        self.OPTS['ipath'] = io_utils.ini_tool(config,'INPUT','FILE_PATH',required=0,defaultParm='')
        try: self.OPTS['ipath'] = eval(self.OPTS['ipath'])
        except: pass
        
        self.OPTS['AMB_PATH'] = io_utils.ini_tool(config,'INPUT','AMB_PATH',required=0,defaultParm='')
        try: self.OPTS['AMB_PATH'] = eval(self.OPTS['AMB_PATH'])
        except: ''

        # Output section
        self.OPTS['outputpath'] = io_utils.ini_tool(config,'OUTPUT','OUTPUT_PATH',required=0,defaultParm=self.FITTER_PATH)
        self.OPTS['outfile'] = io_utils.ini_tool(config,'OUTPUT','OUTPUT_NAME',required=0,defaultParm='output.h5')
        self.OPTS['plotson'] = eval(io_utils.ini_tool(config,'OUTPUT','plotson',required=0,defaultParm='0'))
        self.OPTS['nplots'] = eval(io_utils.ini_tool(config,'OUTPUT','nplots',required=0,defaultParm='1'))
        self.OPTS['plotsdir'] = io_utils.ini_tool(config,'OUTPUT','plotsdir',required=0,defaultParm='plots')
        self.OPTS['saveplots'] = eval(io_utils.ini_tool(config,'OUTPUT','saveplots',required=0,defaultParm='0'))
        self.OPTS['proc_funcname'] = io_utils.ini_tool(config,'OUTPUT','proc_funcname',required=1,defaultParm='')
        self.OPTS['xlims'] = eval(io_utils.ini_tool(config,'OUTPUT','xlims',required=0,defaultParm='[(0.01,10),(0,4),(-20,20)]'))
        self.OPTS['plotfrac'] = eval(io_utils.ini_tool(config,'OUTPUT','plotfrac',required=0,defaultParm='0'))
        self.OPTS['pcolYlims'] = eval(io_utils.ini_tool(config,'OUTPUT','pcolYlims',required=0,defaultParm='[]'))
        self.OPTS['pcolClims'] = eval(io_utils.ini_tool(config,'OUTPUT','pcolClims',required=0,defaultParm='[]'))
        self.OPTS['ComputeMLT'] = eval(io_utils.ini_tool(config,'OUTPUT','ComputeMLT',required=0,defaultParm='1'))
        self.OPTS['dumpSpectra'] = eval(io_utils.ini_tool(config,'OUTPUT','dumpSpectra',required=0,defaultParm='0'))
        self.OPTS['h5DataPath'] = io_utils.ini_tool(config,'OUTPUT','h5DataPath',required=0,defaultParm=self.DEFOPTS['h5DataPaths_DEF'][self.OPTS['proc_funcname']])
        self.OPTS['dynamicAlts'] = eval(io_utils.ini_tool(config,'OUTPUT','dynamicAlts',required=0,defaultParm='0'))
        self.OPTS['saveACFs'] = eval(io_utils.ini_tool(config,'OUTPUT','saveACFs',required=0,defaultParm='0'))

        # Fit Options section
        self.FITOPTS['txpow'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','txpow',required=0,defaultParm='None'))
        self.FITOPTS['DO_FITS'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','DO_FITS',required=1,defaultParm=''))
        self.FITOPTS['PERTURBATION_NOISE'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','PERTURBATION_NOISE',required=1,defaultParm=''))
        self.FITOPTS['useExternalCal'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','useExternalCal',required=0,defaultParm='0'))
        self.FITOPTS['CalToNoiseRatio'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','CalToNoiseRatio',required=0,defaultParm='1.0'))
        self.FITOPTS['beamMapScale'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','beamMapScale',required=0,defaultParm='0'))
        self.FITOPTS['beamMapScaleFile'] = io_utils.ini_tool(config,'INPUT','beamMapScaleFile',required=0,defaultParm='')
        self.FITOPTS['fitcal']  = eval(io_utils.ini_tool(config,'FIT_OPTIONS','fitcal',required=0,defaultParm='0'))
        self.FITOPTS['fit0lag'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','fit0lag',required=0,defaultParm='1'))
        self.FITOPTS['uselag1'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','uselag1',required=0,defaultParm='0'))
        self.FITOPTS['Recs2Integrate'] = float(eval(io_utils.ini_tool(config,'FIT_OPTIONS','Recs2Integrate',required=1,defaultParm='')))
        self.FITOPTS['Beams2do'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','Beams2do',required=0,defaultParm='[]'))
        self.FITOPTS['p_N0'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','p_N0',required=0,defaultParm='1.0e11'))
        self.FITOPTS['p_T0'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','p_T0',required=0,defaultParm='1000.0'))
        self.FITOPTS['p_M0'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','p_M0',required=0,defaultParm='16.0'))
        self.FITOPTS['LagrangeParams'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','LagrangeParams',required=0,defaultParm='[1.0e4,1.0e4,1.0e2]'))
        if len(self.FITOPTS['LagrangeParams']) != 3:
            raise ValueError("LagrangeParams must be a list of length 3. Te, Ti, and Ne penalties.")
        self.FITOPTS['procMedian'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','procMedian',required=0,defaultParm='0'))
        self.FITOPTS['molecularModel'] = float(eval(io_utils.ini_tool(config,'FIT_OPTIONS','molecularModel',required=0,defaultParm='0')))
        self.FITOPTS['molmodFile'] = io_utils.ini_tool(config,'FIT_OPTIONS','molmodFile',required=0,defaultParm='')
        self.FITOPTS['z50'] = float(eval(io_utils.ini_tool(config,'FIT_OPTIONS','z50',required=0,defaultParm='150.0')))
        self.FITOPTS['fitSpectra'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','fitSpectra',required=0,defaultParm='0'))
        self.FITOPTS['mi'] = np.array(eval(io_utils.ini_tool(config,'FIT_OPTIONS','mi',required=0,defaultParm='[16.0]')))
        if self.FITOPTS['DO_FITS']==1:
            self.FITOPTS['SUMMATION_RULE'] = np.array(eval(io_utils.ini_tool(config,'FIT_OPTIONS','SUMMATION_RULE',required=1,defaultParm='')))
            self.FITOPTS['Lags2fit'] = np.array(eval(io_utils.ini_tool(config,'FIT_OPTIONS','Lags2fit',required=0,defaultParm='[]')))
            self.FITOPTS['htmin'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','htmin',required=0,defaultParm='50e3')); htmin=self.FITOPTS['htmin']
            self.FITOPTS['htmax'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','htmax',required=0,defaultParm='10000e3')); htmax=self.FITOPTS['htmax']
            self.FITOPTS['rngmin'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','rngmin',required=0,defaultParm='50e3'))
            self.FITOPTS['Nrngs'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','Nrngs',required=0,defaultParm='10'))
            self.FITOPTS['BinByRange'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','BinByRange',required=0,defaultParm='0'))
            self.FITOPTS['Ngroup'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','Ngroup',required=0,defaultParm='1'))
            self.FITOPTS['NION'] = eval(io_utils.ini_tool(config,'FIT_OPTIONS','NION',required=0,defaultParm='1'))
            self.FITOPTS['GroupHt'] = np.array(eval(io_utils.ini_tool(config,'FIT_OPTIONS','GroupHt',required=0,defaultParm="[self.FITOPTS['htmax']]")))
            self.FITOPTS['Ifit'] = np.array(eval(io_utils.ini_tool(config,'FIT_OPTIONS','Ifit',required=0,defaultParm='[[[0,1,0,1],[0,1,0,-1]]]')))
        self.FITOPTS['DEFOPTS'] = self.DEFOPTS

        # do some error checking
        if self.FITOPTS['DO_FITS']:
            if self.FITOPTS['GroupHt'].size != self.FITOPTS['Ngroup']:
                raise ValueError('GroupHt must have length Ngroup')
            if self.FITOPTS['mi'].size != self.FITOPTS['NION']:
                raise ValueError('mi must have length NION')
            if (self.FITOPTS['Ifit'].shape[0] != self.FITOPTS['Ngroup']) or (self.FITOPTS['Ifit'].shape[1] != self.FITOPTS['NION']+1) or (self.FITOPTS['Ifit'].shape[2] != 4):
                raise ValueError('Ifit must have size (Ngroup) x (NION+1) x 4')
            if (self.FITOPTS['GroupHt'][-1]<self.FITOPTS['htmax']):
                raise ValueError('GroupHt must go up to htmax!')

            if self.FITOPTS['PERTURBATION_NOISE'] == 1:
                if self.FITOPTS['fit0lag'] == 0:
                    raise ValueError('Perturbation noise fitting cannot work without fitlag0=1!')

        self.config = config

        return


    def read_a_datafile(self,files,frec,output=[],Irec=-1,nrecs=-1):

        # the filename
        fname=files[frec]
        if fname and fname[-1] == '\n':
            fname = fname[:-1]
        print('Reading file ' + fname)

        # make sure the file exists
        if os.path.exists(fname)==False:
            raise IOError('The input file does not exist.')

        # read the entire file
        with tables.open_file(fname) as h5file:
            if len(output)==0 or Irec==-1 or nrecs==-1: # we dont need to worry about preserving records
                output={}
                for group in h5file.walk_groups("/"):
                    if 'RAW' in group._v_pathname.upper():
                        continue
                    if 'OLD' in group._v_pathname.upper():
                        continue
                    output[group._v_pathname]={}
                    for array in h5file.list_nodes(group, classname = 'Array'):
                        temp = array.read()
                        shape = np.shape(temp)
                        if len(shape) == 0:
                            temp = temp.tolist()
                            if isinstance(temp,bytes):
                                temp = temp.decode('utf-8')
                        output[group._v_pathname][array.name]=temp
            else: # we do need to worry about preserving records
                output_cp=output
                output={}
                for group in h5file.walk_groups("/"):
                    if 'RAW' in group._v_pathname.upper():
                        continue
                    if 'OLD' in group._v_pathname.upper():
                        continue
                    if 'HISTORY' in group._v_pathname.upper():
                        continue
                    output[group._v_pathname]={}
                    for array in h5file.list_nodes(group, classname = 'Array'):
                        temp = array.read()
                        shape = np.shape(temp)
                        if len(shape) == 0:
                            temp = temp.tolist()
                            if isinstance(temp,bytes):
                                temp = temp.decode('utf-8')
                        output[group._v_pathname][array.name]=temp
                        if (np.ndim(np.asarray(output_cp[group._v_pathname][array.name]))>0) and (np.asarray(output_cp[group._v_pathname][array.name]).shape[0]==nrecs): # it is a record we should preserve
                            output[group._v_pathname][array.name]=np.concatenate((output_cp[group._v_pathname][array.name][Irec:],output[group._v_pathname][array.name]),axis=0)


        return output


    def get_expname(self,fname):
        try:
            with tables.open_file(fname) as h5file:
                expname = h5file.get_node('/Setup/Experimentfile').read().tolist()
            if isinstance(expname,bytes):
                expname = expname.decode('utf-8')
            expname = expname.replace('\r','')
            expname = expname.splitlines()[1].split('=')[1]
        except Exception as e:
            print("Could not determine Experiment Name because: %s" % (str(e)))
            print("Defaulting to blank name.")
            expname=''

        return expname


    def write_config_info(self,h5fhandle,raw_files):
        import platform
        import getpass

        # Configuration Information
        #Fitter Version Number: Follows convention: major.minor.year.month.day
        #version='1.0.2017.10' Get this variable from the global variable defined at the top of the file

        # Computer information:
        PythonVersion   = platform.python_version()
        Type            = platform.machine()
        System          = "%s %s %s" % (platform.system(),platform.release(),platform.version())
        User            = getpass.getuser()
        Hostname        = platform.node()
        if len(Hostname) == 0:
            import socket
            Hostname = socket.gethostname()

        io_utils.createh5groups(h5fhandle,[('/ProcessingParams/ComputerInfo','Processing Computer Information')])
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonVersion',PythonVersion)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/Type',Type)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/System',System)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/User',User)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/Host',Hostname)

        # Python Package Versions
        io_utils.createh5groups(h5fhandle,[('/ProcessingParams/ComputerInfo/PythonPackages','Dependency Information')])
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/numpy',np.__version__)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/matplotlib',matplotlib.__version__)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/tables',tables.__version__)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/scipy',scipy.__version__)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/flipchem',flipchem.__version__)
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/apexpy',apexpy.__version__)
        pymap3d_version = pkg_resources.get_distribution('pymap3d').version
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/ComputerInfo/PythonPackages/pymap3d',pymap3d_version)

        # Fitter configuration information
        Version = version  # Eventually replaced by self.__version__

        io_utils.createh5groups(h5fhandle,[('/ProcessingParams/FittingInfo','Fitter Configuration Information'),('/ProcessingParams/FittingInfo/ConfigFiles','Configuration Files Used')])
        io_utils.createStaticArray(h5fhandle,'/ProcessingParams/FittingInfo/Version',Version)

        # Get all the configuration files
        i = 1
        for cf in self.options.conffile.split(','):
            Path = os.path.dirname(os.path.abspath(cf))
            Name = os.path.basename(cf)

            with open(cf,'r') as f:
                Contents = "".join(f.readlines())

            h5path = '/ProcessingParams/FittingInfo/ConfigFiles/File%s' % (str(i))
            io_utils.createh5groups(h5fhandle,[(h5path,'Config File Information')])
            io_utils.createStaticArray(h5fhandle,h5path +'/Name',Name)
            io_utils.createStaticArray(h5fhandle,h5path +'/Path',Path)
            io_utils.createStaticArray(h5fhandle,h5path +'/Contents',Contents)

            i += 1

        # Record the raw files used
        # Make a string listing all the files
        file_string = ''
        for files in raw_files:
            temp = "\n".join(files)
            if len(file_string):
                file_string = '\n' + file_string
            else:
                file_string += temp
        # Write the string to the h5 file
        h5path = '/ProcessingParams/FittingInfo'
        io_utils.createStaticArray(h5fhandle,h5path +'/RawFiles',file_string)

        # Record the directory where fitted files can be found
        outpath = os.path.abspath(self.OPTS['outputpath'])
        io_utils.createStaticArray(h5fhandle,h5path +'/OutputPath',outpath)


    # run
    def run(self):
    # main routine that runs the fitting loop.
    # call after instantiating a run_fitter instance
        print("***************************************")
        print(self.FITOPTS['fitcal'])
        if (self.FITOPTS['fitcal'] == 1):
            print("Appending -fitcal to file names.")

            #Now change the output file name so it has the -fitcal string appended to it
            temp = self.OPTS['outfile']
            temp2 = temp[:-3]+'-fitcal'+temp[-3:]
            print("Changing output filename to " + temp2)
            self.OPTS['outfile'] = temp2

            #Finally, change the output directory name for the plots directory
            temp = self.OPTS['plotsdir']+'-fitcal'
            print("Changing plotting dirname to " + temp)
            self.OPTS['plotsdir'] = temp

        #Initialize figure handles
        self.ContinueFromLocked = 1

        # close all figures
        pyplot.close('all')

        # check out the FILELIST
        try:
            if (type(self.OPTS['FILELIST']) != tuple):
                self.OPTS['FILELIST'] = tuple([self.OPTS['FILELIST']])
            NFREQ = len(self.OPTS['FILELIST'])
        except:
            print('Problem understanding filelist')
            return

        # check out the raw file paths
        try:
            if (type(self.OPTS['ipath'])!=tuple):
                self.OPTS['ipath']=tuple([self.OPTS['ipath']])
        except:
            print('Problem understanding FILE_PATH')
            return

        # read the file that contains the list of files to process
        files=[]
        input_files = []
        for ii in range(NFREQ): # for each of the frequencies
            #print(self.OPTS['FILELIST'][ii])
            f=open(self.OPTS['FILELIST'][ii]) # open
            files.append(f.readlines()) # read list
            f.close() # close
            input_files.append(list())
            #print(files)
            for ir in range(len(files[ii])):
                files[ii][ir]=files[ii][ir].rstrip('\n')
                files[ii][ir]=files[ii][ir].rstrip('\r')
            #print(files)
            for ir in range(files[ii].count('')):
                files[ii].remove('')
                files[ii].remove('\n')
                files[ii].remove('\r')
            #print(files)
            files2=copy.copy(files[ii])
            for ir in range(len(files2)):
                for path in self.OPTS['ipath']:
                    tfiles=glob.glob(os.path.join(path,files2[ir]))
                        # print(tfiles)
                    files[ii].extend(tfiles)
                files[ii].remove(files2[ir])

            if len(files[ii])!=len(files[0]): # abort! they need to be the same number of files
                raise IOError('For multiple frequency/external cal, need the same number of files for each freq...')
            files[ii]=sorted(files[ii],key=os.path.basename)

        # Get number of raw files
        nfiles = 0
        for file_list in files:
            nfiles += len(file_list) # number of files to process
        print('Found %s raw files and %s frequency band(s)...' % (nfiles,NFREQ))

        if nfiles==0: # abort!
            print('Nothing to do...')
            return

        # Announce number of file groups (essentially the number of files per frequency)
        num_file_groups = nfiles/NFREQ
        print('There are %s file groups to process...' % (num_file_groups))

        # make plot directory
        if self.OPTS['saveplots']==1:
            if not os.path.exists(self.OPTS['plotsdir']):
                try:
                    os.mkdir(self.OPTS['plotsdir'])
                except:
                    print('Cant make plots dir.')

        # create the output file
        self.OPTS['outfileLocked'] = self.OPTS['outfile']+'.lock'
        if os.path.exists(self.OPTS['outfileLocked']) and self.ContinueFromLocked:
            try:
                output      = io_utils.read_whole_h5file(self.OPTS['outfileLocked'])
                NrecsToSkip = output['/Time']['UnixTime'].shape[0]
                del output
                print("Continuing using %s from record %s." % (self.OPTS['outfileLocked'],str(NrecsToSkip)))
            except:
                raise IOError('Unable to continue from locked file: ' + self.OPTS['outfileLocked'])
        else:
            NrecsToSkip = 0
            with tables.open_file(self.OPTS['outfileLocked'], mode = "w",
                                  title = "Fitted Output File") as outh5file:
                io_utils.createh5groups(outh5file,[self.h5Paths['MSIS'],
                                                   self.h5Paths['Geomag'],
                                                   self.h5Paths['RawPower'],
                                                   self.h5Paths['Params'],
                                                   self.h5Paths['Site'],
                                                   self.h5Paths['Time']])
                if self.FITOPTS['DO_FITS']:
                    io_utils.createh5groups(outh5file,[self.h5Paths['Fitted'],self.h5Paths['FitInfo']])
                    if self.OPTS['saveACFs']:
                        io_utils.createh5groups(outh5file,[self.h5Paths['ACFs']])
                if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    io_utils.createh5groups(outh5file,[self.h5Paths['Antenna']])

                # Add calibration information
                if (self.FITOPTS['fitcal'] == 1):
                    print("Adding calibration information.")

                    fname = self.OPTS['outfileLocked']
                    calFname = self.FITOPTS['beamMapScaleFile']
                    calMethodIndex = 0  #Plasma Line! Need to change this so there is a test done to determine which method was used!!!
                    add_calibration_info(fname,calFname,calMethodIndex)

                # Add fitter version number and config files (fit, io, system defaults)
                self.write_config_info(outh5file,files)

        # initialize some vars
        done     = 0 # flag to say when we are done
        IIrec    = 0 # record counter
        Irec     = 0 # record counter within a file
        frec     = 0 # file counter
        newexp   = 0 # experiment switch
        read_new = 1 # flag that says to read a new file
        Iplot    = self.OPTS['nplots'] # plot counter
        Ibeams   = None

        # read the first file
        Ibad      = list()
        outputAll = list()
        for ii in range(NFREQ):
            outputAll.append(self.read_a_datafile(files[ii],frec)) # read the first data files
            if self.FITOPTS['MOTION_TYPE'] == 1: # Az,El
                Ibad.append(np.where((outputAll[ii]['/Antenna']['Mode'][:,0] != outputAll[ii]['/Antenna']['Mode'][:,1]) | (outputAll[ii]['/Antenna']['Event'][:,0] != outputAll[ii]['/Antenna']['Event'][:,1]))[0])

        output      = outputAll[0]
        curexpname  = self.get_expname(files[0][frec])
        RecInt      = np.mean(output['/Time']['UnixTime'][:,1] - output['/Time']['UnixTime'][:,0])

        print('Experiment: %s' % (curexpname))

        ### start: main loop
        while not done:

            # find which records we should read
            #try:
            if 1==1:
                RecStartTime = output['/Time']['UnixTime'][Irec,0]
                RecEndTime = RecStartTime+self.FITOPTS['Recs2Integrate']+RecInt/2.0
                if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    AntennaMode = output['/Antenna']['Mode'][Irec,0]
                    AntennaEvent = output['/Antenna']['Event'][Irec,0]
                Irecs=[]
                for ii in range(NFREQ):
                    uTime = np.mean(outputAll[ii]['/Time']['UnixTime'],axis=1)
                    trecs = np.where((uTime>=RecStartTime) & (uTime<=RecEndTime))[0]
                    if len(trecs)==0:
                        trecs=[min([Irec,uTime.shape[0]-1])]
                    Irecs.append(trecs)

                    if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                        I=np.where((outputAll[ii]['/Antenna']['Mode'][Irecs[ii],0] != AntennaMode) | (outputAll[ii]['/Antenna']['Mode'][Irecs[ii],1] != AntennaMode) |
                            (outputAll[ii]['/Antenna']['Event'][Irecs[ii],0] != AntennaEvent) | (outputAll[ii]['/Antenna']['Event'][Irecs[ii],1] != AntennaEvent))[0]
                        if len(I)>0:
                            Irecs[ii]=Irecs[ii][:I[0]]

            """
            except:  # this handles cases where there are big time gaps between raw files or records?
                Irecs=[]
                for ii in range(NFREQ):
                    Irecs.append([-1])
            """

            breakout=0
            while (Irec>=output['/Time']['UnixTime'].shape[0] or Irecs[0][-1]==(output['/Time']['UnixTime'].shape[0]-1)) and done==0 and breakout==0: # it has read the last record, so try to read another file
                frec+=1
                if frec>=num_file_groups:
                    done=1
                else:
                    # read another file
                    expname=self.get_expname(files[0][frec])
                    if expname==curexpname:
                        Irec=0
                        for ifreq in range(NFREQ):
                            try:
                                trec = Irecs[ifreq][0]
                            except:
                                trec = -1
                            outputAll[ifreq] = self.read_a_datafile(files[ifreq],frec,outputAll[ifreq],trec,outputAll[ifreq]['/Time']['UnixTime'].shape[0])
                            if self.FITOPTS['MOTION_TYPE'] == 1: # Az,El
                                Ibad[ifreq] = np.where((outputAll[ifreq]['/Antenna']['Mode'][:,0] != outputAll[ifreq]['/Antenna']['Mode'][:,1]) | (outputAll[ifreq]['/Antenna']['Event'][:,0] != outputAll[ifreq]['/Antenna']['Event'][:,1]))[0]
                        output = outputAll[0]
                    elif Irec < output['/Time']['UnixTime'].shape[0]: # case where next file is a new experiment, but we want to process the last group of recs
                            frec -= -1
                            breakout=1
                    else: # we've transitioned to a new experiment
                        print('New experiment: ' + expname)
                        curexpname = expname
                        newexp = 1
                        self.BMCODES = None
                        outputAll = list()
                        for ii in range(NFREQ):
                            outputAll.append(self.read_a_datafile(files[ii],frec)) # read the first data files
                        output = outputAll[0]
                        Irec = 0



                    # get records
                    RecStartTime = output['/Time']['UnixTime'][Irec,0]
                    RecEndTime = RecStartTime+self.FITOPTS['Recs2Integrate']+RecInt/2.0
                    if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                        AntennaMode = output['/Antenna']['Mode'][Irec,0]
                        AntennaEvent = output['/Antenna']['Event'][Irec,0]
                    Irecs=[]
                    for ii in range(NFREQ):
                        uTime = np.mean(outputAll[ii]['/Time']['UnixTime'],axis=1)
                        trecs = np.where((uTime>=RecStartTime) & (uTime<=RecEndTime))[0]
                        if len(trecs)==0:
                            trecs=[Irec]
                        Irecs.append(trecs)
                        if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                            I=np.where((output['/Antenna']['Mode'][Irecs[ii],0] != AntennaMode) | (outputAll[ii]['/Antenna']['Mode'][Irecs[ii],1] != AntennaMode) |
                                (outputAll[ii]['/Antenna']['Event'][Irecs[ii],0] != AntennaEvent) | (outputAll[ii]['/Antenna']['Event'][Irecs[ii],1] != AntennaEvent))[0]
                            if len(I)>0:
                                Irecs[ii]=Irecs[ii][:I[0]]
            if done==1 and Irecs[0][-1]==-1:
                break;

            # print(the record numbers)
            print('\nFile Group ' + str(frec) + ' of ' + str(num_file_groups))
            print('Integration Number: ' + str(IIrec+1) + ', Recs Being Integrated: ' + str(Irecs[0][0]) + ':' + str(Irecs[0][-1]))
            fstr='File Group %d of %d, Rec %d, ' % (frec,num_file_groups,IIrec+1)

            # skip records
            if NrecsToSkip>0:
                Irec=Irecs[0][-1]+1
                try:
                    while Ibad[0].__contains__(Irec):
                        Irec=Irec+1
                except: ''
                IIrec=IIrec+1
                Iplot=Iplot+1
                NrecsToSkip-=1
                continue

            #
            if self.FITOPTS['MOTION_TYPE']==0:
                output['/Setup']['BeamcodeMap']=np.asarray(output['/Setup']['BeamcodeMap'])

            try:
                output['/Rx']['CalTemp']
            except:
                print('Using default CalTemp %s' % str(self.DEFOPTS['CAL_TEMP_DEF']))
                output['/Rx']['CalTemp']=self.DEFOPTS['CAL_TEMP_DEF']

            # process a record
#            try:
            #if 1==1:
            if NFREQ==1:
                [S,Noise,Cal] = eval('process_data.'+self.OPTS['proc_funcname']+"(output,Irecs[0],self.FITOPTS,self.AMB,doamb=(not self.AMB['Loaded']),extCal=self.FITOPTS['useExternalCal'],h5DataPath=self.OPTS['h5DataPath'],BeamCodes=self.BMCODES)")
            else:
                [S,Noise,Cal] = eval('process_data.'+self.OPTS['proc_funcname']+"(outputAll,Irecs,self.FITOPTS,self.AMB,doamb=(not self.AMB['Loaded']),extCal=self.FITOPTS['useExternalCal'],h5DataPath=self.OPTS['h5DataPath'],BeamCodes=self.BMCODES)")
#            except:
#                raise RuntimeError('Error calling %s' % self.OPTS['proc_funcname'])
            try:
                (Nbeams,self.Nlags,self.Nranges)=S['Acf']['Data'].shape
            except:
                (Nbeams,self.Nranges)=S['Power']['Data'].shape
            self.S=S

            # Ambiguity function
            if (not self.AMB['Loaded']): # if it hasn't already been loaded, we need to try to get it from data files
                if not 'Acf' in S:
                    if not 'Ambiguity' in S['Power']:
                        raise RuntimeError('No valid ambiguity function in data files or specified external file.')
                    else:
                        self.AMB=S['Power']['Ambiguity']
                        self.AMB['Loaded']=1
                elif not 'Ambiguity' in S['Acf']: # this is the case where we needed to get it from the data file but unable to
                    raise RuntimeError('No valid ambiguity function in data files or specified external file.')
                else:
                    self.AMB=S['Acf']['Ambiguity']
                    self.AMB['Loaded']=1

            # Transmitter
            Tx={}
            try:
                Tx['Frequency']=np.median(np.mean(output['/Tx']['Frequency'][Irecs[0],:],axis=1))
                if Tx['Frequency']<1.0e6:
                    raise ValueError("Tx Frequency Not Set, Using Default")
            except: Tx['Frequency']=self.DEFOPTS['TX_FREQ_DEF']
            if self.FITOPTS['txpow'] is not None:
                Tx['Power'] = self.FITOPTS['txpow']
            else:
                try: Tx['Power']=np.median(np.mean(output['/Tx']['Power'][Irecs[0],:],axis=1))
                except: Tx['Power']=self.DEFOPTS['TX_POWER_DEF']
            if Tx['Power']==0.0:
                Tx['Power']=self.DEFOPTS['TX_POWER_DEF']
            try: Tx['Aeu']=np.median(np.mean(output['/Tx']['AeuTx'][Irecs[0],:],axis=1))
            except: Tx['Aeu']=-1
            try: S['Acf']['Psc']=S['Acf']['Psc']*Tx['Power'];
            except: '' # Power scaling factor
            self.k_radar0=4.0*pi*Tx['Frequency']/v_lightspeed
            self.FITOPTS['p_om0']=self.k_radar0*np.sqrt(2.0*v_Boltzmann*self.FITOPTS['p_T0']/(self.FITOPTS['p_M0']*v_amu))


            # Receive
            Rx={}
            try: Rx['Frequency']=np.median(np.mean(output['/Rx']['Frequency'][Irecs[0],:],axis=1))
            except: Rx['Frequency']=self.DEFOPTS['TX_FREQ_DEF']
            try: Rx['Aeu']=np.median(np.mean(output['/Rx']['AeuRx'][Irecs[0],:],axis=1))
            except: Rx['Aeu']=-1
            try: Rx['AeuTotal']=np.median(np.mean(output['/Site']['AeuTotal'][Irecs[0],:],axis=1))
            except: Rx['AeuTotal']=-1;

            # Time
            self.Time['UnixTime']=np.array([output['/Time']['UnixTime'][Irecs[0][0],0],output['/Time']['UnixTime'][Irecs[0][-1],1]])
            tmp1 = datetime.datetime.utcfromtimestamp(self.Time['UnixTime'][0])
            tmp2 = datetime.datetime.utcfromtimestamp(self.Time['UnixTime'][1])
            diff = (tmp2-tmp1).total_seconds()
            r1 = datetime.date(tmp1.year,tmp1.month,tmp1.day)
            r2 = datetime.date(tmp2.year,tmp2.month,tmp2.day)
            self.Time['datetime'] = tmp1 + datetime.timedelta(seconds = diff / 2.0)
            self.Time['Year'] = np.array([tmp1.year,tmp2.year])
            self.Time['Month'] = np.array([tmp1.month,tmp2.month])
            self.Time['Day'] = np.array([tmp1.day,tmp2.day])
            self.Time['dtime'] = np.array([(float(tmp1.hour)+float(tmp1.minute)/60.0+float(tmp1.second)/3600.0),(float(tmp2.hour)+float(tmp2.minute)/60.0+float(tmp2.second)/3600.0)])
            self.Time['doy'] = np.array([int(r1.strftime('%j')),int(r2.strftime('%j'))])

            # get some standard info the first time through
            if self.BMCODES is None or newexp:

                self.BMCODES = S['BMCODES'] # beamcodes
                self.BMCODES[:,2] = np.absolute(self.BMCODES[:,2])

                # read site information
                self.Site['Latitude']=float(output['/Site']['Latitude']) # site latitude
                self.Site['Longitude']=float(output['/Site']['Longitude']) # site longitude
                if self.Site['Longitude']>0: # some files have the sign of the longitude flipped
                    self.Site['Longitude']*=-1
                self.Site['Altitude']=float(output['/Site']['Altitude']) # site altitude
                try: self.Site['Code']=int(output['/Site']['Code']); # site code
                except: '';
                try: self.Site['Name']=output['/Site']['Name']; # site name
                except: '';

                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    if IIrec==0 or newexp or Ibeams==None: # first time through
                        if newexp:
                            newexp=0
                            try:
                                del self.Gmag
                            except:
                                pass
                        Im1=np.where(self.BMCODES[:,0]!=-1.0)[0]
                        if len(self.FITOPTS['Beams2do'])>0:
                            Ibeams=[]
                            for ii in range(len(self.FITOPTS['Beams2do'])):
                                try:
                                    Ibeams.append(int(np.where(self.BMCODES[:,0]==self.FITOPTS['Beams2do'][ii])[0]))
                                except:
                                    raise RuntimeError('No beamcode: %d!!' % (self.FITOPTS['Beams2do'][ii]))
                        elif len(Im1)!=Nbeams:
                            Ibeams=Im1
                        else:
                            Ibeams=range(Nbeams)
                        self.BMCODES=self.BMCODES[Ibeams,:]
                        self.Nbeams=len(Ibeams)


                    # run the geomagnetic model
                    self.gmag=geomag.geomag(self.Time['Year'][0],self.BMCODES,self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0) # run the geomag model

                    if self.OPTS['plotson']>0: # plot the geomag
                        gfig=plot_utils.geoplot(self.BMCODES[:,1],self.BMCODES[:,2],self.gmag['Range'],self.gmag['Altitude'],self.gmag['MagneticLatitude'],self.gmag['MagneticLongitude'],self.gmag['Dip'],self.gmag['Declination'])
                        if self.OPTS['saveplots']==1: # save the geomag
                            if os.path.exists(self.OPTS['plotsdir']):
                                oname= "%d-%d-%d geoplot.png" % (self.Time['Month'][0],self.Time['Day'][0],self.Time['Year'][0])
                                gfig.savefig(os.path.join(self.OPTS['plotsdir'],oname))
                    if self.FITOPTS['DO_FITS']==0:
                        Gmag=self.gmag

                else: # Az, El
                    self.Nbeams=1
                    self.gmag=geomag.geomag(self.Time['Year'][0],self.BMCODES,self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0,rng=np.array([0.0])) # run the geomag model

                # get geomag info of site to 2 decimal places
                self.Site['MagneticLatitude']=float(np.asarray(self.gmag['MagneticLatitude'][0,0]).round(decimals=2))
                self.Site['MagneticLongitude']=float(np.asarray(self.gmag['MagneticLongitude'][0,0]).round(decimals=2))
                self.Site['MagneticLocalTimeMidnight']=float(np.asarray(self.gmag['MLTMidnightUT'][0,0]).round(decimals=3))

            # calculate MLT at the site
            if self.OPTS['ComputeMLT']>=1: # just calculates MLT at the site
                self.Time['MagneticLocalTimeSite']=self.Time['dtime']-self.Site['MagneticLocalTimeMidnight']
                if self.Time['MagneticLocalTimeSite'][0]<0.0:
                    self.Time['MagneticLocalTimeSite']+=24.0

            # get altitude using geodetic conversion
            if 'Power' in S:
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    S['Power']['Altitude']=proc_utils.range2height(np.squeeze(S['Power']['Range'][0,:])/1000.0,S['BMCODES'][:,1],S['BMCODES'][:,2],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)
                elif self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    S['Power']['Altitude']=proc_utils.range2height(np.squeeze(S['Power']['Range'][0,:])/1000.0,[S['AvgAzimuth']],[S['AvgElevation']],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)
            if 'Acf' in S:
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    S['Acf']['Altitude']=proc_utils.range2height(np.squeeze(S['Acf']['Range'][0,:])/1000.0,S['BMCODES'][:,1],S['BMCODES'][:,2],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)
                elif self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    S['Acf']['Altitude']=proc_utils.range2height(np.squeeze(S['Acf']['Range'][0,:])/1000.0,[S['AvgAzimuth']],[S['AvgElevation']],self.Site['Latitude'],self.Site['Longitude'],self.Site['Altitude']/1000.0)

            # Trim data based on beam
            if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                #print(Cal)
                #print(Noise)
                S=process_data.trim_Ibeams(S,Ibeams,Nbeams)
                Noise=process_data.trim_Ibeams(Noise,Ibeams,Nbeams)

                Cal=process_data.trim_Ibeams(Cal,Ibeams,Nbeams)


            # model for computing Ne from power
            Mod={}
            Mod['Te']=np.ones(S['Power']['Altitude'].shape)*1000
            Mod['Ti']=np.ones(S['Power']['Altitude'].shape)*1000
            Mod['Ne']=np.ones(S['Power']['Altitude'].shape)*1.0e11

            # compute density profile using apriori model for temperatures
            S['Power']['SNR']=np.real(S['Power']['SNR'])
            if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                (S['Power']['Ne_Mod'],S['Power']['Ne_NoTr'],tPsc)=proc_utils.ne_prof(S['Power']['Data'],S['Power']['Range'][0,:],S['Power']['Altitude'],Mod,Tx['Power'],S['Power']['Pulsewidth'],Tx['Frequency'],S['BMCODES'][:,3])
            elif self.FITOPTS['MOTION_TYPE']==1: # Az,El
                (S['Power']['Ne_Mod'],S['Power']['Ne_NoTr'],tPsc)=proc_utils.ne_prof(S['Power']['Data'],S['Power']['Range'][0,:],S['Power']['Altitude'],Mod,Tx['Power'],S['Power']['Pulsewidth'],Tx['Frequency'],S['Ksys'])
            S['Power']['dNeFrac']=1.0/np.sqrt(S['Power']['Kint']*S['Power']['PulsesIntegrated'])*(1.0+np.absolute(1.0/S['Power']['SNR'])+S['Power']['iSCR'])
            S['Power']['dNeFrac'][np.where(S['Power']['SNR']<0.0)]=np.nan
            if self.FITOPTS['uselag1']: # if we are using the 1st lag to estimate the density, then we need to scale it a bit
                S['Power']['Ne_Mod']=S['Power']['Ne_Mod']/np.sum(np.absolute(self.AMB['Wlag'][S['Acf']['Lag1Index'],:]))
                S['Power']['Ne_NoTr']=S['Power']['Ne_NoTr']/np.sum(np.absolute(self.AMB['Wlag'][S['Acf']['Lag1Index'],:]))
            else:
                S['Power']['Ne_Mod']=S['Power']['Ne_Mod']/np.sum(np.absolute(self.AMB['Wlag'][0,:]))
                S['Power']['Ne_NoTr']=S['Power']['Ne_NoTr']/np.sum(np.absolute(self.AMB['Wlag'][0,:]))


                

            # A function to compute the perturbation noise acf based on the assumption that such noise
            # is white and broadband (at least compared to the width of the filter)
            def compute_noise_acf(num_lags,sample_time,impulse_response):

                t_num_taps = impulse_response.size
                t_times = np.arange(t_num_taps)*1e-6         # we have 1 us samples before decimating to final raw output
                t_acf = np.convolve(impulse_response,impulse_response)[t_num_taps-1:]
                t_acf = t_acf / t_acf[0]

                t_lag_times = np.arange(num_lags)*sample_time
                interp_func = scipy.interpolate.interp1d(t_times,t_acf,bounds_error=0, fill_value=0)
                noise_acf = interp_func(t_lag_times)

                return noise_acf

            ### start: DO_FITS
            if self.FITOPTS['DO_FITS']: # do the fits

                # Parameters needed for calculating the perturbation noise_acf
                temp = output['/Rx'].get('SampleTime',output['/Rx'].get('Sampletime',None))
                sample_time = float(temp)
                temp = output['/Setup']['RxFilterfile']
                # old files, from 2007 require this check
                if isinstance(temp,np.ndarray):
                    if len(temp.shape) > 0:
                        temp = temp[0]
                temp = str(temp)
                temp = temp.replace('\r','') 
                filter_coefficients = np.array([float(x) for x in temp.split('\n')[:-1]])

                # Compute the perturbation noise acf
                num_lags = self.Nlags
                if self.FITOPTS['PERTURBATION_NOISE']:
                    perturbation_noise_acf = compute_noise_acf(num_lags,sample_time,filter_coefficients)
                else:
                    perturbation_noise_acf = np.nan * np.zeros(num_lags)

                (trng,tht,tne,tnoise,tfits,terrs,tmod_ACF,tmeas_ACF,terrs_ACF,tfitinfo,modelOut,Gmag)=self.call_fitter(S,Noise,perturbation_noise_acf,sstr=fstr)

                self.FITS['Range']=trng
                self.FITS['Altitude']=tht
                self.FITS['Ne']=tne[:,:,0]
                self.FITS['Noise'] = tnoise
                self.FITS['dNe']=tne[:,:,1]
                self.FITS['Fits']=tfits
                self.FITS['Errors']=terrs
                self.FITS['FitInfo']=tfitinfo

                del self.Time['datetime']

                if self.OPTS['saveACFs']:
                    tshape=list(tmeas_ACF.shape)

                    self.FITS['ACFs']={}
                    self.FITS['ACFs']['ModelACF']=tmod_ACF
                    self.FITS['ACFs']['MeasACF']=tmeas_ACF
                    self.FITS['ACFs']['ErrsACF']=terrs_ACF


                # make the plots if we are supposed to
                if (self.OPTS['plotson']>0) and (Iplot>=self.OPTS['nplots']):
                    figg1 = None
                    figg2 = None
                    figg3 = None
                    figg4 = None
                    figg5 = None
                    ax1 = None
                    ax2 = None
                    ax3 = None
                    ax4 = None
                    ax5 = None

                    Iplot=0
                    try:
                        IbPl=range(len(Ibeams))
                    except:
                        IbPl=[]

                    title= "%d-%d-%d %.3f-%.3f UT" % (self.Time['Month'][0],self.Time['Day'][0],self.Time['Year'][0],self.Time['dtime'][0],self.Time['dtime'][1])

                    print("Making profile plots...")
                    try:
                        (figg1,ax1,_)=plot_utils.test_plot(self,IIrec,[],[],self.OPTS['xlims'],Ibeams=IbPl,dofrac=self.OPTS['plotfrac'])
                    except Exception as e:
                        print("Plotting failed: "+str(e))
                        figg1 = None

                    if (self.OPTS['saveplots']==1) and (figg1 is not None):
                        if os.path.exists(self.OPTS['plotsdir']):
                            oname=title + '.png'
                            figg1.savefig(os.path.join(self.OPTS['plotsdir'],oname))
                        else:
                            print("Can't output plots, path doesn't exist")
                            self.OPTS['saveplots']=0

                    if self.OPTS['plotson']>1:
                        print("Plotting ACFs...")
                        try:
                            (figg2,ax2)=plot_utils.acf_plot(tmeas_ACF,terrs_ACF,tmod_ACF,tht/1000.0,self.BMCODES,title,Ibeams=IbPl)
                        except Exception as e:
                            print("Plotting failed: "+str(e))
                            figg2 = None

                        if (self.OPTS['saveplots']==1) and (os.path.exists(self.OPTS['plotsdir'])) and (figg2 is not None):
                            oname='acf ' + title + '.png'
                            figg2.savefig(os.path.join(self.OPTS['plotsdir'],oname))

                        if self.OPTS['dumpSpectra']>0:
                            print("Plotting Spectra...")
                            try:
                                (figg6,ax6)=plot_utils.spc_plot(tmeas_ACF,terrs_ACF,tmod_ACF,tht/1000.0,self.BMCODES,title,Ibeams=IbPl)
                            except Exception as e:
                                print("Plotting failed: "+str(e))
                                figg6 = None

                            if (self.OPTS['saveplots']==1) and (os.path.exists(self.OPTS['plotsdir'])) and (figg6 is not None):
                                oname='spc ' + title + '.png'
                                figg6.savefig(os.path.join(self.OPTS['plotsdir'],oname))


                    if self.OPTS['dumpSpectra']>1:
                        try:
                            (figg3,ax3,figg4,ax4,figg5,ax5)=plot_utils.spc_pcolor_plot(tmeas_ACF,tmod_ACF,1.0/self.S['Acf']['Lags'][0,-1]/2.0,tht/1000.0,
                                self.BMCODES,title=title,ylim=[self.FITOPTS['htmin']/1000.0,self.FITOPTS['htmax']/1000.0],clim=[-19.0,-16.0])
                        except Exception as e:
                            print("Plotting failed: "+str(e))
                            figg3, figg4, figg5 = None, None, None

                        if (self.OPTS['saveplots']==1) and (os.path.exists(os.path.join(self.OPTS['plotsdir'],'Spectra'))) and (None not in [figg3, figg4, figg5]):
                            figg3.savefig(os.path.join(self.OPTS['plotsdir'],'Spectra','Spectra ' + title + ' Measured.png'))
                            figg4.savefig(os.path.join(self.OPTS['plotsdir'],'Spectra','Spectra ' + title + ' Modeled.png'))
                            figg5.savefig(os.path.join(self.OPTS['plotsdir'],'Spectra','Spectra ' + title + ' Residual.png'))

                    pyplot.close('all')
                    del figg1
                    del figg2
                    del figg3
                    del figg4
                    del figg5
                    del ax1
                    del ax2
                    del ax3
                    del ax4
                    del ax5

            # Output data to file
            # open file
            with tables.open_file(self.OPTS['outfileLocked'], mode = "a") as outh5file:
                # Processing params
                if IIrec==0:
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/ProcessingTimeStamp',np.array(time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())))
                    # Tx/Rx frequency and stuff
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/TxFrequency',np.array(Tx['Frequency']))
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/RxFrequency',np.array(Rx['Frequency']))
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/BaudLength',np.array(S['Power']['TxBaud']))
                    io_utils.createStaticArray(outh5file,self.h5Paths['Params'][0]+'/PulseLength',np.array(S['Power']['Pulsewidth']))
                    # Site
                    io_utils.createStaticArray(outh5file,self.h5Paths['Site'][0],self.Site,self.Site.keys())
                    # Beamcodes
                    if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                        io_utils.createStaticArray(outh5file,'/BeamCodes',self.BMCODES)
                io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/TxPower',np.array(Tx['Power']))
                if self.FITOPTS['MOTION_TYPE']==0: # Beamcodes
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/AeuTx',np.array(Tx['Aeu']))
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/AeuRx',np.array(Rx['Aeu']))
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Params'][0]+'/AeuTotal',np.array(Rx['AeuTotal']))
                # Antenna motion
                if self.FITOPTS['MOTION_TYPE']==1: # Az,El
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Antenna'][0],S,['Azimuth','Elevation','AvgAzimuth','AvgElevation'])
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Antenna'][0]+'/Mode',np.array(output['/Antenna']['Mode'][Irec,0]))
                    io_utils.createDynamicArray(outh5file,self.h5Paths['Antenna'][0]+'/Event',np.array(output['/Antenna']['Event'][Irec,0]))
                # Time
                io_utils.createDynamicArray(outh5file,self.h5Paths['Time'][0],self.Time,self.Time.keys())
                # MSIS model
                if self.FITOPTS['DO_FITS']:
                    io_utils.createDynamicArray(outh5file,self.h5Paths['MSIS'][0],modelOut,modelOut.keys())
                # Geomag
                if self.FITOPTS['MOTION_TYPE']==1 or self.OPTS['dynamicAlts']==1: # Az,El or case when we want to include entire array
                    if self.FITOPTS['DO_FITS']:
                        io_utils.createDynamicArray(outh5file,self.h5Paths['Geomag'][0],Gmag,Gmag.keys())
                else: # Beamcodes
                    if IIrec==0:
                        io_utils.createStaticArray(outh5file,self.h5Paths['Geomag'][0],Gmag,Gmag.keys())
                    self.Gmag=Gmag
                # Raw Power
                S['Power']['Range']=S['Power']['Range'][0,:]
                S['Power']['Range']=np.reshape(S['Power']['Range'],(1,S['Power']['Range'].size))
                if self.FITOPTS['MOTION_TYPE']==1 or self.OPTS['dynamicAlts']==1: # Az,El or case when we want to include entire array
                    io_utils.createDynamicArray(outh5file,self.h5Paths['RawPower'][0],S['Power'],['Ne_NoTr','Ne_Mod','SNR','dNeFrac','Altitude','Range'])
                else: # Beamcodes
                    io_utils.createDynamicArray(outh5file,self.h5Paths['RawPower'][0],S['Power'],['Ne_NoTr','Ne_Mod','SNR','dNeFrac'])
                    if IIrec==0:
                        io_utils.createStaticArray(outh5file,self.h5Paths['RawPower'][0],S['Power'],['Altitude','Range'])
                # Fitted Parameters
                if self.FITOPTS['DO_FITS']:
                    io_utils.createDynamicArray(outh5file,self.h5Paths['FitInfo'][0],self.FITS['FitInfo'],self.FITS['FitInfo'].keys())
                    if self.FITOPTS['MOTION_TYPE']==1 or self.OPTS['dynamicAlts']==1: # Az,El or case when we want to include entire array
                        io_utils.createDynamicArray(outh5file,self.h5Paths['Fitted'][0],self.FITS,['Errors','Fits','Ne','dNe','Range','Altitude','Noise'])
                    else: # beamcodes
                        io_utils.createDynamicArray(outh5file,self.h5Paths['Fitted'][0],self.FITS,['Errors','Fits','Ne','dNe','Noise'])
                        if IIrec==0:
                            io_utils.createStaticArray(outh5file,self.h5Paths['Fitted'][0],self.FITS,['Altitude','Range'])
                    if IIrec==0:
                        io_utils.createStaticArray(outh5file,self.h5Paths['Fitted'][0]+'/IonMass',self.FITOPTS['mi'])
                    if self.OPTS['saveACFs']:
                        io_utils.createDynamicArray(outh5file,self.h5Paths['ACFs'][0],self.FITS['ACFs'],self.FITS['ACFs'].keys())
                # set attributes
                if IIrec==0:
                    io_utils.setAtrributes(outh5file,self.h5Attribs)

            # increment counters
            Irec=Irecs[0][-1]+1
            try:
                while Ibad[0].__contains__(Irec):
                    Irec=Irec+1
            except: ''
            IIrec=IIrec+1
            Iplot=Iplot+1

        ### end: main loop

        # rename output file
        try:
            if os.path.exists(self.OPTS['outfile']):
                os.remove(self.OPTS['outfile'])
            os.rename(self.OPTS['outfileLocked'],self.OPTS['outfile'])
        except:
            raise IOError('Error renaming output file: ' + self.OPTS['outfileLocked'] + 'to ' + self.OPTS['outfile'])


        # make some final color plots
        if (self.OPTS['plotson']>0):
            # Set colour and range limits
            if len(self.OPTS['pcolClims']) > 0:
                clim = self.OPTS['pcolClims']
            else:
                clim = [[10,12],[0,1500],[0,3000],[0,4],[-500,500]]
            if len(self.OPTS['pcolYlims']) > 0:
                nonfitted_ylim = self.OPTS['pcolYlims']
                fitted_ylim = self.OPTS['pcolYlims']
            else:
                nonfitted_ylim = []
                fitted_ylim = []

            if self.FITOPTS['MOTION_TYPE']==1: 
                replot_pcolor_antenna_all(self.OPTS['outfile'],saveplots=1,opath=self.OPTS['plotsdir'],
                                          clims=clim,nonfitted_ylim=nonfitted_ylim,fitted_ylim=fitted_ylim)
            else:
                replot_pcolor_all(self.OPTS['outfile'],saveplots=1,opath=self.OPTS['plotsdir'],
                                  clims=clim,nonfitted_ylim=nonfitted_ylim,fitted_ylim=fitted_ylim)
        return 0

#######


def main():
    # parse input options
    usage = 'run_fitter [OPTIONS]'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option('-c','--conf',dest='conffile',
                        default='configuration.ini',
                        metavar="FILE",
                        help='Configuration file or list of configuration files (default configuration.ini)')
    (options,args) = parser.parse_args()

    # go go go
    RF=Run_Fitter(options)
    RF.run()

    sys.exit(0)


if __name__ == '__main__':
    main()