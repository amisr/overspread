#!/usr/bin/env python

"""

"""

from amisr_py.io import *

class fitterOutputFile(ioclass.outputFileClass):

    def __init__(self):
        self.title = 'Output File from ISR Fitter'
        
        self.h5Paths = {'Params'    :   ('/ProcessingParams','Experiment Parameters'),\
                        'Geomag'    :   ('/Geomag','Geomagnetic Parameters'),\
                        'RawPower'  :   ('/NeFromPower','Electron density From Power'),\
                        'Site'      :   ('/Site','Site Parameters'),\
                        'Time'      :   ('/Time','Time Information'),\
                        'Fitted'    :   ('/FittedParams','Fitted Parameters'),\
                        'FitInfo'   :   ('/FittedParams/FitInfo','Fitting Info'),\
                        'Antenna'   :   ('/Antenna','Antenna Motion Parameters'), \
                        'MSIS'      :   ('/MSIS', 'MSIS Output'), \
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
            '/Geomag/MagneticLatitude' : [('TITLE','Magnetic Latitude')],\
            '/Geomag/MagneticLongitude' : [('TITLE','Magnetic Longitude')],\
            '/Geomag/LshellRe' : [('TITLE','L Shell'),('Unit','Earth radii')],\
            '/Geomag/MLTMidnightUT' : [('TITLE','MLT Midnight'),('Unit','UT Hours')],\
            '/Geomag/ke' : [('TITLE','k East',),('Description','Eastward component of radar k vector')],\
            '/Geomag/kn' : [('TITLE','k North',),('Description','Northward component of radar k vector')],\
            '/Geomag/kz' : [('TITLE','k Up',),('Description','Vertical component of radar k vector')],\
            '/Geomag/kgeo' : [('TITLE','k Vector - Geographic',),('Description','North, East, Up')],\
            '/Geomag/kgmag' : [('TITLE','k Vector - Geomagnetic',),('Description','Anti-Parallel, Perp-East, Perp-North')],\
            '/Geomag/kvec' : [('TITLE','k Vector - Geographic Flat Eart',),('Description','North, East, Up')],\
            '/Geomag/kpar' : [('TITLE','k Anti-Parallel',),('Description','Parallel and upward to geomagnetic field component of radar k vector')],\
            '/Geomag/kpe' : [('TITLE','k Perp East',),('Description','Perpendicular to geomagnetic field and eastward component of radar k vector')],\
            '/Geomag/kpn' : [('TITLE','k Perp North',),('Description','Perpendicular to geomagnetic field and northward component of radar k vector')],\
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

        return