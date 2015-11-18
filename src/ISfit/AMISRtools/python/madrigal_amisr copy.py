"""The madrigal_amisr module is used to automate updating Madrigal with
SRI formated hdf5 files.  Written by Bill Rideout; specification by
Bill Rideout and Todd Valentic.  Meant to be used in realtime by a data
transport application.

This module will automatically create a new Madrigal experiment if needed.
Also allows for switching back to previously existing experiments.  The start
method is used either to start a new experiment, or to switch to an old one.
The update method is used to add data.

Modified 2007-10-25 to also add batch updating.
Modified 2008-01-11 to match latest fitter output.
Modified 2008-01-22 to allow for plot directory creation and plots with same name

$Id: madrigal_amisr.py,v 1.21 2009/08/04 17:25:49 brideout Exp $

May 30, 2007
"""

import os,os.path,sys
import datetime,time
import math
import tempfile
import traceback
import ConfigParser
import logging
import glob
import shutil
import struct
import distutils.dir_util

import scipy

import tables

import matplotlib.numerix

import madrigal.metadata
import madrigal.cedar
import madrigal.admin
import madrigal.ui.madrigalPlot

writeHeader=1

def isNan(num):
    """isNan is my attempt to detect IEEE special values such as nan.  Returns True if not a float with a
    real value.  Works with both python 2.3 and python 2.4

    Algorithm:  if both (num < 10.0) and (num > -10.0) are False, return true.
    """
    if ((num < 10.0) == False and (num > -10.0) == False):
        return True

    else:
        return False

def parseExpId(expId):
    """parseExpId parses an experiment id in the form YYYYMMDD.<inst_code>.<number>, and
    returns a tuple of (datetime, YYYYMMSS string, instId, optional char associated with number,
    and full path to the Madrigal experiment.

    Inputs:  expId - experiment id string in the form YYYYMMDD.<inst_code>.<number>, where
    the date represents the first day of the experiment, <inst_code> is the instrument
    code, and the trailing <number> is between 0 and 26
    
    Returns: a tuple with 5 items: 1. datetime represented by YYYYMMDD, 2. YYYYMMSS string
    itself, 3) the inst id, 4) the optional char associated with the number (0='', 1='a', ...
    26='z'), and 5) the string representing the full path to the Madrigal experiment in form
    $MADROOT/experiments/YYYY/<3_letter_inst>/DDmmmYYY<char>.

    Raises ValueError if expId not in proper format, instrument code not found,
    or trailing number < 0 or > 26.
    """

    madDBObj = madrigal.metadata.MadrigalDB()
    madInstObj = madrigal.metadata.MadrigalInstrument(madDBObj)
    
    try:
        year = int(expId[0:4])
        month = int(expId[4:6])
        day = int(expId[6:8])
    except:
        traceback.print_exc()
        raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))
    
    if year < 1900:
        raise ValueError, 'expId <%s> has too early year %i' % (str(expId), year)

    try:
        thisDate = datetime.datetime(year, month, day)
    except:
        traceback.print_exc()
        raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))

    try:
        items = expId.split('.')
        instCode = int(items[1])
        num = int(items[2])
    except:
        traceback.print_exc()
        raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))

    # get 3 letter instrument mnemonic
    mnem = madInstObj.getInstrumentMnemonic(instCode)

    if mnem == None:
        raise ValueError, 'unknown instrument code in expId: <%i>' % (instCode)

    if num < 0 or num > 26:
        raise ValueError, 'expId must end in number between 0 and 26, not %i' % (num)

    if num == 0:
        extChar = ''
    else:
        extChar = chr(96 + num)


    dirName = os.path.join(madDBObj.getMadroot(),
                           'experiments',
                           '%04i' % year,
                           mnem,
                           '%s%s' % (thisDate.strftime('%d%b%y').lower(), extChar))

    return((thisDate, items[0], instCode, extChar, dirName))


class analyzeHdf5:
    """analyzeHdf5 is a class to analyze a SRI-formated hdf5 file containing standard ISR parameters
    """
    def __init__(self,
                 hdf5File):
        """__init__ gets summary information about hdf5File containing standard ISR parameters.
        """
        self.__startTime = None # will be set to the earliest datetime
        self.__endTime = None # will be set to the latest datetime
        
        # read in all required data
        hdfObj = tables.openFile(hdf5File)

        # time info
        days = hdfObj.root.Time.Day
        dayArray = days.read()
        months = hdfObj.root.Time.Month
        monthArray = months.read()
        years = hdfObj.root.Time.Year
        yearArray = years.read()
        dtimes = hdfObj.root.Time.dtime
        dtimeArray = dtimes.read()

        # electron density (ne)
        ne = hdfObj.root.FittedParams.Ne
        neArray = ne.read()
        self.__numRecs = neArray.shape[0]

        for recIndex in range(self.__numRecs):
            # get start and end times for this record
            startYear = int(yearArray[recIndex][0])
            endYear = int(yearArray[recIndex][1])
            startMonth = int(monthArray[recIndex][0])
            endMonth = int(monthArray[recIndex][1])
            startDay = int(dayArray[recIndex][0])
            endDay = int(dayArray[recIndex][1])
            startDtime = dtimeArray[recIndex][0]
            endDtime = dtimeArray[recIndex][1]
            startHour = int(startDtime)
            endHour = int(endDtime)
            startMin = int(startDtime*60.0 - startHour*60.0)
            endMin = int(endDtime*60.0 - endHour*60.0)
            startSec = int(startDtime*3600.0) % 60
            endSec = int(endDtime*3600.0) % 60
            startTime = datetime.datetime(startYear, startMonth, startDay, startHour, startMin, startSec)
            endTime = datetime.datetime(endYear, endMonth, endDay, endHour, endMin, endSec)

            if self.__startTime == None:
                self.__startTime = startTime
            elif startTime < self.__startTime:
                self.__startTime = startTime

            if self.__endTime == None:
                self.__endTime = endTime
            elif endTime > self.__endTime:
                self.__endTime = endTime

        hdfObj.close()


    def getStartEndTimes(self):
        return (self.__startTime, self.__endTime)



class analyzeUncorrectedHdf5:
    """analyzeHdf5 is a class to analyze a SRI-formated hdf5 file containing uncorrected electron density
    """
    def __init__(self,
                 hdf5File):
        """__init__ gets summary information about hdf5File containing uncorrected electron density.
        """
        self.__startTime = None # will be set to the earliest datetime
        self.__endTime = None # will be set to the latest datetime
        
        # read in all required data
        hdfObj = tables.openFile(hdf5File)

        # time info
        days = hdfObj.root.Time.Day
        dayArray = days.read()
        months = hdfObj.root.Time.Month
        monthArray = months.read()
        years = hdfObj.root.Time.Year
        yearArray = years.read()
        dtimes = hdfObj.root.Time.dtime
        dtimeArray = dtimes.read()

        # uncorrected electron density (ne)
        pop = hdfObj.root.NeFromPower.Ne_NoTr
        popArray = pop.read()
        self.__numRecs = popArray.shape[0]

        for recIndex in range(self.__numRecs):
            # get start and end times for this record
            startYear = int(yearArray[recIndex][0])
            endYear = int(yearArray[recIndex][1])
            startMonth = int(monthArray[recIndex][0])
            endMonth = int(monthArray[recIndex][1])
            startDay = int(dayArray[recIndex][0])
            endDay = int(dayArray[recIndex][1])
            startDtime = dtimeArray[recIndex][0]
            endDtime = dtimeArray[recIndex][1]
            startHour = int(startDtime)
            endHour = int(endDtime)
            startMin = int(startDtime*60.0 - startHour*60.0)
            endMin = int(endDtime*60.0 - endHour*60.0)
            startSec = int(startDtime*3600.0) % 60
            endSec = int(endDtime*3600.0) % 60
            startTime = datetime.datetime(startYear, startMonth, startDay, startHour, startMin, startSec)
            endTime = datetime.datetime(endYear, endMonth, endDay, endHour, endMin, endSec)

            if self.__startTime == None:
                self.__startTime = startTime
            elif startTime < self.__startTime:
                self.__startTime = startTime

            if self.__endTime == None:
                self.__endTime = endTime
            elif endTime > self.__endTime:
                self.__endTime = endTime

        hdfObj.close()


    def getStartEndTimes(self):
        return (self.__startTime, self.__endTime)

class analyzeVectorHdf5:
    """analyzeHdf5 is a class to analyze a SRI-formated hdf5 file containing velocity vectors
    """
    def __init__(self,
                 hdf5File):
        """__init__ gets summary information about hdf5File containing velocity vectors.
        """
        self.__startTime = None # will be set to the earliest datetime
        self.__endTime = None # will be set to the latest datetime
        
        # read in all required data
        hdfObj = tables.openFile(hdf5File)

        # time info
        unixTimes = hdfObj.root.Time.UnixTime
        timeArray = unixTimes.read()

        self.__startTime = datetime.datetime.utcfromtimestamp(timeArray[0][0])
        self.__endTime = datetime.datetime.utcfromtimestamp(timeArray[-1][-1])

        hdfObj.close()


    def getStartEndTimes(self):
        return (self.__startTime, self.__endTime)

class hdf5ToMadrigal:
    """hdf5ToMadrigal is a class to turn a standard SRI-formated hdf5 file into a Madrigal file
    """

    def __init__(self,
                 hdf5File,
                 kinst,
                 kindat,
                 cedarObj,
                 madrigalFile):
        """__init__ will write or update a Madrigal file using data in hdf5File

        Inputs:

            hdf5File - full path to hdf5 file with ISR data in SRI format

            kinst - instrument code (integer)

            kindat - data file kindat (integer)

            cedarObj - existing madrigal.cedar.MadrigalCedarFile to append data to.
                       If None, new madrigal.cedar.MadrigalCedarFile
                       created using madrigalFile.

            madrigalFile - name of Madrigal file to create or append to.

        Sets attributes self.numRecs, self.numTimes, self.numBeams
        """
        
        # hard-coded indices defined by the format of the hdf file
        o_index = 0
        e_index = -1
        fractIndex = 0
        tempIndex = 1
        colIndex = 2
        velIndex = 3

        # parameter boundaries
        minTemp = 100.0 # MJN changed 06/05/2008
        maxTemp = 10000.0
        maxTempErr = 32765.0 # limit on Cedar format
        minTempErr = 1.0 # limit on Cedar format
        minNe = 1.0E9
        maxNe = 1.0E13
        maxNeErr = 3.2E13
        minNeErr = 1.0
        maxVo = 32765.0 # limit on Cedar format
        maxVoErr = 32765.0 # limit on Cedar format
        minVoErr = 0.01
        maxFract = 1.0
        minFract = 0.0
        
        # create cedarObj if needed
        if cedarObj == None:
            cedarObj = madrigal.cedar.MadrigalCedarFile(madrigalFile, True)

        # read in all required data
        hdfObj = tables.openFile(hdf5File)

        # beam codes
        beamCodes = hdfObj.root.BeamCodes
        beamCodeArray = beamCodes.read()
        self.numBeams = beamCodeArray.shape[0]
        
        # geomag
        plat = hdfObj.root.Geomag.MagneticLatitude
        platArray = plat.read()
        plong = hdfObj.root.Geomag.MagneticLongitude
        plongArray = plong.read()

        # ranges
        ranges = hdfObj.root.FittedParams.Range
        rangeArray = ranges.read()
        numRanges = rangeArray.shape[1]
        RangeTime=0
        if rangeArray.ndim==3:
            RangeTime=1
            
        # electron density (ne)
        ne = hdfObj.root.FittedParams.Ne
        neArray = ne.read()
        self.numTimes = neArray.shape[0]

        # error in electron density
        dne = hdfObj.root.FittedParams.dNe
        dneArray = dne.read()

        # ion info
        fits = hdfObj.root.FittedParams.Fits
        fitsArray = fits.read()

        # ion error info
        errors = hdfObj.root.FittedParams.Errors
        errArray = errors.read()

        # time info
        days = hdfObj.root.Time.Day
        dayArray = days.read()
        months = hdfObj.root.Time.Month
        monthArray = months.read()
        years = hdfObj.root.Time.Year
        yearArray = years.read()
        dtimes = hdfObj.root.Time.dtime
        dtimeArray = dtimes.read()

        # number of tx, rx
        numTxAeu = hdfObj.root.ProcessingParams.AeuTx
        numTxAeuArray = numTxAeu.read()
        numRxAeu = hdfObj.root.ProcessingParams.AeuRx
        numRxAeuArray = numRxAeu.read()

        # power info
        txPower = hdfObj.root.ProcessingParams.TxPower
        txPowerArray = txPower.read()

        # baud length
        baudLength = hdfObj.root.ProcessingParams.BaudLength.read()

        # pulse length
        pulseLength = hdfObj.root.ProcessingParams.PulseLength.read()

        baudCount = int(pulseLength/baudLength)
        if baudCount <= 0:
            baudCount = 1

        # tx freq
        txFreq = hdfObj.root.ProcessingParams.TxFrequency.read()

        # rx freq
        rxFreq = hdfObj.root.ProcessingParams.RxFrequency.read()


        # create all data records 
        # loop first through num records, then through num beams
        for recIndex in range(self.numTimes):
            # get start and end times for this record
            startYear = int(yearArray[recIndex][0])
            endYear = int(yearArray[recIndex][1])
            startMonth = int(monthArray[recIndex][0])
            endMonth = int(monthArray[recIndex][1])
            startDay = int(dayArray[recIndex][0])
            endDay = int(dayArray[recIndex][1])
            startDtime = dtimeArray[recIndex][0]
            endDtime = dtimeArray[recIndex][1]
            startHour = int(startDtime)
            endHour = int(endDtime)
            startMin = int(startDtime*60.0 - startHour*60.0)
            endMin = int(endDtime*60.0 - endHour*60.0)
            startSec = int(startDtime*3600.0) % 60
            endSec = int(endDtime*3600.0) % 60
            startTime = datetime.datetime(startYear, startMonth, startDay, startHour, startMin, startSec)
            endTime = datetime.datetime(endYear, endMonth, endDay, endHour, endMin, endSec)

            for beamIndex in range(self.numBeams):
                beamId = beamCodeArray[beamIndex][0]
                az = beamCodeArray[beamIndex][1]
                el = beamCodeArray[beamIndex][2]

                dataRec = madrigal.cedar.MadrigalDataRecord(kinst,
                                                            kindat,
                                                            startTime.year,
                                                            startTime.month,
                                                            startTime.day,
                                                            startTime.hour,
                                                            startTime.minute,
                                                            startTime.second,
                                                            startTime.microsecond/10000,
                                                            endTime.year,
                                                            endTime.month,
                                                            endTime.day,
                                                            endTime.hour,
                                                            endTime.minute,
                                                            endTime.second,
                                                            endTime.microsecond/10000,
                                                            ('azm', 'elm', 'beamid', 'power', 'numtxaeu', 'numrxaeu', 'cbadl', 'pl', 'tfreq', 'rfreq'),
                                                            ('range', 'nel', 'dnel', 'ti', 'dti', 'te', 'dte', 'vo', 'dvo', 'cgm_lat', 'cgm_long', 'po+', 'dpo+'),
                                                            numRanges)

                # set 1d values
                dataRec.set1D('azm', az)
                dataRec.set1D('elm', el)
                dataRec.set1D('beamid', beamId)
                dataRec.set1D('power', txPowerArray[recIndex]/1000.0) # cedar in kWatts, SRI in Watts
                dataRec.set1D('numtxaeu', numTxAeuArray[recIndex])
                dataRec.set1D('numrxaeu', numRxAeuArray[recIndex])
                dataRec.set1D('cbadl', baudCount)
                dataRec.set1D('pl', pulseLength)
                dataRec.set1D('tfreq', txFreq)
                dataRec.set1D('rfreq', rxFreq)

                # set 2d values
                for rangeIndex in range(numRanges):
                    
                    # range
                    try:
                        if RangeTime:
                            if isNan(rangeArray[recIndex][0][rangeIndex]):
                                raise ValueError, ''
                            dataRec.set2D('range', rangeIndex, rangeArray[recIndex][0][rangeIndex]/1000.0) # convert m -> km
                            boogaboogabooga                            
                        else:
                            if isNan(rangeArray[0][rangeIndex]):
                                raise ValueError, ''
                            dataRec.set2D('range', rangeIndex, rangeArray[beamIndex][rangeIndex]/1000.0) # convert m -> km
                    
                    except:
                        dataRec.set2D('range', rangeIndex, 'missing')
                        print 'rangeIndex %d missing' % rangeIndex

                    # ne
                    try:
                        if neArray[recIndex][beamIndex][rangeIndex] < minNe or \
                           neArray[recIndex][beamIndex][rangeIndex] > maxNe or \
                           isNan(neArray[recIndex][beamIndex][rangeIndex]):
                            raise ValueError, ''
                        dataRec.set2D('nel', rangeIndex, math.log10(neArray[recIndex][beamIndex][rangeIndex]))

                        if dneArray[recIndex][beamIndex][rangeIndex] <= minNeErr or \
                            dneArray[recIndex][beamIndex][rangeIndex] > maxNeErr or \
                            isNan(dneArray[recIndex][beamIndex][rangeIndex]):
                            raise ValueError, ''
                        dataRec.set2D('dnel', rangeIndex, math.log10(dneArray[recIndex][beamIndex][rangeIndex]))
                        
                    except:
                        dataRec.set2D('nel', rangeIndex, 'missing')
                        dataRec.set2D('dnel', rangeIndex, 'missing')

                    # ti
                    try:
                        if fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] < minTemp or \
                           fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] > maxTemp or \
                           isNan(fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex]):
                            raise ValueError, ''
                        dataRec.set2D('ti', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex]
                                                                 [o_index][tempIndex])

                        if errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] < minTempErr or \
                           errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] > maxTempErr or \
                           isNan(errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex]):
                            raise ValueError, ''
                        dataRec.set2D('dti', rangeIndex, errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex])
                        
                    except:
                        dataRec.set2D('ti', rangeIndex, 'missing')
                        dataRec.set2D('dti', rangeIndex, 'missing')

                    # te
                    try:
                        if fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] < minTemp or \
                           fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] > maxTemp or \
                           isNan(fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex]):
                            raise ValueError, ''
                        dataRec.set2D('te', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex])
                        
                        if errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] < minTempErr or \
                           errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] > maxTempErr or \
                           isNan(errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex]):
                            raise ValueError, ''
                        dataRec.set2D('dte', rangeIndex, errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex])
                        
                    except:
                        dataRec.set2D('te', rangeIndex, 'missing')
                        dataRec.set2D('dte', rangeIndex, 'missing')

                    # vo
                    try:
                        if isNan(fitsArray[recIndex][beamIndex][rangeIndex][o_index][velIndex]) or \
                           abs(fitsArray[recIndex][beamIndex][rangeIndex][o_index][velIndex]) > maxVo or \
                           isNan(errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex]) or \
                           errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex] > maxVoErr or \
                           errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex] < minVoErr:
                            raise ValueError, ''
                        dataRec.set2D('vo', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex]
                                                                 [o_index][velIndex])
                        dataRec.set2D('dvo', rangeIndex, errArray[recIndex][beamIndex][rangeIndex]
                                                                 [o_index][velIndex])
                        
                    except:
                        dataRec.set2D('vo', rangeIndex, 'missing')
                        dataRec.set2D('dvo', rangeIndex, 'missing')

                    # po+
                    try:
                        if isNan(fitsArray[recIndex][beamIndex][rangeIndex][o_index][fractIndex]) or fitsArray[recIndex][beamIndex][rangeIndex][o_index][fractIndex] > maxFract or fitsArray[recIndex][beamIndex][rangeIndex][o_index][fractIndex] < minFract:
                            raise ValueError, ''
                        dataRec.set2D('po+', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex][o_index][fractIndex])
                        dataRec.set2D('dpo+', rangeIndex, 'assumed')                        
                    except:
                        dataRec.set2D('po+', rangeIndex, 'missing')
                        dataRec.set2D('dpo+', rangeIndex, 'missing')
                        
                    # cgm_lat
                    try:
                        if isNan(platArray[beamIndex][rangeIndex]):
                            raise ValueError, ''
                        dataRec.set2D('cgm_lat', rangeIndex,
                            platArray[beamIndex][rangeIndex]) 
                    
                    except:
                        dataRec.set2D('cgm_lat', rangeIndex, 'missing')
                        
                    # cgm_long
                    try:
                        if isNan(plongArray[beamIndex][rangeIndex]):
                            raise ValueError, ''
                        dataRec.set2D('cgm_long', rangeIndex,
                            plongArray[beamIndex][rangeIndex]) 
                    
                    except:
                        dataRec.set2D('cgm_long', rangeIndex, 'missing')


                # append new data record
                cedarObj.append(dataRec)

            # dump records every 100
            if recIndex % 100 == 0: 
                cedarObj.dump('UnblockedBinary')


        # dump remaining records
        cedarObj.dump('UnblockedBinary')

        hdfObj.close()

        self.numRecs = self.numTimes * self.numBeams



class hdf5VelocityToMadrigal:
    """hdf5VelocityToMadrigal is a class to turn a SRI-formated hdf5 file with vector velocities
    into a Madrigal file
    """

    def __init__(self,
                 hdf5File,
                 kinst,
                 kindat,
                 cedarObj,
                 madrigalFile):
        """__init__ will write or update a Madrigal file using data in hdf5File containing vector velocities

        Inputs:

            hdf5File - full path to hdf5 file with vector velocity data in SRI format

            kinst - instrument code (integer)

            kindat - data file kindat (integer)

            cedarObj - existing madrigal.cedar.MadrigalCedarFile to append data to.
                       If None, new madrigal.cedar.MadrigalCedarFile
                       created using madrigalFile.

            madrigalFile - name of Madrigal file to create or append to.

        Sets attribute self.numRecs
        """
        
        # hard-coded indices defined by the format of the hdf file
        north_index = 0
        east_index = 1
        parallel_index = 2


        maxV = 32765.0 # limit on Cedar format
        maxVErr = 32765.0 # limit on Cedar format
        minVErr = 0.01
        maxE = 32765.0*1e-5 # limit on Cedar format
        maxEErr = 32765.0*1e-5 # limit on Cedar format
        minEErr = 0.01*1e-5
        maxDirErr = 360.0

        # create cedarObj if needed
        if cedarObj == None:
            cedarObj = madrigal.cedar.MadrigalCedarFile(madrigalFile, True)

        # read in all required data
        hdfObj = tables.openFile(hdf5File)
        
        # time info
        unixTimes = hdfObj.root.Time.UnixTime
        timeArray = unixTimes.read()
        self.numRecs = timeArray.shape[0]

        # altitude info
        minAlt = hdfObj.root.ProcessingParams.MinAlt.read()
        maxAlt = hdfObj.root.ProcessingParams.MaxAlt.read()

        # nmeas - number of measurments
        nmeas = hdfObj.root.VectorVels.Nmeas
        nmeasArray = nmeas.read()
        params = ['nsmpta']
        
        try:
            byGeo = hdfObj.root.ProcessingParams.GeographicBinning.read()
        except:
            byGeo = 0

        # integration time
        integrationSecs = hdfObj.root.ProcessingParams.IntegrationTime.read()
        
        # magnetic latitude or latitude bins
        if byGeo:
            platArray = hdfObj.root.VectorVels.Latitude.read()
            params.extend(['gdlat','vi2','dvi2','vi1', 'dvi1', 'vi3', 'dvi3'])
        else:            
            try:
                platArray = hdfObj.root.VectorVels.Plat.read()
            except:
                platArray = hdfObj.root.VectorVels.MagneticLatitude.read()            
            params.extend(['cgm_lat','vipn','dvipn','vipe', 'dvipe', 'vi6', 'dvi6'])
        numPlat = platArray.shape[0]

        # velocity vectors and errors
        try:
            vestArray = hdfObj.root.VectorVels.Vest.read()
            dvestArray = hdfObj.root.VectorVels.dVest.read()
        except:
            vestArray = None
            dvestArray = None            
            
        # velocity magnitude, dir and errors
        try:
            vmagArray = hdfObj.root.VectorVels.Vmag.read()
            dvmagArray = hdfObj.root.VectorVels.dVmag.read()
            vdirArray = hdfObj.root.VectorVels.Vdir.read()
            dvdirArray = hdfObj.root.VectorVels.dVdir.read()
            if byGeo:
                params.extend(['magvel','dmagvel','gnangle','dgnangle'])
            else:
                params.extend(['magvel','dmagvel','nangle','dnangle'])            
        except:
            vmagArray = None
            dvmagArray = None
            vdirArray = None
            dvdirArray = None
                        
        # electric field vectors and errors
        try:
            eestArray = hdfObj.root.VectorVels.Eest.read()
            deestArray = hdfObj.root.VectorVels.dEest.read()
            if byGeo:
                params.extend(['en','den','ee', 'dee'])
            else:
                params.extend(['epn','depn','epe', 'depe'])
        except:
            eestArray = None
            deestArray = None

        # electric field magnitude, dir and errors
        try:
            emagArray = hdfObj.root.VectorVels.Emag.read()
            demagArray = hdfObj.root.VectorVels.dEmag.read()
            edirArray = hdfObj.root.VectorVels.Edir.read()
            dedirArray = hdfObj.root.VectorVels.dEdir.read()
            if byGeo:
                params.extend(['magef','dmagef','geangle','dgeangle'])
            else:
                params.extend(['magef','dmagef','eangle','deangle'])  
        except:
            emagArray = None
            demagArray = None
            edirArray = None
            dedirArray = None
                    
        # create all data records 
        for recIndex in range(self.numRecs):
            # get start and end times for this record
            startTime = datetime.datetime.utcfromtimestamp(timeArray[recIndex][0])
            endTime = datetime.datetime.utcfromtimestamp(timeArray[recIndex][1])

            dataRec = madrigal.cedar.MadrigalDataRecord(kinst,
                                                        kindat,
                                                        startTime.year,
                                                        startTime.month,
                                                        startTime.day,
                                                        startTime.hour,
                                                        startTime.minute,
                                                        startTime.second,
                                                        startTime.microsecond/10000,
                                                        endTime.year,
                                                        endTime.month,
                                                        endTime.day,
                                                        endTime.hour,
                                                        endTime.minute,
                                                        endTime.second,
                                                        endTime.microsecond/10000,
                                                        ('altb', 'alte', 'inttms'),
                                                        params,
                                                        numPlat)

            # set 1d values
            dataRec.set1D('altb', minAlt/1000.0) # m -> km
            dataRec.set1D('alte', maxAlt/1000.0) # m -> km
            dataRec.set1D('inttms', integrationSecs)

            # set 2d values
            for platIndex in range(numPlat):

                for parmIndex in range(len(params)):

                    # number of samples
                    if params[parmIndex] in ('nsmpta'): 
                        try:
                            if isNan(nmeasArray[recIndex][platIndex]):
                                raise ValueError, ''
                            dataRec.set2D('nsmpta', platIndex,nmeasArray[recIndex][platIndex])                
                        except:
                            dataRec.set2D('nsmpta', platIndex, 'missing')
                    # lat / plat
                    elif params[parmIndex] in ('gdlat','cgm_lat'):  
                        try:
                            if isNan(platArray[platIndex][0]) or isNan(platArray[platIndex][1]):
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex,(platArray[platIndex][0]+platArray[platIndex][1])/2.0)
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')                    
                    # vipn or vn
                    elif params[parmIndex] in ('vipn','vi2'):
                        try: 
                            if isNan(vestArray[recIndex][platIndex][north_index]) \
                                or isNan(dvestArray[recIndex][platIndex][north_index]) \
                                or scipy.absolute(dvestArray[recIndex][platIndex][north_index])<minVErr \
                                or scipy.absolute(dvestArray[recIndex][platIndex][north_index])>maxVErr \
                                or scipy.absolute(vestArray[recIndex][platIndex][north_index])>maxV:
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, vestArray[recIndex][platIndex][north_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # dvipn or dvn
                    elif params[parmIndex] in ('dvipn','dvi2'):
                        try:
                            if isNan(vestArray[recIndex][platIndex][north_index]) \
                                or isNan(dvestArray[recIndex][platIndex][north_index]) \
                                or scipy.absolute(dvestArray[recIndex][platIndex][north_index])<minVErr \
                                or scipy.absolute(dvestArray[recIndex][platIndex][north_index])>maxVErr \
                                or scipy.absolute(vestArray[recIndex][platIndex][north_index])>maxV:
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, dvestArray[recIndex][platIndex][north_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # vipe or ve
                    elif params[parmIndex] in ('vipe','vi1'):                    
                        try:
                            if isNan(vestArray[recIndex][platIndex][east_index]) \
                                or isNan(dvestArray[recIndex][platIndex][east_index]) \
                                or scipy.absolute(dvestArray[recIndex][platIndex][east_index])<minVErr \
                                or scipy.absolute(dvestArray[recIndex][platIndex][east_index])>maxVErr \
                                or scipy.absolute(vestArray[recIndex][platIndex][east_index])>maxV:                        
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, vestArray[recIndex][platIndex][east_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # dvipe or dve
                    elif params[parmIndex] in ('dvipe','dvi1'):                    
                        try:
                            if isNan(vestArray[recIndex][platIndex][east_index]) \
                                or isNan(dvestArray[recIndex][platIndex][east_index]) \
                                or scipy.absolute(dvestArray[recIndex][platIndex][east_index])<minVErr \
                                or scipy.absolute(dvestArray[recIndex][platIndex][east_index])>maxVErr \
                                or scipy.absolute(vestArray[recIndex][platIndex][east_index])>maxV:                        
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, dvestArray[recIndex][platIndex][east_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')                
                    # vi6 or vi3
                    elif params[parmIndex] in ('vi6','vi3'):                
                        try:
                            if isNan(vestArray[recIndex][platIndex][parallel_index]) \
                                or isNan(dvestArray[recIndex][platIndex][parallel_index]) \
                                or scipy.absolute(dvestArray[recIndex][platIndex][parallel_index])<minVErr \
                                or scipy.absolute(dvestArray[recIndex][platIndex][parallel_index])>maxVErr \
                                or scipy.absolute(vestArray[recIndex][platIndex][parallel_index])>maxV:                                                
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, vestArray[recIndex][platIndex][parallel_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # dvi6 or dvi3
                    elif params[parmIndex] in ('dvi6','dvi3'):     
                        try:
                            if isNan(vestArray[recIndex][platIndex][parallel_index]) \
                                or isNan(dvestArray[recIndex][platIndex][parallel_index]) \
                                or scipy.absolute(dvestArray[recIndex][platIndex][parallel_index])<minVErr \
                                or scipy.absolute(dvestArray[recIndex][platIndex][parallel_index])>maxVErr \
                                or scipy.absolute(vestArray[recIndex][platIndex][parallel_index])>maxV:                                                
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, dvestArray[recIndex][platIndex][parallel_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # epn or en
                    elif params[parmIndex] in ('epn','en'):
                        try: 
                            if isNan(eestArray[recIndex][platIndex][north_index]) \
                                or isNan(deestArray[recIndex][platIndex][north_index]) \
                                or scipy.absolute(deestArray[recIndex][platIndex][north_index])<minEErr \
                                or scipy.absolute(deestArray[recIndex][platIndex][north_index])>maxEErr \
                                or scipy.absolute(eestArray[recIndex][platIndex][north_index])>maxE: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, eestArray[recIndex][platIndex][north_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # depn or den
                    elif params[parmIndex] in ('depn','den'):
                        try:
                            if isNan(eestArray[recIndex][platIndex][north_index]) \
                                or isNan(deestArray[recIndex][platIndex][north_index]) \
                                or scipy.absolute(deestArray[recIndex][platIndex][north_index])<minEErr \
                                or scipy.absolute(deestArray[recIndex][platIndex][north_index])>maxEErr \
                                or scipy.absolute(eestArray[recIndex][platIndex][north_index])>maxE: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, deestArray[recIndex][platIndex][north_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # epe or ee
                    elif params[parmIndex] in ('epe','ee'):                    
                        try:
                            if isNan(eestArray[recIndex][platIndex][east_index]) \
                                or isNan(deestArray[recIndex][platIndex][east_index]) \
                                or scipy.absolute(deestArray[recIndex][platIndex][east_index])<minEErr \
                                or scipy.absolute(deestArray[recIndex][platIndex][east_index])>maxEErr \
                                or scipy.absolute(eestArray[recIndex][platIndex][east_index])>maxE:                         
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, eestArray[recIndex][platIndex][east_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # depe or dee
                    elif params[parmIndex] in ('depe','dee'):                    
                        try:
                            if isNan(eestArray[recIndex][platIndex][east_index]) \
                                or isNan(deestArray[recIndex][platIndex][east_index]) \
                                or scipy.absolute(deestArray[recIndex][platIndex][east_index])<minEErr \
                                or scipy.absolute(deestArray[recIndex][platIndex][east_index])>maxEErr \
                                or scipy.absolute(eestArray[recIndex][platIndex][east_index])>maxE: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, deestArray[recIndex][platIndex][east_index])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')                
                    # magvel
                    elif params[parmIndex] in ('magvel'):
                        try: 
                            if isNan(vmagArray[recIndex][platIndex]) \
                                or isNan(dvmagArray[recIndex][platIndex]) \
                                or scipy.absolute(dvmagArray[recIndex][platIndex])<minVErr \
                                or scipy.absolute(dvmagArray[recIndex][platIndex])>maxVErr \
                                or scipy.absolute(vmagArray[recIndex][platIndex])>maxV: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, vmagArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # dmagvel
                    elif params[parmIndex] in ('dmagvel'):
                        try: 
                            if isNan(vmagArray[recIndex][platIndex]) \
                                or isNan(dvmagArray[recIndex][platIndex]) \
                                or scipy.absolute(dvmagArray[recIndex][platIndex])<minVErr \
                                or scipy.absolute(dvmagArray[recIndex][platIndex])>maxVErr \
                                or scipy.absolute(vmagArray[recIndex][platIndex])>maxV:                         
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, dvmagArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')                            
                    # magef
                    elif params[parmIndex] in ('magef'):
                        try: 
                            if isNan(emagArray[recIndex][platIndex]) \
                                or isNan(demagArray[recIndex][platIndex]) \
                                or scipy.absolute(demagArray[recIndex][platIndex])<minEErr \
                                or scipy.absolute(demagArray[recIndex][platIndex])>maxEErr \
                                or scipy.absolute(emagArray[recIndex][platIndex])>maxE: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, emagArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    # dmagef
                    elif params[parmIndex] in ('dmagef'):
                        try: 
                            if isNan(emagArray[recIndex][platIndex]) \
                                or isNan(demagArray[recIndex][platIndex]) \
                                or scipy.absolute(demagArray[recIndex][platIndex])<minEErr \
                                or scipy.absolute(demagArray[recIndex][platIndex])>maxEErr \
                                or scipy.absolute(emagArray[recIndex][platIndex])>maxE: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, demagArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')                                                        
                   # nangle or gnangle
                    elif params[parmIndex] in ('nangle','gnangle'):
                        try: 
                            if isNan(vdirArray[recIndex][platIndex]) or isNan(dvdirArray[recIndex][platIndex]) or dvdirArray[recIndex][platIndex]>maxDirErr or dvdirArray[recIndex][platIndex]<1e-2: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, vdirArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                   # dnangle or dgnangle
                    elif params[parmIndex] in ('dnangle','dgnangle'):
                        try: 
                            if isNan(vdirArray[recIndex][platIndex]) or isNan(dvdirArray[recIndex][platIndex]) or dvdirArray[recIndex][platIndex]>maxDirErr or dvdirArray[recIndex][platIndex]<1e-2: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, dvdirArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                   # eangle or geangle
                    elif params[parmIndex] in ('eangle','geangle'):
                        try: 
                            if isNan(edirArray[recIndex][platIndex]) or isNan(dedirArray[recIndex][platIndex]) or dedirArray[recIndex][platIndex]>maxDirErr or dedirArray[recIndex][platIndex]<1e-2: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, edirArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')                            
                   # deangle or dgeangle
                    elif params[parmIndex] in ('deangle','dgeangle'):
                        try: 
                            if isNan(edirArray[recIndex][platIndex]) or isNan(dedirArray[recIndex][platIndex]) or dedirArray[recIndex][platIndex]>maxDirErr or dedirArray[recIndex][platIndex]<1e-2: 
                                raise ValueError, ''
                            dataRec.set2D(params[parmIndex], platIndex, dedirArray[recIndex][platIndex])
                        except:
                            dataRec.set2D(params[parmIndex], platIndex, 'missing')
                    else:
                        raise ValueError, 'Unhandled parameter %s' % (params[parmIndex])
                            
            # append new data record
            cedarObj.append(dataRec)

            # dump records every 100
            if recIndex % 100 == 0: 
                cedarObj.dump('UnblockedBinary')

        # dump remaining records
        cedarObj.dump('UnblockedBinary')

        hdfObj.close()




class hdf5UncorrectedToMadrigal:
    """hdf5ToMadrigal is a class to turn a SRI-formated hdf5 file with uncorrected electron
    density into a Madrigal file
    """

    def __init__(self,
                 hdf5File,
                 kinst,
                 kindat,
                 cedarObj,
                 madrigalFile,
                 lowerRange=None,
                 upperRange=None):
        """__init__ will write or update a Madrigal file with uncorrected electron
        density using data in hdf5File

        Inputs:

            hdf5File - full path to hdf5 file with ISR data in SRI format

            kinst - instrument code (integer)

            kindat - data file kindat (integer)

            cedarObj - existing madrigal.cedar.MadrigalCedarFile to append data to.
                       If None, new madrigal.cedar.MadrigalCedarFile
                       created using madrigalFile.

            madrigalFile - name of Madrigal file to create or append to.

            lowerRange - lower range cutoff.  If None (the default), no lower range cutoff.

            upperRange - upper range cutoff.  If None (the default), no upper range cutoff.

        Sets attributes self.numRecs, self.numTimes, self.numBeams

        Raises IOError if no ranges fit in range limits
        """
        
        # hard-coded indices defined by the format of the hdf file
        o_index = 0
        e_index = 2
        fractIndex = 0
        tempIndex = 1
        colIndex = 2
        velIndex = 3

        # create cedarObj if needed
        if cedarObj == None:
            cedarObj = madrigal.cedar.MadrigalCedarFile(madrigalFile, True)

        # read in all required data
        hdfObj = tables.openFile(hdf5File)

        # beam codes
        beamCodes = hdfObj.root.BeamCodes
        beamCodeArray = beamCodes.read()
        self.numBeams = beamCodeArray.shape[0]

        # ranges (in meters)
        ranges = hdfObj.root.NeFromPower.Range
        rangeArray = ranges.read()
        print rangeArray.shape
        if rangeArray.ndim==2:
            RangeTime=0
            numRanges = rangeArray.shape[1]
            print ranges.shape
            lowerRangeIndex = None
            if lowerRange == None:
                lowerRangeIndex = 0
            upperRangeIndex = None
            if upperRange == None:
                upperRangeIndex = numRanges
            for i in range(numRanges):
                if lowerRangeIndex == None and rangeArray[0][i]/1000.0 >=  lowerRange:
                    lowerRangeIndex = i
                if upperRangeIndex == None and rangeArray[0][-1 - i]/1000.0 <  upperRange:
                    upperRangeIndex = numRanges - i
            if lowerRangeIndex == None:
                # no ranges accepted
                raise IOError, 'No valid ranges found between limits %s and %s' % (str(lowerRange), str(upperRange))
            if upperRangeIndex == None:
                upperRangeIndex = numRanges
            
            numUsedRanges = upperRangeIndex-lowerRangeIndex
            if numUsedRanges <= 0:
                # no ranges accepted
                raise IOError, 'No valid ranges found between limits %s and %s' % (str(lowerRange), str(upperRange))
        else:
            RangeTime=1


        # uncorrected electron density (pop)
        pop = hdfObj.root.NeFromPower.Ne_NoTr
        popArray = pop.read()
        self.numTimes = popArray.shape[0]

        # error in electron density
        dpop = hdfObj.root.NeFromPower.dNeFrac
        dpopArray = dpop.read()

        # time info
        days = hdfObj.root.Time.Day
        dayArray = days.read()
        months = hdfObj.root.Time.Month
        monthArray = months.read()
        years = hdfObj.root.Time.Year
        yearArray = years.read()
        dtimes = hdfObj.root.Time.dtime
        dtimeArray = dtimes.read()

        # number of tx, rx
        numTxAeu = hdfObj.root.ProcessingParams.AeuTx
        numTxAeuArray = numTxAeu.read()
        numRxAeu = hdfObj.root.ProcessingParams.AeuRx
        numRxAeuArray = numRxAeu.read()

        # power info
        txPower = hdfObj.root.ProcessingParams.TxPower
        txPowerArray = txPower.read()

        # baud length
        baudLength = hdfObj.root.ProcessingParams.BaudLength.read()

        # pulse length
        pulseLength = hdfObj.root.ProcessingParams.PulseLength.read()

        baudCount = int(pulseLength/baudLength)
        if baudCount <= 0:
            baudCount = 1

        # tx freq
        txFreq = hdfObj.root.ProcessingParams.TxFrequency.read()

        # rx freq
        rxFreq = hdfObj.root.ProcessingParams.RxFrequency.read()

        # create all data records 
        # loop first through num records, then through num beams
        for recIndex in range(self.numTimes):
            if RangeTime:
                numRanges = rangeArray.shape[2]
                lowerRangeIndex = None
                if lowerRange == None:
                    lowerRangeIndex = 0
                upperRangeIndex = None
                if upperRange == None:
                    upperRangeIndex = numRanges
                for i in range(numRanges):
                    if lowerRangeIndex == None and rangeArray[recIndex][0][i]/1000.0 >=  lowerRange:
                        lowerRangeIndex = i
                    if upperRangeIndex == None and rangeArray[recIndex][0][-1 - i]/1000.0 <  upperRange:
                        upperRangeIndex = numRanges - i
                if lowerRangeIndex == None:
                    # no ranges accepted
                    raise IOError, 'No valid ranges found between limits %s and %s' % (str(lowerRange), str(upperRange))
                if upperRangeIndex == None:
                    upperRangeIndex = numRanges
            
                numUsedRanges = upperRangeIndex-lowerRangeIndex
                if numUsedRanges <= 0:
                    # no ranges accepted
                    raise IOError, 'No valid ranges found between limits %s and %s' % (str(lowerRange), str(upperRange))
                
            # get start and end times for this record
            startYear = int(yearArray[recIndex][0])
            endYear = int(yearArray[recIndex][1])
            startMonth = int(monthArray[recIndex][0])
            endMonth = int(monthArray[recIndex][1])
            startDay = int(dayArray[recIndex][0])
            endDay = int(dayArray[recIndex][1])
            startDtime = dtimeArray[recIndex][0]
            endDtime = dtimeArray[recIndex][1]
            startHour = int(startDtime)
            endHour = int(endDtime)
            startMin = int(startDtime*60.0 - startHour*60.0)
            endMin = int(endDtime*60.0 - endHour*60.0)
            startSec = int(startDtime*3600.0) % 60
            endSec = int(endDtime*3600.0) % 60
            startTime = datetime.datetime(startYear, startMonth, startDay, startHour, startMin, startSec)
            endTime = datetime.datetime(endYear, endMonth, endDay, endHour, endMin, endSec)

            for beamIndex in range(self.numBeams):
                beamId = beamCodeArray[beamIndex][0]
                az = beamCodeArray[beamIndex][1]
                el = beamCodeArray[beamIndex][2]

                dataRec = madrigal.cedar.MadrigalDataRecord(kinst,
                                                            kindat,
                                                            startTime.year,
                                                            startTime.month,
                                                            startTime.day,
                                                            startTime.hour,
                                                            startTime.minute,
                                                            startTime.second,
                                                            startTime.microsecond/10000,
                                                            endTime.year,
                                                            endTime.month,
                                                            endTime.day,
                                                            endTime.hour,
                                                            endTime.minute,
                                                            endTime.second,
                                                            endTime.microsecond/10000,
                                                            ('azm', 'elm', 'beamid', 'power',
                                                             'numtxaeu', 'numrxaeu',
                                                             'cbadl', 'pl',
                                                             'tfreq', 'rfreq'),
                                                            ('range', 'popl', 'dpopl'),
                                                            numUsedRanges)

                # set 1d values
                dataRec.set1D('azm', az)
                dataRec.set1D('elm', el)
                dataRec.set1D('beamid', beamId)
                dataRec.set1D('power', txPowerArray[recIndex]/1000.0) # cedar in kWatts, SRI in Watts
                dataRec.set1D('numtxaeu', numTxAeuArray[recIndex])
                dataRec.set1D('numrxaeu', numRxAeuArray[recIndex])
                dataRec.set1D('cbadl', baudCount)
                dataRec.set1D('pl', pulseLength)
                dataRec.set1D('tfreq', txFreq)
                dataRec.set1D('rfreq', rxFreq)

                # set 2d values
                for rangeIndex in range(lowerRangeIndex, upperRangeIndex):
                    
                    # range
                    try:
                        if RangeTime:
                            if isNan(rangeArray[recIndex][0][rangeIndex]):
                                raise ValueError, ''
                            dataRec.set2D('range', rangeIndex-lowerRangeIndex,
                                rangeArray[recIndex][0][rangeIndex]/1000.0) # convert m -> km
                        else:
                            if isNan(rangeArray[0][rangeIndex]):
                                raise ValueError, ''
                            dataRec.set2D('range', rangeIndex-lowerRangeIndex,
                                rangeArray[0][rangeIndex]/1000.0) # convert m -> km
                    
                    except:
                        dataRec.set2D('range', rangeIndex-lowerRangeIndex, 'missing')
                                    

                    # pop
                    try:
                        if isNan(popArray[recIndex][beamIndex][rangeIndex]):
                            raise ValueError, 'popl isNaN'
                        dataRec.set2D('popl', rangeIndex-lowerRangeIndex,
                                      math.log10(popArray[recIndex][beamIndex][rangeIndex]))

                        if dpopArray[recIndex][beamIndex][rangeIndex] <= 0.0 or \
                           isNan(dpopArray[recIndex][beamIndex][rangeIndex]):
                            raise ValueError, 'problem with dpopl'
                        dataRec.set2D('dpopl', rangeIndex-lowerRangeIndex, math.log10(dpopArray[recIndex][beamIndex][rangeIndex] * \
                                      popArray[recIndex][beamIndex][rangeIndex]))
                        
                    except:
                        dataRec.set2D('popl', rangeIndex-lowerRangeIndex, 'missing')
                        dataRec.set2D('dpopl', rangeIndex-lowerRangeIndex, 'missing')


                # append new data record
                cedarObj.append(dataRec)

            # dump records every 100
            if recIndex % 100 == 0: 
                cedarObj.dump('UnblockedBinary')


        # dump remaining records
        cedarObj.dump('UnblockedBinary')

        hdfObj.close()

        self.numRecs = self.numTimes * self.numBeams


class hdf5Handler:
    """hdf5Handler is a class calls other classes depending on the hdf5 type.  Presetly supports the following
    types: standard, velocity
    """
    
    def __init__(self, hdf5Type):
        """__init__ creates a new hdf5Handler of a given type.

        Inputs: hdf5Type - string representing hdf5 file type to handle.  For now must be either
        standard or velocity
        """
        if hdf5Type.lower() == 'standard':
            self.__type = 'standard'
        elif hdf5Type.lower() == 'velocity':
            self.__type = 'velocity'
        elif hdf5Type.lower() == 'uncorrected_ne_only':
            self.__type = 'uncorrected_ne_only'
        elif hdf5Type.lower() == 'velocityalt':
            self.__type = 'velocityAlt'
        else:
            raise ValueError, 'Unknown hdf5 file type %s' % (str(hdf5Type))
        
        
    def getStartEndTimes(self, hdf5File):
        """getStartEndTimes returns a tuple of (earliest datetime, latest datetime) for a given hdf5 file.

        Calls correct class based on self.__type
        """
        if self.__type == 'standard':
            o = analyzeHdf5(hdf5File)
            return(o.getStartEndTimes())

        elif self.__type == 'velocity' or  self.__type == 'velocityAlt':
            o = analyzeVectorHdf5(hdf5File)
            return(o.getStartEndTimes())

        elif self.__type == 'uncorrected_ne_only':
            o = analyzeUncorrectedHdf5(hdf5File)
            return(o.getStartEndTimes())
       
        raise ValueError, 'Unknown self.__type %s' % (str(self.__type))
        

    def createMadrigalFile(self,
                           hdf5File,
                           kinst,
                           kindat,
                           cedarObj,
                           madrigalFile,
                           lowerRange=None,
                           upperRange=None):
        """__init__ will write or update a Madrigal file using data in hdf5File using class set by self.__type

        Inputs:

            hdf5File - full path to hdf5 file in SRI format

            kinst - instrument code (integer)

            kindat - data file kindat (integer)

            cedarObj - existing madrigal.cedar.MadrigalCedarFile to append data to.
                       If None, new madrigal.cedar.MadrigalCedarFile
                       created using madrigalFile.

            madrigalFile - name of Madrigal file to create or append to.

            lowerRange - lower range cutoff.  If None (the default), no lower range cutoff.  Only effects
                uncorrected_ne_only.

            upperRange - upper range cutoff.  If None (the default), no upper range cutoff.  Only effects
                uncorrected_ne_only.
                
        """
        if self.__type == 'standard':
            o = hdf5ToMadrigal(hdf5File,
                               kinst,
                               kindat,
                               cedarObj,
                               madrigalFile)
            self.numRecs = o.numRecs
            return

        elif self.__type == 'velocity':
            o = hdf5VelocityToMadrigal(hdf5File,
                                       kinst,
                                       kindat,
                                       cedarObj,
                                       madrigalFile)
            self.numRecs = o.numRecs
            return

        elif self.__type == 'uncorrected_ne_only':
            o = hdf5UncorrectedToMadrigal(hdf5File,
                                          kinst,
                                          kindat,
                                          cedarObj,
                                          madrigalFile,
                                          lowerRange,
                                          upperRange)
            self.numRecs = o.numRecs
            return

        elif self.__type == 'velocityAlt':
            o = hdf5VelocityAltToMadrigal(hdf5File,
                                       kinst,
                                       kindat,
                                       cedarObj,
                                       madrigalFile)
            self.numRecs = o.numRecs
            return
            
        raise ValueError, 'Unknown self.__type <%s>' % (str(self.__type))