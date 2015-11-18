"""cedar is the module that allows the creation and editting of Cedar (and possible future format) files.

This module abstracts away many of the details of the Cedar file format, which may change in the future,
and instead simply follows the Cedar data model (Prolog-1d parms-2d parms).  For example, all values are accessed and set via doubles, so
users do not need to deal with scale factors, "additional increment" parameters, etc.  The Cedar file format has
limited dynamic range, so for now an exception is raised if any value would voilate this limited range.  Furthermore,
the Cedar data model defines time in the prolog.  Unfortunately, time may also be set in the data itself, leading
to the possibility of inconsistent data.  This module will issue warnings in that case.

This module  is built on top of the Madrigal C API as found in libmadrec.so through the python
extension class __Madrec

$Id: cedar.py,v 1.33 2009/04/16 18:47:25 brideout Exp $
"""

import os, os.path, sys
import array
import types
import time
import datetime
import traceback

import numpy

import madrigal._Madrec
import madrigal.metadata
import madrigal.data
import madrigal.admin

# cedar special values
missing  = 1.e-38
assumed  = 2.e-38
knownbad = 3.e-38
timeParms = 30 # any Cedar parameter below this number is a time parameter that conflicts with prolog

def setToMissing(x):
        """ a private method to generate default data"""
        return missing
    

def isNan(num):
    """isNan is my attempt to detect IEEE special values such as nan.  Returns True if not a float with a
    real value.  Works with both python 2.3 and python 2.4

    Algorithm:  if both (num < 10.0) and (num > -10.0) are False, return true.
    """
    if ((num < 10.0) == False and (num > -10.0) == False):
        return True

    else:
        return False

def floatEquals(first, second):
    """floatEquals is a method to replace == for comparing two floats.  Two floats are equal if
    they are equal to one part in 10^5
    """
    lower = second * (0.99999)
    upper = second * (1.00001)
    if first > 0:
        return (first > lower and first < upper)
    else:
        return (first < lower and first > upper)

def floatIn(testFloat, floatList):
    """floatIn returns True if thisFloat in floatList, where equals defined by floatEquals
    """
    for thisFloat in floatList:
        if floatEquals(testFloat, thisFloat):
            return(True)

    return(False)
    

class MadrigalCedarFile:
    """MadrigalCedarFile is an object that allows the creation and editting of Cedar files.

    This class emulates a python list, and so users may treat it just like a python list.  The
    restriction enforced is that all items in the list must be either MadrigalCatalogRecords,
    MadrigalHeaderRecords, or MadrigalDataRecords (all also defined in the madrigal.cedar module).
    Each of these three classes supports the method getType(), which returns 'catalog', 'header',
    and 'data', respectively.
 

    Usage example::

        # the following example inserts a catalog record at the beginning of an existing file

        import madrigal.cedar.MadrigalCedarFile, time
    
        cedarObj = madrigal.cedar.MadrigalCedarFile('/opt/madrigal/experiments/1998/mlh/20jan98/mil980120g.003')

        startTime = time.mktime((1998,1,20,0,0,0,0,0,0)) - time.timezone

        endTime = time.mktime((1998,1,21,23,59,59,0,0,0)) - time.timezone

        # catLines is a list of 80 character lines to be included in catalog record

        catObj = madrigal.cedar.MadrigalCatalogRecord(31, 1000, 1998,1,20,0,0,0,0,
                                                      1998,1,21,23,59,59,99, catLines)

        cedarObj.insert(0, catObj)

        cedarObj.write()


    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  April. 6, 2005
    """

    def __init__(self, fullFilename,
		 createFlag=False,
		 startDatetime=None,
		 endDatetime=None):
        """__init__ initializes MadrigalCedarFile by reading in existing file, if any.

        Inputs:

            fullFilename - either the existing Cedar file (in any allowed Cedar format),
                           or a file to be created.

            createFlag - tells whether this is a file to be created.  If False and
                         fullFilename cannot be read, an error is raised.  If True and
                         fullFilename already exists, or fullFilename cannot be created,
                         an error is raised.

	    startDatetime - if not None (the default), reject all input records where
	          record end time < startDatetime (datetime.datetime object)

	    endDatetime - if not None (the default), reject all input records where
	          record start time > endDatetime (datetime.datetime object)
	        

        Affects: populates self.__privList if file exists, sets self.__fullFilename
        
        Returns: void
        """
        self.__privList = []
        self.__fullFilename = fullFilename

        if createFlag not in (True, False):
            raise ValueError, 'in MadrigalCedarFile, createFlag must be either True or False'

        if createFlag == False:
            if not os.access(fullFilename, os.R_OK):
                raise ValueError, 'in MadrigalCedarFile, fullFilename %s does not exist' % (str(fullFilename))

        if createFlag == True:
            if os.access(fullFilename, os.R_OK):
                raise ValueError, 'in MadrigalCedarFile, fullFilename %s already exists' % (str(fullFilename))
            if not os.access(os.path.dirname(fullFilename), os.W_OK):
                raise ValueError, 'in MadrigalCedarFile, fullFilename %s cannot be created' % (str(fullFilename))

        if startDatetime != None:
            if not isinstance(startDatetime, datetime.datetime):
                raise ValueError, 'in MadrigalCedarFile, startDatetime %s must be datetime' % (str(startDatetime))
        self.startDatetime = startDatetime

        if endDatetime != None:
            if not isinstance(endDatetime, datetime.datetime):
                raise ValueError, 'in MadrigalCedarFile, endDatetime %s must be datetime' % (str(endDatetime))
        self.endDatetime = endDatetime
	
        if createFlag == False:
            self.__parseFile()


        self.__format = None # used to check that partial writes via dump are consistent


    def write(self, format='Madrigal', newFilename=None):
        """write persists a MadrigalCedarFile to file.

        Inputs:

            format - a format to save the file in.  For now, the allowed values are 'Madrigal', 'BlockedBinary',
            'UnblockedBinary', 'Cbf', and 'Ascii'.  Defaults to Madrigal.

            newFilename - a filename to save to.  Defaults to self.__fullFilename passed into initializer if not given.

        Outputs: None

        Affects: writes a MadrigalCedarFile to file
        """
        if self.__format != None:
            raise ValueError, 'Cannot call write method after calling dump method'
        
        if newFilename == None:
            newFilename = self.__fullFilename
            
        cedarFile = madrigal._Madrec.madrecOpen(newFilename, 'w', format)

        for rec in self.__privList:
            # all three objects define writeRecord
            rec.writeRecord(cedarFile, format)

        madrigal._Madrec.madrecClose(cedarFile)



    def dump(self, format='UnblockedBinary'):
        """dump appends all the present records in MadrigalCedarFile to file, and removes present records from MadrigalCedarFile.

        Can be used to append records to a file.  Only works with file formats UnblockedBinary and Ascii, since other formats
        do not have clear record -> file mapping.

        Inputs:

            format - a format to save the file in.  For now, the allowed values are 'UnblockedBinary' and 'Ascii'.
            Defaults to UnblockedBinary.  If dump was previously called with other format, exception raised.


        Outputs: None

        Affects: writes a MadrigalCedarFile to file
        """
        
        if self.__format != None:
            if self.__format != format:
                raise ValueError, 'Previous dump format was %s, cannot now use %s' % (str(self.__format), str(format))

        if format not in ('UnblockedBinary', 'Ascii'):
            raise ValueError, 'Format must be UnblockedBinary or Ascii for dump, not %s' % (str(format))

        self.__format = format
        
        newFilename = self.__fullFilename
        tmpFilename = self.__fullFilename + '.tmp'
            
        cedarFile = madrigal._Madrec.madrecOpen(tmpFilename, 'w', format)

        for rec in self.__privList:
            # all three objects define writeRecord
            rec.writeRecord(cedarFile, format)

        madrigal._Madrec.madrecClose(cedarFile)

        # dump records out of memory
        self.__privList = []

        # append to existing file
        outFile = open(newFilename, 'ab')
        inFile = open(tmpFilename)
        newData = inFile.read()
        outFile.write(newData)
        outFile.close()
        inFile.close()
        os.remove(tmpFilename)
        
        
    def createCatalogTimeSection(self):
        """createCatalogTimeSection will return all the lines in the catalog record that
        describe the start and end time of the data records.

        Inputs: None

        Returns:  a tuple with three items 1) a string in the format of the time section of a
        catalog record, 2) earliest datetime, 3) latest datetime
        """

        earliestStartTime = None
        latestEndTime = None

        for rec in self.__privList:
            if rec.getType() != 'data':
                continue

            #earliest time
            year,month,day,hour,minute,second,centisecond = rec.getStartTimeList()
            thisTime = datetime.datetime(year,month,day,
                                         hour,minute,second,centisecond*10000)
            if earliestStartTime == None:
                earliestStartTime = thisTime
            if earliestStartTime > thisTime:
                earliestStartTime = thisTime

            #latest time
            year,month,day,hour,minute,second,centisecond = rec.getEndTimeList()
            thisTime = datetime.datetime(year,month,day,
                                         hour,minute,second,centisecond*10000)
            if latestEndTime == None:
                latestEndTime = thisTime
            if latestEndTime < thisTime:
                latestEndTime = thisTime
        

        sy = 'IBYRE       %4s Beginning year' % (str(earliestStartTime.year))
        sd = 'IBDTE       %4s Beginning month and day' % (str(earliestStartTime.month*100 + \
                                                              earliestStartTime.day))
        sh = 'IBHME       %4s Beginning UT hour and minute' % (str(earliestStartTime.hour*100 + \
                                                                   earliestStartTime.minute))
        totalCS = earliestStartTime.second*100 + (earliestStartTime.microsecond/10000)
        ss = 'IBCSE       %4s Beginning centisecond'  % (str(totalCS))
        
        ey = 'IEYRE       %4s Ending year' % (str(latestEndTime.year))
        ed = 'IEDTE       %4s Ending month and day' % (str(latestEndTime.month*100 + \
                                                           latestEndTime.day))
        eh = 'IEHME       %4s Ending UT hour and minute' % (str(latestEndTime.hour*100 + \
                                                                latestEndTime.minute))
        totalCS = latestEndTime.second*100 + (latestEndTime.microsecond/10000)
        es = 'IECSE       %4s Ending centisecond'  % (str(totalCS))

        retStr = ''
        retStr += sy + (80-len(sy))*' '
        retStr += sd + (80-len(sd))*' '
        retStr += sh + (80-len(sh))*' '
        retStr += ss + (80-len(ss))*' '
        retStr += ey + (80-len(ey))*' '
        retStr += ed + (80-len(ed))*' '
        retStr += eh + (80-len(eh))*' '
        retStr += es + (80-len(es))*' '

        return((retStr, earliestStartTime, latestEndTime))


    
    def createHeaderTimeSection(self, dataRecList=None):
        """createHeaderTimeSection will return all the lines in the header record that
        describe the start and end time of the data records.

        Inputs:

            dataRecList - if given, examine only those MadrigalDataRecords in dataRecList.
                          If None (the default), examine all MadrigalDataRecords in this
                          MadrigalCedarFile

        Returns:  a tuple with three items 1) a string in the format of the time section of a
        header record, 2) earliest datetime, 3) latest datetime
        """
        if dataRecList == None:
            dataRecList = self.__privList

        earliestStartTime = None
        latestEndTime = None

        for rec in dataRecList:
            if rec.getType() != 'data':
                continue

            #earliest time
            year,month,day,hour,minute,second,centisecond = rec.getStartTimeList()
            thisTime = datetime.datetime(year,month,day,
                                         hour,minute,second,centisecond*10000)
            if earliestStartTime == None:
                earliestStartTime = thisTime
            if earliestStartTime > thisTime:
                earliestStartTime = thisTime

            #latest time
            year,month,day,hour,minute,second,centisecond = rec.getEndTimeList()
            thisTime = datetime.datetime(year,month,day,
                                         hour,minute,second,centisecond*10000)
            if latestEndTime == None:
                latestEndTime = thisTime
            if latestEndTime < thisTime:
                latestEndTime = thisTime
                

        sy = 'IBYRT               %4s Beginning year' % (str(earliestStartTime.year))
        sd = 'IBDTT               %4s Beginning month and day' % (str(earliestStartTime.month*100 + \
                                                              earliestStartTime.day))
        sh = 'IBHMT               %4s Beginning UT hour and minute' % (str(earliestStartTime.hour*100 + \
                                                                   earliestStartTime.minute))
        totalCS = earliestStartTime.second*100 + (earliestStartTime.microsecond/10000)
        ss = 'IBCST               %4s Beginning centisecond'  % (str(totalCS))
        
        ey = 'IEYRT               %4s Ending year' % (str(latestEndTime.year))
        ed = 'IEDTT               %4s Ending month and day' % (str(latestEndTime.month*100 + \
                                                           latestEndTime.day))
        eh = 'IEHMT               %4s Ending UT hour and minute' % (str(latestEndTime.hour*100 + \
                                                                latestEndTime.minute))
        totalCS = latestEndTime.second*100 + (latestEndTime.microsecond/10000)
        es = 'IECST               %4s Ending centisecond'  % (str(totalCS))

        retStr = ''
        retStr += sy + (80-len(sy))*' '
        retStr += sd + (80-len(sd))*' '
        retStr += sh + (80-len(sh))*' '
        retStr += ss + (80-len(ss))*' '
        retStr += ey + (80-len(ey))*' '
        retStr += ed + (80-len(ed))*' '
        retStr += eh + (80-len(eh))*' '
        retStr += es + (80-len(es))*' '

        return((retStr, earliestStartTime, latestEndTime))
        
        
        

    def __parseFile(self):
        """__parseFile reads an existing Cedar file, and populates self.__privList with MadrigalCatalogRecords,
           MadrigalHeaderRecords, or MadrigalDataRecords.
        """
        
        madInstObj = madrigal.metadata.MadrigalInstrument()
        madParmObj = madrigal.data.MadrigalParameters()
        
        cedarFile = madrigal._Madrec.madrecOpen(self.__fullFilename, 'r', 'Madrigal')
        try:
            while 1:
                cedarType = madrigal._Madrec.madrecGetNextRec(cedarFile)
                if cedarType == 1:
                    # handle catalog
                    results = madrigal._Madrec.madrecDumpCatalogRecord(cedarFile)
                    kinst = results[0]
                    modexp = results[1]
                    startDateList = madrigal._Madrec.getDateFromUt(results[2])
                    endDateList = madrigal._Madrec.getDateFromUt(results[3])

                    # create a MadrigalCatalogRecord
                    madCatRec = MadrigalCatalogRecord(kinst,
                                                      modexp,
                                                      startDateList[0],
                                                      startDateList[1],
                                                      startDateList[2],
                                                      startDateList[3],
                                                      startDateList[4],
                                                      startDateList[5],
                                                      startDateList[6],
                                                      endDateList[0],
                                                      endDateList[1],
                                                      endDateList[2],
                                                      endDateList[3],
                                                      endDateList[4],
                                                      endDateList[5],
                                                      endDateList[6],
                                                      results[4])
                    self.__privList.append(madCatRec)
                    
                elif cedarType == 2:
                    # handle header
                    results = madrigal._Madrec.madrecDumpHeaderRecord(cedarFile)
                    kinst = results[0]
                    kindat = results[1]
                    startDateList = madrigal._Madrec.getDateFromUt(results[2])
                    endDateList = madrigal._Madrec.getDateFromUt(results[3])

                    # create a MadrigalHeaderRecord
                    madHeadRec = MadrigalHeaderRecord(kinst,
                                                      kindat,
                                                      startDateList[0],
                                                      startDateList[1],
                                                      startDateList[2],
                                                      startDateList[3],
                                                      startDateList[4],
                                                      startDateList[5],
                                                      startDateList[6],
                                                      endDateList[0],
                                                      endDateList[1],
                                                      endDateList[2],
                                                      endDateList[3],
                                                      endDateList[4],
                                                      endDateList[5],
                                                      endDateList[6],
                                                      results[4],
                                                      results[5],
                                                      results[6])
                    self.__privList.append(madHeadRec)
                    
                elif cedarType == 0:
                    # handle data
                    results = madrigal._Madrec.madrecDumpDataRecord(cedarFile)
                    kinst = results[0]
                    kindat = results[1]
                    startDateList = madrigal._Madrec.getDateFromUt(results[2])
                    if self.endDatetime != None:
			# check time filter
                        sDT = datetime.datetime(startDateList[0],
						startDateList[1],
						startDateList[2],
						startDateList[3],
						startDateList[4],
						startDateList[5])
                        if sDT > self.endDatetime:
                            continue
                    endDateList = madrigal._Madrec.getDateFromUt(results[3])
                    if self.startDatetime != None:
			# check time filter
                        eDT = datetime.datetime(endDateList[0],
						endDateList[1],
						endDateList[2],
						endDateList[3],
						endDateList[4],
						endDateList[5])
                        if eDT < self.startDatetime:
                            continue
                    oneDParmList = results[4]
                    jpar = len(oneDParmList)
                    twoDParmList = results[5]
                    mpar = len(twoDParmList)
                    nrow = results[6]
                    oneDDataList = results[7]
                    twoDDataList = results[8]
 
                    # create a MadrigalDataRecord
                    madDataRec = MadrigalDataRecord(kinst,
                                                    kindat,
                                                    startDateList[0],
                                                    startDateList[1],
                                                    startDateList[2],
                                                    startDateList[3],
                                                    startDateList[4],
                                                    startDateList[5],
                                                    startDateList[6],
                                                    endDateList[0],
                                                    endDateList[1],
                                                    endDateList[2],
                                                    endDateList[3],
                                                    endDateList[4],
                                                    endDateList[5],
                                                    endDateList[6],
                                                    oneDParmList,
                                                    twoDParmList,
                                                    nrow,
                                                    madInstObj,
                                                    madParmObj)
                    # set 1d data
                    for i in range(len(oneDParmList)):
                        madDataRec.set1D(oneDParmList[i], oneDDataList[i])

                    # set 2d data
                    for i in range(nrow):
                        for j in range(len(twoDParmList)):
                            madDataRec.set2D(twoDParmList[j], i, twoDDataList[j + i*mpar])

                    self.__privList.append(madDataRec)
                    
                else:
                    raise 'Illegal return value'


        except:
            err = sys.exc_info()[1]
            if str(err).find('problem calling madrecGetNextRec - error code -1') == -1:
                traceback.print_exc()
                raise IOError, 'Error parsing Cedar file %s' % (self.__fullFilename)
            madrigal._Madrec.madrecClose(cedarFile)
        

    """ the following methods are added to allow this class to emulate a list."""

    def __len__(self):
        return len(self.__privList)
        
    def __getitem__(self, key):
        return self.__privList[key]

    def __setitem__(self, key, value):
        # check that value in (MadrigalCatalogRecord, MadrigalHeaderRecord, MadrigalDataRecord)
        if not isinstance(value, MadrigalCatalogRecord) and \
           not isinstance(value, MadrigalHeaderRecord) and \
           not isinstance(value, MadrigalDataRecord):
            # check that its not an empty list (used to delete records)
            okay = False
            if type(value) == types.ListType:
                if len(value) == 0:
                    okay = True
            if not okay:
                raise ValueError, 'In MadrigalCedarFile, can only add MadrigalCatalogRecord, MadrigalHeaderRecord, or MadrigalDataRecord'
        self.__privList[key] = value

    def __getslice__(self, i, j):
        return self.__privList[i:j]

    def __setslice__(self,i,j,seq):
        # check every item in seq
        for item in seq:
            if not isinstance(value, MadrigalCatalogRecord) and \
               not isinstance(value, MadrigalHeaderRecord) and \
               not isinstance(value, MadrigalDataRecord):
                raise ValueError, 'In MadrigalCedarFile, can only add MadrigalCatalogRecord, MadrigalHeaderRecord, or MadrigalDataRecord'
        self.__privList[max(0, i):max(0, j):] = seq

    def __delslice__(self, i, j):
        del self.__privList[max(0, i):max(0, j):]
            
            

    def __delitem__(self, key):
        del self.__privList[key]

    def __iter__(self):
        return iter(self.__privList)

    def __contains__(self, other):
        for item in self.__privList:
            if item == other:
                return 1
        # not found
        return 0

    def __str__(self):
        retStr = ''
        for item in self.__privList:
            retStr += '%s\n' % (str(item))
        return retStr

    def append(self, item):
        # check that value in (MadrigalCatalogRecord, MadrigalHeaderRecord, MadrigalDataRecord)
        if not isinstance(item, MadrigalCatalogRecord) and \
           not isinstance(item, MadrigalHeaderRecord) and \
           not isinstance(item, MadrigalDataRecord):
            raise ValueError, 'In MadrigalCedarFile, can only add MadrigalCatalogRecord, MadrigalHeaderRecord, or MadrigalDataRecord'
        self.__privList.append(item)

    def count(self, other):
        return self.__privList.count(other)

    def index(self, other):
        return self.__privList.index(other)

    def insert(self, i, x):
       self.__privList.insert(i, x)

    def pop(self, i):
        return self.__privList.pop(i)
        
    def remove(self, x):
        self.__privList.remove(x)

    def reverse(self):
        self.__privList.reverse()


class MadrigalDataRecord:
    """MadrigalDataRecord holds all the information in a Cedar data record."""

    # cedar special values
    missing  = 1.e-38
    assumed  = 2.e-38
    knownbad = 3.e-38
    
    def __init__(self,kinst,
                 kindat,
                 sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                 eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec,
                 oneDList,
                 twoDList,
                 nrow,
                 madInstObj=None,
                 madParmObj=None):
        """__init__ creates a MadrigalDataRecord with all missing data.

        Inputs:

            kinst - the kind of instrument code.  A warning will be raised if not in instTab.txt.

            kindat - kind of data code. Must be a non-negative integer.

            sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec - record start time. sCentisec must be 0-99

            eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec - record end time. eCentisec must be 0-99

            oneDList - list of one-dimensional parameters in record. Parameters can be defined as codes
                       (integers) or case-insensitive mnemonic strings (eg, "Gdalt")

            twoDList - list of two-dimensional parameters in record. Parameters can be defined as codes
                       (integers) or case-insensitive mnemonic strings (eg, "Gdalt")

            nrow - number of rows of 2D data to create. Until set, all values default to missing.

            madInstObj - a madrigal.metadata.MadrigalInstrument object.  If None, one will be created.
                              Used to verify kinst.

            madParmObj - a madrigal.data.MadrigalParameter object.  If None, one will be created.
                              Used to verify convert parameters to codes.

        Outputs: None

        Returns: None
        """

        # create any needed Madrigal objects, if not passed in
        if madInstObj == None:
            self.__madInstObj = madrigal.metadata.MadrigalInstrument()
        else:
            self.__madInstObj = madInstObj

        if madParmObj == None:
            self.__madParmObj = madrigal.data.MadrigalParameters()
        else:
            self.__madParmObj = madParmObj

        # verify  and set kinst
        instList = self.__madInstObj.getInstrumentList()
        found = False
        for inst in instList:
            if inst[2] == kinst:
                self.__instrumentName = inst[0]
                found = True
                break
        if found == False:
            self.__instrumentName = 'Unknown instrument'
            sys.stderr.write('Warning: kinst %i not found in instTab.txt\n' % (kinst))

        self.__kinst = kinst

        if kindat < 0:
            raise 'kindat cannot be negative: %i' % (kindat)
        self.__kindat = int(kindat)

        # verify times
        sTime = datetime.datetime(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec*10000)
        eTime = datetime.datetime(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec*10000)

        if eTime < sTime:
            sys.stderr.write('Warning: Starting time %s after ending time %s\n' % (str(sTime), str(eTime)))

        self.__sTime = madrigal._Madrec.getUtFromDate(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec)
        self.__eTime = madrigal._Madrec.getUtFromDate(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec)

        self.__sYear = sYear
        self.__sMonth = sMonth
        self.__sDay = sDay
        self.__sHour = sHour
        self.__sMin = sMin
        self.__sSec = sSec
        self.__sCentisec = sCentisec

        
        self.__eYear = eYear
        self.__eMonth = eMonth
        self.__eDay = eDay
        self.__eHour = eHour
        self.__eMin = eMin
        self.__eSec = eSec
        self.__eCentisec = eCentisec

        # one-d parameters - create a list of tuples, where each tuple has five elements:
        # (code, lower-case mnemonic, parameter description, scaleFactor, hasAddIncrement)
        self.__oneDList = []
        for item in oneDList:
            self.add1D(item)

        # check nrow
        if nrow < 0:
            raise ValueError, 'nrow must not be less that zero: = %i' % (nrow)

        self.__nrow = nrow
        
        # two-d parameters - create a list of tuples, where each tuple has five elements:
        # (code, lower-case mnemonic, parameter description, scaleFactor, hasAddIncrement)
        self.__twoDList = []
        for item in twoDList:
            self.add2D(item)
        

        
    def getType(self):
        """ returns the type 'data'"""
        return 'data'
    

    def add1D(self, oneDParm):
        """add1D adds a new one-dim parameter to a MadrigalDataRecord

        Input: oneDParm - Parameter can be defined as codes (integer) or case-insensitive
               mnemonic string (eg, "Gdalt")

        Affects: adds tuple to self.__oneDList, where tuple has five elements:
        (code, lower-case mnemonic, parameter description, scaleFactor, hasAddIncrement)

        Also adds one item to self.__oneDData, with value = missing.  Creates self.__oneDData if
        does not exist.
        """
        # see if its an integer
        isInt = False
        try:
            int(oneDParm)
            isInt = True
        except:
            pass

        if isInt:
            # try to look up everthing else, but set defaults if it fails
            mnem = self.__madParmObj.getParmMnemonic(int(oneDParm)).lower()
            if mnem == str(oneDParm):
                self.__oneDList.append((int(oneDParm), 'Unknown', 'Unknown parameter', 1.0, False))
            else:
                desc = self.__madParmObj.getParmDescription(int(oneDParm))
                # make sure this isn't an "Additional increment" parameter,
                #   which doesn't make sense when using doubles to get and set values
                if desc.lower().find('additional increment') != -1:
                    raise ValueError, 'Illegal use of an "additional increment" Cedar parameter %s, since this module sets values via doubles, not ints' % (mnem)
                scaleFactor = self.__madParmObj.getParmScaleFactor(int(oneDParm))
                # check whether this is a parameter with an associated "Additional increment" parameter,
                #  which will be used when writing data to a Cedar file - is always this code + 1
                hasAddIncrement = False
                try:
                    if int(oneDParm) > 0:
                        nextdesc = self.__madParmObj.getParmDescription(int(oneDParm)+1)
                    else:
                        nextdesc = self.__madParmObj.getParmDescription(int(oneDParm)-1)
                    if nextdesc.lower().find('additional increment') != -1:
                        hasAddIncrement = True
                except:
                    pass
                self.__oneDList.append((int(oneDParm), mnem, desc, scaleFactor, hasAddIncrement))


        else:
            # this must succeed or an exception raised
            code = self.__madParmObj.getParmCodeFromMnemonic(oneDParm.lower())
            mnem = oneDParm.lower()
            desc = self.__madParmObj.getParmDescription(code)
            # make sure this isn't an "Additional increment" parameter,
            #   which doesn't make sense when using doubles to get and set values
            if desc.lower().find('additional increment') != -1:
                raise ValueError, 'Illegal use of an "additional increment" Cedar parameter %s, since this module sets values via doubles, not ints' % (mnem)
            scaleFactor = self.__madParmObj.getParmScaleFactor(code)
            # check whether this is a parameter with an associated "Additional increment" parameter,
            #  which will be used when writing data to a Cedar file - is always this code + 1
            hasAddIncrement = False
            try:
                if code > 0:
                    nextdesc = self.__madParmObj.getParmDescription(code+1)
                else:
                    nextdesc = self.__madParmObj.getParmDescription(code-1)
                if nextdesc.lower().find('additional increment') != -1:
                    hasAddIncrement = True
            except:
                pass
            self.__oneDList.append((code, mnem, desc, scaleFactor, hasAddIncrement))

        # expand self.__oneDData
        try:
            self.__oneDData.append(missing)
        except:
            tempOneDArray = (missing,)
            self.__oneDData = array.array('d', tempOneDArray)

        # issue warning if an unneeded time parameter being added
        parm = self.__oneDList[-1]
        if parm[0] > 0 and parm[0] <= timeParms:
            sys.stderr.write('WARNING: Parameter %s is a time parameter that potentially conflicts with prolog times\n' % (parm[1]))



    def add2D(self, twoDParm):
        """add2D adds a new two-dim parameter to a MadrigalDataRecord

        Input: twoDParm - Parameter can be defined as codes (integer) or case-insensitive
               mnemonic string (eg, "Gdalt")

        Affects: adds tuple to self.__twoDList, where tuple has five elements:
        (code, lower-case mnemonic, parameter description, scaleFactor, hasAddIncrement)


        Also adds one column with self.__nrow rows to self.__twoDData, with value = missing.
        Creates self.__twoDData if does not exist.
        """
        # see if its an integer
        isInt = False
        try:
            int(twoDParm)
            isInt = True
        except:
            pass

        if isInt:
            # try to look up everthing else, but set defaults if it fails
            mnem = self.__madParmObj.getParmMnemonic(int(twoDParm)).lower()
            if mnem == str(twoDParm):
                self.__twoDList.append((int(twoDParm), 'Unknown', 'Unknown parameter', 1.0, False))
            else:
                desc = self.__madParmObj.getParmDescription(int(twoDParm))
                # make sure this isn't an "Additional increment" parameter,
                #   which doesn't make sense when using doubles to get and set values
                if desc.lower().find('additional increment') != -1:
                    raise ValueError, 'Illegal use of an "additional increment" Cedar parameter %s, since this module sets values via doubles, not ints' % (mnem)
                scaleFactor = self.__madParmObj.getParmScaleFactor(int(twoDParm))
                # check whether this is a parameter with an associated "Additional increment" parameter,
                #  which will be used when writing data to a Cedar file - is always this code + 1
                hasAddIncrement = False
                try:
                    if int(twoDParm) > 0:
                        nextdesc = self.__madParmObj.getParmDescription(int(twoDParm)+1)
                    else:
                        nextdesc = self.__madParmObj.getParmDescription(int(twoDParm)-1)
                    if nextdesc.lower().find('additional increment') != -1:
                        hasAddIncrement = True
                except:
                    pass
                self.__twoDList.append((int(twoDParm), mnem, desc, scaleFactor, hasAddIncrement))


        else:
            # this must succeed or an exception raised
            code = self.__madParmObj.getParmCodeFromMnemonic(twoDParm.lower())
            mnem = twoDParm.lower()
            desc = self.__madParmObj.getParmDescription(code)
            # make sure this isn't an "Additional increment" parameter,
            #   which doesn't make sense when using doubles to get and set values
            if desc.lower().find('additional increment') != -1:
                raise ValueError, 'Illegal use of an "additional increment" Cedar parameter %s, since this module sets values via doubles, not ints' % (mnem)
            scaleFactor = self.__madParmObj.getParmScaleFactor(code)
            # check whether this is a parameter with an associated "Additional increment" parameter,
            #  which will be used when writing data to a Cedar file - is always this code + 1
            hasAddIncrement = False
            try:
                if code > 0:
                    nextdesc = self.__madParmObj.getParmDescription(code+1)
                else:
                    nextdesc = self.__madParmObj.getParmDescription(code-1)
                if nextdesc.lower().find('additional increment') != -1:
                    hasAddIncrement = True
            except:
                pass
            self.__twoDList.append((code, mnem, desc, scaleFactor, hasAddIncrement))

        # expand self.__twoDData
        if hasattr(self, '_MadrigalDataRecord__twoDData'):
            newData = numpy.ones((self.__nrow, 1), numpy.float)
            self.__twoDData = numpy.concatenate((self.__twoDData, newData), 1)
        else:
            self.__twoDData = numpy.ones((self.__nrow, 1), numpy.float)

        # set all new values to missing
        self.__twoDData[:,-1] = missing

        # finally, issue warning to stderr if any time parameters used that conflict with prolog timestamps
        parm = self.__twoDList[-1]
        if parm[0] > 0 and parm[0] <= timeParms:
            sys.stderr.write('WARNING: Parameter %s is a time parameter used as a 2D parameter in conflict with prolog times - separate records for each time should be used instead\n' % (parm[1]))



    def set1D(self, parm, value):
        """set1D sets a 1D value for a given 1D parameter

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")


            value - double (or string convertable to double) value to set 1D parameter to.  To set special Cedar values, the global values
                    missing, assumed, or knownbad may be used, or the strings "missing", "assumed", or "knownbad"

        Outputs: None

        Note: if value exceeds the dynamic range of given parameter (ie,
                    if value < -32766*scaleFactor or value > 32767*scaleFactor, then WARNING printed to stderr
                    and value set to missing
        """
        if value == 'missing':
            value = missing


        # find index for parm
        index = None
        isInt = False
        try:
            code = int(parm)
            isInt = True
        except:
            parm = parm.lower()

        for i in range(len(self.__oneDList)):
            item = self.__oneDList[i]
            if isInt:
                if item[0] == code:
                    index = i
                    break
            else:
                if item[1] == parm:
                    index = i
                    break

        if index == None:
            raise ValueError, 'Parameter %s not found as 1D parameter in this data record' % (str(parm))

        # if its an error parameter, allow assumed or knownbad
        if self.__oneDList[index][0] < 0:
            if value == 'assumed':
                value = assumed
            if value == 'knownbad':
                value = knownbad

        value = float(value)

        # check that is not NaN
        if isNan(value):
            raise ValueError, 'Cannot set a Madrigal parameter to %s' % (str(value))

        # verify value
        scale = self.__oneDList[index][3]
        if self.__oneDList[index][0] < 0:
            isError = True
        else:
            isError = False

        if isError:
            if not floatIn(value, (missing, assumed, knownbad)):
                if -32765.0*scale > value or value > 32766.0*scale:
                    sys.stderr.write('WARNING: Value %f exceeds dynamic range of parameter %s - being set to missing\n' % (value, self.__oneDList[index][1]))
                    value = missing
                if value <= 0.0:
                    sys.stderr.write('WARNING: Value %f of error parameter %s must be positive - being set to missing\n' % (value, self.__oneDList[index][1]))
                    value = missing
        else:
            if not floatIn(value, (missing,)):
                if -32766.0*scale > value or value > 32767.0*scale:
                    sys.stderr.write('WARNING: Value %f exceeds dynamic range of parameter %s - being set to missing\n' % (value, self.__oneDList[index][1]))
                    value = missing

        self.__oneDData[index] = value


    def set2D(self, parm, row, value):
        """set2D sets a 2D value for a given 2D parameter and row

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")

            row - row number to set data.  Starts at 0.

            value - double (or string convertable to double) value to set 2D parameter to. To set special Cedar values, the global values
                    missing, assumed, or knownbad may be used, or the strings "missing", "assumed", or "knownbad"

        Outputs: None

        Note: if value exceeds the dynamic range of given parameter (ie,
                    if value < -32766*scaleFactor or value > 32767*scaleFactor, then WARNING printed to stderr
                    and value set to missing
        """
        if value == 'missing':
            value = missing
            
        if row >= self.__nrow or row < 0:
            raise ValueError, 'Illegal value of row %i with nrow = %i' % (row, self.__nrow)

        # find index for parm
        index = None
        isInt = False
        try:
            code = int(parm)
            isInt = True
        except:
            parm = parm.lower()

        for i in range(len(self.__twoDList)):
            item = self.__twoDList[i]
            if isInt:
                if item[0] == code:
                    index = i
                    break
            else:
                if item[1] == parm:
                    index = i
                    break

        if index == None:
            raise ValueError, 'Parameter %s not found as 2D parameter in this data record' % (str(parm))

        # if its an error parameter, allow assumed or knownbad
        if self.__twoDList[index][0] < 0:
            if value == 'assumed':
                value = assumed
            if value == 'knownbad':
                value = knownbad
                
        value = float(value)

        # check that is not NaN
        if isNan(value):
            raise ValueError, 'Cannot set a Madrigal parameter to %s' % (str(value))
                
        # verify value
        scale = self.__twoDList[index][3]
        if self.__twoDList[index][0] < 0:
            isError = True
        else:
            isError = False

        if isError:
            if not floatIn(value, (missing, assumed, knownbad)):
                if -32765.0*scale > value or value > 32766.0*scale:
                    sys.stderr.write('WARNING: Value %f exceeds dynamic range of parameter %s - being set to missing\n' % (value, self.__twoDList[index][1]))
                    value = missing
                if value <= 0.0:
                    sys.stderr.write('WARNING: Value %f of error parameter %s must be positive - being set to missing\n' % (value, self.__twoDList[index][1]))
                    value = missing
        else:
            if not floatIn(value, (missing,)):
                if -32766.0*scale > value or value > 32767.0*scale:
                    sys.stderr.write('WARNING: Value %f exceeds dynamic range of parameter %s - being set to missing\n' % (value, self.__twoDList[index][1]))
                    value = missing

        self.__twoDData[row][index] = value



    def get1D(self, parm):
        """get1D returns the 1D value for a given 1D parameter

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")

        Outputs: double value, or the strings "missing", "assumed", or "knownbad"
        """    
                
        # find index for parm
        index = None
        isInt = False
        try:
            code = int(parm)
            isInt = True
        except:
            parm = parm.lower()

        for i in range(len(self.__oneDList)):
            item = self.__oneDList[i]
            if isInt:
                if item[0] == code:
                    index = i
                    break
            else:
                if item[1] == parm:
                    index = i
                    break

        if index == None:
            raise ValueError, '1D Parameter %s not found in this data record' % (str(parm))

        value = self.__oneDData[index]

        # check for special values
        if floatEquals(value, missing):
            return 'missing'

        # if its an error parameter, allow assumed or knownbad
        if self.__oneDList[index][0] < 0:
            if floatEquals(value, assumed):
                return 'assumed'
            if floatEquals(value, knownbad):
                return 'knownbad'

        return value


    def get2D(self, parm, row):
        """get2D returns the 2D value for a given 2D parameter

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")

            row - row number to get data.  Starts at 0.

        Outputs: double value, or the strings "missing", "assumed", or "knownbad"
        """    
        if row >= self.__nrow or row < 0:
            raise ValueError, 'Illegal value of row %i with nrow = %i' % (row, self.__nrow)
        
        # find index for parm
        index = None
        isInt = False
        try:
            code = int(parm)
            isInt = True
        except:
            parm = parm.lower()

        for i in range(len(self.__twoDList)):
            item = self.__twoDList[i]
            if isInt:
                if item[0] == code:
                    index = i
                    break
            else:
                if item[1] == parm:
                    index = i
                    break

        if index == None:
            raise ValueError, '2D Parameter %s not found in this data record' % (str(parm))

        value = self.__twoDData[row][index]

        # check for special values
        if floatEquals(value, missing):
            return 'missing'

        # if its an error parameter, allow assumed or knownbad
        if self.__twoDList[index][0] < 0:
            if floatEquals(value, assumed):
                return 'assumed'
            if floatEquals(value, knownbad):
                return 'knownbad'

        return value


    def delete1D(self, parm):
        """delete1D removes the given 1D parameter from the record

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")

        Outputs: None

        Raise exception if 1D parm does not exist
        """
        # find index for parm
        index = None
        isInt = False
        try:
            code = int(parm)
            isInt = True
        except:
            parm = parm.lower()

        for i in range(len(self.__oneDList)):
            item = self.__oneDList[i]
            if isInt:
                if item[0] == code:
                    index = i
                    break
            else:
                if item[1] == parm:
                    index = i
                    break

        if index == None:
            raise ValueError, 'Parameter %s not found as 1D parameter in this data record' % (str(parm))
            
        self.__oneDList.remove(self.__oneDList[index])
        del self.__oneDData[index]



    def delete2DParm(self, parm):
        """delete2DParm removes the given 2D parameter from every row in the record

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")

        Outputs: None

        Raise exception if 2D parm does not exist
        """
        
        # find index for parm
        index = None
        isInt = False
        try:
            code = int(parm)
            isInt = True
        except:
            parm = parm.lower()

        for i in range(len(self.__twoDList)):
            item = self.__twoDList[i]
            if isInt:
                if item[0] == code:
                    index = i
                    break
            else:
                if item[1] == parm:
                    index = i
                    break

        if index == None:
            raise ValueError, '2D Parameter %s not found in this data record' % (str(parm))

        self.__twoDList.remove(self.__twoDList[index])
        # remove data
        keepIndexes = range(len(self.__twoDList)+1)
        keepIndexes.remove(index)
        self.__twoDData = numpy.take(self.__twoDData, keepIndexes, 1) # keep all but index column



    def delete2DRows(self, rows):
        """delete2DRows removes the given 2D row or rows in the record (first is row 0)

        Inputs:

            row number (integer) or list of row numbers to delete (first is row 0)

        Outputs: None

        Raise exception if row does not exist
        """
        # make sure row is a list
        if type(rows) in (types.IntType, types.LongType):
            rows = [rows]

        for thisRow in rows:
            if thisRow >= self.__nrow or thisRow < 0:
                raise ValueError, 'Illegal value of row %i with nrow = %i' % (thisRow, self.__nrow)
        
        # remove data
        keepIndexes = range(self.__nrow)
        for thisRow in rows:
            keepIndexes.remove(thisRow)
        self.__twoDData = numpy.take(self.__twoDData, keepIndexes, 0) 
        self.__nrow -= len(rows)
        

    def getKinst(self):
        """getKinst returns the kind of instrument code (int) for a given data record.

        Inputs: None

        Outputs: the kind of instrument code (int) for a given data record.
        """
        return(self.__kinst)


    def setKinst(self, newKinst):
        """setKinst sets the kind of instrument code (int) for a given data record.

        Inputs: newKinst - new instrument code (integer)

        Outputs: None

        Affects: sets self.__kinst
        """
        newKinst = int(newKinst)
        if newKinst < 0:
            raise ValueError, 'Kinst must not be less than 0, not %i' % (newKinst)
        # verify  and set kinst
        instList = self.__madInstObj.getInstrumentList()
        found = False
        for inst in instList:
            if inst[2] == newKinst:
                self.__instrumentName = inst[0]
                found = True
                break
        if found == False:
            self.__instrumentName = 'Unknown instrument'
            sys.stderr.write('Warning: kinst %i not found in instTab.txt\n' % (newKinst))

        self.__kinst = newKinst


    def getKindat(self):
        """getKindat returns the kind of data code (int) for a given data record.

        Inputs: None

        Outputs: the kind of data code (int) for a given data record.
        """
        return(self.__kindat)


    def setKindat(self, newKindat):
        """setKindat sets the kind of data code (int) for a given data record.

        Inputs: newKindat (integer)

        Outputs: None

        Affects: sets self.__kindat
        """
        if newKindat < 0:
            raise 'kindat cannot be negative: %i' % (kindat)
        self.__kindat = int(newKindat)

    def getNrow(self):
        """getNrow returns the number of 2D data rows (int) for a given data record.

        Inputs: None

        Outputs: the number of 2D data rows.
        """
        return(self.__nrow)


    def getStartTimeList(self):
        """getStartTimeList returns a tuple containing sYear, sMonth, sDay, sHour, sMin, sSec, and sCentisec

        Inputs: None

        Outputs: a tuple containing sYear, sMonth, sDay, sHour, sMin, sSec, and sCentisec.
        """
        return((self.__sYear,
                self.__sMonth,
                self.__sDay,
                self.__sHour,
                self.__sMin,
                self.__sSec,
                self.__sCentisec))


    def setStartTimeList(self, sYear, sMonth, sDay, sHour, sMin, sSec, sCentisec=0):
        """setStartTimeList changes the data record start time

        Inputs: integers sYear, sMonth, sDay, sHour, sMin, sSec. sCentisec defaults to 0

        Outputs: None

        Affects: changes self.__sYear, self.__sMonth, self.__sDay, self.__sHour, self.__sMin,
                 self.__sSec, and self.__sCentisec.  Also self.__sTime

        Prints warning if new start time after present end time
        """
        # check validity of input time
        try:
            datetime.datetime(sYear, sMonth, sDay, sHour, sMin, sSec)
        except:
            raise ValueError, 'Illegal datetime %s' % (str((sYear, sMonth, sDay, sHour, sMin, sSec)))

        sCentisec = int(sCentisec)
        if sCentisec < 0 or sCentisec > 99:
            raise ValueError, 'Illegal sCentisec %i' % (sCentisec)

        # verify times
        sTime = datetime.datetime(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec*10000)
        eTime = datetime.datetime(self.__eYear,self.__eMonth,self.__eDay,
                                  self.__eHour,self.__eMin,self.__eSec,self.__eCentisec*10000)

        if eTime < sTime:
            sys.stderr.write('Warning: Starting time %s after ending time %s\n' % (str(sTime), str(eTime)))

        self.__sTime = madrigal._Madrec.getUtFromDate(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec)

        self.__sYear = sYear
        self.__sMonth = sMonth
        self.__sDay = sDay
        self.__sHour = sHour
        self.__sMin = sMin
        self.__sSec = sSec
        self.__sCentisec = sCentisec



    def getEndTimeList(self):
        """getEndTimeList returns a tuple containing eYear, eMonth, eDay, eHour, eMin, eSec, and eCentisec

        Inputs: None

        Outputs: a tuple containing eYear, eMonth, eDay, eHour, eMin, eSec, and eCentisec.
        """
        return((self.__eYear,
                self.__eMonth,
                self.__eDay,
                self.__eHour,
                self.__eMin,
                self.__eSec,
                self.__eCentisec))


    def setEndTimeList(self, eYear, eMonth, eDay, eHour, eMin, eSec, eCentisec=0):
        """setEndTimeList changes the data record end time

        Inputs: integers eYear, eMonth, eDay, eHour, eMin, eSec. eCentisec defaults to 0

        Outputs: None

        Affects: changes self.__eYear, self.__eMonth, self.__eDay, self.__eHour, self.__eMin,
                 self.__eSec, and self.__eCentisec. Also self.__eTime

        Prints warning if new start time after present end time
        """
        # check validity of input time
        try:
            datetime.datetime(eYear, eMonth, eDay, eHour, eMin, eSec)
        except:
            raise ValueError, 'Illegal datetime %s' % (str((eYear, eMonth, eDay, eHour, eMin, eSec)))

        eCentisec = int(eCentisec)
        if eCentisec < 0 or eCentisec > 99:
            raise ValueError, 'Illegal eCentisec %i' % (eCentisec)

        # verify times
        eTime = datetime.datetime(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec*10000)
        sTime = datetime.datetime(self.__sYear,self.__sMonth,self.__sDay,
                                  self.__sHour,self.__sMin,self.__sSec,self.__sCentisec*10000)

        if eTime < sTime:
            sys.stderr.write('Warning: Starting time %s after ending time %s\n' % (str(sTime), str(eTime)))

        self.__eTime = madrigal._Madrec.getUtFromDate(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec)

        self.__eYear = eYear
        self.__eMonth = eMonth
        self.__eDay = eDay
        self.__eHour = eHour
        self.__eMin = eMin
        self.__eSec = eSec
        self.__eCentisec = eCentisec





    def get1DParms(self):
        """get1DParms returns a list of 1D parameters in the MadrigalDataRecord.

        Inputs: None

        Outputs: a list of 1D parameters in the MadrigalDataRecord. Each parameter
        has the attributes: code (int), mnemonic (string), and description (string)
        """
        retList = []

        for parm in self.__oneDList:
            retList.append(CedarParameter(parm[0], parm[1], parm[2]))

        return(retList)


    def get2DParms(self):
        """get2DParms returns a list of 2D parameters in the MadrigalDataRecord.

        Inputs: None

        Outputs: a list of 2D parameters in the MadrigalDataRecord. Each parameter
        has the attributes: code (int), mnemonic (string), and description (string)
        """
        retList = []

        for parm in self.__twoDList:
            retList.append(CedarParameter(parm[0], parm[1], parm[2]))

        return(retList)
    

    def getHeaderKodLines(self):
        """getHeaderKodLines creates the lines in the Madrigal header record that start KOD and describe parms

        Inputs: None

        Returns: a string of length 80*num 1D parms.  Each 80 characters contains a description
                 of a single parm accodring to the Cedar Standard
        """
        # create a list of oneDCedar codes for the data record.  Include additional increment parameters
        #   if they exist. Each item has four elements:
        # (code, parameter description, scaleFactor, units)
        oneDCedarCodes = []
        for parm in self.__oneDList:
            oneDCedarCodes.append((parm[0], self.__madParmObj.getSimpleParmDescription(parm[0]),
                                   parm[3], self.__madParmObj.getParmUnits(parm[0])))
            if parm[4]:
                # get scaling factor for additional increment
                scaleFactor = self.__madParmObj.getParmScaleFactor(abs(parm[0]) + 1)
                if parm[0] > 0:
                    oneDCedarCodes.append((parm[0] + 1,
                                           self.__madParmObj.getSimpleParmDescription(parm[0] + 1),
                                           scaleFactor,
                                           self.__madParmObj.getParmUnits(parm[0] + 1)))
                else:
                    oneDCedarCodes.append((parm[0] - 1,
                                           self.__madParmObj.getSimpleParmDescription(parm[0] - 1),
                                           scaleFactor,
                                           self.__madParmObj.getParmUnits(parm[0] - 1)))
        
        oneDCedarCodes.sort(compareParms)

        # create a list of twoDCedar codes for the data record.  Include additional increment parameters
        #   if they exist. Each item has four elements:
        # (code, parameter description, scaleFactor, units)
        twoDCedarCodes = []
        for parm in self.__twoDList:
            twoDCedarCodes.append((parm[0], self.__madParmObj.getSimpleParmDescription(parm[0]),
                                   parm[3], self.__madParmObj.getParmUnits(parm[0])))
            if parm[4]:
                # get scaling factor for additional increment
                scaleFactor = self.__madParmObj.getParmScaleFactor(abs(parm[0]) + 1)
                if parm[0] > 0:
                    twoDCedarCodes.append((parm[0] + 1,
                                           self.__madParmObj.getSimpleParmDescription(parm[0] + 1),
                                           scaleFactor,
                                           self.__madParmObj.getParmUnits(parm[0] + 1)))
                else:
                    twoDCedarCodes.append((parm[0] - 1,
                                           self.__madParmObj.getSimpleParmDescription(parm[0] - 1),
                                           scaleFactor,
                                           self.__madParmObj.getParmUnits(parm[0] - 1)))

        twoDCedarCodes.sort(compareParms)
        

        # write out lines - one D
        retStr = ''
        if len(oneDCedarCodes) > 0:
            retStr += 'C 1D Parameters:' + (80 - len('C 1D Parameters:'))*' '
        for i in range(len(oneDCedarCodes)):
            code = oneDCedarCodes[i][0]
            desc = oneDCedarCodes[i][1]
            scaleFactor = oneDCedarCodes[i][2]
            units = oneDCedarCodes[i][3]
            kods = 'KODS(%i)' % (i)
            kods += (10-len(kods))*' '
            rowNum = str(i + 17)
            rowNum = (6-len(rowNum))* ' ' + rowNum
            codeNum = str(code)
            codeNum = (8-len(codeNum))* ' ' + codeNum
            if len(desc) > 38:
                desc = ' ' + desc[:38] + ' '
            else:
                desc = ' ' + desc + (39-len(desc))* ' '
            scale = '%0.0E' % (scaleFactor)
            scale = scale + (8-len(scale))*' '
            units = units + (8-len(units))*' '
            retStr += kods + rowNum + codeNum + desc + scale + units

        # two D
        if len(twoDCedarCodes) > 0:
            retStr += 'C 2D Parameters:' + (80 - len('C 2D Parameters:'))*' '
        startIndex = 17 + 2*(len(oneDCedarCodes))
        for i in range(len(twoDCedarCodes)):
            code = twoDCedarCodes[i][0]
            desc = twoDCedarCodes[i][1]
            scaleFactor = twoDCedarCodes[i][2]
            units = twoDCedarCodes[i][3]
            kods = 'KODM(%i)' % (i)
            kods += (10-len(kods))*' '
            rowNum = str(i + startIndex)
            rowNum = (6-len(rowNum))* ' ' + rowNum
            codeNum = str(code)
            codeNum = (8-len(codeNum))* ' ' + codeNum
            if len(desc) > 38:
                desc = ' ' + desc[:38] + ' '
            else:
                desc = ' ' + desc + (39-len(desc))* ' '
            scale = '%0.0E' % (scaleFactor)
            scale = scale + (8-len(scale))*' '
            units = units + (8-len(units))*' '
            retStr += kods + rowNum + codeNum + desc + scale + units

        return(retStr)

                    

    def writeRecord(self, cedarFile, format):
        """writeRecord writes a MadrigalDataRecord to an open cedarFile.

           Users should not call this method directly.  Use MadrigalCedarFile.write
           to persist data.

           Inputs:

               cedarFile - pointer to madrec as returned by madrigal._Madrec.madrecOpen
               
                format - a format to save the file in.  For now, the allowed values are 'Madrigal', 'BlockedBinary',
                'UnblockedBinary', 'Cbf', and 'Ascii'.  
                
           Outputs: None

           Affects: writes MadrigalDataRecord to cedar file
        """
        # create a list of oneDCedar codes for the data record.  Include additional increment parameters
        #   if they exist. Each item has three elements:
        # (code, scaleFactor, hasAddIncrement)
        oneDCedarCodes = []
        for parm in self.__oneDList:
            oneDCedarCodes.append((parm[0],parm[3],parm[4]))
            if parm[4]:
                # get scaling factor for additional increment
                scaleFactor = self.__madParmObj.getParmScaleFactor(abs(parm[0]) + 1)
                if parm[0] > 0:
                    oneDCedarCodes.append((parm[0] + 1,scaleFactor,False))
                else:
                    oneDCedarCodes.append((parm[0] - 1,scaleFactor,False))
                

        # create a list of twoD codes for the data record.  Include additional increment parameters
        #   if they exist.
        twoDCedarCodes = []
        for parm in self.__twoDList:
            twoDCedarCodes.append((parm[0],parm[3],parm[4]))
            if parm[4]:
                # get scaling factor for additional increment
                scaleFactor = self.__madParmObj.getParmScaleFactor(abs(parm[0]) + 1)
                if parm[0] > 0:
                    twoDCedarCodes.append((parm[0] + 1, scaleFactor,False))
                else:
                    twoDCedarCodes.append((parm[0] - 1, scaleFactor,False))

        # create data record (data will be filled in next)
        result = madrigal._Madrec.madrecCreateDataRecord(cedarFile,
                                                         format,
                                                         self.__kinst,
                                                         self.__kindat,
                                                         self.__sTime,
                                                         self.__eTime,
                                                         len(oneDCedarCodes),
                                                         len(twoDCedarCodes),
                                                         self.__nrow)

        # loop through 1D data, and set it
        previousHadIncrement = False
        previousIncrementValue = None
        for i in range(len(oneDCedarCodes)):
            thisParm = oneDCedarCodes[i]
            if not previousHadIncrement:
                # see if this parameter has an increment
                if thisParm[2]:
                    previousHadIncrement = True
                    value = self.get1D(thisParm[0])
                    if type(value) == types.StringType:
                        previousIncrementValue = value
                    else:
                        previousIncrementValue = value % thisParm[1]
                        if value < 0.0:
                            previousIncrementValue = -1.0*(thisParm[1] - previousIncrementValue)
                        
                    # subtract off increment from value
                    if type(value) != types.StringType:
                        value = value - previousIncrementValue

                    if madrigal._Madrec.cedarSet1dParm(cedarFile, thisParm[0], str(value), i):
                        raise IOError, 'error in cedarSet1dParm'
                else:
                    # this parm doesn't have any additional increment
                    previousHadIncrement = False
                    previousIncrementValue = None
                    value = self.get1D(thisParm[0])
                    if madrigal._Madrec.cedarSet1dParm(cedarFile, thisParm[0], str(value), i):
                        raise IOError, 'error in cedarSet1dParm'
                    # now see if this is an error parameter illegally set to zero
                    try: # this may fail if value not a number
                        thisValue = float(value)
                        if thisParm[0] < 0 and thisParm[1] > thisValue:
                            raise IOError, 'Tried to set 1D error parm %s to zero' % (thisParm[1])
                    except ValueError:
                        pass
                    
            else:
                # deal with increment parameter
                previousHadIncrement = False
                if madrigal._Madrec.cedarSet1dParm(cedarFile, thisParm[0], str(previousIncrementValue), i):
                    raise IOError, 'error in cedarSet1dParm'
                
                # now see if this is an error parameter illegally set to zero
                try: # this may fail if value not a number
                    thisValue = float(previousIncrementValue)
                    if thisParm[0] < 0 and thisParm[1] > thisValue and value == 0.0:
                        raise IOError, 'Tried to set 1D error parm %s to zero' % (thisParm[1])
                except ValueError:
                    pass

                previousIncrementValue = None


        # loop through 2D data, and set it
        previousHadIncrement = False
        previousIncrementValueList = None
        for i in range(len(twoDCedarCodes)):
            thisParm = twoDCedarCodes[i]
            if not previousHadIncrement:
                # see if this parameter has an increment
                if thisParm[2]:
                    previousHadIncrement = True
                    values = self.__get2DMainValueList__(thisParm[0], thisParm[1])
                    previousIncrementValueList = self.__get2DIncrValueList__(thisParm[0], thisParm[1])

                    madrigal._Madrec.cedarSet2dParm(cedarFile, thisParm[0], values, i)
                else:
                    # this parm doesn't have any additional increment
                    previousHadIncrement = False
                    previousIncrementValueList = None
                    values = self.__get2DValueList__(thisParm[0])
                    madrigal._Madrec.cedarSet2dParm(cedarFile, thisParm[0], values, i)
                    # now see if any of these are an error parameter illegally set to zero
                    for thisValue in values:
                        try: # this may fail if value not a number
                            testValue = float(thisValue)
                            if thisParm[0] < 0 and thisParm[1] > testValue:
                                raise IOError, 'Tried to set 2D error parm %s to zero' % (thisParm[0])
                        except ValueError:
                            pass
                    
            else:
                # deal with increment parameter
                previousHadIncrement = False
                madrigal._Madrec.cedarSet2dParm(cedarFile, thisParm[0], previousIncrementValueList, i)

                # now see if any of these values an error parameter illegally set to zero
                incrValues = previousIncrementValueList
                for i in range(len(values)):
                    try: # this may fail if value not a number
                        thisValue = float(values[i])
                        thisIncrValue = float(incrValues[i])
                        if thisParm[0] < 0 and thisParm[1] > thisIncrValue and thisValue == 0.0:
                            raise IOError, 'Tried to set 2D error parm %s to zero' % (thisParm[0])
                    except ValueError:
                        pass
                
                previousIncrementValueList = None

        madrigal._Madrec.madrecPutNextRec(cedarFile)
                    
            

    def __get2DValueList__(self, parm):
        """__get2DValueList__ returns a list containing all the 2D values of a given parameter.

        Inputs:

            parm - can be defined as code (integer) or case-insensitive mnemonic string (eg, "Gdalt")


        Outputs: a list containing all the 2D values of a given parameter.  Special values will
                 be given the values 'missing', 'assumed', or 'knownbad'
        """
        retList = []
        nrow = self.getNrow()
        for i in range(nrow):
            retList.append(self.get2D(parm,i))

        return(retList)


    def __get2DMainValueList__(self, code, scaleFactor):
        """__get2DMainValueList__ returns a list containing all the 2D values of a given main parameter.

        Inputs:

            code - parameter code (integer).  Must
                   be a parameter with an additional increment parameter.


        Outputs: a list containing all the 2D values of a given main parameter that has an
                 additional increment parameter.  Special values will be given the values 'missing', 'assumed', or 'knownbad'
        """
        retList = []
        nrow = self.getNrow()
        for i in range(nrow):
            value = self.get2D(code,i)
            if type(value) != types.StringType:
                # subtract off additional increment part
                addIncr = value % scaleFactor
                if value < 0:
                    addIncr = -1.0 * (scaleFactor - addIncr)
                value = value - addIncr
            retList.append(value)

        return(retList)


    def __get2DIncrValueList__(self, code, scaleFactor):
        """__get2DIncrValueList__ returns a list containing all the additional increment 2D values of a given main parameter.

        Inputs:

            parm - parameter code (integer).  Must
                   be a parameter with an additional increment parameter.


        Outputs: a list containing all the additional increment 2D values of a given main parameter.
                 Special values will be given the values 'missing', 'assumed', or 'knownbad'
        """
        retList = []
        nrow = self.getNrow()
        for i in range(nrow):
            value = self.get2D(code,i)
            if type(value) != types.StringType:
                # get additional increment part
                incr = value % scaleFactor
                if value < 0:
                    incr = -1.0 * (scaleFactor - incr)
                value = incr
            retList.append(value)

        return(retList)


    def __str__(self):
        """ returns a string representation of a MadrigalDataRecord """
        retStr = 'Data record:\n'
        retStr += 'kinst = %i (%s)\n' % (self.__kinst, self.__instrumentName)
        retStr += 'kindat = %i\n' % (self.__kindat)
        retStr += 'record start: %04i-%02i-%02i %02i:%02i:%02i.%02i\n' % (self.__sYear,
                                                                        self.__sMonth,
                                                                        self.__sDay,
                                                                        self.__sHour,
                                                                        self.__sMin,
                                                                        self.__sSec,
                                                                        self.__sCentisec)
        retStr += 'record end:   %04i-%02i-%02i %02i:%02i:%02i.%02i\n' % (self.__eYear,
                                                                        self.__eMonth,
                                                                        self.__eDay,
                                                                        self.__eHour,
                                                                        self.__eMin,
                                                                        self.__eSec,
                                                                        self.__eCentisec)
        retStr += 'one-dim parameters:\n'
        for parm in self.__oneDList:
            retStr += '\t%s\n' % (str(parm))
        retStr += 'two-dim parameters:\n'
        for parm in self.__twoDList:
            retStr += '\t%s\n' % (str(parm))
        retStr += '%s\n' % (str(self.__oneDData))
        retStr += '%s\n' % (str(self.__twoDData))

        return(retStr)

        
        

class MadrigalCatalogRecord:
    """MadrigalCatalogRecord holds all the information in a Cedar catalog record."""
    
    def __init__(self,kinst,
                 modexp,
                 sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                 eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec,
                 text,
                 madInstObj = None):
        """__init__ creates a MadrigalCatalogRecord.

        Inputs:

            kinst - the kind of instrument code.  A warning will be raised if not in instTab.txt.

            modexp - Code to indicate experimental mode employed. Must be a non-negative integer.

            sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec - experiment start time. sCentisec must be 0-99

            eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec - experiment end time. eCentisec must be 0-99

            text - string containing text in catalog record.  Length must be divisible by 80.  No linefeeds
                   allowed.

            madInstObj - a madrigal.metadata.MadrigalInstrument object.  If None, one will be created.
                              Used to verify kinst.

        Outputs: None

        Returns: None
        """
        # create any needed Madrigal objects, if not passed in
        if madInstObj == None:
            self.__madInstObj = madrigal.metadata.MadrigalInstrument()
        else:
            self.__madInstObj = madInstObj
            
        self.setKinst(kinst)

        self.setModexp(modexp)

        self.setTimeLists(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                          eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec)

        self.setText(text)
        

        
    def getType(self):
        """ returns the type 'catalog'"""
        return 'catalog'


    def writeRecord(self, cedarFile, format):
        """writeRecord writes a MadrigalCatalogRecord to an open cedarFile.

           Users should not call this method directly.  Use MadrigalCedarFile.write
           to persist data.

           Inputs:

               cedarFile - pointer to madrec as returned by madrigal._Madrec.madrecOpen
               
                format - a format to save the file in.  For now, the allowed values are 'Madrigal', 'BlockedBinary',
                'UnblockedBinary', 'Cbf', and 'Ascii'.  
                
           Outputs: None

           Affects: writes MadrigalCatalogRecord to cedar file
        """
        if madrigal._Madrec.madrecCreateCatalogRecord(cedarFile,
                                                      format,
                                                      self.__kinst,
                                                      self.__modexp,
                                                      self.__sTime,
                                                      self.__eTime,
                                                      self.__text):
            raise IOError, 'error in madrecCreateCatalogRecord'
        
        if madrigal._Madrec.madrecPutNextRec(cedarFile):
            raise IOError, 'error in madrecPutNextRec'


    def getKinst(self):
        """getKinst returns the kind of instrument code (int) for a given catalog record.

        Inputs: None

        Outputs: the kind of instrument code (int) for a given catalog record.
        """
        return(self.__kinst)


    def setKinst(self, kinst):
        """setKinst sets the kind of instrument code (int) for a given catalog record.

        Inputs: kind of instrument code (integer)

        Outputs: None

        Affects: sets the kind of instrument code (int) (self.__kinst) for a given catalog record.
        Prints warning if kinst not found in instTab.txt
        """
        kinst = int(kinst)
        # verify  and set kinst
        instList = self.__madInstObj.getInstrumentList()
        found = False
        for inst in instList:
            if inst[2] == kinst:
                self.__instrumentName = inst[0]
                found = True
                break
        if found == False:
            self.__instrumentName = 'Unknown instrument'
            sys.stderr.write('Warning: kinst %i not found in instTab.txt\n' % (kinst))

        self.__kinst = kinst

    def getModexp(self):
        """getModexp returns the mode of experiment code (int) for a given catalog record.

        Inputs: None

        Outputs: the mode of experiment code (int) for a given catalog record.
        """
        return(self.__modexp)

    def setModexp(self, modexp):
        """setModexp sets the mode of experiment code (int) for a given catalog record.

        Inputs: the mode of experiment code (int)

        Outputs: None

        Affects: sets the mode of experiment code (int) (self.__modexp)
        """
        self.__modexp = int(modexp)


    def getText(self):
        """getText returns the catalog text.

        Inputs: None

        Outputs: the catalog text.
        """
        return(self.__text)


    def setText(self, text):
        """setText sets the catalog text.

        Inputs: text: text to be set.  Must be length divisible by 80, and not contain line feeds.
        Also, for now cannot exceed 2^(16) - 80

        Outputs: None.

        Affects: sets self.__text

        Raise TypeError if problem with test
        """
        if type(text) != types.StringType:
            raise TypeError, 'text must be of type string'

        if len(text) % 80 != 0:
            raise TypeError, 'text length must be divisible by 80: len is %i' % (len(text))

        if text.find('\n') != -1:
            raise TypeError, 'text must not contain linefeed character'

        if len(text) > 65536 - 80:
            raise TypeError, 'text exceeds ability of Cedar format to store'

        self.__text = text


    def getStartTimeList(self):
        """getStartTimeList returns a tuple containing sYear, sMonth, sDay, sHour, sMin, sSec, and sCentisec

        Inputs: None

        Outputs: a tuple containing sYear, sMonth, sDay, sHour, sMin, sSec, and sCentisec.
        """
        return((self.__sYear,
                self.__sMonth,
                self.__sDay,
                self.__sHour,
                self.__sMin,
                self.__sSec,
                self.__sCentisec))


    def getEndTimeList(self):
        """getEndTimeList returns a tuple containing eYear, eMonth, eDay, eHour, eMin, eSec, and eCentisec

        Inputs: None

        Outputs: a tuple containing eYear, eMonth, eDay, eHour, eMin, eSec, and eCentisec.
        """
        return((self.__eYear,
                self.__eMonth,
                self.__eDay,
                self.__eHour,
                self.__eMin,
                self.__eSec,
                self.__eCentisec))
    

    def setTimeLists(self, sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                     eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec):
        """setTimeList resets start and end times

        Inputs:

            sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec - experiment start time. sCentisec must be 0-99

            eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec - experiment end time. eCentisec must be 0-99

        Outputs: None

        Affects: sets all time attributes (see code).

        Exceptions: Raises ValueError if startTime > endTime
        """
        # verify times
        sTime = datetime.datetime(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec*10000)
        eTime = datetime.datetime(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec*10000)

        if eTime < sTime:
            raise ValueError, 'Starting time cannot be after ending time'
        
        self.__sTime = madrigal._Madrec.getUtFromDate(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec)
        self.__eTime = madrigal._Madrec.getUtFromDate(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec)
        
        self.__sYear = sYear
        self.__sMonth = sMonth
        self.__sDay = sDay
        self.__sHour = sHour
        self.__sMin = sMin
        self.__sSec = sSec
        self.__sCentisec = sCentisec

        
        self.__eYear = eYear
        self.__eMonth = eMonth
        self.__eDay = eDay
        self.__eHour = eHour
        self.__eMin = eMin
        self.__eSec = eSec
        self.__eCentisec = eCentisec
        
        
    def __str__(self):
        """ returns a string representation of a MadrigalCatalogRecord """
        retStr = 'Catalog Record:\n'
        retStr += 'kinst = %i (%s)\n' % (self.__kinst, self.__instrumentName)
        retStr += 'modexp = %i\n' % (self.__modexp)
        retStr += 'record start: %04i-%02i-%02i %02i:%02i:%02i.%02i\n' % (self.__sYear,
                                                                        self.__sMonth,
                                                                        self.__sDay,
                                                                        self.__sHour,
                                                                        self.__sMin,
                                                                        self.__sSec,
                                                                        self.__sCentisec)
        retStr += 'record end:   %04i-%02i-%02i %02i:%02i:%02i.%02i\n' % (self.__eYear,
                                                                        self.__eMonth,
                                                                        self.__eDay,
                                                                        self.__eHour,
                                                                        self.__eMin,
                                                                        self.__eSec,
                                                                        self.__eCentisec)
        for i in range(0, len(self.__text) -1, 80):
            retStr += '%s\n' % (self.__text[i:i+80])

        return(retStr)

    

class MadrigalHeaderRecord:
    """MadrigalHeaderRecord holds all the information in a Cedar header record."""
    
    def __init__(self,kinst,
                 kindat,
                 sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                 eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec,
                 jpar, mpar,
                 text,
                 madInstObj = None):
        """__init__ creates a MadrigalCatalogRecord.

        Inputs:

            kinst - the kind of instrument code.  A warning will be raised if not in instTab.txt.

            kindat - kind of data code. Must be a non-negative integer.

            sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec - experiment start time. sCentisec must be 0-99

            eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec - experiment end time. eCentisec must be 0-99

            jpar - the number of 1d parameters in the following data records

            mpar - the number of 2d parameters in the following data records

            text - string containing text in catalog record.  Length must be divisible by 80.  No linefeeds
                   allowed.

            madInstObj - a madrigal.metadata.MadrigalInstrument object.  If None, one will be created.
                              Used to verify kinst.

        Outputs: None

        Returns: None
        """
        # create any needed Madrigal objects, if not passed in
        if madInstObj == None:
            self.__madInstObj = madrigal.metadata.MadrigalInstrument()
        else:
            self.__madInstObj = madInstObj

        self.setKinst(kinst)

        self.setKindat(kindat)
        
        self.setTimeLists(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                          eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec)

        self.setJpar(jpar)

        self.setMpar(mpar)

        self.setText(text)

        
    def getType(self):
        """ returns the type 'header'"""
        return 'header'


    def writeRecord(self, cedarFile, format):
        """writeRecord writes a MadrigalHeaderRecord to an open cedarFile.

           Users should not call this method directly.  Use MadrigalCedarFile.write
           to persist data.

           Inputs:

               cedarFile - pointer to madrec as returned by madrigal._Madrec.madrecOpen
               
                format - a format to save the file in.  For now, the allowed values are 'Madrigal', 'BlockedBinary',
                'UnblockedBinary', 'Cbf', and 'Ascii'.  
                
           Outputs: None

           Affects: writes MadrigalHeaderRecord to cedar file
        """
        if madrigal._Madrec.madrecCreateHeaderRecord(cedarFile,
                                                     format,
                                                     self.__kinst,
                                                     self.__kindat,
                                                     self.__sTime,
                                                     self.__eTime,
                                                     self.__jpar,
                                                     self.__mpar,
                                                     self.__text):
            raise IOError, 'error in madrecCreateHeaderRecord'
        
        if madrigal._Madrec.madrecPutNextRec(cedarFile):
            raise IOError, 'error in madrecPutNextRec'

    def getKinst(self):
        """getKinst returns the kind of instrument code (int) for a given header record.

        Inputs: None

        Outputs: the kind of instrument code (int) for a given header record.
        """
        return(self.__kinst)


    def setKinst(self, kinst):
        """setKinst sets the kind of instrument code (int) for a given header record.

        Inputs: kind of instrument code (integer)

        Outputs: None

        Affects: sets the kind of instrument code (int) (self.__kinst) for a given header record.
        Prints warning if kinst not found in instTab.txt
        """
        kinst = int(kinst)
        # verify  and set kinst
        instList = self.__madInstObj.getInstrumentList()
        found = False
        for inst in instList:
            if inst[2] == kinst:
                self.__instrumentName = inst[0]
                found = True
                break
        if found == False:
            self.__instrumentName = 'Unknown instrument'
            sys.stderr.write('Warning: kinst %i not found in instTab.txt\n' % (kinst))

        self.__kinst = kinst


    def getKindat(self):
        """getKindat returns the kind of data code (int) for a given header record.

        Inputs: None

        Outputs: the kind of data code (int) for a given header record.
        """
        return(self.__kindat)
    
    
    def setKindat(self, kindat):
        """setKindat sets the mode of kind of data code (int) for a given header record.

        Inputs: the kind of data code (int)

        Outputs: None

        Affects: sets the kind of data code (int) (self.__kindat)

        Exceptions: Raises ValueError if kindat less than 0
        """
        self.__kindat = int(kindat)
        if self.__kindat < 0:
            raise ValueError, 'kindat must not be less than 0, not %i' % (self.__kindat)

    def getText(self):
        """getText returns the header text.

        Inputs: None

        Outputs: the header text.
        """
        return(self.__text)
    

    def setText(self, text):
        """setText sets the header text.

        Inputs: text: text to be set.  Must be length divisible by 80, and not contain line feeds.
        For now, must not exceed 2^16 - 80 bytes to be able to be handled by Cedar format.

        Outputs: None.

        Affects: sets self.__text

        Raises TypeError if problem with text
        """
        if type(text) != types.StringType:
            raise TypeError, 'text must be of type string'

        if len(text) % 80 != 0:
            raise TypeError, 'text length must be divisible by 80: len is %i' % (len(text))

        if text.find('\n') != -1:
            raise TypeError, 'text must not contain linefeed character'

        if len(text) > 65536 - 80:
            raise TypeError, 'text exceeds ability of Cedar format to store'

        self.__text = text


    def getJpar(self):
        """returns the number of one-dimensional parameters in the associated data records.
        """
        return self.__jpar


    def setJpar(self, jpar):
        """ set the number of one-dimensional parameters in the associated data records.

        Must not be negative.
        """
        self.__jpar = int(jpar)
        if self.__jpar < 0:
            raise TypeError, 'jpar must not be less than 0'


    def getMpar(self):
        """returns the number of two-dimensional parameters in the associated data records.
        """
        return self.__mpar
        

    def setMpar(self, mpar):
        """ set the number of two-dimensional parameters in the associated data records.

        Must not be negative.
        """
        self.__mpar = int(mpar)
        if self.__mpar < 0:
            raise TypeError, 'mpar must not be less than 0'


    def getStartTimeList(self):
        """getStartTimeList returns a tuple containing sYear, sMonth, sDay, sHour, sMin, sSec, and sCentisec

        Inputs: None

        Outputs: a tuple containing sYear, sMonth, sDay, sHour, sMin, sSec, and sCentisec.
        """
        return((self.__sYear,
                self.__sMonth,
                self.__sDay,
                self.__sHour,
                self.__sMin,
                self.__sSec,
                self.__sCentisec))


    def getEndTimeList(self):
        """getEndTimeList returns a tuple containing eYear, eMonth, eDay, eHour, eMin, eSec, and eCentisec

        Inputs: None

        Outputs: a tuple containing eYear, eMonth, eDay, eHour, eMin, eSec, and eCentisec.
        """
        return((self.__eYear,
                self.__eMonth,
                self.__eDay,
                self.__eHour,
                self.__eMin,
                self.__eSec,
                self.__eCentisec))

    
    def setTimeLists(self, sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                     eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec):
        """setTimeList resets start and end times

        Inputs:

            sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec - experiment start time. sCentisec must be 0-99

            eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec - experiment end time. eCentisec must be 0-99

        Outputs: None

        Affects: sets all time attributes (see code).

        Exceptions: Raises ValueError if startTime > endTime
        """
        # verify times
        sTime = datetime.datetime(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec*10000)
        eTime = datetime.datetime(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec*10000)

        if eTime < sTime:
            raise ValueError, 'Starting time cannot be after ending time'
        
        self.__sTime = madrigal._Madrec.getUtFromDate(sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec)
        self.__eTime = madrigal._Madrec.getUtFromDate(eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec)
        
        self.__sYear = sYear
        self.__sMonth = sMonth
        self.__sDay = sDay
        self.__sHour = sHour
        self.__sMin = sMin
        self.__sSec = sSec
        self.__sCentisec = sCentisec

        
        self.__eYear = eYear
        self.__eMonth = eMonth
        self.__eDay = eDay
        self.__eHour = eHour
        self.__eMin = eMin
        self.__eSec = eSec
        self.__eCentisec = eCentisec
        
    
    def __str__(self):
        """ returns a string representation of a MadrigalHeaderRecord """
        retStr = 'Header Record:\n'
        retStr += 'kinst = %i (%s)\n' % (self.__kinst, self.__instrumentName)
        retStr += 'kindat = %i\n' % (self.__kindat)
        retStr += 'record start: %04i-%02i-%02i %02i:%02i:%02i.%02i\n' % (self.__sYear,
                                                                        self.__sMonth,
                                                                        self.__sDay,
                                                                        self.__sHour,
                                                                        self.__sMin,
                                                                        self.__sSec,
                                                                        self.__sCentisec)
        retStr += 'record end:   %04i-%02i-%02i %02i:%02i:%02i.%02i\n' % (self.__eYear,
                                                                        self.__eMonth,
                                                                        self.__eDay,
                                                                        self.__eHour,
                                                                        self.__eMin,
                                                                        self.__eSec,
                                                                        self.__eCentisec)
        
        retStr += 'jpar = %i, mpar = %i' % (self.__jpar, self.__mpar)
        
        for i in range(0, len(self.__text) -1, 80):
            retStr += '%s\n' % (self.__text[i:i+80])

        return(retStr)



class CatalogHeaderCreator:
    """CatalogHeaderCreator is a class that automates the creation of catalog and header records
    
    This class creates and adds catalog and header records that meet the Cedar standards.  It does this 
    by examining the input Cedar file for all summary information possible.  The user needs only 
    add text that describes their experiment.  A Cedar file must already be written to disk before
    this class is created.
    """
    def __init__(self, madFilename):
        """__init__ reads in all summary information about madFilename using madrigal.data
        """
        self._madFilename = madFilename
        self._summary = madrigal.data.MadrigalFile(self._madFilename)
        self._cedar = MadrigalCedarFile(madFilename) # parse file into MadrigalCedarFile object
        self._fileType = madrigal._Madrec.getFileType(madFilename)
        # create default header and catalog records
        self._header = None
        self._catalog = None
        self._lineLen = 80
        
        
    def createCatalog(self, principleInvestigator=None,
                      expPurpose=None,
                      expMode=None,
                      cycleTime=None,
                      correlativeExp=None,
                      sciRemarks=None,
                      instRemarks=None):
        """createCatalog will create a catalog record appropriate for this file.  The additional
        information fields are all optional, and are all simple text strings (except for
        cycleTime, which is in minutes).  If the text contains line feeds, those will be used 
        as line breaks in the catalog record.
        
        The descriptions of these fields all come from Barbara Emery's documentation
        cedarFormat.pdf
        
        Inputs:
        
            principleInvestigator - Names of responsible Principal Investigator(s) or others knowledgeable 
                                    about the experiment.
            
            expPurpose - Brief description of the experiment purpose
            
            expMode - Further elaboration of meaning of MODEXP; e.g. antenna patterns and 
                      pulse sequences.
                      
            cycleTime - Minutes for one full measurement cycle
            
            correlativeExp - Correlative experiments (experiments with related data)
            
            sciRemarks - scientific remarks
            
            instRemarks - instrument remarks
            
        Returns: None
        
        Affects: sets self._catalog
        """
        # the first step is to create the text part
        text = ''
        
        # start with parameter summary lines
        if cycleTime != None:
            text += 'TIMCY  %9i minutes' % (int(cycleTime))
            text = self._padStr(text, self._lineLen)
            
        text += self._createMaxMinSummaryLines()
        
        # add the time lines
        text += self._cedar.createCatalogTimeSection()[0]
        
        # then add any text from input arguments
        if expMode != None:
            text += self._createCedarLines('CMODEXP ', expMode)
            
        if expPurpose != None:
            text += self._createCedarLines('CPURP   ', expPurpose)
            
        if correlativeExp != None:
            text += self._createCedarLines('CCOREXP ', correlativeExp)
            
        if sciRemarks != None:
            text += self._createCedarLines('CSREM   ', sciRemarks)
            
        if instRemarks != None:
            text += self._createCedarLines('CIREM   ', instRemarks)
            
        if principleInvestigator != None:
            text += self._createCedarLines('CPI     ', principleInvestigator)
            
        # get some other metadata
        kinst = self._summary.getKinstList()[0]
        modexp = self._summary.getKindatList()[0]
        sYear,sMonth,sDay,sHour,sMin,sSec = self._summary.getEarliestTime()
        sCentisec = 0
        eYear,eMonth,eDay,eHour,eMin,eSec = self._summary.getLatestTime()
        eCentisec = 0
        
        # now create the catalog record
        self._catalog = MadrigalCatalogRecord(kinst, modexp,
                                              sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                                              eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec,
                                              text)
        
        
    def createHeader(self, kindatDesc=None, 
                           analyst=None, 
                           comments=None, 
                           history=None):
        """createHeader will create a header record appropriate for this file.  The additional
        information fields are all optional, and are all simple text strings.  If the text contains l
        ine feeds, those will be used as line breaks in the header record.
        
        Inputs:
        
            kindatDesc - description of how this data was analyzed (the kind of data)
            
            analyst - name of person who analyzed this data
            
            comments - additional comments about data (describe any instrument-specific parameters) 
            
            history - a description of the history of the processing of this file
            
        Returns: None
        
        Affects: sets self._header
        """
        # the first step is to create the text part
        text = ''
        
        if kindatDesc != None:
            text += self._createCedarLines('CKINDAT', kindatDesc)
            
        if history != None:
            text += self._createCedarLines('CHIST  ', history)
            
        # add the time lines
        text += self._cedar.createHeaderTimeSection()[0]
        
        # add the KOD linesfrom the last record of file (must be data record)
        text += self._cedar[-1].getHeaderKodLines()
        
        if comments != None:
            text += self._createCedarLines('C      ', comments)
            
        if analyst != None:
            text += self._createCedarLines('CANALYST', analyst)
            
        # last - time of analysis line
        now = datetime.datetime.utcnow()
        nowStr = now.strftime('%a %b %d %H:%M:%S %Y')
        text += 'CANDATE  %s UT' % (nowStr)
        text = self._padStr(text, self._lineLen)
        
        # get some other metadata
        kinst = self._summary.getKinstList()[0]
        kindat = self._summary.getKindatList()[0]
        sYear,sMonth,sDay,sHour,sMin,sSec = self._summary.getEarliestTime()
        sCentisec = 0
        eYear,eMonth,eDay,eHour,eMin,eSec = self._summary.getLatestTime()
        eCentisec = 0
        jpar = len(self._cedar[-1].get1DParms())
        mpar = len(self._cedar[-1].get2DParms())
        
        self._header = MadrigalHeaderRecord(kinst, kindat,
                                            sYear,sMonth,sDay,sHour,sMin,sSec,sCentisec,
                                            eYear,eMonth,eDay,eHour,eMin,eSec,eCentisec,
                                            jpar, mpar, text)
        
        
    def write(self, newFilename=None, format=None):
        """write will output the new file with prepended catalog and header records
        
        Raises an IOError if no new catalog or header records to prepend
        
        Inputs:
        
            newFilename - if None, overwrite original file
            
            format - Cedar format to use.  If None, use same format as original file.
        """
        if self._catalog == None and self._header == None:
            raise IOError, 'Does not make sense to save a new file if no catalog or header has been added'
        
        if self._header != None:
            self._cedar.insert(0, self._header)
            
        if self._catalog != None:
            self._cedar.insert(0, self._catalog)
            
        if newFilename == None:
            newFilename = self._madFilename
            
        if format == None:
            format = self._fileType
            
        self._cedar.write(format, newFilename)
        
        
    
    def _createMaxMinSummaryLines(self):
        """_createMaxMinSummaryLines is a private method that creates the max and min summary 
        lines (e.g., alt, gdlat, etc)
        """
        alt1 = 'ALT1 %11i km. Lowest altitude measured'
        alt2 = 'ALT2 %11i km. Highest altitude measured'
        lat1 = 'GGLAT1    %6i degrees. Lowest geographic latitude measured'
        lat2 = 'GGLAT2    %6i degrees. Highest geographic latitude measured'
        lon1 = 'GGLON1    %6i degrees. Westmost geographic longitude measured'
        lon2 = 'GGLON2    %6i degrees. Eastmost geographic longitude measured'
        pl1 = 'PL1  %11i Shortest radar pulse length'
        pl2 = 'PL2  %11i Longest radar pulse length'
        
        retStr = ''
        
        minAlt = self._summary.getMinValidAltitude()
        maxAlt = self._summary.getMaxValidAltitude()
        if minAlt > 0 and minAlt < 1E9:
            retStr += alt1 % (int(minAlt))
            retStr = self._padStr(retStr, self._lineLen)
            retStr += alt2 % (int(maxAlt))
            retStr = self._padStr(retStr, self._lineLen)
            
        minLat = self._summary.getMinLatitude()
        maxLat = self._summary.getMaxLatitude()
        if minLat > -91 and minLat < 91:
            retStr += lat1 % (int(minLat))
            retStr = self._padStr(retStr, self._lineLen)
            retStr += lat2 % (int(maxLat))
            retStr = self._padStr(retStr, self._lineLen)
            
        minLon = self._summary.getMinLongitude()
        maxLon = self._summary.getMaxLongitude()
        if minLon > -181 and minLon < 360:
            retStr += lon1 % (int(minLon))
            retStr = self._padStr(retStr, self._lineLen)
            retStr += lon2 % (int(maxLon))
            retStr = self._padStr(retStr, self._lineLen)
            
        minPl = self._summary.getMinPulseLength()
        maxPl = self._summary.getMaxPulseLength()
        if minPl > 0.001 and minPl < 10E9:
            retStr += pl1 % (int(minPl))
            retStr = self._padStr(retStr, self._lineLen)
            retStr += pl2 % (int(maxPl))
            retStr = self._padStr(retStr, self._lineLen)
            
        return(retStr)

    
    def _createCedarLines(self, prefix, text):
        """_createCedarLines is a private method that returns a string which is a multiple of 
        80 characters (no line feeds) where each 80 character block starts with prefix,then a space, 
        and then the next part of text that fits on line, padded with spaces
        """
        lineLen = self._lineLen
        if len(prefix) > self._lineLen/2:
            raise IOError, 'Too long prefix %s' % (str(prefix))
        retStr = ''
        
        # first check for line feeds
        lines = text.split('\n')
        for line in lines:
            # now split by words
            words = line.split()
            for word in words:
                # see if this word can fit on one line
                if len(word) + 1 > lineLen - len(prefix) + 1:
                    raise IOError, 'Can not fit the word <%s> in a Cedar text record' % (word)
                # see if there's room for this word
                if (lineLen - (len(retStr) % lineLen) <= len(word) + 1) or \
                (len(retStr) % lineLen == 0):
                    retStr = self._padStr(retStr, lineLen)
                    retStr += '%s ' % (prefix)
                retStr += '%s ' % (word)
            # at line break, we always pad
            retStr = self._padStr(retStr, lineLen)
            
        return(retStr)
    
    def _padStr(self, thisStr, lineLen):
        """_padStr is a private method that pads a string with spaces so its length is module lineLen
        """
        spacesToPad = lineLen - (len(thisStr) % lineLen)
        if spacesToPad == lineLen:
            return(thisStr)
        thisStr += ' ' * spacesToPad
        return(thisStr)
        
            
                    

    


class CedarParameter:
    """CedarParameter is a class with attributes code, mnemonic, and description"""
    
    def __init__(self, code, mnemonic, description):
        self.code = int(code)
        self.mnemonic = str(mnemonic)
        self.description = str(description)

    def __str__(self):
        return('%6i: %20s: %s' % (self.code, self.mnemonic, self.description))
    


def compareParms(firstParm, secondParm):
        """compareParms is used internally by getHeaderKodLines to order the parameters

        Inputs:

            firstParm - tuple of (code, parameter description, scaleFactor, units) for
                        the first parameter

            secondParm - tuple of (code, parameter description, scaleFactor, units) for
                         the second parameter

        Returns - -1 if first<second, 0 if first=second, 1 if first>second.  First compare
                  abs(code).  If equal, positive smaller than negative to make error follow
                  main parm.
        """
        result = cmp(abs(firstParm[0]), abs(secondParm[0]))
        if result != 0:
            return(result)
        return(cmp(secondParm[0], firstParm[0]))
    
    

        
if __name__ == '__main__':

    cedarObj = MadrigalCedarFile('/home/grail/brideout/madroot/experiments/1998/mlh/20jan98/mlh980120g.001')

    print 'len of cedarObj is %i' % (len(cedarObj))

    for i in range(2):
        print cedarObj[i]
    
    cedarObj.write('Madrigal', '/home/grail/brideout/junk.001')

    newCedarObj = MadrigalCedarFile('/home/grail/brideout/madroot/experiments/1998/mlh/20jan98/mlh980120g.002', True)

    dataObj = MadrigalDataRecord(31,
                 1000,
                 2001,1,1,0,0,0,0,
                 2001,1,1,0,2,59,99,
                 ['azm', 'elm', 'rgate'],
                 ['range', 'ti', 'drange'],
                 4)

    dataObj.set1D('azm',45.0)
    dataObj.set1D('elm',85.0)
    dataObj.set2D(-120,2,'assumed')

    newCedarObj.append(dataObj)
    
    print len(newCedarObj)

    print newCedarObj

    print dataObj.get1D('azm')
    print dataObj.get1D('elm')
    print dataObj.get1D('rgate')

    print dataObj.get2D('range', 2)
    print dataObj.get2D('drange', 2)

    dataObj.set2D('range', 0, 100)
    dataObj.set2D('range', 1, 150)
    dataObj.set2D('range', 2, 200)
    dataObj.set2D('range', 3, 250)

    print 'kinst is %i' % (dataObj.getKinst())
    print 'kindat is %i' % (dataObj.getKindat())

    oneDList = dataObj.get1DParms()
    print 'The following are 1D parms:'
    for parm in oneDList:
        print parm

    print 'now removing 1d rgate'
    dataObj.delete1D('rgate')

    oneDList = dataObj.get1DParms()
    print 'The following are now the 1D parms:'
    for parm in oneDList:
        print parm

    twoDList = dataObj.get2DParms()
    print '\nThe following are 2D parms:'
    for parm in twoDList:
        print parm

    print dataObj

    print 'now deleting drange'
    dataObj.delete2DParm('drange')

    print dataObj

    print 'now deleting 2nd and 3rd row'
    dataObj.delete2DRows((1,2))

    print dataObj
