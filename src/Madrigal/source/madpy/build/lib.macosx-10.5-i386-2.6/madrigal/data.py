"""data is the module that interfaces to madrigal data files, or to Cedar standards about data.

This module includes the api to get information from a single madrigal file, and the api to
get information about the Cedar standard (such as parameter and category definitions).  It is
built on top of the Madrigal C API as found in libmadrec.so through the python extension class
__Madrec

$Id: data.py,v 1.39 2009/04/29 15:55:52 brideout Exp $
"""

import os, sys
import StringIO
import types
import copy
import time
import gc

# the math library is imported directly into the namespace so
#   that users can call those methods directly 
from math import *

import madrigal._Madrec
import madrigal.metadata
import madrigal.admin
import madrigal.ui.web

class MadrigalFile:
    """MadrigalFile is an object that provides access to information in a single Madrigal File.

    This object provides access to a single Madrigal file.  

    Usage example:

        import os, madrigal.data
    
        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

        test = madrigal.data.MadrigalFile(filepath)

        print test.toString()

        print test.getMaxValidAltitude()

    Non-standard Python modules used:
    
    None

    MadrigalError exception thrown if:
    
        1.  No data records found in file

    Notes:  This class depends on the python extension class madrigal._Madrec, which in turn depends on the madrigal
    C API as implemented in libmadrec.so.  When Madrigal is installed, this file is put in $madroot/lib.  If madroot
    is changed, this file can still be located by python by setting the LD_LIBRARY_PATH to the new location of
    libmadrec.so.

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 26, 2001

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu June 4, 2002 to use summary information in header records
    if available.

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu Jan 24, 2003 to use the high level maddata module.

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu Jan 24, 2003 to use overview/[filename].summary file
    if available.

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu Feb 15, 2005 to add more summary data.
    """

    # getSummary return argument positions (from getSummary in _Madrec.c)
    __kinstListPosition        = 0
    __kindatListPosition       = 1
    __parameterListPosition    = 2
    __missingListPosition      = 3
    __maxPulseLenPosition      = 4
    __minPulseLenPosition      = 5
    __maxValidAltitudePosition = 6
    __minValidAltitudePosition = 7
    __maxLatitudePosition      = 8
    __minLatitudePosition      = 9
    __maxLongitudePosition     = 10
    __minLongitudePosition     = 11
    __earliestTimePosition     = 12
    __latestTimePosition       = 13
    __param1dListPosition      = 14
    __param2dListPosition      = 15
    
    # cedar special values
    missingVal  = 1.e-38
    assumedVal  = 2.e-38
    knownbadVal = 3.e-38
    

    def __init__(self, initFile, madDB=None):
        """__init__ initializes MadrigalFile by finding all summary data.

        Inputs: self, String representing the full path to the madrigal file.

            Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__summary, which is a list of summary data about the file.  The
        information is first searched for in the header records, and if not found, then the file
        overview/[filename].summary is used.  If that also fails, the file is analyzed using
        _Madrec.getSummary, which will write its results to overview/[filename].summary.  All
        public functions simply return this summarized data.

        Exceptions: MadrigalError thrown if no data record in file.
        """
        import time

        # get metadata dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        self.__filename = initFile

        # read metadata about instrument
        self.__instMetadata = madrigal.metadata.MadrigalInstrument(self.__madDB)

        # read metadata about files
        self.__fileMetadata = madrigal.metadata.MadrigalMetaFile(self.__madDB)
        self.__fileCategory = self.__fileMetadata.getCategoryByFilename(self.__filename)
            
        # first try to get file summary data from catalog and header records, or
        # from overview/[filename].summary
        tempSummary = self.__getExistingSummary()
        usingHeader = 1

        # if tempSummary = None, revert to complete file read via _Madrec.getSummary
        if tempSummary == None:
            usingHeader = 0
            tempSummary = madrigal._Madrec.getSummary(self.__filename)

        # copy returned information into self.__summary list
        self.__summary = []

        if usingHeader:
            # append kinst list as a list of integers
            kinstList = []
            # verify list not empty
            if len(tempSummary[self.__kinstListPosition]) > 0:
                # split comma separated string into individual integers
                kinstStrList = tempSummary[self.__kinstListPosition].split(',')
                for kinst in kinstStrList:
                    kinstList.append(int(kinst))
            # append kinstList to self.__summary
            self.__summary.append(kinstList)

            # append kindat list as a list of integers
            kindatList = []
            # verify list not empty
            if len(tempSummary[self.__kindatListPosition]) > 0:
                # split comma separated string into individual integers
                kindatStrList = tempSummary[self.__kindatListPosition].split(',')
                for kindat in kindatStrList:
                    kindatList.append(int(kindat))
            # append kindatList to self.__summary
            self.__summary.append(kindatList)

            # append parm list as a list of integers
            parmList = []
            # verify list not empty
            if len(tempSummary[self.__parameterListPosition]) > 0:
                # split comma separated string into individual integers
                parmStrList = tempSummary[self.__parameterListPosition].split(',')
                for parm in parmStrList:
                    parmList.append(int(parm))
            # append parmList to self.__summary
            self.__summary.append(parmList)

            # append missing list as a list of integers (1 if missing, -1 if never missing)
            missingList = []
            tmpList = [] # list of missing parms, if any
            # remove possible spaces
            tmpStr = tempSummary[self.__missingListPosition].replace(' ', '')
            # split comma separated string into individual integers
            missingStrList = tmpStr.split(',')
            for parm in missingStrList:
                if len(parm) == 0:
                    continue
                tmpList.append(int(parm))
            for parm in parmList:
                if parm in tmpList:
                    missingList.append(1)
                else:
                    missingList.append(-1)
            # append missingList to self.__summary
            self.__summary.append(missingList)

        else: # list already in right form
            # append kinstList to self.__summary
            self.__summary.append(tempSummary[self.__kinstListPosition])
            # append kindatList to self.__summary
            self.__summary.append(tempSummary[self.__kindatListPosition])
            # append parmList to self.__summary
            self.__summary.append(tempSummary[self.__parameterListPosition])
            # append missingList to self.__summary
            self.__summary.append(tempSummary[self.__missingListPosition])

            
        # append maximum pulse length with units of seconds
        if usingHeader:
            self.__summary.append(float(tempSummary[self.__maxPulseLenPosition])/1000000.0)
        else:
            self.__summary.append(float(tempSummary[self.__maxPulseLenPosition]))

        # append minimum pulse length with units of seconds
        if usingHeader:
            self.__summary.append(float(tempSummary[self.__minPulseLenPosition])/1000000.0)
        else:
            self.__summary.append(float(tempSummary[self.__minPulseLenPosition]))

        # append maximum valid altitude
        self.__summary.append(float(tempSummary[self.__maxValidAltitudePosition]))

        # append minimum valid altitude
        self.__summary.append(float(tempSummary[self.__minValidAltitudePosition]))

        # append maximum valid latitude
        self.__summary.append(float(tempSummary[self.__maxLatitudePosition]))

        # append minimum valid latitude
        self.__summary.append(float(tempSummary[self.__minLatitudePosition]))

        # append maximum valid longitude
        self.__summary.append(float(tempSummary[self.__maxLongitudePosition]))

        # append minimum valid longitude
        self.__summary.append(float(tempSummary[self.__minLongitudePosition]))

        if usingHeader:
            # append earliest time as a list of integers
            earliestList = []
            # verify list not empty
            if len(tempSummary[self.__earliestTimePosition]) > 0:
                # split comma separated string into individual integers
                earliestStrList = tempSummary[self.__earliestTimePosition].split(',')
                for time in earliestStrList:
                    earliestList.append(int(time))
            # append earliestList to self.__summary
            self.__summary.append(earliestList)

            # append latest time as a list of integers
            latestList = []
            # verify list not empty
            if len(tempSummary[self.__latestTimePosition]) > 0:
                # split comma separated string into individual integers
                latestStrList = tempSummary[self.__latestTimePosition].split(',')
                for time in latestStrList:
                    latestList.append(int(time))
            # append latestList to self.__summary
            self.__summary.append(latestList)

        else: # already in right format
            # append earliestList to self.__summary
            self.__summary.append(tempSummary[self.__earliestTimePosition])
            # append latestList to self.__summary
            self.__summary.append(tempSummary[self.__latestTimePosition])

        if usingHeader:
            # append parm1d list as a list of integers
            parm1dList = []
            # verify list not empty
            if len(tempSummary[self.__param1dListPosition]) > 0:
                # split comma separated string into individual integers
                parmStrList = tempSummary[self.__param1dListPosition].split(',')
                for parm in parmStrList:
                    parm1dList.append(int(parm))
            # append parm1dList to self.__summary
            self.__summary.append(parm1dList)

            # append parm2d list as a list of integers
            parm2dList = []
            # verify list not empty
            if len(tempSummary[self.__param2dListPosition]) > 0:
                # split comma separated string into individual integers
                parmStrList = tempSummary[self.__param2dListPosition].split(',')
                for parm in parmStrList:
                    parm2dList.append(int(parm))
            # append parm1dList to self.__summary
            self.__summary.append(parm2dList)
            
        else:
            # append parm1dList to self.__summary
            self.__summary.append(tempSummary[self.__param1dListPosition])
            # append parm2dList to self.__summary
            self.__summary.append(tempSummary[self.__param2dListPosition])

            # write summary file to avoid this step in the future
            self.__writeSummary()
            
	# end init
        

    def getKinstList(self):
        """getKinstList returns a list of integers of all kinst values in file.

        Inputs: self
        
        Returns: a list of integers of all kinst values in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__kinstListPosition]
    

    def getKinstListStr(self):
        """getKinstListStr returns a comma-separated string with the names of kinst values in file.

        Inputs: self
        
        Returns: a comma-separated string with the names of kinst values in file.

        Affects: Nothing

        Exceptions: None
        """

        kinstList = self.__summary[self.__kinstListPosition]

        kinstStr = ''
        
        # first kinst has no preceeding comma
        isFirstKinst = 1
        
        for inst in kinstList:
            if not isFirstKinst:
                kinstStr = kinstStr + ', '
            kinstStr = kinstStr + self.__instMetadata.getInstrumentName(inst)
            isFirstKinst = 0
            
        return kinstStr


    def getKindatList(self):
        """getKindatList returns a list of integers of all kindat values in file.

        Inputs: self
        
        Returns: a list of integers of all kindat values in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__kindatListPosition]


    def getMeasuredParmList(self):
        """getMeasuredParmList returns a list of integers of all parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__parameterListPosition]


    def getMeasured1dParmList(self):
        """getMeasured1dParmList returns a list of integers of all 1d parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all 1d parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__param1dListPosition]
    

    def getMeasured2dParmList(self):
        """getMeasured2dParmList returns a list of integers of all 2d parameters stored in file.

        Inputs: self
        
        Returns: a list of integers of all 2d parameters found in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__param2dListPosition]


    def getMissingParmList(self):
        """getMissingParmList returns a list of integers, one for each parameters stored in file.

        Inputs: self
        
        Returns: a list of integers, one for each parameters stored in file. If 1, that parameter
        was found to missing from at least one record in the file.  If -1, not missing in any
        data record.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__missingListPosition]
    

    def getMeasDervBothParmLists(self, parmList, measParmList, derivedParmList, allParmList, sureParmList):
        """getMeasDervBothParmLists sets up four lists: measured parms, derived parms, both, and sure parms given a parm list to verify.

        Inputs: parmList: A list of parameters (integers or mnemonics to be considered)

            measParmList: an empty python list.  Will be filled with an list of all measured parameters (mnemonics)
            found in file when function returns.

            derivedParmList: an empty python list.  Will be filled with an list of all parameters (mnemonics) in parmList
            that can be derived from file when function returns.

            allParmList: an empty python list.  Will be filled with an list of all parameters in
            measParmList or derivedParmList when function returns.

            sureParmList: an empty python list.  Will be filled with an list of all parameters from the measured list
            that are never missing, and parameters that can be derived from those.  These parameters can then be derived
            for every record (excluding the fact that the value of the parameter in the record may be "missing").
        
        Returns: void (see Affects below)

        Affects: adds items to measParmList, derivedParmList, and allParmList.  All items will be mnemonics.

        Exceptions: None

        Usage example:

            import os, madrigal.data

            filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

            test = madrigal.data.MadrigalFile(filepath)

            measParmList = []
            
            derivedParmList = []
            
            allParmList = []

            sureParmList = []


            test.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Short'),
                                          measParmList,
                                          derivedParmList,
                                          allParmList,
                                          sureParmList)
                                          
            #print lists
            
            print 'Measured parms are: ' + str(measParmList)
            
            print 'Derived parms are: ' + str(derivedParmList)
            
            print 'All good parms are: ' + str(allParmList)

            print 'Parameters sure to exist are: ' + str(sureParmList)
        """

        #create needed MadrigalParameters object:
        madParmObj = MadrigalParameters(self.__madDB)

        # be sure input is in the form of mnemonics
        mnemParmList = madParmObj.getParmMnemonicList(parmList)

        # get list of parameters in file in mnemonic form
        fileParmList = madParmObj.getParmMnemonicList(self.__summary[self.__parameterListPosition])

        # divide all parameters into either measured (and all) list, or to be verified list
        # every parameter in file is put in measList
        toBeVerifiedList = []

        # also create of list of measured parameters that were never missing
        sureMeasParmList = []

        for parm in mnemParmList:
            if parm not in fileParmList:
                toBeVerifiedList.append(parm)
        
        # add measured parameters to measParmList and tempAllList
        index = 0
        for parm in fileParmList:
            measParmList.append(parm)
            allParmList.append(parm)
            if self.__summary[self.__missingListPosition][index] == -1:
                sureMeasParmList.append(parm)
            index += 1

        # get list of derivable parameters
        derivableParmsList = madrigal._Madrec.getDerivableParms(fileParmList)

        # get list of sure derivable parameters
        sureDerivableParmsList = madrigal._Madrec.getDerivableParms(sureMeasParmList)
            
        for item in toBeVerifiedList:
            if item in derivableParmsList:
                derivedParmList.append(item)
                allParmList.append(item)

        # populate sureParmList
        for parm in sureMeasParmList:
            sureParmList.append(parm)

        for parm in sureDerivableParmsList:
            if parm not in sureParmList:
                sureParmList.append(parm)

        

    def getMaxPulseLength(self):
        """getMaxPulseLength returns a double representing maximum pulse length in microseconds in file.

        Inputs: self
        
        Returns: a double representing maximum pulse length in seconds in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__maxPulseLenPosition]

    def getMinPulseLength(self):
        """getMinPulseLength returns a double representing minimum pulse length in microseconds in file.

        Inputs: self
        
        Returns: a double representing minimum pulse length in seconds in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__minPulseLenPosition]


    def getMaxValidAltitude(self):
        """getMaxValidAltitude returns a double representing maximum valid altitude in km in file.

        Inputs: self
        
        Returns: a double representing maximum valid altitude in km in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__maxValidAltitudePosition]


    def getMinValidAltitude(self):
        """getMinValidAltitude returns a double representing minimum valid altitude in km in file.

        Inputs: self
        
        Returns: a double representing minimum valid altitude in km in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__minValidAltitudePosition]


    def getMaxLatitude(self):
        """getMaxLatitude returns a double representing maximum latitude in degrees in file.

        Inputs: self
        
        Returns: a double representing maximum latitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__maxLatitudePosition]
    

    def getMinLatitude(self):
        """getMinLatitude returns a double representing minimum latitude in degrees in file.

        Inputs: self
        
        Returns: a double representing minimum latitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__minLatitudePosition]


    def getMaxLongitude(self):
        """getMaxLongitude returns a double representing maximum longitude in degrees in file.

        Inputs: self
        
        Returns: a double representing maximum longitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__maxLongitudePosition]
    

    def getMinLongitude(self):
        """getMinLongitude returns a double representing minimum longitude in degrees in file.

        Inputs: self
        
        Returns: a double representing minimum longitude in degrees in file.

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__minLongitudePosition]


    def getEarliestTime(self):
        """getEarliestTime returns a list of 6 numbers representing the earliest time in the file.

        Inputs: self
        
        Returns: a list of 6 numbers representing the earliest time in the file.  The format is
                [Year, Month, Day, Hour, Minute, Second]

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__earliestTimePosition]


    def getLatestTime(self):
        """getLatestTime returns a list of 6 numbers representing the latest time in the file.

        Inputs: self
        
        Returns: a list of 6 numbers representing the latest time in the file.  The format is
                [Year, Month, Day, Hour, Minute, Second]

        Affects: Nothing

        Exceptions: None
        """

        return self.__summary[self.__latestTimePosition]


    def getCatalogHeaderStr(self):
        """getCatalogHeaderStr returns a string formatted for printing containing all catalog and header records.

        Input: None

        Returns: a string formatted for printing containing all catalog and header records. Returns '' if no
        catalog or header records.
        """
        retStr = ''
        
        catList, headList = madrigal._Madrec.cedarCatalogHeaderList(self.__filename)

        for cat in catList:
            retStr += 'Catalog information from record %i:\n\n' % (cat)
            retStr += madrigal._Madrec.cedarGetInformation(self.__filename, cat)
            retStr += '\n'

        for head in headList:
            retStr += 'Header information from record %i:\n\n' % (head)
            retStr += madrigal._Madrec.cedarGetInformation(self.__filename, head)
            retStr += '\n'

        return retStr
        
    
        

    def toString(self):
        """toString returns a simple string representation of a MadrigalFile object.

        Inputs: None
        
        Returns: String describing a simple representation of a MadrigalFile object.

        Affects: Nothing

        Exceptions: None
        """

        output =  "Object type: MadrigalFile\n"
        output += "Filename = "                     + self.__filename + "\n"
        output += "Kinst list = "                   + str(self.getKinstList()) + "\n"
        output += "Kindat list = "                  + str(self.getKindatList()) + "\n"
        output += "Measured parm list = "           + str(self.getMeasuredParmList()) + "\n"
        output += "Missing parm list = "            + str(self.getMissingParmList()) + "\n"
        output += "Maximum pulse length (sec) = "   + str(self.getMaxPulseLength()) + "\n"
        output += "Minimum pulse length (sec) = "   + str(self.getMinPulseLength()) + "\n"
        output += "Maximum valid altitude (km) = "  + str(self.getMaxValidAltitude()) + "\n"
        output += "Minimum valid altitude (km) = "  + str(self.getMinValidAltitude()) + "\n"
        output += "Maximum latitude (degrees) = "   + str(self.getMaxLatitude()) + "\n"
        output += "Minimum latitude (degrees) = "   + str(self.getMinLatitude()) + "\n"
        output += "Maximum longitude (degrees) = "  + str(self.getMaxLongitude()) + "\n"
        output += "Minimum longitude (degrees) = "  + str(self.getMinLongitude()) + "\n"
        output += "Earliest time list = "           + str(self.getEarliestTime()) + "\n"
        output += "Latest time list = "             + str(self.getLatestTime()) + "\n"
        output += "Kinst string = "                 + self.getKinstListStr() + "\n"
        output += "Measured 1d parm list = "        + str(self.getMeasured1dParmList()) + "\n"
        output += "Measured 2d parm list = "        + str(self.getMeasured2dParmList()) + "\n"

        return output

    def __str__(self):
        return(self.toString())


    def __getExistingSummary(self):
        """__getExistingSummary returns a list of strings summarizing a file via header/cat records or overview file if possible.

        If all the required information is not found in the first two records of the file, the file
        overview/[filename].summary is used.  If that fails, returns None.

        Inputs: None
        
        Returns: A list of strings summarizing the MadrigalFile.  These strings are:
                    kinstList: a string of comma separated integers
                    kindatList: a string of comma separated integers
                    parameterList: a string of comma separated integers
                    missingList: a string of comma separated integers
                    maxPulseLen: a single integer string
                    minPulseLen: a single integer string
                    maxValidAltitude: a single integer string
                    minValidAltitude: a single integer string
                    maxLatitude: a single integer string
                    minLatitude: a single integer string
                    maxLongitude: a single integer string
                    minLongitude: a single integer string
                    earliestTime: a string of six comma sepatated integers:
                        [year, month, day, hour, min, sec]
                    latestTime: a string of six comma sepatated integers:
                        [year, month, day, hour, min, sec]
                    param1dList: a string of comma separated integers
                    param2dList: a string of comma separated integers

                If all required information not found in either source, returns None.  The following
                items are not required, and default to missingVal or empty: minPulseLen, minValidAltitude,
                maxLatitude, minLatitude, maxLongitude, minLongitude, param1dList, param2dList

        Affects: Nothing

        Exceptions: None
        """
        
        # try to read header and catalog records from record
        catData = madrigal._Madrec.cedarGetInformation(self.__filename, 0)
        
        headerData = madrigal._Madrec.cedarGetInformation(self.__filename, 1)

        # parse using combined data, if any
        if catData != None and headerData != None:
            if len(catData) > 0 and len (headerData) > 0:
               tempSummary = self.__parseSummary(catData + headerData)
               if tempSummary != None:
                   return tempSummary

        # if failure, try summary file
        summaryFilename = os.path.dirname(self.__filename) + '/overview/'
        summaryFilename += os.path.basename(self.__filename) + '.summary'
        if os.path.exists(summaryFilename):
            # check that summaryFile is newer than data file, otherwise it might be out of date
            if os.stat(summaryFilename).st_mtime < os.stat(self.__filename).st_mtime:
                return None
            summaryFile = open(summaryFilename)
            summaryStr = summaryFile.read()
            summaryFile.close()
            tempSummary = self.__parseSummary(summaryStr)
            return tempSummary
        else:
            return None



    def __parseSummary(self, summaryStr):
        """__parseSummary returns a list of strings summarizing a string if possible.

        If all the required information is not found in the string, returns None. 

        Inputs: summaryStr read from header records or summary file.
        
        Returns: A list of strings summarizing the MadrigalFile.  These strings are:
                    kinstList: a string of comma separated integers
                    kindatList: a string of comma separated integers
                    parameterList: a string of comma separated integers
                    missingList: a string of comma separated integers
                    maxPulseLen: a single integer string
                    minPulseLen: a single integer string
                    maxValidAltitude: a single integer string
                    minValidAltitude: a single integer string
                    maxLatitude: a single integer string
                    minLatitude: a single integer string
                    maxLongitude: a single integer string
                    minLongitude: a single integer string
                    earliestTime: a string of six comma sepatated integers:
                        [year, month, day, hour, min, sec]
                    latestTime: a string of six comma sepatated integers:
                        [year, month, day, hour, min, sec]
                    param1dList: a string of comma separated integers
                    param2dList: a string of comma separated integers

                If all required information not found in either source, returns None.  The following
                items are not required, and default to missingVal or empty list: minPulseLen, minValidAltitude,
                maxLatitude, minLatitude, maxLongitude, minLongitude, param1dList, param2dList

        Affects: Nothing

        Exceptions: None
        """

        kinstList            = ''
        kindatList           = ''
        parameterList        = ''
        missingList          = None   # indicates failure, 0 length string means success
        maxPulseLen          = ''
        minPulseLen          = ''
        maxValidAltitude     = ''
        minValidAltitude     = ''
        maxLatitude          = ''
        minLatitude          = ''
        maxLongitude         = ''
        minLongitude         = ''
        earliestYear         = ''
        earliestMonth        = ''
        earliestDay          = ''
        earliestHour         = ''
        earliestMin          = ''
        earliestSec          = ''
        earliestTime         = ''
        latestYear           = ''
        latestMonth          = ''
        latestDay            = ''
        latestHour           = ''
        latestMin            = ''
        latestSec            = ''
        latestTime           = ''
        param1dList          = ''
        param2dList          = ''

        
        # split string into lists of strings by separating lines
        summaryStrLineList = summaryStr.split('\n')

        # finally break each line into a list of words
        summaryList = []

        for line in summaryStrLineList:
            summaryList.append(line.split())

        # get info
        for line in summaryList:

            # ignore empty lines
            if len(line) == 0:
                continue

            #kinst
            if line[0].lower() == 'kinste':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if kinstList == '':
                    kinstList = line[1]
                else:
                    kinstList += ',' + line[1]

            # max pulse length
            if line[0].lower() == 'pl2':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if maxPulseLen == '':
                    maxPulseLen = maxPulseLen + line[1]

            # min pulse length
            if line[0].lower() == 'pl1':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if minPulseLen == '':
                    minPulseLen = minPulseLen + line[1]

            # max valid altitude
            if line[0].lower() == 'alt2':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if maxValidAltitude == '':
                    maxValidAltitude = maxValidAltitude + line[1]

            # min valid altitude
            if line[0].lower() == 'alt1':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if minValidAltitude == '':
                    minValidAltitude = minValidAltitude + line[1]

            # max latitude
            if line[0].lower() == 'gglat2':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if maxLatitude == '':
                    maxLatitude = maxLatitude + line[1]

            # min latitude
            if line[0].lower() == 'gglat1':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if minLatitude == '':
                    minLatitude = minLatitude + line[1]

            # max longitude
            if line[0].lower() == 'gglon2':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if maxLongitude == '':
                    maxLongitude = maxLongitude + line[1]

            # min longitude
            if line[0].lower() == 'gglon1':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if minLongitude == '':
                    minLongitude = minLongitude + line[1]

            # date
            if line[0].lower() == 'ibyre':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if earliestYear == '':
                    earliestYear = earliestYear + line[1]

            if line[0].lower() == 'ieyre':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if latestYear == '':
                    latestYear = latestYear + line[1]

            if line[0].lower() == 'ibdte':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if earliestMonth == '':
                    earliestMonth = earliestMonth + line[1][:-2]
                    earliestDay = earliestDay + line[1][-2:]

            if line[0].lower() == 'iedte':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if latestMonth == '':
                    latestMonth = latestMonth + line[1][:-2]
                    latestDay = latestDay + line[1][-2:]

            if line[0].lower() == 'ibhme':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if earliestHour == '':
                    earliestHour = earliestHour + line[1][:-2]
                    if len(earliestHour) == 0:
                        earliestHour = '0'
                    earliestMin = earliestMin + line[1][-2:]

            if line[0].lower() == 'iehme':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if latestHour == '':
                    latestHour = latestHour + line[1][:-2]
                    if len(latestHour) == 0:
                        latestHour = '0'
                    latestMin = latestMin + line[1][-2:]

            if line[0].lower() == 'ibcse':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if earliestSec == '':
                    earliestSec = earliestSec + line[1][:-2]
                    if len(earliestSec) == 0:
                        earliestSec = '0'

            if line[0].lower() == 'iecse':
                try:
                    # test if its an integer
                    int(line[1])
                except:
                    return None
                if latestSec == '':
                    latestSec = latestSec + line[1][:-2]
                    if len(latestSec) == 0:
                        latestSec = '0'


            #kindat
            if line[0].lower() == 'kindat':
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                if kindatList == '':
                    # kindat is third item in list
                    kindatList = kindatList + line[2]
		else:
		    kindatList = kindatList + ',' + line[2]

            # parameters
            if line[0][0:3].lower() == 'kod':
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                # check if this is first parameter, otherwise add comma
                # code is third item in list
                if len(parameterList) == 0:
                    parameterList = parameterList + line[2]
                else:
                    parameterList = parameterList + ', ' + line[2]

            if line[0][0:4].lower() == 'kods':
                # 1d parameter
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                # check if this is first parameter, otherwise add comma
                # code is third item in list
                if len(param1dList) == 0:
                    param1dList = param1dList + line[2]
                else:
                    param1dList = param1dList + ', ' + line[2]

            if line[0][0:4].lower() == 'kodm':
                # 2d parameter
                try:
                    # test if its an integer
                    int(line[2])
                except:
                    return None
                # check if this is first parameter, otherwise add comma
                # code is third item in list
                if len(param2dList) == 0:
                    param2dList = param2dList + line[2]
                else:
                    param2dList = param2dList + ', ' + line[2]

            # missing parameters
            if line[0].lower() in ('cmissing', 'missing'):
                # line may be empty
                if missingList == None:
                    missingList = ''
                # simply append rest of line
                if missingList != '':
                    # more than one line, add a comma
                    missingList += ','
                for word in line[1:]:
                    missingList += word


        # create date strings
        delimiter = ', '
        earliestTime = delimiter.join((earliestYear,
                                       earliestMonth,
                                       earliestDay,
                                       earliestHour,
                                       earliestMin,
                                       earliestSec))

        latestTime = delimiter.join((latestYear,
                                     latestMonth,
                                     latestDay,
                                     latestHour,
                                     latestMin,
                                     latestSec))

        # set any needed defaults
        if minPulseLen == '':
            minPulseLen = str(self.missingVal)

        if minValidAltitude == '':
            minValidAltitude = str(self.missingVal)

        if maxLatitude == '':
            maxLatitude = str(self.missingVal)

        if minLatitude == '':
            minLatitude = str(self.missingVal)

        if maxLongitude == '':
            maxLongitude = str(self.missingVal)

        if minLongitude == '':
            minLongitude = str(self.missingVal)

        # verify that all strings are complete
        if len(kinstList) != 0 and \
           len(kindatList) != 0 and \
           len(parameterList) != 0 and \
           missingList != None and \
           len(maxPulseLen) != 0 and \
           len(maxValidAltitude) != 0 and \
           len(earliestYear) != 0 and \
           len(earliestMonth) != 0 and \
           len(earliestDay) != 0 and \
           len(earliestHour) != 0 and \
           len(earliestMin) != 0 and \
           len(earliestSec) != 0 and \
           len(latestYear) != 0 and \
           len(latestMonth) != 0 and \
           len(latestDay) != 0 and \
           len(latestHour) != 0 and \
           len(latestMin) != 0 and \
           len(latestSec) != 0:
            return [kinstList,
                    kindatList,
                    parameterList,
                    missingList,
                    maxPulseLen,
                    minPulseLen,
                    maxValidAltitude,
                    minValidAltitude,
                    maxLatitude,
                    minLatitude,
                    maxLongitude,
                    minLongitude,
                    earliestTime,
                    latestTime,
                    param1dList,
                    param2dList]

        else:
            return None


    def __writeSummary(self):
        """__writeSummary writes a summary file to overview/[filename].summary.

        Uses data from self.__summary. 

        Inputs: None.
        
        Returns: None

        Affects: writes a summary file to overview/[filename].summary

        Exceptions: If file cannot be written
        """
        
        summaryFilename = os.path.dirname(self.__filename) + '/overview'

        # if overview directory does not yet exist, create it
        if os.path.exists(summaryFilename) == 0:
            try:
                os.mkdir(summaryFilename)
            except OSError:
                try:
                    adminObj =  madrigal.admin.MadrigalNotify()
                    adminObj.sendAlert('Unable to create summary file %s - please check permissions' % (summaryFilename), 'Madrigal permission error')
                except:
                    pass
                return
            os.chmod(summaryFilename, 0777)
        
        summaryFilename += '/' + os.path.basename(self.__filename) + '.summary'
        try:
            summaryFile = open(summaryFilename, 'w')
        except OSError:
            try:
                adminObj =  madrigal.admin.MadrigalNotify()
                adminObj.sendAlert('Unable to create summary file %s - please check permissions' % (summaryFilename), 'Madrigal permission error')
            except:
                pass
            return

        # write kinst
        for kinst in self.__summary[self.__kinstListPosition]:
            summaryFile.write('KINSTE  ' + str(kinst) + '\n')

        # write kindat
        for kindat in self.__summary[self.__kindatListPosition]:
            summaryFile.write('KINDAT  0  ' + str(kindat) + '\n')

        # write 1d parameters
        for parm in self.__summary[self.__param1dListPosition]:
            summaryFile.write('KODS  0  ' + str(parm) + '\n')

        # write 2d parameters
        for parm in self.__summary[self.__param2dListPosition]:
            summaryFile.write('KODM  0  ' + str(parm) + '\n')
            
        # write missing parameters - even if list empty
        summaryFile.write('CMISSING ')
        count = 0
        first = 1
        for parm in self.__summary[self.__missingListPosition]:
            if parm == 1:
                if first != 1:
                    summaryFile.write(',')
                first = 0
                summaryFile.write(str(self.__summary[self.__parameterListPosition][count]))
            count += 1
        summaryFile.write('\n')

        # write max pulse length in microseconds as an int
        # round up if needed
        if modf(self.__summary[self.__maxPulseLenPosition]*1000000)[0] > 0.0:
            pl2Str = str(1 + int(self.__summary[self.__maxPulseLenPosition]*1000000))
        else:
            pl2Str = str(int(self.__summary[self.__maxPulseLenPosition]*1000000))
        summaryFile.write('PL2     ' + pl2Str + '\n')

        # write min pulse length in microseconds as an int
        # round up if needed
        if modf(self.__summary[self.__minPulseLenPosition]*1000000)[0] > 0.0:
            pl1Str = str(1 + int(self.__summary[self.__minPulseLenPosition]*1000000))
        else:
            pl1Str = str(int(self.__summary[self.__minPulseLenPosition]*1000000))
        summaryFile.write('PL1     ' + pl1Str + '\n')

        # write max valid alt as an int
        # round up if needed
        if modf(self.__summary[self.__maxValidAltitudePosition])[0] > 0.0:
            alt2Str = str(1 + int(self.__summary[self.__maxValidAltitudePosition]))
        else:
            alt2Str = str(int(self.__summary[self.__maxValidAltitudePosition]))
        summaryFile.write('ALT2    ' + alt2Str + '\n')

        # write min valid alt as an int
        # round up if needed
        if modf(self.__summary[self.__minValidAltitudePosition])[0] > 0.0:
            alt1Str = str(1 + int(self.__summary[self.__minValidAltitudePosition]))
        else:
            alt1Str = str(int(self.__summary[self.__minValidAltitudePosition]))
        summaryFile.write('ALT1    ' + alt1Str + '\n')

        # write max latitude as an int
        # round up if needed
        if modf(self.__summary[self.__maxLatitudePosition])[0] > 0.0:
            gglat2Str = str(1 + int(self.__summary[self.__maxLatitudePosition]))
        else:
            gglat2Str = str(int(self.__summary[self.__maxLatitudePosition]))
        summaryFile.write('GGLAT2    ' + gglat2Str + '\n')

        # write min valid latitude as an int
        # round up if needed
        if modf(self.__summary[self.__minLatitudePosition])[0] > 0.0:
            gglat1Str = str(1 + int(self.__summary[self.__minLatitudePosition]))
        else:
            gglat1Str = str(int(self.__summary[self.__minLatitudePosition]))
        summaryFile.write('GGLAT1    ' + gglat1Str + '\n')

        # write max longitude as an int
        # round up if needed
        if modf(self.__summary[self.__maxLongitudePosition])[0] > 0.0:
            gglon2Str = str(1 + int(self.__summary[self.__maxLongitudePosition]))
        else:
            gglon2Str = str(int(self.__summary[self.__maxLongitudePosition]))
        summaryFile.write('GGLON2    ' + gglon2Str + '\n')

        # write min valid longitude as an int
        # round up if needed
        if modf(self.__summary[self.__minLongitudePosition])[0] > 0.0:
            gglon1Str = str(1 + int(self.__summary[self.__minLongitudePosition]))
        else:
            gglon1Str = str(int(self.__summary[self.__minLongitudePosition]))
        summaryFile.write('GGLON1    ' + gglon1Str + '\n')

        # write beginning time
        summaryFile.write('IBYRE   ' + str(self.__summary[self.__earliestTimePosition][0]) + '\n')
        md = 100*self.__summary[self.__earliestTimePosition][1] + self.__summary[self.__earliestTimePosition][2]
        summaryFile.write('IBDTE   ' + str(md) + '\n')
        hm = 100*self.__summary[self.__earliestTimePosition][3] + self.__summary[self.__earliestTimePosition][4]
        summaryFile.write('IBHME   ' + str(hm) + '\n')
        summaryFile.write('IBCSE   ' + str(100*self.__summary[self.__earliestTimePosition][5]) + '\n')

        # write ending time
        summaryFile.write('IEYRE   ' + str(self.__summary[self.__latestTimePosition][0]) + '\n')
        md = 100*self.__summary[self.__latestTimePosition][1] + self.__summary[self.__latestTimePosition][2]
        summaryFile.write('IEDTE   ' + str(md) + '\n')
        hm = 100*self.__summary[self.__latestTimePosition][3] + self.__summary[self.__latestTimePosition][4]
        summaryFile.write('IEHME   ' + str(hm) + '\n')
        summaryFile.write('IECSE   ' + str(100*self.__summary[self.__latestTimePosition][5]) + '\n')
        
        summaryFile.close()

        # set permissions wide open to avoid problems if possible
        try:
            os.chmod(summaryFilename, 0666)
        except:
            pass


    def __setToOne(self, x):
        """ Private function used to initialize a list to ones"""
        return 1


    


class MadrigalParameters:
    """MadrigalParameters is an object that provides information about Madrigal parameters.

    This class provides access to the Cedar/Madrigal standards for parameters
    (such as getMnemonic, getDescription, getCodeFromMnemonic) and
    categories.  It will also examine an expression (string) and return the parameter mnemonics it contains.

    Usage example:

        import madrigal.data.MadrigalParameters
    
        test = madrigal.data.MadrigalParameters()

        parcode = test.getParmCodeFromMnemonic("YEAR")

        print parcode

    Non-standard Python modules used:
    None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 27, 2001
    Added getMnemonicListFromExpression Jul. 16, 2002

    """


    def __init__(self, madDB = None):
        """__init__ initializes MadrigalParameters by getting some basic information from MadrigalDB.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__binDir.

        Exceptions: None.
        """

        # get metadata dir
        if madDB == None:
            thisMadDB = madrigal.metadata.MadrigalDB()
        else:
            thisMadDB = madDB

        self.__binDir = thisMadDB.getBinDir()
        self.__madDB  = thisMadDB



    def __sortList(self, parmList):
        """ Private function that returns a new list of parameters sorted as isprint sorts parameters. """

        # create new list with negative parameters replaced by abs(parm) + 0.5 if positive parameter in list,
	# otherwise insert it as 100000 + abs(parm) so it goes to the end of the list (this is how isprint works)
        sortList = []
        for parm in parmList:
            if parm < 0:
                if abs(parm) in parmList:
                    sortList.append(abs(parm) + 0.5)
                else:
                    sortList.append(100000 + abs(parm))
            else:
                sortList.append(parm)

        sortList.sort()

        # convert back to negative numbers
        listIndex = 0
        for parm in sortList:
            if type(parm) == types.FloatType:
                sortList[listIndex] = -1 * int(parm)
            elif parm > 100000:
                sortList[listIndex] = 100000 - parm
            listIndex = listIndex + 1

        return sortList


    def __reorder(self, validList, sortedParmList, parmList):
        """ Private function that returns a new valid list sorted in the same parameter order as parmList. """

        newValidList = []

        for parm in parmList:
            i = sortedParmList.index(parm)
            newValidList.append(validList[i])

        return newValidList


    def getParmType(self, mnemonic):
        """ getParmType returns 1 if mnemonic is a standard parameter, 0 if an error parameter, or -1 if not found.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer in string form)

        Returns: 1 if mnemonic is a standard parameter, 0 if an error parameter, or -1 if not found

        Affects: none

        Exceptions: If non-string passed in.
        """
        return madrigal._Madrec.madGetParType(mnemonic)


    def getParmScaleFactor(self, mnemonic):
        """ getParmScaleFactor returns scale factor as double of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: scale factor as double of given mnemonic

        Affects: none

        Exceptions: If mnemonic not found.
        """
        code = self.getParmCodeFromMnemonic(mnemonic)
        return madrigal._Madrec.cedarGetParScaleFactor(code)


   
    def getParmCodeFromMnemonic(self, mnemonic):
        """ getParmCodeFromMnemonic converts a string to the cedar code (integer).
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer in string form)

        Returns: integer (cedar code)

        Affects: none

        Exceptions: MadrigalError thrown if code not found.
        """
        # if its an integer in string form, return the integer
        try:
            retValue = int(mnemonic)
            return retValue
        except:
            pass
        
        retValue = madrigal._Madrec.cedarGetParCodeFromMnemonic(mnemonic)

        if retValue == -1:
            raise madrigal.admin.MadrigalError('Mnemonic: ' + str(mnemonic) + ' not a legal mnemonic.', None)

        return retValue


    def getParmCategory(self, parm):
        """ getParmCategory returns a category (String) given a cedar parameter (integer or mnemonic string).
        
        Inputs: a cedar code (integer)

        Returns: a category string

        Affects: none

        Exceptions: none
        """
        # if its a string form of an integer, convert it to an integer
        if type(parm) == types.StringType:
            try:
                int(parm)
                parm = int(parm)
            except ValueError:
                pass
            
        if type(parm) == types.StringType:
            retValue = madrigal._Madrec.madGetParMnemType(parm)
        else:
            retValue = madrigal._Madrec.cedarGetParCodeType(parm)

        return retValue


    def hasHtmlDesc(self, parm):
        """ hasHtmlDesc returns 1 if that parameter has a html description in parmDesc.html.
        
        Inputs: a Madrigal mnemonic (string) or parameter

        Returns: 1 if that parameter has a html description in parmDesc.html, 0 if not

        Affects: none

        Exceptions: none
        """
        # if its an integer, convert it to a mnemonic
        if type(parm) == types.IntType:
            parm = getParmMnemonic(parm)
            

        return madrigal._Madrec.madHasHtmlDesc(parm)


    def getParmDescription(self, parm):
        """ getParmDescription returns a description including units and possible links (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: a description string including units and possible links

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        retValue = madrigal._Madrec.madGetParDescription(parm)

        # append link to parmDesc.html if exists
        if self.hasHtmlDesc(parm):
            retValue += '<br><br>Click <a href=/madrigal/parmDesc.html#' + parm
            retValue += '>here</a> for a more detailed description of ' + parm + '.'

        return retValue


    def getSimpleParmDescription(self, parm):
        """ getSimpleParmDescription returns a description without units or links (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: a description string without units or links

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        retValue = madrigal._Madrec.madGetSimpleParDescription(parm).strip()

        return retValue


    def getParmUnits(self, parm):
        """ getParmUnits returns units  (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter (integer or mnemonic)

        Returns: units  (String)

        Affects: none

        Exceptions: none
        """
    
        # convert to mnemonic
        parm = self.getParmMnemonic(parm)
            
        retValue = madrigal._Madrec.madGetParUnits(parm).strip()

        return retValue


    def hasAddIncrement(self, parm):
        """hasAddIncrement returns True if parm has additional increment parameter, False otherwise
        
        Inputs: a parameter (integer or mnemonic)

        Returns: True if parm has additional increment parameter, False otherwise

        Affects: none

        Exceptions: none
        """
        # convert to code
        try:
            code = int(parm)
        except:
            code = self.getParmCodeFromMnemonic(parm)

        if code > 0:
            testCode = code + 1 # additional increment parameter always one higher
        else:
            testCode = code - 1

        desc = self.getSimpleParmDescription(testCode)

        if desc.lower().find('additional increment') != -1:
            return(True)
        else:
            return(False)


    def isAddIncrement(self, parm):
        """isAddIncrement returns True if parm is an additional increment parameter, False otherwise
        
        Inputs: a parameter (integer or mnemonic)

        Returns: True if parm is an additional increment parameter, False otherwise

        Affects: none

        Exceptions: none
        """
        # convert to code
        try:
            code = int(parm)
        except:
            code = self.getParmCodeFromMnemonic(parm)

        desc = self.getSimpleParmDescription(code)

        if desc.lower().find('additional increment') != -1:
            return(True)
        else:
            return(False)


    def getParmFormat(self, mnemonic):
        """ getParmFormat returns format string from parcods.tab of given mnemonic.
        
        Inputs: mnemonic:  the cedar mnemonic (string or integer)

        Returns: format string from parcods.tab of given mnemonic

        Affects: none

        Exceptions: If mnemonic not found.
        """
        # make sure its a mnemonic
        mnemonic = self.getParmMnemonic(mnemonic)
        return madrigal._Madrec.madGetParFormat(mnemonic)
    
    

    def getParmDescriptionList(self, parmList):
        """ getParmDescriptionList returns a list of descriptions (String) given a list of parameters (integer or mnemonic).
        
        Inputs: a list of parameters (integer or mnemonic)

        Returns: a list of descriptions (String) given a list of parameters (integer or mnemonic).  

        Affects: none

        Exceptions: none
        """

        returnList = []

        for parm in parmList:
            returnList.append(self.getParmDescription(parm).strip())

        return returnList


    def getParmMnemonic(self, code):
        """ getParmMnemonic returns a mnemonic (String) given a parameter (integer or mnemonic).
        
        Inputs: a parameter: integer, an integer in string form, or a mnemonic string

        Returns: a mnemonic string.  If integer not found, returns integer in string form.

        Affects: none

        Exceptions: none
        """
        if type(code) == types.StringType:
            # try to convert to an integer, if can't, assume its already a mnemonic
            try:
                intCode = int(code)
                code = intCode
            except:
                return code.upper()

        # otherwise it should already be an integer
        # if unknown code, return int as string
        try:
            retValue = madrigal._Madrec.cedarGetParMnemonic(code)
        except:
            retValue = str(code)

        return retValue


    def getParmMnemonicList(self, codeList):
        """ getParmMnemonicList returns a list of upper case mnemonics (String) given a list of cedar codes (integer, integer as string, or mnemonic string).
        
        Inputs: a list of cedar codes (integer, integer as string, or mnemonic string)

        Returns: a list of upper case mnemonics (String) given a list of cedar codes (integer).  If illegal value,
        returns str(code) for that item

        Affects: none

        Exceptions: none
        """

        returnList = []

        for code in codeList:
            mnemonic = self.getParmMnemonic(code).strip()
            if mnemonic == 'Illegal Parameter Code':
                mnemonic = str(code)
            returnList.append(mnemonic.upper())


        return returnList
    

    def normalizeParmList(self, parmList):
        """ normalizeParmList returns an ordered list of parameters with all mnemonics changed to integers.
        
        Inputs: parmList - the list of parameters (integers or mnemonics) to convert

        Returns:  a new parmList that is ordered (negitive values are placed directly after the same positive values)
        and all parameters are converted to integers

        Affects: None

        Exceptions: none
        """

        #create a copy of the parmList
        newParmList = copy.deepcopy(parmList)

        # convert all parameters to integers
        itemCount = 0
        for parm in newParmList:
            if type(parm) == types.StringType:
                # check if its already an integer
                try:
                    newParm = int(parm)
                    newParmList[itemCount] = newParm
                except:
                    # must be a mnemonic
                    newParmList[itemCount] = self.getParmCodeFromMnemonic(parm)

            # otherwise it must already be an integer
            else:
                newParmList[itemCount] = parm
                    
            itemCount = itemCount + 1

        #put in correct order
        return self.__sortList(newParmList)


    def getMadCategoryIndex(self, category):
        """ getMadCategoryIndex returns the index (order) of a given category.
        
        Inputs: a Madrigal category (string)

        Returns: an integer representing the index (order).  Returns -32767
                 if not found.

        Affects: none

        Exceptions: none
        """
        return (madrigal._Madrec.madGetCategoryIndex(category))


    def getCategoryDict(self, parmList):
        """ getCategoryDict returns a python dict with key = category index, item = category name and ordered parameters        
        Inputs: parmList - the list of parameters (integers or mnemonics) 

        Returns:  a python dict, with key = category index. Each item is a list of two items.
        The first item is the category name (string).  The second item is a list of parameter mnemonics
        from parmList belonging in that category.  Ordering is alphabetical, except that an error parameter
        immediately follows its non-error parameter.

        Affects: None

        Exceptions: none
        """
        catDict = {}

        # create a parm List in mnemonic form
        mnemParmList = self.getParmMnemonicList(parmList)

        for parm in mnemParmList:
            category = self.getParmCategory(parm)
            catId = self.getMadCategoryIndex(category)

            # check if category already included
            try:
                if not parm in catDict[catId][1]:
                    catDict[catId][1].append(parm)
                    
            except KeyError:
                # create new item
                catDict[catId] = [category, [parm]]

        # now order all parm lists
        for id in catDict.keys():
            self.orderParms(catDict[id][1])

        
        return catDict


    def orderParms(self, parmList):
        """ orderParms sorts mnemonic parameters alphabetically, with error parms directly after non-error.
        
        Inputs: a list of mnemonics (strings) in standard form (all caps)

        Returns: None.

        Affects: sorts input parmList alphabetically, with error parms directly after non-error

        Exceptions: none
        """
        parmList.sort()

        # store all error parameters with non-error
        errList = []
        for parm in parmList:
            if parm[0] == 'D':
                nonErrParm = parm[1:]
                if nonErrParm in parmList:
                    errList.append(parm)

        # now remove all error parameters
        for errparm in errList:
            parmList.remove(errparm)

        # now insert the error parameters where they belong
        for errparm in errList:
            nonErrParm = errparm[1:]
            # get position of non-error parm
            index = parmList.index(nonErrParm)
            parmList.insert(index+1, errparm)

   

    def getMnemonicListFromExpression(self, expressionStr):
        """ getMnemonicListFromExpression returns a list of unique cedar mnemonics in a python logical expression.
        
        Inputs: expressionStr - a string containing a valid python logical expression with cedar mnemonics as
        variables.  Expression can contain any python logical operator (and, or, not, ==, <, >, <=, >=, !=), any
        operator, any number, and valid python math function, and any variable that is a valid cedar mnemonic.  A substring
        is assumed to be a cedar mnemonic if it begins with a letter, contains only alphanumerics, period, plus, or
        underscore, and is not immediately followed by a open parenthesis. Each potential cedar mnemonic is
        verified, and an exception is thrown if it is not valid.  The validity of the entire expression is then
        verified by replacing all the valid cedar mnemonics by "1.0" and executing the resulting expression.  If any
        exception besides divide by zero or value error occurs, an exception is thrown.  Otherwise, the list of cedar
        mnemonics found is returned.

        Returns:  a list of unique cedar mnemonics (upper case).

        Affects: None

        Exceptions: Error thrown if any non-valid mnemonic found, or if expression throws an exception when run
        (except divide by zero or value error).
        """

        # convert expressionStr to lower case, as required for functions
        expressionStr = expressionStr.lower()

        # list of found mnemonics
        foundMnemonics = []

        # create a test string with all mnemonics replaced by "1.0"
        testStr = ''

        # search expressionStr for possible mnemonics, and
        # fill testStr
        inMnemonic = 0
        thisMnemonic = ''
        for char in expressionStr + ' ': 
            # see if a new mnemonic might have begun
            if inMnemonic == 0 and char.isalpha():
                inMnemonic = 1
                thisMnemonic = char
            # else see if a new mnemonic is done
            elif inMnemonic == 1 and ((not char.isalnum())  and char != '.' and char != '_' and char != '+'):
                inMnemonic = 0
                # if char is '(', its a function - ignore it
                if char == '(':
                    testStr = testStr + thisMnemonic + char
                    continue
                # if its just a logical operator or 'e', ignore it
                if thisMnemonic in ('and', 'or', 'not', 'e'):
                    testStr = testStr + thisMnemonic + char
                    continue
                else:
                    # verify its a valid mnemonic
                    try:
                        self.getParmCodeFromMnemonic(thisMnemonic)
                        # now convert to upper case
                        thisMnemonic = thisMnemonic.upper()
                        # append it if its unique
                        if thisMnemonic not in foundMnemonics:
                            foundMnemonics.append(thisMnemonic)
                        testStr = testStr + '1.0' + char
                    except:
                        # not valid - throw an error
                        raise 'The expression ' + expressionStr + \
                              ' contains an illegal mnemonic: ' + thisMnemonic
            # else see if its another char to add to the present mnemonic
            elif inMnemonic == 1:
                thisMnemonic = thisMnemonic + char
            else:
                testStr = testStr + char
                continue

        # now try to evaluate testStr to see if its a reasonable logical expression
        try:
            obj = eval(testStr)
            
        except ZeroDivisionError:
            # this is not a problem
            pass
        
        except ValueError:
            # this is not a problem
            pass
        
        except:
            # some other error occurred - this is a problem
            raise 'The expression "' + expressionStr + '" contains an error: ' + str(sys.exc_info()[1])


        return foundMnemonics


    def getStdExpression(self, expressionStr):
        """ getStdExpression returns an expression in standard form (upper case mnemonic, all else lower case).
        
        Inputs: expressionStr - a string containing a valid python logical expression with cedar mnemonics as
        variables.  Expression can contain any python logical operator (and, or, not, ==, <, >, <=, >=, !=), any
        operator, any number, and valid python math function, and any variable that is a valid cedar mnemonic.  A substring
        is assumed to be a cedar mnemonic if it begins with a letter, contains only alphanumerics, period, plus, or
        underscore, and is not immediately followed by a open parenthesis. Each potential cedar mnemonic is
        verified, and an exception is thrown if it is not valid.  

        Returns:  an expression (string) in standard form (upper case mnemonic, all else lower case).

        Affects: None

        Exceptions: Error thrown if any non-valid mnemonic found.
        """

        # convert expressionStr to lower case, as required for functions
        expressionStr = expressionStr.lower()

        # create a new stdExpStr to be returned 
        stdExpStr = ''

        # search expressionStr for possible mnemonics
        inMnemonic = 0
        thisMnemonic = ''
        for char in expressionStr + ' ': 
            # see if a new mnemonic might have begun
            if inMnemonic == 0 and char.isalpha():
                inMnemonic = 1
                thisMnemonic = char
            # else see if a new mnemonic is done
            elif inMnemonic == 1 and ((not char.isalnum())  and char != '.' and char != '_' and char != '+'):
                inMnemonic = 0
                # if char is '(', its a function - ignore it
                if char == '(':
                    stdExpStr = stdExpStr + thisMnemonic + char
                    continue
                # if its just a logical operator or 'e', ignore it
                if thisMnemonic in ('and', 'or', 'not', 'e'):
                    stdExpStr =  stdExpStr + thisMnemonic + char
                    continue
                else:
                    # verify its a valid mnemonic
                    try:
                        self.getParmCodeFromMnemonic(thisMnemonic)
                        # now convert to upper case
                        thisMnemonic = thisMnemonic.upper()
                        stdExpStr = stdExpStr + thisMnemonic + char
                    except:
                        # not valid - throw an error
                        raise 'The expression ' + expressionStr + \
                              ' contains an illegal mnemonic: ' + thisMnemonic
            # else see if its another char to add to the present mnemonic
            elif inMnemonic == 1:
                thisMnemonic = thisMnemonic + char
            else:
                stdExpStr = stdExpStr + char
                continue

        return stdExpStr


                        
    def getParametersForInstruments(self, instrumentList, parmListName='Comprehensive'):
        """getParametersForInstruments returns a list of unique Madrigal mnemonics associated with a list of instruments.

        This method's purpose is to return a list of parameters appropriate for a user to select from
        given that a certain list of instruments is under consideration.  This method will return a list
        of all measured parameters found in data files associated with those instruments, and also all
        parameters in the parmNameList that can be derived from those measured parameters.  The passed
        in parmNameList must be a valid name of a parameter list found in the madrigal.ui.web.MadrigalWebFormat
        class, and defaults to the "Comprehensive" list of parameters used in the madDataBrowse web page.
        
        Inputs: instrumentList - a python list on instruments as integers (kinst values).
        
                parmListName - a name (string) of a list of parameters in the MadrigalWebFormat class.
                Defaults to "Comprehensive"

        Returns:  an ordered list of unique Madrigal mnemonics.

        Affects: None

        Exceptions: None.
        """

        # list of found mnemonics
        foundMnemonics = []

        # create MadrigalInstrumentParameters object
        instParmsObj = madrigal.metadata.MadrigalInstrumentParameters(self.__madDB)

        # create MadrigalWebFormat object
        webFormatObj = madrigal.ui.web.MadrigalWebFormat()

        # check that passed-in instrumentList is really a list
        if type(instrumentList) == types.IntType:
            instrumentList = [instrumentList]
        elif instrumentList == None:
            instrumentList = []

        # loop through each instrument
        for inst in instrumentList:
            # get measured parameters associated with that instrument
            measParms = instParmsObj.getParameters(inst)
            # if None, skip instrument
            if measParms == None:
                continue
            # if unique, add them to foundMnemonics
            for parm in measParms:
                if not parm.upper() in foundMnemonics:
                    foundMnemonics.append(parm.upper())

        # now add whatever parameters can be derived from these measured parameters
        derivedParmList = webFormatObj.getFormat(parmListName)

        derivableParmList = madrigal._Madrec.getDerivableParms(foundMnemonics)


        # add all unique, allowed parameters from derivedParmList
        for item in derivableParmList:
            # add if unique
            if not item in foundMnemonics:
                foundMnemonics.append(item)
            

        return foundMnemonics


    def getIsprintHeader(self, mnemonicList):
        """ getIsprintHeader returns a string with mnemonics as it would appear at the top of isprint.
        
        Inputs: mnemonic:  a list of Madrigal mnemonics (string or integer)

        Returns: a string with mnemonics as it would appear at the top of isprint

        Affects: none

        Exceptions: If any mnemonic not found.
        """
        retStr = ''

        for mnem in mnemonicList:
            strLength = madrigal._Madrec.madGetParWidth(self.getParmMnemonic(mnem))
            newFormatStr = '%'
            newFormatStr += '%is' % (strLength)
            retStr += newFormatStr % (self.getParmMnemonic(mnem).upper())

        return retStr[4:]
            
        

if __name__ == '__main__':

    # test MadrigalFile
    try:

        #filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'
        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1998/mlh/20jan98/mlh980120g.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print 'File analysis took ' + str(t2-t1) + ' seconds.'

        print test.toString()

        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1995/jro/01feb95/jic950201g.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print 'File analysis took ' + str(t2-t1) + ' seconds.'

        print test.toString()

        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1997/aro/06jan97/are970106g.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print 'File analysis took ' + str(t2-t1) + ' seconds.'

        print test.toString()

        filepath = os.environ.get('MAD' + 'ROOT') + '/experiments/1990/mui/23apr90/mui900423a.001'

        t1 = time.time()

        test = MadrigalFile(filepath)

        t2 = time.time()

        print 'File analysis took ' + str(t2-t1) + ' seconds.'

        print test.toString()

        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []

        t1 = time.time()


        test.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Comprehensive'),
                                      measParmList,
                                      derivedParmList,
                                      allParmList,
                                      sureParmList)
        t2 = time.time()

        print 'Parameter analysis took ' + str(t2-t1) + ' seconds.'

        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []
        
        t1 = time.time()


        test.getMeasDervBothParmLists(madrigal.ui.web.MadrigalWebFormat().getFormat('Comprehensive'),
                                      measParmList,
                                      derivedParmList,
                                      allParmList,
                                      sureParmList)
        t2 = time.time()

        print 'Parameter analysis without verification of derived parameters took ' + str(t2-t1) + ' seconds.'
        
        #print lists
        print 'Measured parms are: ' + str(measParmList)
        print 'Derived parms are: ' + str(derivedParmList)
        print 'All good parms are: ' + str(allParmList)
        print 'Sure parms are: ' + str(sureParmList)

    except madrigal.admin.MadrigalError, e:

        print e.getExceptionStr()

    #test MadrigalParameters
    madDB = madrigal.metadata.MadrigalDB()
    
    print 'madDB loaded'

    test = MadrigalParameters(madDB)

    paramList = ['YEAR', 20, 21, 34, 'gdalt', 120, 121, 125, 126, 130, 132, 133, 140, -120, 'dgdalt', 142, 143, 160, 170, 204, 206, 208,
                 210, 213, 216, 218, 220, 222,  226, -121, 246, 310,
                 340, 354, 356, 402, 411, 420, 430, 461, 482, 483, 505]

    print str(test.getCategoryDict(allParmList))

    shortParmList = [120, 125, 126]

    strFile = os.environ.get('MADROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'


    print 'Cedar code for diplat is:' + str(test.getParmCodeFromMnemonic('diplat'))

    print 'Cedar scale factor for diplat is:' + str(test.getParmScaleFactor('diplat'))

    try:
        
        print 'Cedar code for BillR is:' + str(test.getParmCodeFromMnemonic('BillR'))

    except madrigal.admin.MadrigalError, e:
        
        print e.getExceptionStr()
	
    print 'Mnemonic for 75 is: ' + test.getParmMnemonic(75)

    print 'Mnemonic for 110 is: ' + test.getParmMnemonic(110)

    print 'Mnemonic list for [110, 120] is: ' + str(test.getParmMnemonicList([110, 120]))

    print 'Description for 110 is: ' + test.getParmDescription(110)

    print 'Description list for [110, 120] is: ' + str(test.getParmDescriptionList([110, 120]))

    print 'Converting GDALT POPL DPOPL TI DTI TE DTE VO DVO to int string:'

    strList = 'GDALT POPL DPOPL TI DTI TE DTE VO DVO'.split()

    strInt = ''
    for mnemStr in strList:
        parmInt = test.getParmCodeFromMnemonic(mnemStr)
        strInt = strInt + str(parmInt) + ' '

    print strInt

    
    # test getMnemonicListFromExpression
    print 'Mnemonics found in "po+ <= 1000 and range != log10(f10.7) and range > 1e-10" are:'
    print test.getMnemonicListFromExpression('po+ <= 1000 and range != log10(f10.7) and range > 1e-10')
    print 'Standard expression is: '
    print test.getStdExpression('po+ <= 1000 and range != log10(f10.7) and range > 1e-10')

    print test.getMnemonicListFromExpression('1<Kp<6')

    # test getParametersForInstruments
    print 'The following parameters are appropriate for instruments 30 and 80:'
    print test.getParametersForInstruments([80, 30])

    # test getParmCategory
    print test.getParmCategory(9)
    print test.getParmCategory('BYear')

    # test hasHtmlDesc
    print "Does ut1 have an html description = " + str(test.hasHtmlDesc('uT1'))

    # test getParmType
    print "Type of 1234 = " + str(test.getParmType('1234'))
    print "Type of -1234 = " + str(test.getParmType('-1234'))
    print "Type of gdalt = " + str(test.getParmType('gdalt'))
    print "Type of dgdalt = " + str(test.getParmType('dgdalt'))
    print "Type of dgdalt2 = " + str(test.getParmType('dgdalt2'))

    # test getSimpleParmDescription and getParmUnits
    print 'The follow are the simple description and units for ti:'
    print '"%s"' % (test.getSimpleParmDescription('ti'))
    print '"%s"' % (test.getParmUnits('ti'))
    
