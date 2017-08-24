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

import tables

#import matplotlib.numerix

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


class BatchExperiment:
    """BatchExperiment is a class to create and update AMISR Madrigal experiments
    """

    # defines length of line in Cedar catalog/header file
    __CEDAR_LEN__ = 80 

    def uploadExperiment(self,iniFile,plotsdir='quicklook-plots'):

        # create needed Madrigal objects
        self.madDBObj = madrigal.metadata.MadrigalDB()
        madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)  

        # read ini file
        self.__iniData__ = ConfigParser.ConfigParser()
        self.__iniData__.read(iniFile)

        # get reqiured experiment info
        expTitle = self.__iniData__.get('Experiment', 'title')
        self.instrument = int(self.__iniData__.get('Experiment', 'instrument'))
        logFile = self.__iniData__.get('Experiment', 'logFile')
        expId = self.__iniData__.get('Experiment', 'expId')
        OutPath = self.__iniData__.get('Experiment','OutPath')

        # parse the expId
        try:
            items = expId.split('.')
            date = int(items[0])
            num = int(items[1])
            expId = items[0] + '.' + str(self.instrument) + '.' + items[1]
        except:
            traceback.print_exc()
            raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))      

        # find the number of files being created
        numFiles = 0
        while True:
            try:
                self.__iniData__.get('File%i' % (numFiles + 1), 'hdf5Filename')
                numFiles += 1
            except ConfigParser.NoSectionError:
                break

        # get optional character, if any
        optChar = parseExpId(expId)[3]
            
        # next find the time range in the data
        firstTime = None
        lastTime = None
        for fileNum in range(numFiles):

            self.fileSection = 'File%i' % (fileNum + 1)

            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')

            fileHandler = hdf5Handler(hdf5Type)

            startTime, endTime = fileHandler.getStartEndTimes(hdf5Filename)

            if firstTime == None:
                firstTime = startTime
            elif firstTime > startTime:
                firstTime = startTime

            if lastTime == None:
                lastTime = endTime
            elif lastTime < endTime:
                lastTime = endTime
        
        # create new madrigal file name template
        instMnemonic = self.madInstObj.getInstrumentMnemonic(self.instrument).lower()
        madFilenameTemplate = '%s%02i%02i%02i.' % (instMnemonic, firstTime.year % 100,
                                                   firstTime.month, firstTime.day)
            
        madAdminObj = madrigal.admin.MadrigalDBAdmin()
        for fileNum in range(numFiles):

            self.fileSection = 'File%i' % (fileNum + 1)
            madFilename = madFilenameTemplate + '%03i' % (fileNum + 1)
            fullMadFilename = os.path.join(OutPath,madFilename)
            
            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')
            status = self.__iniData__.get(self.fileSection, 'status')
            category = int(self.__iniData__.get(self.fileSection, 'category'))
            fileDesc=status

            shutil.copyfile(fullMadFilename, os.path.join('/tmp',madFilename))
            fullMadFilename=os.path.join('/tmp',madFilename)

            if fileNum==0:# create the experiment
                expPath = madAdminObj.createMadrigalExperiment(fullMadFilename, expTitle, 0, fileDesc,None,category=category,optChar=optChar)
            else: 
                madAdminObj.addMadrigalFile(expPath,fullMadFilename,0, fileDesc,category=category,kindat=None)
    
        names = os.listdir(os.path.join(OutPath,plotsdir))
        for name in names:
            src = os.path.join(OutPath,plotsdir, name)
            dst = os.path.join(expPath, name)
            shutil.copyfile(src, dst)

        self.expPath = expPath

        os.remove(fullMadFilename)

        return(expPath)

    def createNewExperimentFromIni(self,
                                   iniFile):
        """createNewExperimentFromIni will try to create a single new Madrigal experiment using
        information parsed from an input ini file.

        This method will also allow importing summary plots created outside this script into Madrigal.
        It will also allow the option of the automatic creation of individual record plots.

        Example ini file:

        [Experiment]

        title:              World Day
        instrument:         61
        logFile:            $BASE/amisr/pokerflat/face1/20071011-120000/log.txt
        expId:              20071011.61.000
        pi:                 Craig Heiselman
        modexp:             This is a one line experiment title
        cmodexp:            In this section you can write a multi-line description
                            of your experiment.  An ini file recognizes multiline
                            descriptions as long as every continuation line begins
                            with whitespace (as in this example).

        [File1]

        hdf5Filename:       $BASE/amisr/pokerflat/face1/20071011-120000/20071011.004_lp_3min.h5
        kindat:             5950
        type:               standard
        createRecPlots:     True
        imageTitle1:        Electron density - long pulse - beam 1
        image1:             /tmp/elecDensBeam1_lp.jpg
        imageTitle2:        Electron density - long pulse - beam 2
        image2:             /tmp/elecDensBeam2_lp.jpg
        ckindat:            In this section you can write a multi-line description
                            of how this particular file was created.  An ini file
                            recognizes multiline descriptions as long as every continuation line begins
                            with whitespace (as in this example)

        [File2]

        hdf5Filename:       $BASE/amisr/pokerflat/face1/20071011-120000/20071011.004_ac_3min.h5
        kindat:             5951
        type:               standard
        createRecPlots:     False
        imageTitle1:        Electron density - alternating code - beam 1
        image1:             /tmp/elecDensBeam1_ac.jpg
        imageTitle2:        Electron density - alternating code - beam 2
        image2:             /tmp/elecDensBeam2_ac.jpg

        [File3]

        hdf5Filename:       $BASE/amisr/pokerflat/face1/20071011-120000/20071011.004_lp_3min-vvels.h5
        kindat:             5953
        type:               velocity

        The input ini file is made up of an [Experiment] section and one or more [File*] section.
        No time values are used here. since the experiment times will be determined from the data itself.
        The names title, instrument, logFile, and expId are all required in the Experiment section.
        The expId name must be in the form YYYYMMDD.<inst_code>.<number>, where
        the date represents the first day of the experiment, <inst_code> is the instrument
        code, and the trailing <number> is between 0 and 26.

        In addition to the required fields in the Experiment section, there are also some optional
        fields designed to add experiment level information to the files' catalog record:

            pi - the experiment principle investigator
            
            modexp - a short experiment title

            cmodexp - a full description of the experiment.  These fields describe the experiment
                as a whole, and so will be the same for each file.  The field cmodexp will
                typically be multiple lines.  It is legal to have multiline values in an ini
                file as long as each new line begins with some white space.  

        For each file to be added to the experiment, a section called File* is required, and the
        numbering must be in increasing order starting with 1.  The names hdf5Filename, kindat, and type
        are required.  It is highly recommended that every file in an experiment have a unique
        kindat, because the kindat description is how the user determines the differences between
        files.  Madrigal administrators can always add additional kindats by editing the
        $MADROOT/metadata.typeTab.txt file (kindat descriptions must not contain commas).
        type deterimines the type of hdfs being loaded.  Presently supported types are standard,
        velocity, and uncorrected_ne_only.

        In addition to the required fields in the File* section, there are also some optional fields:

            createRecPlots -If set to True, the createRecPlots name will allow the creation of
                individual record plots. If not given or False, these plots will not be created.
                If type != standard, createRecPlots is ignored, since only works with standard data.

            imageTitle%i and image%i - must be given as a pair with matching numbers.  Allows images
                relating to that experiment to be imported into Madrigal.  The user will see a link to
                the imported image with text set by imageTitle.

            ckindat - a description of how this particular file was processed.  Will be included in the
                header record prepended to the file.  The field ckindat will typically be multiple lines.
                It is legal to have multiline values in an ini file as long as each new line begins
                with some white space.

            lowerRange - sets a upper range cutoff in km for uncorrected_ne_only files

            lowerRange - sets a upper range cutoff in km for uncorrected_ne_only files
        """

        # create needed Madrigal objects
        self.madDBObj = madrigal.metadata.MadrigalDB()
        madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)        
        
        # read ini file
        self.__iniData__ = ConfigParser.ConfigParser()
        self.__iniData__.read(iniFile)

        # get reqiured experiment info
        expTitle = self.__iniData__.get('Experiment', 'title')
        self.instrument = int(self.__iniData__.get('Experiment', 'instrument'))
        logFile = self.__iniData__.get('Experiment', 'logFile')
        expId = self.__iniData__.get('Experiment', 'expId')
        OutPath = self.__iniData__.get('Experiment','OutPath')

        # parse the expId
        try:
            items = expId.split('.')
            date = int(items[0])
            num = int(items[1])
            expId = items[0] + '.' + str(self.instrument) + '.' + items[1]
        except:
            traceback.print_exc()
            raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))        

        logging.basicConfig(level=logging.DEBUG,
                            format='%(asctime)s %(levelname)s %(message)s',
                            filename=logFile,
                            filemode='w')

        logging.info('Creating exp using ini file %s with instrument %i and title <%s> and expId <%s>' % \
                     (iniFile,  self.instrument, expTitle, expId))

        # find the number of files being created
        numFiles = 0
        while True:
            try:
                self.__iniData__.get('File%i' % (numFiles + 1), 'hdf5Filename')
                numFiles += 1
            except ConfigParser.NoSectionError:
                break

        if numFiles == 0:
            raise IOError, 'No File* section specified in ini file'

        # next find the time range in the data
        firstTime = None
        lastTime = None
        for fileNum in range(numFiles):

            self.fileSection = 'File%i' % (fileNum + 1)

            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')

            fileHandler = hdf5Handler(hdf5Type)

            startTime, endTime = fileHandler.getStartEndTimes(hdf5Filename)

            if firstTime == None:
                firstTime = startTime
            elif firstTime > startTime:
                firstTime = startTime

            if lastTime == None:
                lastTime = endTime
            elif lastTime < endTime:
                lastTime = endTime
        
        # create new madrigal file name template
        instMnemonic = self.madInstObj.getInstrumentMnemonic(self.instrument).lower()
        madFilenameTemplate = '%s%02i%02i%02i.' % (instMnemonic, firstTime.year % 100,
                                                   firstTime.month, firstTime.day)

        # header
        try: principleInvestigator = self.__iniData__.get('Experiment', 'pi')
        except: principleInvestigator = None
        try: expPurpose = self.__iniData__.get('Experiment', 'modexp')
        except: expPurpose = None
        try: expMode = self.__iniData__.get('Experiment', 'cmodexp')
        except: expMode = None
        try: cycleTime = self.__iniData__.get('Experiment', 'cycletime')
        except: cycleTime = None
        try: correlativeExp = self.__iniData__.get('Experiment', 'correxp')
        except: correlativeExp = None
        try: sciRemarks = self.__iniData__.get('Experiment', 'remarks')
        except: sciRemarks = None

        # loop through all the files, and add to madrigal
        for fileNum in range(numFiles):
            self.fileSection = 'File%i' % (fileNum + 1)
            madFilename = madFilenameTemplate + '%03i' % (fileNum + 1)
            fullMadFilename = os.path.join(OutPath,madFilename)
            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            kindat = int(self.__iniData__.get(self.fileSection, 'kindat'))
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')
            try:
                thisLowerRange = float(self.__iniData__.get(self.fileSection, 'lowerRange'))
            except:
                thisLowerRange = None
            try:
                thisUpperRange = float(self.__iniData__.get(self.fileSection, 'upperRange'))
            except:
                thisUpperRange = None

            # add file
            fileHandler = hdf5Handler(hdf5Type)
            try:
            # if 1==1:
                fileHandler.createMadrigalFile(hdf5Filename, self.instrument, kindat, None, fullMadFilename, thisLowerRange, thisUpperRange)
            
                
                # header
                if writeHeader:
                    try: kindatDesc = self.__iniData__.get(self.fileSection, 'ckindat')
                    except: kindatDesc = None
                    try: analyst = self.__iniData__.get(self.fileSection, 'analyst')
                    except: analyst = None
                    try: comments = self.__iniData__.get(self.fileSection, 'comments')
                    except: comments = None
                    try: history = self.__iniData__.get(self.fileSection, 'history')
                    except: history = None
                    catHeadObj = madrigal.cedar.CatalogHeaderCreator(fullMadFilename)
                    catHeadObj.createCatalog(principleInvestigator=principleInvestigator, expPurpose=expPurpose, expMode=expMode, cycleTime=cycleTime, correlativeExp=correlativeExp, sciRemarks=sciRemarks)
                    catHeadObj.createHeader(kindatDesc=kindatDesc, analyst=analyst, comments=comments, history=history)
                    catHeadObj.write()
                
                #self.__prependCatHeader__(fullMadFilename)
                logging.info('added file from %s to %s using %s, kindat %i, type %s, lowerRange=%s, upperRange=%s' % \
                                 (str(firstTime), str(lastTime), hdf5Filename, kindat, hdf5Type, str(thisLowerRange), str(thisUpperRange)))
            except ValueError:
                print fullMadFilename + ' already exists... skipping onto next one.'

    def __prependCatHeader__(self, fullMadFilename):
        """__prependCatHeader__ prepends a catalog and header record to existing fullMadFilename
        based on information in fullMadFilename and from the ini file.

        Inputs:

            fullMadFilename - full path to newly created madrigal file

        Returns: None

        Affects: prepends a catalog and header record to existing fullMadFilename
        """

        # first create catalog record as string
        catStr = ''

        catStr += self.__padLine__('KRECC       2001 Catalogue Record, Version 1')
        catStr += self.__padLine__('KINSTE     %5i %s' % \
                                   (self.instrument,
                                    self.madInstObj.getInstrumentName(self.instrument)))
        
        # add experiment description from ini file (if any)
        try:
            modexp = self.__iniData__.get('Experiment', 'modexp')
            newLine = 'MODEXP         0 ' + modexp
            catStr += self.__padLine__(newLine)
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass

        try:
            cmodexp = self.__iniData__.get('Experiment', 'cmodexp')
            lines = self.__breakTextIntoLines__(cmodexp, 'CMODEXP ')
            for line in lines:
                catStr += self.__padLine__(line)
            catStr += self.__padLine__('C')
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass
        
        # add summary info from the newly created Madrigal file
        madFileObj = madrigal.data.MadrigalFile(fullMadFilename, self.madDBObj)
        madKeyList, madDict = self.__getCatalogInfoFromMadrigal__(madFileObj)

        for key in madKeyList:
            catStr += self.__padLine__(madDict[key])

        # add PI from ini file if there
        try:
            catStr += self.__padLine__('C')
            pi = self.__iniData__.get('Experiment', 'pi')
            newLine = 'CPI      ' + pi
            catStr += self.__padLine__(newLine)
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass

        catStr += self.__padLine__('CPREPDAT  %s' % time.asctime())
            
        
        catStr = self.__getCatalogProlog__(madFileObj, len(catStr)/80) + catStr

        # header
        
        headStr = ''
        headStr += self.__padLine__('KRECH               3002 Header Record, Version 3')
        headStr += self.__padLine__('KINST          3   %5i %s' % \
                                    (self.instrument,
                                     self.madInstObj.getInstrumentName(self.instrument)))

        # add kindat and kindat description if given
        kindat = int(self.__iniData__.get(self.fileSection, 'kindat'))
        madKindatObj = madrigal.metadata.MadrigalKindat(self.madDBObj)
        kindatDesc = madKindatObj.getKindatDescription(kindat)
        if kindatDesc != None:
            try:
                headStr += self.__padLine__('KINDAT         4   %5i %s' % (kindat, kindatDesc))
            except:
                # description too long - truncate
                kindatDesc = kindatDesc[0:50]
                headStr += self.__padLine__('KINDAT         4   %5i %s' % (kindat, kindatDesc))
        
        
        headStr += self.__padLine__('C')

        try:
            ckindat = self.__iniData__.get(self.fileSection, 'ckindat')
            lines = self.__breakTextIntoLines__(ckindat, 'CKINDAT ')
            for line in lines:
                headStr += self.__padLine__(line)
            headStr += self.__padLine__('C')

        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass

        # add source file line
        source_file = os.path.basename(self.__iniData__.get(self.fileSection, 'hdf5Filename'))
        headStr += self.__padLine__('C SOURCE_FILE %s' % (source_file))
        headStr += self.__padLine__('C')

        # add summary info from the newly created Madrigal file
        madKeyList, madDict, madLineList = self.__getHeaderInfoFromMadrigal__(madFileObj)

        for key in madKeyList:
            headStr += self.__padLine__(madDict[key])
        for line in madLineList:
            headStr += self.__padLine__(line)

        headStr += self.__padLine__('CANDATE  %s' % time.asctime())

        headStr = self.__getHeaderProlog__(madFileObj, len(headStr)/80) + headStr

        # combine catalog, header, and data records
        orgFile = open(fullMadFilename)
        orgFileData = orgFile.read()
        orgFile.close()
        newFile = open(fullMadFilename, 'w')
        newFile.write(catStr)
        newFile.write(headStr)
        newFile.write(orgFileData)
        newFile.close()

        
    def __getCatalogInfoFromMadrigal__(self, madFileObj):
        """__getCatalogInfoFromMadrigal__ gets needed catalog information from a Madrigal file.

            Input: madFileObj - a MadrigalFile object based on Madrigal file

            Returns: a tuple containing:

                1. An ordered list of line headers produced
                
                2  A dictionary with keys = line headers produced, values = ordered list of lines
                generated for each line header. 
        """

        retKeyList = []
        retDict = {}

        templateLines = ('ALT1      %6i km. Lowest altitude measured',
                         'ALT2      %6i km. Highest altitude measured',
                         'GGLAT1    %6i degrees. Lowest geographic latitude measured',
                         'GGLAT2    %6i degrees. Highest geographic latitude measured',
                         'GGLON1    %6i degrees. Westmost geographic longitude measured',
                         'GGLON2    %6i degrees. Eastmost geographic longitude measured',
                         'PL1       %6i Shortest radar pulse length (microsec)',
                         'PL2       %6i Longest radar pulse length (microsec)',
                         'IBYRE     %6i Beginning year',
                         'IBDTE     %6i Beginning month and day',
                         'IBHME     %6i Beginning UT hour and minute',
                         'IBCSE     %6i Beginning centisecond',
                         'IEYRE     %6i Ending year',
                         'IEDTE     %6i Ending month and day',
                         'IEHME     %6i Ending UT hour and minute',
                         'IECSE     %6i Ending centisecond')

        for tempLine in templateLines:
            # get key
            firstSpace = tempLine.find(' ')
            key = tempLine[:firstSpace]

            try:
            
                if key == 'ALT1':
                    # check data good
                    if madFileObj.getMinValidAltitude() > madFileObj.getMaxValidAltitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMinValidAltitude()))
                elif key == 'ALT2':
                    # check data good
                    if madFileObj.getMinValidAltitude() > madFileObj.getMaxValidAltitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMaxValidAltitude()))
                elif key == 'GGLAT1':
                    # check data good
                    if madFileObj.getMinLatitude() > madFileObj.getMaxLatitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMinLatitude()))
                elif key == 'GGLAT2':
                    # check data good
                    if madFileObj.getMinLatitude() > madFileObj.getMaxLatitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMaxLatitude()))
                elif key == 'GGLON1':
                    # check data good
                    if madFileObj.getMinLongitude() > madFileObj.getMaxLongitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMinLongitude()))
                elif key == 'GGLON2':
                    # check data good
                    if madFileObj.getMinLongitude() > madFileObj.getMaxLongitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMaxLongitude()))
                elif key == 'PL1':
                    # check data good
                    if madFileObj.getMinPulseLength() > madFileObj.getMaxPulseLength():
                        continue
                    if madFileObj.getMinPulseLength() <= 0.0 or  madFileObj.getMaxPulseLength() <= 0.0:
                        continue
                    retDict[key] = tempLine % (int(round(madFileObj.getMinPulseLength() * 1000000.0)))
                elif key == 'PL2':
                    # check data good
                    if madFileObj.getMinPulseLength() > madFileObj.getMaxPulseLength():
                        continue
                    if madFileObj.getMinPulseLength() <= 0.0 or  madFileObj.getMaxPulseLength() <= 0.0:
                        continue
                    retDict[key] = tempLine % (int(round(madFileObj.getMaxPulseLength() * 1000000.0)))
                elif key == 'IBYRE':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[0])
                elif key == 'IBDTE':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2])
                elif key == 'IBHME':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4])
                elif key == 'IBCSE':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[5] * 100)
                elif key == 'IEYRE':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[0])
                elif key == 'IEDTE':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2])
                elif key == 'IEHME':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4])
                elif key == 'IECSE':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[5] * 100)
                elif key == 'CPREPDAT':
                    retDict[key] = tempLine % (time.asctime())
                else:
                    raise 'illegal key %s' % (str(key))
                
                retKeyList.append(key)

            except:
                continue

        return (retKeyList, retDict)

    def __getCatalogProlog__(self, madFileObj, numLines):
        """__getCatalogProlog__ generates the prolog part of the catalog record.

            Input:

                madFileObj - a MadrigalFile object based on Madrigal file created by inscal

                numLines -  number of 80 byte lines in catalog record

            Returns: the catalog record prolog as a 16*2 byte string (big-endian)
        """
        LTOT = 40 * (1+numLines)
        KRECC = 2001
        # set kinste to the first instrument found in the file
        KINSTE = madFileObj.getKinstList()[0]
        # set modexp to the first kindat found in the file
        MODEXP = madFileObj.getKindatList()[0]
        IBYRE = madFileObj.getEarliestTime()[0]
        IBDTE = madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2]
        IBHME = madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4]
        IBCSE = madFileObj.getEarliestTime()[5] * 100
        IEYRE = madFileObj.getLatestTime()[0]
        IEDTE = madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2]
        IEHME = madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4]
        IECSE = madFileObj.getLatestTime()[5] * 100
        ZERO = 0
        # pack 40 big-endian 2 byte ints
        return struct.pack('>hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh',
                           LTOT,
                           KRECC,
                           KINSTE,
                           MODEXP,
                           IBYRE,
                           IBDTE,
                           IBHME,
                           IBCSE,
                           IEYRE,
                           IEDTE,
                           IEHME,
                           IECSE,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO)


    def __getHeaderProlog__(self, madFileObj, numLines):
        """__getHeaderProlog__ generates the prolog part of the header record.

            Input:

                madFileObj - a MadrigalFile object based on Madrigal file created by inscal

                numLines -  number of 80 byte lines in header record

            Returns: the header record prolog as a 16*2 byte string (big-endian)
        """
        LTOT = 40 * (1+numLines)
        KRECC = 3002
        # set kinste to the first instrument found in the file
        KINSTE = madFileObj.getKinstList()[0]
        # set modexp to the first kindat found in the file
        MODEXP = madFileObj.getKindatList()[0]
        IBYRE = madFileObj.getEarliestTime()[0]
        IBDTE = madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2]
        IBHME = madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4]
        IBCSE = madFileObj.getEarliestTime()[5] * 100
        IEYRE = madFileObj.getLatestTime()[0]
        IEDTE = madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2]
        IEHME = madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4]
        IECSE = madFileObj.getLatestTime()[5] * 100
        LPROL = 16
        JPAR = len(madFileObj.getMeasured1dParmList())
        MPAR = len(madFileObj.getMeasured2dParmList())
        ZERO = 0
        # pack 40 big-endian 2 byte ints
        return struct.pack('>hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh',
                           LTOT,
                           KRECC,
                           KINSTE,
                           MODEXP,
                           IBYRE,
                           IBDTE,
                           IBHME,
                           IBCSE,
                           IEYRE,
                           IEDTE,
                           IEHME,
                           IECSE,
                           LPROL,JPAR,MPAR,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO)



    def __getHeaderInfoFromMadrigal__(self, madFileObj):
        """__getHeaderInfoFromMadrigal__ get needed information fro a header record from a Madrigal file.

            Input: madFileObj - a MadrigalFile object based on Madrigal file

            Returns: a tuple containing:

                1. An ordered list of line headers produced
                
                2  A dictionary with keys = line headers produced, values = ordered list of lines

                3. A list of lines containing the KOD lines and the parameter descriptions
        """
        madParmObj = madrigal.data.MadrigalParameters()

        retKeyList = []
        retDict = {}
        parmLines = []

        templateLines = ('IBYRT             %6i Beginning year',
                         'IBDTT             %6i Beginning month and day',
                         'IBHMT             %6i Beginning UT hour and minute',
                         'IBCST             %6i Beginning centisecond',
                         'IEYRT             %6i Ending year',
                         'IEDTT             %6i Ending month and day',
                         'IEHMT             %6i Ending UT hour and minute',
                         'IECST             %6i Ending centisecond',
                         'LPROL         13     16  Length of prologue in data records',
                         'JPAR          14 %6i  Number of single-valued parameters',
                         'MPAR          15 %6i  Number of multiple-values parameters',
                         'NROW          16         Number of entries for multiple valued parameter')

        for tempLine in templateLines:
            # get key
            firstSpace = tempLine.find(' ')
            key = tempLine[:firstSpace]
            retKeyList.append(key)
            if key == 'IBYRT':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[0])
            elif key == 'IBDTT':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2])
            elif key == 'IBHMT':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4])
            elif key == 'IBCST':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[5] * 100)
            elif key == 'IEYRT':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[0])
            elif key == 'IEDTT':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2])
            elif key == 'IEHMT':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4])
            elif key == 'IECST':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[5] * 100)
            elif key == 'LPROL':
                retDict[key] = tempLine
            elif key == 'JPAR':
                retDict[key] = tempLine % (len(madFileObj.getMeasured1dParmList()))
            elif key == 'MPAR':
                retDict[key] = tempLine % (len(madFileObj.getMeasured2dParmList()))
            elif key == 'NROW':
                retDict[key] = tempLine
            else:
                raise 'illegal key %s' % (str(key))

        # generate 1d lines
        parmLines.append('C 1D Parameters:')

        count = 0

        onedParmList = madFileObj.getMeasured1dParmList()

        for parm in onedParmList:
            count += 1
            # KODS
            thisStr = 'KODS(%i)' % (count)
            thisStr += ' ' * (8 - len(thisStr))
            # position in prolog
            nextStr = '%i' % (count + 16)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm code
            nextStr = '%i' % (parm)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm desc
            nextStr = ' %s' % (madParmObj.getSimpleParmDescription(parm))
            if len(nextStr) > 40:
                nextStr = nextStr[:40]
            else:
                nextStr += ' ' * (40 - len(nextStr))
            thisStr += nextStr
            # unit scale
            nextStr = '%1.0E' % (madParmObj.getParmScaleFactor(parm))
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # units
            nextStr = ' %s' % (madParmObj.getParmUnits(parm))
            if len(nextStr) > 8:
                nextStr[:8]
            else:
                nextStr = ' ' * (8 - len(nextStr)) + nextStr
            thisStr += nextStr
            
            parmLines.append(thisStr)

        # generate 2d lines
        parmLines.append('C 2D Parameters:')

        count += len(madFileObj.getMeasured1dParmList())
        count2d = 0

        twodParmList = madFileObj.getMeasured2dParmList()

        for parm in twodParmList:
            count += 1
            count2d += 1
            # KODM
            thisStr = 'KODM(%i)' % (count2d)
            thisStr += ' ' * (8 - len(thisStr))
            # position in prolog
            nextStr = '%i' % (count + 16)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm code
            nextStr = '%i' % (parm)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm desc
            nextStr = ' %s' % (madParmObj.getSimpleParmDescription(parm))
            if len(nextStr) > 40:
                nextStr = nextStr[:40]
            else:
                nextStr += ' ' * (40 - len(nextStr))
            thisStr += nextStr
            # unit scale
            nextStr = '%1.0E' % (madParmObj.getParmScaleFactor(parm))
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # units
            nextStr = ' %s' % (madParmObj.getParmUnits(parm))
            if len(nextStr) > 8:
                nextStr[:8]
            else:
                nextStr = ' ' * (8 - len(nextStr)) + nextStr
            thisStr += nextStr
            
            parmLines.append(thisStr)
            
        
        return (retKeyList, retDict, parmLines)



    def __padLine__(self, inputStr):
        """__padLine__ returns a string padded with spaces to CEDAR_LEN and with carriage returns removed.

        Raises error if inputStr with final carriage return removed longer that CEDAR_LEN.
        """
        
        if inputStr[-1] == '\n':
            inputStr = inputStr[:-1]

        if len(inputStr) > self.__CEDAR_LEN__:
            raise ValueError, 'Line %s has length greater than %i' % (inputStr, self.__CEDAR_LEN__)

        inputStr = inputStr.replace('\n', ' ')

        retStr = inputStr + ' ' * (self.__CEDAR_LEN__ - len(inputStr))

        return retStr


    def __breakTextIntoLines__(self, text, prefixText):
        """__breakTextIntoLines__ returns a list of lines all less than CEDAR_LEN all starting with
        prefixText, containing the words in text
        """
        retLineList = []

        words = text.split()
        thisLine = prefixText
        for word in words:
            if len(word) + 1 > self.__CEDAR_LEN__ - len(prefixText):
                raise ValueError, 'Cannot fit word <%s> into Cedar record' % (word)
            
            if len(word) + len(thisLine) < self.__CEDAR_LEN__:
                thisLine += '%s ' % (word)
            else:
                # new line
                retLineList.append(thisLine)
                thisLine = prefixText
                thisLine += '%s ' % (word)

        # append remaining line
        retLineList.append(thisLine)

        return(retLineList)

    

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
        m_index = 1
        e_index = -1
        fractIndex = 0
        tempIndex = 1
        colIndex = 2
        velIndex = 3

        # parameter boundaries
        minTemp = 100.0 # MJN changed 06/05/2008
        maxTemp = 10000.0
        maxTempErr = 32765.0 # limit on Cedar format
        minNe = 1.0E9
        maxNe = 1.0E13
        maxNeErr = 3.2E13
        maxVo = 32765.0 # limit on Cedar format
        maxVoErr = 32765.0 # limit on Cedar format
        maxFract = 1.0
        minFract = 0.0

        # create cedarObj if needed
        if cedarObj == None:
            cedarObj = madrigal.cedar.MadrigalCedarFile(madrigalFile, True)

        # read in all required data
        hdfObj = tables.openFile(hdf5File)

        # ranges
        ranges = hdfObj.root.FittedParams.Range
        rangeArray = ranges.read()
        numRanges = rangeArray.shape[2]

        # magnetic latitude and longitude
        platArray = hdfObj.root.Geomag.MagneticLatitude.read()
        plonArray = hdfObj.root.Geomag.MagneticLongitude.read()

        # alts
        alts = hdfObj.root.FittedParams.Altitude
        altArray = alts.read()
        
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

        # azimuth
        azArray = hdfObj.root.Antenna.Azimuth.read()
        azmArray = hdfObj.root.Antenna.AvgAzimuth.read()
        
        # elevation
        elArray = hdfObj.root.Antenna.Elevation.read()
        elmArray = hdfObj.root.Antenna.AvgElevation.read()        
        
        # event
        evntArray = hdfObj.root.Antenna.Event.read()

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
                                                        ('azm', 'elm', 'az1','az2','el1','el2','power','cbadl', 'pl', 'tfreq', 'rfreq','posf'),
                                                        ('range', 'gdalt', 'nel', 'dnel', 'ti', 'dti','te', 'dte', 'vo', 'dvo','po+','dpo+','col','dcol','cgm_lat','cgm_long'), numRanges)


            # set 1d values
            tazm=azmArray[recIndex]
            while tazm>180.0:
                tazm=tazm-360.0
            taz1=azArray[recIndex,0]
            taz2=azArray[recIndex,1]
            while taz1>180.0:
                taz1=taz1-360.0
            while taz2>180.0:
                taz2=taz2-360.0                            
            dataRec.set1D('azm', tazm)
            dataRec.set1D('elm', elmArray[recIndex])
            dataRec.set1D('az1', taz1)
            dataRec.set1D('az2', taz2)
            dataRec.set1D('el1', elArray[recIndex,0])
            dataRec.set1D('el2', elArray[recIndex,1])
            dataRec.set1D('power', txPowerArray[recIndex]/1000.0) # cedar in kWatts, SRI in Watts
            dataRec.set1D('cbadl', baudCount)
            dataRec.set1D('pl', pulseLength)
            dataRec.set1D('tfreq', txFreq)
            dataRec.set1D('rfreq', rxFreq)
            if recIndex<(self.numTimes-1) and evntArray[recIndex]==evntArray[recIndex+1]:
                dataRec.set1D('posf', 0)
            else:
                dataRec.set1D('posf', 1)
                
            beamIndex=0

            # set 2d values
            for rangeIndex in range(numRanges):
                    
                # range
                try:
                    if isNan(rangeArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('range', rangeIndex,rangeArray[recIndex][beamIndex][rangeIndex]/1000.0) # convert m -> km
                except:
                    dataRec.set2D('range', rangeIndex, 'missing')
                                    
                # altitude
                try:
                    if isNan(altArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('gdalt', rangeIndex,altArray[recIndex][beamIndex][rangeIndex]/1000.0) # convert m -> km
                except:
                    dataRec.set2D('gdalt', rangeIndex, 'missing')
  
                # mag latitude
                try:
                    if isNan(platArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('cgm_lat', rangeIndex,platArray[recIndex][beamIndex][rangeIndex]) 
                except:
                    dataRec.set2D('cgm_lat', rangeIndex, 'missing')

                # mag longitude
                try:
                    if isNan(plonArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('cgm_long', rangeIndex,plonArray[recIndex][beamIndex][rangeIndex]) 
                except:
                    dataRec.set2D('cgm_long', rangeIndex, 'missing')
                                                                              
                # ne
                try:
                    if neArray[recIndex][beamIndex][rangeIndex] < minNe or neArray[recIndex][beamIndex][rangeIndex] > maxNe or isNan(neArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('nel', rangeIndex, math.log10(neArray[recIndex][beamIndex][rangeIndex]))

                    if dneArray[recIndex][beamIndex][rangeIndex] <= 0.0 or dneArray[recIndex][beamIndex][rangeIndex] > maxNeErr or isNan(dneArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('dnel', rangeIndex, math.log10(dneArray[recIndex][beamIndex][rangeIndex]))                    
                except:
                    dataRec.set2D('nel', rangeIndex, 'missing')
                    dataRec.set2D('dnel', rangeIndex, 'missing')

                # ti
                try:
                    if fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] < minTemp or fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] > maxTemp or isNan(fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex]):
                        raise ValueError, ''
                    dataRec.set2D('ti', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex])

                    if errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] <= 1.0 or errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex] > maxTempErr or isNan(errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex]):
                        raise ValueError, ''
                    dataRec.set2D('dti', rangeIndex, errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex])
                except:
                    dataRec.set2D('ti', rangeIndex, 'missing')
                    dataRec.set2D('dti', rangeIndex, 'missing')

                # te
                try:
                    if fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] < minTemp or fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] > maxTemp or isNan(fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex]):
                        raise ValueError, ''
                    dataRec.set2D('te', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex])
                    
                    if isNan(errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex]):
                        errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex]=errArray[recIndex][beamIndex][rangeIndex][o_index][tempIndex]
                    
                    if errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] <= 1.0 or errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex] > maxTempErr or isNan(errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex]):
                        raise ValueError, ''
                    dataRec.set2D('dte', rangeIndex, errArray[recIndex][beamIndex][rangeIndex][e_index][tempIndex])
                except:
                    dataRec.set2D('te', rangeIndex, 'missing')
                    dataRec.set2D('dte', rangeIndex, 'missing')

                # vo
                try:
                    if isNan(fitsArray[recIndex][beamIndex][rangeIndex][o_index][velIndex]) or abs(fitsArray[recIndex][beamIndex][rangeIndex][o_index][velIndex]) > maxVo or isNan(errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex]) or errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex] > maxVoErr or errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex] <= 1.0e-3:
                            raise ValueError, ''
                    dataRec.set2D('vo', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex][o_index][velIndex])
                    dataRec.set2D('dvo', rangeIndex, errArray[recIndex][beamIndex][rangeIndex][o_index][velIndex])                        
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

                # pm
                """
                try:
                    if isNan(fitsArray[recIndex][beamIndex][rangeIndex][m_index][fractIndex]) or fitsArray[recIndex][beamIndex][rangeIndex][m_index][fractIndex] > maxFract or fitsArray[recIndex][beamIndex][rangeIndex][m_index][fractIndex] < minFract:
                            raise ValueError, ''
                    dataRec.set2D('pm', rangeIndex, fitsArray[recIndex][beamIndex][rangeIndex][m_index][fractIndex])
                    dataRec.set2D('dpm', rangeIndex, 'assumed')                        
                except:
                    dataRec.set2D('pm', rangeIndex, 'missing')
                    dataRec.set2D('dpm', rangeIndex, 'missing')
                """
                
                # col
                try:
                    if isNan(fitsArray[recIndex][beamIndex][rangeIndex][o_index][colIndex]) or fitsArray[recIndex][beamIndex][rangeIndex][o_index][colIndex] <= 0.0:
                            raise ValueError, ''
                    dataRec.set2D('col', rangeIndex, math.log10(fitsArray[recIndex][beamIndex][rangeIndex][o_index][colIndex]))
                    dataRec.set2D('dcol', rangeIndex, 'assumed')                        
                except:
                    dataRec.set2D('col', rangeIndex, 'missing')
                    dataRec.set2D('dcol', rangeIndex, 'missing')                    
                                        
            # append new data record
            cedarObj.append(dataRec)

            # dump records every 100
            if recIndex % 100 == 0: 
                cedarObj.dump('UnblockedBinary')


        # dump remaining records
        cedarObj.dump('UnblockedBinary')

        hdfObj.close()

        self.numRecs = self.numTimes



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

        maxDirErr=327.65

        # create cedarObj if needed
        if cedarObj == None:
            cedarObj = madrigal.cedar.MadrigalCedarFile(madrigalFile, True)

        # read in all required data
        hdfObj = tables.openFile(hdf5File)
        
        # time info
        unixTimes = hdfObj.root.Time.UnixTime
        timeArray = unixTimes.read()
        self.numRecs = timeArray.shape[0]

        # params info
        minAlt = hdfObj.root.ProcessingParams.MinAlt.read()
        maxAlt = hdfObj.root.ProcessingParams.MaxAlt.read()
        pulseLength = hdfObj.root.ProcessingParams.PulseLength.read()
        
        # nmeas - number of measurments
        nmeas = hdfObj.root.VectorVels.Nmeas
        nmeasArray = nmeas.read()
        
        # magnetic latitude bins
        if hasattr(hdfObj.root.VectorVels,'Plat'):
            platArray = hdfObj.root.VectorVels.Plat.read()
        elif hasattr(hdfObj.root.VectorVels,'MagneticLatitude'):
            platArray = hdfObj.root.VectorVels.MagneticLatitude.read()
        numPlat = platArray.shape[0]

        # velocity vectors and errors
        vestArray = hdfObj.root.VectorVels.Vest.read()
        if hasattr(hdfObj.root.VectorVels,'dVest'):
            dvestArray = hdfObj.root.VectorVels.dVest.read()
        elif hasattr(hdfObj.root.VectorVels,'errVest'):
            dvestArray = hdfObj.root.VectorVels.errVest.read()

        # electric field vectors and errors
        eestArray = hdfObj.root.VectorVels.Eest.read()
        if hasattr(hdfObj.root.VectorVels,'dEest'):
            deestArray = hdfObj.root.VectorVels.dEest.read()
        elif hasattr(hdfObj.root.VectorVels,'errEest'):
            deestArray = hdfObj.root.VectorVels.errEest.read()

        # velocity magnitude and direction
        vmagArray = hdfObj.root.VectorVels.Vmag.read()
        if hasattr(hdfObj.root.VectorVels,'dVmag'):
            dvmagArray = hdfObj.root.VectorVels.dVmag.read()
        elif hasattr(hdfObj.root.VectorVels,'errVmag'):
            dvmagArray = hdfObj.root.VectorVels.errVmag.read()
        vdirArray = hdfObj.root.VectorVels.Vdir.read()
        if hasattr(hdfObj.root.VectorVels,'dVdir'):
            dvdirArray = hdfObj.root.VectorVels.dVdir.read()
        elif hasattr(hdfObj.root.VectorVels,'errVdir'):
            dvdirArray = hdfObj.root.VectorVels.errVdir.read()

        # electric field magnitude and direction
        emagArray = hdfObj.root.VectorVels.Emag.read()
        if hasattr(hdfObj.root.VectorVels,'dEmag'):
            demagArray = hdfObj.root.VectorVels.dEmag.read()
        elif hasattr(hdfObj.root.VectorVels,'errEmag'):
            demagArray = hdfObj.root.VectorVels.errEmag.read()
        edirArray = hdfObj.root.VectorVels.Edir.read()
        if hasattr(hdfObj.root.VectorVels,'dEdir'):
            dedirArray = hdfObj.root.VectorVels.dEdir.read()
        elif hasattr(hdfObj.root.VectorVels,'errEdir'):
            dedirArray = hdfObj.root.VectorVels.errEdir.read()
        
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
                                                        ('altb', 'alte','pl'),
                                                        ('nsmpta', 'cgm_lat','vipn','dvipn',
                                                         'vipe', 'dvipe', 'epn', 'depn',
                                                         'epe','depe','magvel','dmagvel',
                                                         'eangle','deangle','nangle','dnangle',
                                                         'magef','dmagef'),
                                                        numPlat)

            # set 1d values
            dataRec.set1D('altb', minAlt/1000.0) # m -> km
            dataRec.set1D('alte', maxAlt/1000.0) # m -> km
            dataRec.set1D('pl', pulseLength) # s

            # set 2d values
            for platIndex in range(numPlat):

                #number of samples
                try:
                    if isNan(nmeasArray[recIndex][platIndex]):
                        raise ValueError, ''
                    dataRec.set2D('nsmpta', platIndex, nmeasArray[recIndex][platIndex])
                except:
                    dataRec.set2D('nsmpta', platIndex, 'missing')
                
                # plat
                try:
                    if isNan(platArray[platIndex][0]) or isNan(platArray[platIndex][1]):
                        raise ValueError, ''
                    dataRec.set2D('cgm_lat', platIndex, (platArray[platIndex][0]+platArray[platIndex][1])/2.0)
                except:
                    dataRec.set2D('cgm_lat', platIndex, 'missing')
                                                    
                # vipn, dvipn
                try:
                    if isNan(vestArray[recIndex][platIndex][north_index]) or isNan(dvestArray[recIndex][platIndex][north_index]) or dvestArray[recIndex][platIndex][north_index]<1e-3:
                        raise ValueError, ''
                    dataRec.set2D('vipn', platIndex, vestArray[recIndex][platIndex][north_index])
                    dataRec.set2D('dvipn', platIndex, dvestArray[recIndex][platIndex][north_index])                
                except:
                    dataRec.set2D('vipn', platIndex, 'missing')
                    dataRec.set2D('dvipn', platIndex, 'missing')

                # vipe, dvipe
                try:
                    if isNan(vestArray[recIndex][platIndex][east_index]) or isNan(dvestArray[recIndex][platIndex][east_index]) or dvestArray[recIndex][platIndex][east_index]<1e-3:
                        raise ValueError, ''
                    dataRec.set2D('vipe', platIndex, vestArray[recIndex][platIndex][east_index])
                    dataRec.set2D('dvipe', platIndex, dvestArray[recIndex][platIndex][east_index])                
                except:
                    dataRec.set2D('vipe', platIndex, 'missing')
                    dataRec.set2D('dvipe', platIndex, 'missing')

                # epn, depn
                try:
                    if isNan(eestArray[recIndex][platIndex][north_index]) or isNan(deestArray[recIndex][platIndex][north_index]) or deestArray[recIndex][platIndex][north_index]<1e-5: 
                        raise ValueError, ''
                    dataRec.set2D('epn', platIndex, eestArray[recIndex][platIndex][north_index])
                    dataRec.set2D('depn', platIndex, deestArray[recIndex][platIndex][north_index])
                except: 
                    dataRec.set2D('epn', platIndex, 'missing')
                    dataRec.set2D('depn', platIndex, 'missing')

                # epe, depe
                try:
                    if isNan(eestArray[recIndex][platIndex][east_index]) or isNan(deestArray[recIndex][platIndex][east_index]) or deestArray[recIndex][platIndex][east_index]<1e-5: 
                        raise ValueError, ''
                    dataRec.set2D('epe', platIndex, eestArray[recIndex][platIndex][east_index])
                    dataRec.set2D('depe', platIndex, deestArray[recIndex][platIndex][east_index])
                except: 
                    dataRec.set2D('epe', platIndex, 'missing')
                    dataRec.set2D('dvipe', platIndex, 'missing')
                
                # vmag, dvmag
                try:
                    if isNan(vmagArray[recIndex][platIndex]) or isNan(dvmagArray[recIndex][platIndex]) or dvmagArray[recIndex][platIndex]<1.0: 
                        raise ValueError, ''
                    dataRec.set2D('magvel', platIndex, vmagArray[recIndex][platIndex])
                    dataRec.set2D('dmagvel', platIndex, dvmagArray[recIndex][platIndex])
                except: 
                    dataRec.set2D('magvel', platIndex, 'missing')
                    dataRec.set2D('dmagvel', platIndex, 'missing')

                # emag, demag
                try:
                    if isNan(emagArray[recIndex][platIndex]) or isNan(demagArray[recIndex][platIndex]) or demagArray[recIndex][platIndex]<2e-5: 
                        raise ValueError, ''
                    dataRec.set2D('magef', platIndex, emagArray[recIndex][platIndex])
                    dataRec.set2D('dmagef', platIndex, demagArray[recIndex][platIndex])
                except: 
                    dataRec.set2D('magef', platIndex, 'missing')
                    dataRec.set2D('dmagef', platIndex, 'missing')

                # vdir, dvdir
                try:
                    if isNan(vdirArray[recIndex][platIndex]) or isNan(dvdirArray[recIndex][platIndex]) or dvdirArray[recIndex][platIndex]>maxDirErr or dvdirArray[recIndex][platIndex]<1e-2: 
                        raise ValueError, ''
                    dataRec.set2D('nangle', platIndex, vdirArray[recIndex][platIndex])
                    dataRec.set2D('dnangle', platIndex, dvdirArray[recIndex][platIndex])
                except: 
                    dataRec.set2D('nangle', platIndex, 'missing')                
                    dataRec.set2D('dnangle', platIndex, 'missing')

                # edir, dedir
                try:
                    if isNan(edirArray[recIndex][platIndex]) or isNan(dedirArray[recIndex][platIndex]) or dedirArray[recIndex][platIndex]>maxDirErr or dedirArray[recIndex][platIndex]<1e-2: 
                        raise ValueError, ''
                    dataRec.set2D('eangle', platIndex, edirArray[recIndex][platIndex])
                    dataRec.set2D('deangle', platIndex, dedirArray[recIndex][platIndex])
                except: 
                    dataRec.set2D('eangle', platIndex, 'missing')
                    dataRec.set2D('deangle', platIndex, 'missing')

                #print "%d %2.2f %2.2f" % (platIndex,1e5*eestArray[recIndex][platIndex][north_index],1e5*deestArray[recIndex][platIndex][north_index])
                #print "%d %2.2f %2.2f" % (platIndex,vestArray[recIndex][platIndex][east_index],dvestArray[recIndex][platIndex][east_index])

            # append new data record
            cedarObj.append(dataRec)

            # dump records every 100
            if recIndex % 100 == 0: 
                cedarObj.dump('UnblockedBinary')


        # dump remaining records
        cedarObj.dump('UnblockedBinary')

        hdfObj.close()


class hdf5VelocityAltToMadrigal:
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

        maxDirErr=327.65

        # create cedarObj if needed
        if cedarObj == None:
            cedarObj = madrigal.cedar.MadrigalCedarFile(madrigalFile, True)

        # read in all required data
        hdfObj = tables.openFile(hdf5File)
        
        # time info
        unixTimes = hdfObj.root.Time.UnixTime
        timeArray = unixTimes.read()
        self.numRecs = timeArray.shape[0]

        # params info
        minAlt = hdfObj.root.ProcessingParams.MinAlt.read()
        maxAlt = hdfObj.root.ProcessingParams.MaxAlt.read()
        pulseLength = hdfObj.root.ProcessingParams.PulseLength.read()
        
        # nmeas - number of measurments
        nmeas = hdfObj.root.VectorVels.Nmeas
        nmeasArray = nmeas.read()
        
        # altitude
        alt = hdfObj.root.VectorVels.Altitude
        altArray = alt.read()
        numAlt = altArray.shape[1]

        # velocity vectors and errors
        vestArray = hdfObj.root.VectorVels.Vest.read()
        dvestArray = hdfObj.root.VectorVels.dVest.read()

        # velocity magnitude and direction
        vmagArray = hdfObj.root.VectorVels.Vmag.read()
        dvmagArray = hdfObj.root.VectorVels.dVmag.read()
        vdirArray = hdfObj.root.VectorVels.Vdir.read()
        dvdirArray = hdfObj.root.VectorVels.dVdir.read()         
        
        # Avg electric field
        eestArray = hdfObj.root.VectorVels.AvgElectricField.read()
        deestArray = hdfObj.root.VectorVels.dAvgElectricField.read()
        
        # create all data records 
        for recIndex in range(self.numRecs):
            #print 'rec: %d' % recIndex
         
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
                                                        ('altb', 'alte','pl','epe','depe','epn','depn'),
                                                        ('nsmpta', 'gdalt','vipn','dvipn','vipe', 'dvipe',
                                                         'vi6', 'dvi6','magvel','dmagvel','nangle','dnangle'),
                                                        numAlt)

            # set 1d values
            dataRec.set1D('altb', minAlt/1000.0) # m -> km
            dataRec.set1D('alte', maxAlt/1000.0) # m -> km
            dataRec.set1D('pl', pulseLength) # s

            # epn, depn
            try:
                if isNan(eestArray[recIndex][0][north_index]) or isNan(deestArray[recIndex][0][north_index]):
                    raise ValueError, ''
                dataRec.set1D('epn',eestArray[recIndex][0][north_index])
                dataRec.set1D('depn',deestArray[recIndex][0][north_index])
            except:
                dataRec.set1D('epn','missing')
                dataRec.set1D('depn','missing')
            
            # epe, depe
            try:
                if isNan(eestArray[recIndex][0][east_index]) or isNan(deestArray[recIndex][0][east_index]):
                    raise ValueError, ''
                dataRec.set1D('epe',eestArray[recIndex][0][east_index])
                dataRec.set1D('depe',deestArray[recIndex][0][east_index])
            except:
                dataRec.set1D('epe','missing')
                dataRec.set1D('depe','missing')                
                
            # set 2d values
            for altIndex in range(numAlt):
                BadRecord=0

                #number of samples
                try:
                    if isNan(nmeasArray[recIndex][altIndex]) or nmeasArray[recIndex][altIndex]<0:
                        BadRecord=1
                        raise ValueError, ''
                    dataRec.set2D('nsmpta', altIndex, nmeasArray[recIndex][altIndex])
                except:
                    dataRec.set2D('nsmpta', altIndex, 'missing')
                
                # alt
                try:
                    if isNan(altArray[recIndex][altIndex]) or BadRecord:
                        BadRecord=1
                        raise ValueError, ''
                    dataRec.set2D('gdalt', altIndex, altArray[recIndex][altIndex]/1000.0)
                except:
                    dataRec.set2D('gdalt', altIndex, 'missing')
                                                    
                # vipn, dvipn
                try:
                    if BadRecord or isNan(vestArray[recIndex][altIndex][north_index]) or isNan(dvestArray[recIndex][altIndex][north_index]) or dvestArray[recIndex][altIndex][north_index]<1e-3:
                        raise ValueError, ''
                    dataRec.set2D('vipn', altIndex, vestArray[recIndex][altIndex][north_index])
                    dataRec.set2D('dvipn', altIndex, dvestArray[recIndex][altIndex][north_index])                
                except:
                    dataRec.set2D('vipn', altIndex, 'missing')
                    dataRec.set2D('dvipn', altIndex, 'missing')

                # vipe, dvipe
                try:
                    if BadRecord or isNan(vestArray[recIndex][altIndex][east_index]) or isNan(dvestArray[recIndex][altIndex][east_index]) or dvestArray[recIndex][altIndex][east_index]<1e-3:
                        raise ValueError, ''
                    dataRec.set2D('vipe', altIndex, vestArray[recIndex][altIndex][east_index])
                    dataRec.set2D('dvipe', altIndex, dvestArray[recIndex][altIndex][east_index])                
                except:
                    dataRec.set2D('vipe', altIndex, 'missing')
                    dataRec.set2D('dvipe', altIndex, 'missing')
					
                # vipar, dvipar
                try:
                    if BadRecord or isNan(vestArray[recIndex][altIndex][parallel_index]) or isNan(dvestArray[recIndex][altIndex][parallel_index]) or dvestArray[recIndex][altIndex][parallel_index]<1.0:
                        raise ValueError, ''
                    dataRec.set2D('vi6', altIndex, vestArray[recIndex][altIndex][parallel_index])
                    dataRec.set2D('dvi6', altIndex, dvestArray[recIndex][altIndex][parallel_index])                
                except:
                    dataRec.set2D('vi6', altIndex, 'missing')
                    dataRec.set2D('dvi6', altIndex, 'missing')					

                # vmag, dvmag
                try:
                    if BadRecord or isNan(vmagArray[recIndex][altIndex]) or isNan(dvmagArray[recIndex][altIndex]) or dvmagArray[recIndex][altIndex]<1.0: 
                        raise ValueError, ''
                    dataRec.set2D('magvel', altIndex, vmagArray[recIndex][altIndex])
                    dataRec.set2D('dmagvel', altIndex, dvmagArray[recIndex][altIndex])
                except: 
                    dataRec.set2D('magvel', altIndex, 'missing')
                    dataRec.set2D('dmagvel', altIndex, 'missing')

                # vdir, dvdir
                try:
                    if BadRecord or isNan(vdirArray[recIndex][altIndex]) or isNan(dvdirArray[recIndex][altIndex]) or dvdirArray[recIndex][altIndex]>maxDirErr or dvdirArray[recIndex][altIndex]<1e-2: 
                        raise ValueError, ''
                    dataRec.set2D('nangle', altIndex, vdirArray[recIndex][altIndex])
                    dataRec.set2D('dnangle', altIndex, dvdirArray[recIndex][altIndex])
                except: 
                    dataRec.set2D('nangle', altIndex, 'missing')                
                    dataRec.set2D('dnangle', altIndex, 'missing')

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

        # ranges (in meters)
        ranges = hdfObj.root.NeFromPower.Range
        rangeArray = ranges.read()
        numRanges = rangeArray.shape[2]
        rangeArray = rangeArray[0,0,:]
        lowerRangeIndex = None
        if lowerRange == None:
            lowerRangeIndex = 0
        upperRangeIndex = None
        if upperRange == None:
            upperRangeIndex = numRanges
        for i in range(numRanges):
            if lowerRangeIndex == None and rangeArray[i]/1000.0 >=  lowerRange:
                lowerRangeIndex = i
            if upperRangeIndex == None and rangeArray[-1 - i]/1000.0 <  upperRange:
                upperRangeIndex = numRanges - i
        if lowerRangeIndex == None:
            # no ranges accepted
            raise IOError, 'No valid ranges found between limits %s and %s' % (str(lowerRange), str(upperRange))
        if upperRangeIndex == None:
            upperRangeIndex = numRanges
        rangeArray = ranges.read()
            
        numUsedRanges = upperRangeIndex-lowerRangeIndex
        if numUsedRanges <= 0:
            # no ranges accepted
            raise IOError, 'No valid ranges found between limits %s and %s' % (str(lowerRange), str(upperRange))

        # altitude
        altArray = hdfObj.root.NeFromPower.Altitude.read()

        # uncorrected electron density (pop)
        pop = hdfObj.root.NeFromPower.Ne_NoTr
        popArray = pop.read()
        self.numTimes = popArray.shape[0]

        # error in electron density
        dpop = hdfObj.root.NeFromPower.dNeFrac
        dpopArray = dpop.read()

        # snr
        snr = hdfObj.root.NeFromPower.SNR
        snrArray = snr.read()

        # time info
        days = hdfObj.root.Time.Day
        dayArray = days.read()
        months = hdfObj.root.Time.Month
        monthArray = months.read()
        years = hdfObj.root.Time.Year
        yearArray = years.read()
        dtimes = hdfObj.root.Time.dtime
        dtimeArray = dtimes.read()
        
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

        # azimuth
        azArray = hdfObj.root.Antenna.Azimuth.read()
        azmArray = hdfObj.root.Antenna.AvgAzimuth.read()
        
        # elevation
        elArray = hdfObj.root.Antenna.Elevation.read()
        elmArray = hdfObj.root.Antenna.AvgElevation.read()
        
        # event
        evntArray = hdfObj.root.Antenna.Event.read()
        
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
                                                        ('azm', 'elm', 'az1','az2','el1','el2','power','cbadl', 'pl', 'tfreq', 'rfreq','posf'),
                                                        ('range','gdalt', 'popl', 'dpopl','sn'),
                                                        numUsedRanges)

            # set 1d values
            tazm=azmArray[recIndex]
            taz1=azArray[recIndex,0]
            taz2=azArray[recIndex,1]
            while tazm>180.0:
                tazm=tazm-360.0
            while taz1>180.0:
                taz1=taz1-360.0
            while taz2>180.0:
                taz2=taz2-360.0                            
            dataRec.set1D('azm', tazm)
            dataRec.set1D('elm', elmArray[recIndex])
            dataRec.set1D('az1', taz1)
            dataRec.set1D('az2', taz2)
            dataRec.set1D('el1', elArray[recIndex,0])
            dataRec.set1D('el2', elArray[recIndex,1])
            dataRec.set1D('power', txPowerArray[recIndex]/1000.0) # cedar in kWatts, SRI in Watts
            dataRec.set1D('cbadl', baudCount)
            dataRec.set1D('pl', pulseLength)
            dataRec.set1D('tfreq', txFreq)
            dataRec.set1D('rfreq', rxFreq)
            if recIndex<(self.numTimes-1) and evntArray[recIndex]==evntArray[recIndex+1]:
                dataRec.set1D('posf', 0)
            else:
                dataRec.set1D('posf', 1)

            beamIndex=0

            # set 2d values
            for rangeIndex in range(lowerRangeIndex, upperRangeIndex):
                
                # range
                try:
                    if isNan(rangeArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('range', rangeIndex-lowerRangeIndex, rangeArray[recIndex][beamIndex][rangeIndex]/1000.0) # convert m -> km                    
                except:
                    dataRec.set2D('range', rangeIndex-lowerRangeIndex, 'missing')

                # altitude
                try:
                    if isNan(altArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, ''
                    dataRec.set2D('gdalt', rangeIndex-lowerRangeIndex, altArray[recIndex][beamIndex][rangeIndex]/1000.0) # convert m -> km                    
                except:
                    dataRec.set2D('gdalt', rangeIndex-lowerRangeIndex, 'missing')
                    
                # pop
                try:
                    if isNan(popArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, 'popl isNaN'
                    dataRec.set2D('popl', rangeIndex-lowerRangeIndex, math.log10(popArray[recIndex][beamIndex][rangeIndex]))

                    if dpopArray[recIndex][beamIndex][rangeIndex] <= 0.0 or isNan(dpopArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, 'problem with dpopl'
                    dataRec.set2D('dpopl', rangeIndex-lowerRangeIndex, math.log10(dpopArray[recIndex][beamIndex][rangeIndex]*popArray[recIndex][beamIndex][rangeIndex]))
                    
                except:
                    dataRec.set2D('popl', rangeIndex-lowerRangeIndex, 'missing')
                    dataRec.set2D('dpopl', rangeIndex-lowerRangeIndex, 'missing')
                    
                # snr
                try:
                    if isNan(snrArray[recIndex][beamIndex][rangeIndex]):
                        raise ValueError, 'snr isNaN'
                    dataRec.set2D('sn', rangeIndex-lowerRangeIndex,snrArray[recIndex][beamIndex][rangeIndex])

                except:
                    dataRec.set2D('sn', rangeIndex-lowerRangeIndex, 'missing')
                    dataRec.set2D('sn', rangeIndex-lowerRangeIndex, 'missing')                        

            # append new data record
            cedarObj.append(dataRec)

            # dump records every 100
            if recIndex % 100 == 0: 
                cedarObj.dump('UnblockedBinary')


        # dump remaining records
        cedarObj.dump('UnblockedBinary')

        hdfObj.close()

        self.numRecs = self.numTimes

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
