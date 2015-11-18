"""The admin module contains all administrative classes relating to the madrigal python api.

The main role of this module is to update the data in the Madrigal database.  Also contains a
notification class and a standard error handing class.

$Id: admin.py,v 1.35 2009/04/21 21:09:53 brideout Exp $
"""

import os, os.path, sys
import smtplib
import datetime, time
import types
import traceback
import shutil
import md5
import urllib2
import re

import madrigal.metadata
import madrigal.openmadrigal
import madrigal.data
import madrigal._Madrec


class MadrigalDBAdmin:
    """MadrigalDBAdmin is a class that allows modifications to be made to the Madrigal database

    dbAdminObj = madrigal.admin.MadrigalDBAdmin()

    expDir = dbAdminObj.createMadrigalExperiment('/home/hyperion/brideout/mlh050429c.000',
                                        'Dummy experiment',
                                        0,
                                        'test exp',
                                        30,
                                        1)

    Non-standard Python modules used: None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  May. 5, 2005

    """
    
    def __init__(self, madDB = None):
        """__init__ initializes MadrigalDBAdmin
        
        Inputs: madDB - Existing MadrigalDB object.  Default = None.
        
        Returns: void

        Affects:

            Sets self.__madDB to MadrigalDB object
            Sets self.__madInst to MadrigalInstrument object
        """

        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        self.__madInst = madrigal.metadata.MadrigalInstrument(self.__madDB)
        self.__openMad = madrigal.openmadrigal.OpenMadrigal(self.__madDB)
        self.__madSite = madrigal.metadata.MadrigalSite(self.__madDB)

        
            

    def createRTExperiment(self,
                           startTime,
                           numDays,
                           instrument,
                           expTitle,
                           rtFilenameList,
                           kindatList,
                           permissionList,
                           fileDescList,
                           optChar = '',
                           endTime = None,
                           security = 0,
                           dirName = None):
        """createRTExperiment creates a new experiment on Madrigal in preparation for realtime data.

        Since the experiment is presumably not yet complete, metadata such as the duration of the experiment
        must be estimated.  This metadata will be overwritten when the first batch file is added.

        Inputs:
        
            startTime - experiment start time.  If a number, assumed to be seconds since 1/1/1970.  May also
            be a datetime.datetime object
            
            numDays - number of days the experiment is estimated to run.  Ignored if optional endTime given.
            
            instrument - instrument code or 3-letter Madrigal mnenonic
            
            expTitle - experiment title
            
            rtFilenameList - list of realtime filenames to be created
            
            kindatList - list of ints of kindats for each realtime file.  Len = len(rtFilenameList)
            
            permissionList - list of 0 (public) or 1 (private). Len = len(rtFilenameList)
            
            fileDescList - list of realtime file descriptions

            optChar - optional character to be added to experiment directory if no dirName
                      given.  If dirName argument given, this argument ignored.  optChar
                      is used if the default directory name DDmmmYY is used for
                      more than one experiment created for a given instrument on a given day.
                      For example, if --optChar=h for a MLH experiment on September 12, 2005,
                      then the experiment directory created would be experiments/2005/mlh/12sep05h.

            endTime - optional end date and time of experiment.  If a number, assumed to be seconds since
            1/1/1970.  May also be a datetime.datetime object

            security - experiment security setting.  If 0 (the default) public.  If 1, private.
                       If -1, entire experiment ignored.  Any given file permission is the more
                       restricted of experiment permission and file permission.

            dirName - directory name to use for experiment.  If None (the default), the directory
                      name will be the default name DDmmmYY[optChar].  Cannot contain "/"

        Returns:

            Full path to directory created
            
        """
        # check optChar
        if type(optChar) != types.StringType:
            raise 'optChar must be an empty or a one character string, not %s' % (str(optChar))

        if len(optChar) > 1:
            raise 'optChar must be an empty or a one character string, not %s' % (str(optChar))

        security = int(security)
        if security not in (-1,0,1):
            raise ValueError, 'security must be -1, 0, or 1, not %i' % (security)
        
        # convert startTime to datetime if needed
        if type(startTime) in (types.IntType, types.LongType, types.FloatType):
            startTime = datetime.datetime.utcfromtimestamp(startTime)

        # create endtTime based on numDays
        if endTime != None:
            if type(endTime) in (types.IntType, types.LongType, types.FloatType):
                endTime = datetime.datetime.utcfromtimestamp(startTime)
        else:
            if numDays >= 0:
                endTime = startTime + datetime.timedelta(numDays) - datetime.timedelta(0,1)
            else:
                raise ValueError, 'numDays must not be negative'

        if startTime >= endTime:
            raise ValueError, 'Experiment start time %s after end time %s' % (str(startTime),
                                                                               str(endTime))
        

        # get instrument mnemonic and instCode
        try:
            instCode = int(instrument)
            instMnemonic = self.__madInst.getInstrumentMnemonic(instCode)
        except ValueError:
            if len(instrument) != 3:
                raise ValueError, '%s not a legal instrument mnemonic' % (str(instrument))
            instMnemonic = instrument.lower()
            # verify its a legal mnemonic
            instList = self.__madInst.getInstrumentList()
            found = False
            for inst in instList:
                if instMnemonic == inst[1]:
                    found = True
                    instCode = inst[2]
                    break
            if found == False:
                raise ValueError, '%s not a legal instrument mnemonic or code' % (str(instrument))

        if instMnemonic == None:
            raise ValueError, '%s not a legal instrument mnemonic or code' % (str(instrument))

        # expTitle
        if type(expTitle) != types.StringType and expTitle != None:
            raise ValueError, 'expTitle not a string'
        if expTitle == None:
            expTitle = ''

        # rtFilenameList
        if type(rtFilenameList) not in (types.ListType, types.TupleType):
            raise ValueError, 'rtFilenameList not a list or tuple'
        if len(rtFilenameList) == 0:
            raise ValueError, 'rtFilenameList cannot be empty'
        # make sure each rtFilename is a string without /
        for filename in rtFilenameList:
            if type(filename) != types.StringType:
                raise ValueError, 'rtFilenameList must contain strings'
            if filename.find('/') != -1:
                raise ValueError, 'rtFilenameList must contain strings without /'

        # kindatList
        if len(kindatList) != len(rtFilenameList):
            raise ValueError, 'length of kindatList not equal length of rtFilenameList'
        for item in kindatList:
            try:
                int(item)
            except:
                raise ValueError, 'kindatList must contain integers'

        # permissionList
        if len(permissionList) != len(rtFilenameList):
            raise ValueError, 'length of permissionList not equal length of rtFilenameList'
        for item in permissionList:
            try:
                permission = int(item)
            except:
                raise ValueError, 'kindatList must contain integers'

            if permission not in (0,1):
                raise ValueError, 'kindatList must contain integers of value 0 (public) or 1 (private)'

        
        # fileDescList
        if len(fileDescList) != len(rtFilenameList):
            raise ValueError, 'length of fileDescList not equal length of rtFilenameList'
        for item in fileDescList:
            if type(item) != types.StringType:
                raise ValueError, 'fileDescList must only contain strings'

        # all the arguments check out - create the directory
        # create experiment dir
        expDir2 = os.path.join('%i' % (startTime.year),
                               instMnemonic)
        
        if dirName == None:
            dirName = startTime.strftime('%d%b%y').lower() + optChar
        else:
            # verify dirName is basename
            if dirName.find('/') != -1:
                raise ValueError, 'dirName must be base directory name, not %s' % (dirName)

        expDir2 = os.path.join(expDir2, dirName)

        expDir = os.path.join(self.__madDB.getMadroot(), 'experiments', expDir2)


        # if the directory already exists, raise error
        if os.access(expDir, os.R_OK):
            raise IOError, 'Directory %s already exists' % (expDir)

        os.makedirs(expDir)
        os.chmod(expDir, 0775)

        # expTab.txt
        expTabText = '0,' + self.__madDB.getCGIHomeBase()
        if expTabText[-1] != '/':
            expTabText += '/madtoc/'
        else:
            expTabText += 'madtoc/'
        expTabText += '%s,%s,%i,%04i%02i%02i,%02i%02i%02i,%04i%02i%02i,%02i%02i%02i,%i,%i\n' %(expDir2,
                                                                                               expTitle,
                                                                                               self.__madDB.getSiteID(),
                                                                                               startTime.year,
                                                                                               startTime.month,
                                                                                               startTime.day,
                                                                                               startTime.hour,
                                                                                               startTime.minute,
                                                                                               startTime.second,
                                                                                               endTime.year,
                                                                                               endTime.month,
                                                                                               endTime.day,
                                                                                               endTime.hour,
                                                                                               endTime.minute,
                                                                                               endTime.second,
                                                                                               instCode,
                                                                                               security)

        # write expTab.txt
        f = open(os.path.join(expDir, 'expTab.txt'), 'w')
        f.write(expTabText)
        f.close()
        os.chmod(os.path.join(expDir, 'expTab.txt'), 0664)

        # fileTab.txt
        fileTabText = ''
        for index in range(len(rtFilenameList)):
            
            fileTabText += rtFilenameList[index]
            fileTabText += ',0,%i,4,0,0,0,0,0,' % (kindatList[index])
            fileTabText += '%s,%i\n' % (fileDescList[index], permissionList[index])

        # write fileTab.txt
        f = open(os.path.join(expDir, 'fileTab.txt'), 'w')
        f.write(fileTabText)
        f.close()
        os.chmod(os.path.join(expDir, 'fileTab.txt'), 0664)

        # create all writeable directory overview
        os.makedirs(os.path.join(expDir, 'overview'))
        os.chmod(os.path.join(expDir, 'overview'), 0777)

        return(expDir)
        
        
    def writeRTMadrigalFile(self,
                            expDir,
                            rtFilename,
                            rtFile):
        """writeRTMadrigalFile writes a realtime Madrigal file to a Madrigal experiment directory.

        Fails if rtFilename does not match one listed in fileTab.txt.

        Inputs:
        
            expDir - full path to experiment directory (as returned by createRTExperiment)
            
            rtFilename - basename of realtime file to be writtem
            
            rtFile - a string containing the realtime file contents

        Returns: None

        Raises exception if rtFilename does not match one listed in fileTab.txt.
        """
        # verify rtFilename listed in fileTab.txt
        try:
            fileInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, 'Unable to open fileTab.txt in %s' % (expDir)
        
        if fileInfo.getHasCatalogByFilename(rtFilename) == None:
            raise ValueError, 'Filename %s not found in fileTab.txt' % (rtFilename)

        # okay - write it
        f = open(os.path.join(expDir, rtFilename), 'w')
        f.write(rtFile)
        f.close()
        os.chmod(os.path.join(expDir, rtFilename), 0664)


    def appendRTMadrigalFile(self,
                             expDir,
                             rtFilename,
                             rtFile):
        """appendRTMadrigalFile allows appending to a realtime Madrigal file.

        Fails if rtFilename does not match one listed in fileTab.txt.  If the file doesn't exist, will
        create it, and will then check that the format is unblocked binary.  This is because unblocked binary
        is written by logical record, without physical records that may later be modified.  If the file does
        exist, rtFile will be appended with checking the file format.

        Inputs:
        
            expDir - full path to experiment directory (as returned by createRTExperiment)
            
            rtFilename - basename of realtime file to be writtem
            
            rtFile - a string containing the new realtime file contents

        Returns: None

        Raises exception if rtFilename does not match one listed in fileTab.txt, or if format != unblocked binary
        when new file is created.
        """
        # verify rtFilename listed in fileTab.txt
        try:
            fileInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, 'Unable to open fileTab.txt in %s' % (expDir)
        
        if fileInfo.getHasCatalogByFilename(rtFilename) == None:
            raise ValueError, 'Filename %s not found in fileTab.txt' % (rtFilename)

        # see if it already exists
        exists = os.access(os.path.join(expDir, rtFilename), os.R_OK)

        # okay - append it
        f = open(os.path.join(expDir, rtFilename), 'a')
        f.write(rtFile)
        f.close()
        if not exists:
            # check that its unblocked binary
            filetype = madrigal._Madrec.getFileType(os.path.join(expDir, rtFilename))
            if filetype != 'UnblockedBinary':
                os.remove(os.path.join(expDir, rtFilename))
                raise ValueError, 'Expected file %s to be UnblockedBinary, is %s' % (os.path.join(expDir, rtFilename),
                                                                                     filetype)
            os.chmod(os.path.join(expDir, rtFilename), 0664)


    def createMadrigalExperiment(self,
                                 madFilename,
                                 expTitle,
                                 permission,
                                 fileDesc,
                                 instCode = None,
                                 category = 1,
                                 optChar = '',
                                 dirName = None,
                                 kindat = None):
        """createMadrigalExperiment creates a new experiment on Madrigal using metadata read from madFilename.

        Inputs:
        
            madFilename - full path to the complete Madrigal file.  Basename will be maintained.
            
            expTitle - experiment title
            
            permission - 0 (public) or 1 (private) or -1 (ignore). 
            
            fileDesc - file description

            instCode - instrument code.  If default (None), instrument code is taken from file, but error
            is thrown if more than one kinst found.

            category - 1=default, 2=variant, 3=history, or 4=realtime. Default is 1 (default file)

            optChar - optional character to be added to experiment directory if no dirName
                      given.  If dirName argument given, this argument ignored.  optChar
                      is used if the default directory name DDmmmYY is used for
                      more than one experiment created for a given instrument on a given day.
                      For example, if --optChar=h for a MLH experiment on September 12, 2005,
                      then the experiment directory created would be experiments/2005/mlh/12sep05h.

            dirName - directory name to use for experiment.  If None (the default), the directory
                      name will be the default name DDmmmYY[optChar].  Cannot contain "/"

            kindat - if not None (the default), use this kindat instead of what is found in the file.

        Returns:

            Full path to directory created
            
        """
        # check optChar
        if type(optChar) != types.StringType:
            raise ValueError, 'optChar must be an empty or a one character string, not %s' % (str(optChar))

        if len(optChar) > 1:
            raise ValueError, 'optChar must be an empty or a one character string, not %s' % (str(optChar))
        
        fileInfo = madrigal.data.MadrigalFile(madFilename, self.__madDB)

        # get startTime
        sTime = fileInfo.getEarliestTime()
        startTime = datetime.datetime(sTime[0],sTime[1],sTime[2],sTime[3],sTime[4],sTime[5])

        # get endTime
        eTime = fileInfo.getLatestTime()
        endTime = datetime.datetime(eTime[0],eTime[1],eTime[2],eTime[3],eTime[4],eTime[5])
        
        # get instrument mnemonic and instCode
        if instCode == None:
            kinstList = fileInfo.getKinstList()
            if len(kinstList) == 0:
                raise ValueError, 'No kinst values found in file'
            if len(kinstList) > 1:
                raise ValueError, 'More than one kinst value found in file: %s' % (str(kinstList))
            instCode = kinstList[0]
        instMnemonic = self.__madInst.getInstrumentMnemonic(instCode)
        if instMnemonic == None:
            raise ValueError, 'Unable to find mnemonic for kinst %i' % (instCode)

        # expTitle
        if type(expTitle) != types.StringType and expTitle != None:
            raise ValueError, 'expTitle not a string'
        if expTitle == None:
            expTitle = ''

        # kindat
        if kindat == None:
            kindatList = fileInfo.getKindatList()
            if len(kindatList) == 0:
                raise ValueError, 'No kindat values found in file'
            if len(kindatList) > 1:
                raise ValueError, 'More than one kindat value found in file: %s' % (str(kindatList))
            kindat = kindatList[0]
        else:
            kindat = int(kindat)

        # permission
        if permission not in (0,1, -1):
            raise ValueError, 'permission must be either 0 or 1 or -1, not %s' % (str(permission))

        # fileDesc
        if type(fileDesc) != types.StringType and fileDesc != None:
            raise ValueError, 'fileDesc not a string'
        if fileDesc == None:
            fileDesc = ''

        # category
        if category not in (1,2,3,4):
            raise ValueError, 'category must be 1=default, 2=variant, 3=history, or 4=realtime; not %s' % (str(category))

        # hasCatalog and hasHeader
        catList, headList = madrigal._Madrec.cedarCatalogHeaderList(madFilename)
        if len(catList) > 0:
            hasCatalog = 1
        else:
            hasCatalog = 0
        if len(headList) > 0:
            hasHeader = 1
        else:
            hasHeader = 0
        

        # all the arguments check out - create the directory
        # create experiment dir
        expDir2 = os.path.join('%i' % (startTime.year),
                              instMnemonic)
        
        if dirName == None:
            dirName = startTime.strftime('%d%b%y').lower() + optChar
        else:
            # verify dirName is basename
            if dirName.find('/') != -1:
                raise ValueError, 'dirName must be base directory name, not %s' % (dirName)
            
        expDir2 = os.path.join(expDir2, dirName)

        expDir = os.path.join(self.__madDB.getMadroot(), 'experiments', expDir2)

        # if the directory already exists, raise error
        if os.access(expDir, os.R_OK):
            raise IOError, 'Directory %s already exists' % (expDir)

        os.makedirs(expDir)
        os.chmod(expDir, 0775)

        # expTab.txt
        expTabText = '0,' + self.__madDB.getCGIHomeBase()
        if expTabText[-1] != '/':
            expTabText += '/madtoc/'
        else:
            expTabText += 'madtoc/'
        expTabText += '%s,%s,%i,%04i%02i%02i,%02i%02i%02i,%04i%02i%02i,%02i%02i%02i,%i,%i\n' %(expDir2,
                                                                                               expTitle,
                                                                                               self.__madDB.getSiteID(),
                                                                                               startTime.year,
                                                                                               startTime.month,
                                                                                               startTime.day,
                                                                                               startTime.hour,
                                                                                               startTime.minute,
                                                                                               startTime.second,
                                                                                               endTime.year,
                                                                                               endTime.month,
                                                                                               endTime.day,
                                                                                               endTime.hour,
                                                                                               endTime.minute,
                                                                                               endTime.second,
                                                                                               instCode,
                                                                                               permission)

        # write expTab.txt
        f = open(os.path.join(expDir, 'expTab.txt'), 'w')
        f.write(expTabText)
        f.close()
        os.chmod(os.path.join(expDir, 'expTab.txt'), 0664)

        # fileTab.txt
        fileTabText = os.path.basename(madFilename)
        fileTabText += ',0,%i,%i,0,%i,%i,0,0,' % (kindat,category,hasCatalog,hasHeader)
        fileTabText += '%s,%i\n' % (fileDesc, permission)

        # write fileTab.txt
        f = open(os.path.join(expDir, 'fileTab.txt'), 'w')
        f.write(fileTabText)
        f.close()
        os.chmod(os.path.join(expDir, 'fileTab.txt'), 0664)

        # create all writeable directory overview
        os.makedirs(os.path.join(expDir, 'overview'))
        os.chmod(os.path.join(expDir, 'overview'), 0777)

        # cp madFilename to new directory
        shutil.copy(madFilename, os.path.join(expDir, os.path.basename(madFilename)))
        os.chmod(os.path.join(expDir, os.path.basename(madFilename)), 0664)

        # populate overview
        fileInfo = madrigal.data.MadrigalFile(os.path.join(expDir, os.path.basename(madFilename)), self.__madDB)

        return(expDir)



    def changeExpStatus(self,
                        expDir,
                        expUrl=None,
                        expName = None,
                        siteID = None,
                        startDatetime = None,
                        endDatetime = None,
                        inst = None,
                        security = None):
        """changeExpStatus is used to change attributes in expTab.txt.  If None, no change.

        Inputs:
        
            expDir - full path to experiment directory. Required.  Example:
               "/opt/madrigal/experiments/1998/mlh/20jan98". If None, do not change.
            
            expUrl - must be in form <cgi base>/madtoc/YYYY/<3 letter lower case inst code>/<expDir>
                       example: http://www.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97g.
                       If None, do not change.

            expName - experiment name.  Quotes required if contains spaces.  Example: "World Day"
                        If None, do not change.

            siteID - Madrigal siteID (int) of where data will be stored.  Error raised if not the siteID
                       of the local Madrigal site. Example: 4. If None, do not change.

            startDatetime - new start datetime of experiment (UT). If None, do not change.

            endDatetime - new end datetime of experiment (UT). If None, do not change.

            inst - new instrument code (int).  Example: 30. If None, do not change.  Prints
                    warning if not found in instTab.txt

            security - new security code.  Allowed values are 0 for public, 1 for private (limited IP range access)
                    -1 for ignore, 2 for archived experiment, 3 for private (limited IP range access) archived
                    experiment. If None, do not change.
            
        """
        try:
            expTabInfo = madrigal.metadata.MadrigalExperiment(self.__madDB, os.path.join(expDir, 'expTab.txt'))
        except:
            raise ValueError, 'Unable to open expTab.txt in %s' % (expDir)

        # be sure only one experiment
        if expTabInfo.getExpCount() != 1:
            raise ValueError, 'expTab.txt in %s has %i experiments, should have exactly 1' % (expDir,
                                                                                              expTabInfo.getExpCount())

        # expUrl
        if expUrl != None:
            # print warning if not this directory
            index = expUrl.find('/madtoc/')
            thisDir = expUrl[index+8:]
            if thisDir[-1] == '/':
                thisDir = thisDir[:-1]
            if expDir.find(thisDir) == -1:
                raise ValueError, 'The experiment url you are setting this experiment to <%s> conflicts with experiment directory %s' % (expUrl, expDir)
            expTabInfo.setExpUrlByPosition(0, expUrl)

        # expName
        if expName != None:
            expTabInfo.setExpNameByPosition(0, expName)

        # siteID
        if siteID != None:
            siteID = int(siteID)
            if siteID != self.__madDB.getSiteID():
                raise ValueError, 'Setting experiment to a siteID %i different from this site\'s id %i' % (siteID,
                                                                                                                    self.__madDB.getSiteID())
            expTabInfo.setExpSiteIdByPosition(0, siteID)

        # startDatetime
        if startDatetime != None:
            # verify before endDatetime if that also being set
            if endDatetime != None:
                if startDatetime > endDatetime:
                    raise ValueError, 'startDatetime %s must be before endDatetime %s' % (str(startDatetime),
                                                                                          str(endDatetime))
            expTabInfo.setExpStartDateTimeByPosition(startDatetime, 0)

        # endDatetime
        if endDatetime != None:
            expTabInfo.setExpEndDateTimeByPosition(endDatetime, 0)

        # inst
        if inst != None:
            inst = int(inst)
            if self.__madInst.getInstrumentName(inst) == None:
                print 'WARNING: instrument %i not found in instTab.txt'
            expTabInfo.setExpKinstByPosition(0, inst)

        # security
        if security != None:
            security = int(security)
            if security not in (-1, 0, 1, 2, 3):
                raise ValueError, 'security must be in (-1, 0, 1, 2, 3), not %i' % (inst)
            expTabInfo.setSecurityByPosition(0, security)

        # everything successfully changed - write new values
        expTabInfo.writeMetadata()
        
            

    def addMadrigalFile(self,
                        expDir,
                        madFilename,
                        permission,
                        fileDesc,
                        category = 1,
                        kindat = None):
        """addMadrigalFile adds a new file to an experiment using metadata read from madFilename.

        Inputs:

            expDir - full path to experiment directory (as returned by createMadriogalExperiment)
        
            madFilename - full path to the complete Madrigal file.  Basename will be maintained.
            
            permission - 0 (public) or 1 (private). 
            
            fileDesc - file description

            category - 1=default, 2=variant, 3=history, or 4=realtime. Default is 1 (default file)

            kindat - if not None (the default), use this kindat instead of what is found in the file.

        Returns: None
            
        """
        fileInfo = madrigal.data.MadrigalFile(madFilename, self.__madDB)

        # kindat
        if kindat == None:
            kindatList = fileInfo.getKindatList()
            if len(kindatList) == 0:
                raise ValueError, 'No kindat values found in file'
            if len(kindatList) > 1:
                raise ValueError, 'More than one kindat value found in file: %s' % (str(kindatList))
            kindat = kindatList[0]
        else:
            kindat = int(kindat)

        # permission
        if permission not in (0,1):
            raise ValueError, 'permission must be either 0 or 1, not %s' % (str(permission))

        # fileDesc
        if type(fileDesc) != types.StringType and fileDesc != None:
            raise ValueError, 'fileDesc not a string'
        if fileDesc == None:
            fileDesc = ''
        # check that fileDesc does not illegally contain a comma
        if fileDesc.find(',') != -1:
            raise ValueError, 'fileDesc string in fileTab.txt cannot contain a comma: <%s> is illegal' % (fileDesc)

        # category
        if category not in (1,2,3,4):
            raise ValueError, 'category must be 1=default, 2=variant, 3=history, or 4=realtime; not %s' % (str(category))

        # hasCatalog and hasHeader
        catList, headList = madrigal._Madrec.cedarCatalogHeaderList(madFilename)
        if len(catList) > 0:
            hasCatalog = 1
        else:
            hasCatalog = 0
        if len(headList) > 0:
            hasHeader = 1
        else:
            hasHeader = 0
        

        # all the arguments check out - add line to fileTab.txt
        if not os.access(os.path.join(expDir, 'fileTab.txt'), os.R_OK):
            raise ValueError, ' file %s does not yet exist' % (os.path.join(expDir, 'fileTab.txt'))

        # check that this is a new filename
        fileTabObj = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        for i in range(fileTabObj.getFileCount()):
            filename = fileTabObj.getFilenameByPosition(i)
            if filename == os.path.basename(madFilename):
                raise 'File %s already exists - must be deleted first' % (filename)
            

        # fileTab.txt
        fileTabText = os.path.basename(madFilename)
        fileTabText += ',0,%i,%i,0,%i,%i,0,0,' % (kindat,category,hasCatalog,hasHeader)
        fileTabText += '%s,%i\n' % (fileDesc, permission)

        # write fileTab.txt
        f = open(os.path.join(expDir, 'fileTab.txt'), 'a')
        f.write(fileTabText)
        f.close()                                                                                                                

        # cp madFilename to new directory
        shutil.copy(madFilename, os.path.join(expDir, os.path.basename(madFilename)))
        os.chmod(os.path.join(expDir, os.path.basename(madFilename)), 0664)

        # populate overview
        fileInfo = madrigal.data.MadrigalFile(os.path.join(expDir, os.path.basename(madFilename)), self.__madDB)

        # update expTab.txt against all registered files
        self.updateExpTab(expDir)



    def changeFileStatus(self,
                         expDir,
                         filename,
                         category = None,
                         fileDesc = None,
                         permission = None,
                         kindat = None):
        """changeFileStatus is used to change category, fileDesc, or permission of a register file in fileTab.txt.

        Inputs:

            expDir - full path to experiment directory 
        
            filename - basename of existing Madrigal file already registered in fileTab.txt
            
            permission - 0 (public) or 1 (private). If None (default), leave unchanged. 
            
            fileDesc - file description. If None (default), leave unchanged.

            category - 1=default, 2=variant, or 3=history. If None (default), leave unchanged.

            kindat - kindat (int). If None (default), leave unchanged.
        """
        try:
            fileTabInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, 'Unable to open fileTab.txt in %s' % (expDir)

        # be sure filename is basename
        filename = os.path.basename(filename)

        # search all Madrigal files in fileTab.txt for right file
        found = False
        fileCount = fileTabInfo.getFileCount()

        for index in range(fileCount):
            thisFilename = fileTabInfo.getFilenameByPosition(index)
            if thisFilename == filename:
                found = True
                if category != None:
                    fileTabInfo.setCategoryByPosition(index, category)
                if fileDesc != None:
                    fileTabInfo.setStatusByPosition(index, fileDesc)
                if permission != None:
                    fileTabInfo.setAccessByPosition(index, permission)
                if kindat != None:
                    fileTabInfo.setKindatByPosition(index, kindat)
                fileTabInfo.writeMetadata()
                break

        if found == False:
            raise ValueError, 'Madrigal file %s not found in %s' % (filename, os.path.join(expDir, 'fileTab.txt'))


    def overwriteMadrigalFile(self,
                              expDir,
                              madFilename):
        """overwriteMadrigalFile overwrites a file already registered in fileTab.txt.

        Automatically updates expTab.txt with any start or end experiment times.

        Inputs:

            expDir - full path to experiment directory
        
            madFilename - full path to the new Madrigal file.  Basename must match that of one in fileTab.txt.

        Returns: None

        Affects: Overwrites existing Madrigal file.  May modify expTab.txt with new start/end times
        """
        # verify this file registered
        try:
            fileTabInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, 'Unable to open fileTab.txt in %s' % (expDir)

        # get basename
        filename = os.path.basename(madFilename)

        # search all Madrigal files in fileTab.txt for right file
        found = False
        fileCount = fileTabInfo.getFileCount()

        for index in range(fileCount):
            thisFilename = fileTabInfo.getFilenameByPosition(index)
            if thisFilename == filename:
                found = True
                break

        if found == False:
            raise ValueError, '%s not found in %s' % (filename, os.path.join(expDir, 'fileTab.txt'))

        # cp madFilename to new directory
        shutil.copy(madFilename, os.path.join(expDir, filename))
        os.chmod(os.path.join(expDir, filename), 0664)

        # rm and re-populate overview
        os.remove(os.path.join(expDir, 'overview', filename + '.summary'))
        fileInfo = madrigal.data.MadrigalFile(os.path.join(expDir, filename), self.__madDB)

        # update expTab.txt against all registered files
        self.updateExpTab(expDir)



    def removeMadrigalFile(self,
                           expDir,
                           madFilename):
        """removeMadrigalFile removes a file already registered in fileTab.txt.

        Automatically updates expTab.txt with any start or end experiment times.

        Inputs:

            expDir - full path to experiment directory
        
            madFilename - Name of Madrigal file to be removed.  Basename must match that of one in fileTab.txt.

        Returns: None

        Affects: Removes existing Madrigal file and removes its line from fileTab.txt.  May modify expTab.txt
        with new start/end times
        """
        # verify this file registered
        try:
            fileTabInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, 'Unable to open fileTab.txt in %s' % (expDir)

        # get basename
        filename = os.path.basename(madFilename)

        # delete line from fileTab.txt
        fileTabInfo.deleteRowByFilename(filename)

        # write new version
        fileTabInfo.writeMetadata()

        # rm filename and overview data
        os.remove(os.path.join(expDir, filename))
        try:
            os.remove(os.path.join(expDir, 'overview', filename + '.summary'))
        except:
            pass

        # update expTab.txt against all registered files
        self.updateExpTab(expDir)



    def addWebFile(self,
                   expDir,
                   source,
                   relativePath):
        """addWebFile writes a non-Madrigal file meant to be displayed on the web to somewhere within a Madrigal experiment directory.

            All needed directories will be created if needed.

            Inputs:

                expDir - full path to experiment directory

                source - local web file to write to Madrigal

                relativePath - path relative to expDir to write source file to.  If relativePath ends
                with /, then basename from source used.  Otherwise, basename from relativePath used.

            Returns: None

            Affects: writes a non-Madrigal file to expDir on Madrigal
        """
        # verify expDir is a real Madrigal experiment directory
        try:
            fileTabInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, '%s not a valid experiment directory - no fileTab.txt' % (expDir)

        if not os.access(os.path.join(expDir, os.path.dirname(relativePath)), os.R_OK):
            # make all dirs
            os.umask(0000)
            os.makedirs(os.path.join(expDir, os.path.dirname(relativePath)), 0777)
            
        if os.path.basename(relativePath) == '':
            shutil.copy(source, os.path.join(expDir, relativePath, os.path.basename(source)))
            os.chmod(os.path.join(expDir, os.path.dirname(relativePath), os.path.basename(source)), 0664)
        else:
            shutil.copy(source, os.path.join(expDir, relativePath))
            os.chmod(os.path.join(expDir, relativePath), 0664)

        

    def updateExpTab(self, expDir):
        """updateExpTab rewrites expTab.txt based on all the Madrigal files registered in fileTab.txt.

        Inputs:

            expDir - full path to experiment directory

        Returns: None

        Affects: rewrites expTab.txt in expDir based on all the Madrigal files registered in fileTab.txt.
        """
        try:
            fileTabInfo = madrigal.metadata.MadrigalMetaFile(self.__madDB, os.path.join(expDir, 'fileTab.txt'))
        except:
            raise ValueError, 'Unable to open fileTab.txt in %s' % (expDir)

        try:
            expInfo = madrigal.metadata.MadrigalExperiment(self.__madDB, os.path.join(expDir, 'expTab.txt'))
        except:
            raise ValueError, 'Unable to open expTab.txt in %s' % (expDir)

        # we only need to modify start and end times of experiment
        startTime = None
        endTime = None

        # search all Madrigal files for earliest time, latest time
        fileCount = fileTabInfo.getFileCount()
	if fileCount == 0:
	    return

        for index in range(fileCount):
            thisMadfilename = fileTabInfo.getFilenameByPosition(index)
            try:
                fileInfo = madrigal.data.MadrigalFile(os.path.join(expDir, thisMadfilename))
            except:
                print 'WARNING: Problem with expDir=%s and thisMadfilename=%s' % (expDir, thisMadfilename)
                continue
            thisStartTimeList = fileInfo.getEarliestTime()
            thisStartTime = datetime.datetime(thisStartTimeList[0],
                                              thisStartTimeList[1],
                                              thisStartTimeList[2],
                                              thisStartTimeList[3],
                                              thisStartTimeList[4],
                                              thisStartTimeList[5])
            thisEndTimeList = fileInfo.getLatestTime()
            thisEndTime = datetime.datetime(thisEndTimeList[0],
                                            thisEndTimeList[1],
                                            thisEndTimeList[2],
                                            thisEndTimeList[3],
                                            thisEndTimeList[4],
                                            thisEndTimeList[5])

            if startTime == None:
                startTime = thisStartTime
            else:
                if startTime > thisStartTime:
                    startTime = thisStartTime

            if endTime == None:
                endTime = thisEndTime
            else:
                if endTime < thisEndTime:
                    endTime = thisEndTime

        # modify expTab.txt and write
        expInfo.setExpStartDateTimeByPosition(startTime)
        expInfo.setExpEndDateTimeByPosition(endTime)
        expInfo.writeMetadata()


    def updateMaster(self, skipGeo=False):
        """updateMaster is a method to update the local metadata.

        Replaces the former tcl script.

        Gathers data from experiment directories into metadata/expTab.txt and metadata/fileTab.txt.
        Also gathers metadata from OpenMadrigal to update metadata/expTabAll.txt and metadata/fileAllTab.txt,
        to update high level metadata siteTab.txt, instTab.txt, instType.txt, madCatTab.txt, parcods.tab.
        Also updates geophysical data.
        """
        binDir = os.path.join(self.__madDB.getMadroot(), 'bin')

        # update geophysical data
        if not skipGeo:
            print '*** Checking for any geophysical file updates ***'
            cmd = os.path.join(binDir, 'checkGeoUpdate.py')
            os.system(cmd)
        else:
            print('Warning - skipping updating geophysical files too often will make them out of date.')
        
        print '*** Updating local metadata ***'
        self.__updateLocalMetadata__()
        print '*** Updating metadata from other Madrigal sites ***'
        self.__updateGlobalMetadata__()
        print '*** Checking OpenMadrigal for any metadata updates ***'
        self.__checkOpenMadrigalMetadata__()

        # instParmTab.txt
        print '*** Rebuilding instParmTab.txt ***'
        obj = madrigal.metadata.MadrigalInstrumentParameters(self.__madDB)
        obj.rebuildInstParmTable()
        
        # instKindatTab.txt
        print '*** Rebuilding instKindatTab.txt ***'
        obj = madrigal.metadata.MadrigalInstrumentKindats(self.__madDB)
        obj.rebuildInstKindatTable()

        print 'updateMaster complete...'



    def __updateLocalMetadata__(self):
        """__updateLocalMetadata__ is a private method to update metadata/expTab.txt and metadata/fileTab.txt
        from the local metadata in the experiments directory
        """
        localSiteID = self.__madDB.getSiteID() # used to check that experiments do not have wrong siteID
        
        metaDict = {}
        metaDict['expText'] = '' # text of combined expTab.txt file
        metaDict['fileText'] = '' # text of combined fileTab.txt file
        metaDict['presentCount'] = 0 # experiment count so far
        metaDict['localSiteId'] = localSiteID
        
        os.path.walk(os.path.join(self.__madDB.getMadroot(), 'experiments'),
                     self.__walkExpDir__,
                     metaDict)
        

        # update expTab.txt
        f = open(os.path.join(self.__madDB.getMadroot(), 'metadata/expTab.txt'), 'w')
        f.write(metaDict['expText'])
        f.close()

        # update fileTab.txt
        f = open(os.path.join(self.__madDB.getMadroot(), 'metadata/fileTab.txt'), 'w')
        f.write(metaDict['fileText'])
        f.close()


    def __updateGlobalMetadata__(self):
        """__updateGlobalMetadata__ is a private method to update metadata/expTabAll.txt and metadata/fileTabAll.txt
        from the main madrigal server.
        """

        expTabAll = ''
        fileTabAll = ''

        localSiteID = self.__madDB.getSiteID()

        siteList = self.__madSite.getSiteList()

        for site in siteList:
            siteID = site[0]
            # skip local site
            if siteID == localSiteID:
                continue
            siteName = site[1]
            siteDir = '%s_%i' % (siteName, siteID)

            expMetadataFile = os.path.join(siteDir, 'expTab.txt')
            fileMetadataFile = os.path.join(siteDir, 'fileTab.txt')

            try:
                thisExpText  = self.__openMad.getMetadataFromOpenMadrigal(expMetadataFile)
                thisFileText = self.__openMad.getMetadataFromOpenMadrigal(fileMetadataFile)
            except:
                continue

            expTabAll += thisExpText
            fileTabAll += thisFileText

        # append local data
        f = open(os.path.join(self.__madDB.getMadroot(), 'metadata/expTab.txt'))
        expTabAll += f.read()
        f.close()

        f = open(os.path.join(self.__madDB.getMadroot(), 'metadata/fileTab.txt'))
        fileTabAll += f.read()
        f.close()

        # write *All.txt files
        f = open(os.path.join(self.__madDB.getMadroot(), 'metadata/expTabAll.txt'), 'w')
        f.write(expTabAll)
        f.close()

        f = open(os.path.join(self.__madDB.getMadroot(), 'metadata/fileTabAll.txt'), 'w')
        f.write(fileTabAll)
        f.close()


    def __checkOpenMadrigalMetadata__(self):
        """__checkOpenMadrigalMetadata__ is a method that check the openmadrigal site for any
        updates to the following metadata files:

            1. siteTab.txt - the list of all Madrigal installations
            2. instTab.txt - the list of all Madrigal instruments
            3. instType.txt - the list of all instrument categories

        If an update is available, and the existing metadata file is an old one, it will be updated.
        However, if the local Madrigal adminstrator edits one of these files, then the file will
        not be updated.  If you want to change these files, it is best to contact the OpenMadrigal
        development administrator (madrigal@haystack.mit.edu)
        """
        metadataFiles = ('siteTab.txt', 'instTab.txt', 'instType.txt')
        url = 'http://www.haystack.mit.edu/cgi-bin/madrigal/compareToArchive.py?filePath=%s&fileTextMd5=%s'
        # for test url = 'http://grail/cgi-bin/madrigal/compareToArchive.py?filePath=%s&fileTextMd5=%s'
        
        for metadataFile in metadataFiles:
            localMetadataFile = os.path.join(self.__madDB.getMadroot(), 'metadata', metadataFile)
            archivePath = 'madroot/metadata/%s' % (metadataFile)
            
            f = open(localMetadataFile)
            text = f.read()
            f.close()
            textMd5 = md5.md5(text)
            md5Str = textMd5.hexdigest() # md5 checksum of local metadata file

            thisUrl = url % (archivePath, md5Str)

            f = urllib2.urlopen(thisUrl)
            result = f.read()
            f.close()

            items = result.split() # first item is latest revision tag, second is matching revision tag
            if len(items) != 2:
                raise IOError, 'Problem with url %s' % (thisUrl)

            if items[0] != 'None' and items[0] == items[1]:
                # everything is up to date
                continue

            if items[0] == 'None':
                # failed to find this metadata file
                raise IOError, 'Problem with url %s' % (thisUrl)

            if items[1] == 'None':
                # this metadata file must have been locally editted, print warning
                print 'Metadata file %s has been locally edited - contact the OpenMadrigal administrator at madrigal@haystack.mit.edu to update central metadata' % (metadataFile)
                continue

            # this metadata file needs updating
            print 'Downloading revised version of metadata file %s from OpenMadrigal' % (metadataFile)
            text = self.__openMad.getMetadataFromOpenMadrigal(metadataFile)
            f = open(localMetadataFile, 'w')
            f.write(text)
            f.close()
        

    def __walkExpDir__(self, arg, dirname, names):
        """__walkExpDir__ is a private method called by os.path.walk.  arg is a dict with keys:
        1. extText = text of combined expTab.txt to be appended to
        2. fileText = text of combined fileTab.txt to be appended to
        3. presentCount = total experiments done so far
        4. localSiteId = local site id (int)
        
        Sets values in arg
        """
        if 'expTab.txt' not in names:
            return

        # defines allowed experiment directory names
        dirConvStr1 = '/[0-9][0-9][0-9][0-9]/[a-z][a-z][a-z]/[^/]*'

        # check that dirname follows rule experiments/YYYY/sss/*
        startIndex = len(os.path.join(self.__madDB.getMadroot(), 'experiments'))
        testDir = dirname[startIndex:]
        if re.match(dirConvStr1, testDir) == None:
            return
        
        # make sure we only descend three levels
        count = 0
        items = testDir.split('/')
        for item in items:
            if len(item) > 0:
                count += 1
        if count != 3:
            return

        expObj = madrigal.metadata.MadrigalExperiment(self.__madDB,
                                                      os.path.join(dirname, 'expTab.txt'))

        # skip it if security == -1 (ignore flag)
        if expObj.getSecurityByPosition(0) == -1:
            return

        # skip if wrong site id
        if expObj.getExpSiteIdByPosition(0) != arg['localSiteId']:
            print 'Warning: Experiment %s has wrong site id = %i.  This site id = %i' % \
                  (dirname, expObj.getExpSiteIdByPosition(0), arg['localSiteId'])
            return

        # modify experiment id
        newExpId = self.__madDB.getSiteID() * 10000000 + arg['presentCount']
        arg['presentCount'] += 1

        expObj.setExpIdByPosition(0, newExpId)

        arg['expText'] += str(expObj)

        if 'fileTab.txt' not in names:
            print 'Info: Experiment %s has no fileTab.txt' % (dirname)
            return

        fileObj = madrigal.metadata.MadrigalMetaFile(self.__madDB,
                                                     os.path.join(dirname, 'fileTab.txt'))

        # set expId for all files
        for i in range(fileObj.getFileCount()):
            fileObj.setExpIdByPosition(i, newExpId)

        arg['fileText'] += str(fileObj)

        
        
        

class MadrigalNotify:
    """MadrigalNotify is an object used to send messages to an administrator about a Madrigal database.

    This object provides functions needed to send messages to an administrator about a Madrigal database, for now
    only sendAlert, which sends an email to the site administrator found is siteTab.txt (or if not
    possible, the admin in madrigal.cfg, and finally if all else fails, to root).

    Usage example:

        import madrigal.admin
    
        try:
        
            adminObj =  madrigal.admin.MadrigalNotify()
            adminObj.sendAlert('This is important!', 'Important Message')
            
        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()


    Non-standard Python modules used:
    None

    Exceptions thrown: None - Note that MadrigalNotify tries every trick it knows to avoid
    throwing exceptions, since this is the class that will generally be called when there is a problem.

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 4, 2001
    """
    

    #constants
    __defaultUser  = "root"
    """ Sets the default user to email to when all else fails. """

    __defaultServer  = "localhost"
    """ Sets the default server to send mail when all else fails. """


    def __init__(self, madDB = None):
        """__init__ initializes MadrigalNotify by getting some basic information from MadrigalDB and MadrigalSite.

        Note that MadrigalNotify tries every trick it knows to avoid throwing exceptions, since
        this is the class that will generally be called when there is a problem.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__binDir.

        Exceptions: None.
        """

        # get metadata dir
        if madDB == None:
            try:
                thisMadDB = madrigal.metadata.MadrigalDB()
            except:
                # note that the main configuration file is unavailable 
                # the best that can be done is send an email to root using localhost mailserver
                self.__emailAddress = self.__defaultUser
                self.__emailServer  = self.__defaultServer
                thisMadDB = None
        else:
            thisMadDB = madDB

        if thisMadDB != None:
            self.__emailServer  = thisMadDB.getMailserver()
            # now try to get email from site metadata, if failure, use config contact info
            try:
                thisSite = madrigal.metadata.MadrigalSite()
                self.__emailAddress = thisSite.getSiteEmail(thisMadDB.getSiteID())
                if self.__emailAddress == None:
                    # couldn't read metadata - use madrigal.cfg
                    self.__emailAddress = thisMadDB.getContactEmail()
            except:
                # couldn't read metadata - use madrigal.cfg
                self.__emailAddress = thisMadDB.getContactEmail()

            #make sure madrigal.cfg worked - if not use root
            if self.__emailAddress == None:
                self.__emailAddress = self.__defaultUser


    def sendAlert(self, message, subject = None):
        """sendAlert sends an email with the given message and optional title.

        Inputs: message (string), and optional title (string)
        
        Returns: void

        Affects: none

        Exceptions: None.
        """

        # set up message
        message = 'From: (Python Madrigal API) ' + self.__emailAddress + '\n' + \
        'To: ' + self.__emailAddress + '\n' + \
        'Subject: ' + str(subject) + '\n' + \
        'Content-type: text/html\n\n' + message

        server = smtplib.SMTP(self.__emailServer)
        server.sendmail(self.__emailAddress.split(',')[0],
                        self.__emailAddress.split(','), message)
        server.quit()


class MadrigalError:
    """MadrigalError is an exception class that is thrown for all known errors in using Madrigal Py lib.

    Usage example:

        import sys, traceback
        import madrigal.admin
    
        try:
        
            test = open('ImportantFile.txt', 'r')
            
        except:
        
            raise madrigal.admin.MadrigalError('ImportantFile.txt not opened!',
                                                traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
    """


    def __init__(self, strInterpretation, exceptionList):
        """ __init__ gathers the interpretation string along with all information from sys.exc_info().

        Inputs: strIntepretation - A string representing the programmer's interpretation of
        why the exception occurred

                exceptionList - a list of strings completely describing the exception.
                Generated by traceback.format_exception(sys.exc_info()[0],
                                                        sys.exc_info()[1],
                                                        sys.exc_info()[2])
        
        Returns: Void.

        Affects: Initializes class member variables _strInterp, _strExcList.

        Exceptions: None.
        """
        
        self._strInterp = strInterpretation
        self._strExcList = exceptionList

        
    def getExceptionStr(self):
        """ getExceptionStr returns a formatted string ready for printing completely describing the exception.

        Inputs: None
        
        Returns: A formatted string ready for printing completely describing the exception.

        Affects: None

        Exceptions: None.
        """
        excStr = 'The following Madrigal Python exception has occurred:\n'
        excStr = excStr + self._strInterp + '\n\n'

        if self._strExcList != None:
            for item in self._strExcList:
                excStr = excStr + str(item) + '\n'

        return excStr


    def getExceptionHtml(self):
        """ getExceptionHtml returns an Html formatted string completely describing the exception.

        Inputs: None
        
        Returns: A formatted string ready for printing completely describing the exception.

        Affects: None

        Exceptions: None.
        """
        
        excStr = '<BR>The following Madrigal Python exception has occurred:\n<BR>'
        excStr = excStr + self._strInterp + '\n<BR>\n'

        if self._strExcList != None:
            for item in self._strExcList:
                excStr = excStr + str(item) + '\n<BR>'

        return excStr

    

if __name__ == '__main__':

    test = MadrigalNotify()

    test.sendAlert('This is a message from the python module MadrigalNotify', 'Test from MadrigalNotify')

    print 'Hopefully message sent - check.'


    test = MadrigalDBAdmin()

    """
    expDir = test.createRTExperiment(datetime.datetime.now(),
                           4,
                           32,
                           'Dummy experiment',
                           ('mlhrt1','mlhrt2'),
                           (30,30),
                           (0,0),
                           ('preliminary','very preliminary'))

    mlhrtFile = open('/home/hyperion/brideout/mlhrt1')
    mlhrt = mlhrtFile.read()
    mlhrtFile.close()
    
    
    test.appendRTMadrigalFile(expDir,
                            'mlhrt1',
                            mlhrt)

    # not real data, but shouldn't be checked
    test.appendRTMadrigalFile(expDir,
                            'mlhrt1',
                            'qqqqqqqqqqqqqqqqqqqqqqqq')

    mlhrtFile = open('/home/hyperion/brideout/mlhrt2')
    mlhrt = mlhrtFile.read()
    mlhrtFile.close()

    try:
        # this should raise a wrong file type exception
        test.appendRTMadrigalFile(expDir,
                                'mlhrt2',
                                mlhrt)
    except:
        traceback.print_exc()


    expDir = test.createMadrigalExperiment('/home/hyperion/brideout/mlh050429c.000',
                                        'Dummy experiment',
                                        0,
                                        'test exp',
                                        30,
                                        1)
    test.overwriteMadrigalFile(expDir,
                              '/home/hyperion/brideout/junk/mlh050429c.000')

    test.addWebFile(expDir,
                   '/home/hyperion/brideout/junk.html',
                   'html/second/')"""

    expDir = '/home/grail/brideout/madroot/experiments/1998/mlh/20jan98'

    try:
        test.addMadrigalFile(expDir,
                             '/tmp/mlh980120g.001',
                             0,
                             'second file')
    except:
        traceback.print_exc()
    """
    
    test.changeFileStatus(expDir,
                          '/home/hyperion/brideout/mlh050429c.000',
                          3,
                          'new status',
                          1)

    print 'about to run updateMaster'
    test.updateMaster(True)
    """
