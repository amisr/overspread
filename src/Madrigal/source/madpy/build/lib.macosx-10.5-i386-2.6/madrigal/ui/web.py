"""web is the module that interfaces to cgi madrigal web pages.

The web module contains general functions to produce html, along with producing
html relating to specific user data or madrigal data.
"""

# $Id: web.py,v 1.41 2009/03/06 21:38:07 brideout Exp $

import time, os
import os.path
import calendar
import fnmatch
import datetime
import types, traceback
import Cookie

import madrigal.metadata
import madrigal.data
import madrigal.ui.userData
import madrigal.admin

class MadrigalWeb:
    """MadrigalWeb is the class that produces output for the web.

    All text written to the web is produced in this class.

    Non-standard Python modules used:
    None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 17, 2001
    """
    

    __MaxSleep     = 10

    def __init__(self, madDB = None):
        """__init__ initializes MadrigalWeb by reading from MadridalDB..

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__metaDir, self._logFile.

        Exceptions: None.
        """

        if madDB == None:
            thisMadDB = madrigal.metadata.MadrigalDB()
        else:
            thisMadDB = madDB

        self.__binDir = thisMadDB.getBinDir()
        self.__madDB  = thisMadDB

        metaDir = thisMadDB.getMetadataDir()

        # get todays year
        now = datetime.datetime.now()

        thisYear = '%04i' % (now.year)

        self.__logFile = os.path.join(metaDir, 'userdata', 'access_%s.log' % (thisYear))

        # be sure it exists
        if not os.access(self.__logFile, os.R_OK):
            f=open(self.__logFile, 'w')
            f.close()

        # keep track of whether user trusted
        self.__isTrusted__ = None


    def outputHead(self, title):
        """outputHead outputs a standard html <head> with the given title

        Inputs: title - string : title of cgi page
        
        Returns: a standard html <head> with the given title.  This function
        also defines the "lb" class used to format buttons, so changing that
        format here changes it in all madrigal python cgi pages.

        Affects: None.

        Exceptions: None.
        """
        outputStr =             '<html>\n'
        outputStr = outputStr + '<head>\n'
        outputStr = outputStr + '\t<title>' + title + '</title>\n'
        outputStr = outputStr + '\t<style type="text/css">.lb {background: #ADD8E6}</style>\n'
        outputStr = outputStr + '</head>\n'

        return outputStr


    def getCookieDateOneYearAhead(self):
        """getCookieDateOneYearAhead returns a string formated as per cookie standard of time one year in future.

        Inputs: none
        
        Returns: a string formated as per cookie standard of time one year in future. Used to set the expires
        attribute of a cookie to be persisted.

        Affects: None

        Exceptions: None.
        """

        # get a time one year in the future
        futureTime = (time.time() + (365*24*3600))
        # get in right format
        tupleFutureTime = time.gmtime(futureTime)
        return time.strftime("%A, %d-%b-%Y %H:%M:%S GMT", tupleFutureTime)


    def getCookieDateOneYearAgo(self):
        """getCookieDateOneYearAgo returns a string formated as per cookie standard of time one year in the past.

        Inputs: none
        
        Returns: a string formated as per cookie standard of time one year in the past. Used to set the expires
        attribute of a cookie to be deleted

        Affects: None

        Exceptions: None.
        """

        # get a time one year in the past
        pastTime = (time.time() - (365*24*3600))
        # get in right format
        tuplePastTime = time.gmtime(pastTime)
        return time.strftime("%A, %d-%b-%Y %H:%M:%S GMT", tuplePastTime)

    
    
    def getIntListFromStrList(self, strList):
        """getIntListFromStrList returns a list containing integers given a list of integers in string form

        Inputs: strList - a list a strings that can be converted to integers
        
        Returns: a list containing integers given a list of integers in string form

        Affects: None

        Exceptions: MadrigalError thrown if a string in the list cannot be converted to an integer.
        """

        returnList = []
        
        try:
            for str in strList:
                returnList.append(int(str))

            return returnList

        except:
            raise madrigal.admin.MadrigalError("Unable to convert following list to integers: " + str(strList),
                                               traceback.format_exception(sys.exc_info()[0],
                                                                          sys.exc_info()[1],
                                                                          sys.exc_info()[2]))


    def getCgiString(self, strWithSpaces):
        """getCgiString returns a string based on strWithSpaces with all spaces replaced by %20's

        Inputs: strWithSpaces - a string that may contain spaces
        
        Returns: a string based on strWithSpaces with all spaces replaced by %20's

        Affects: None.

        Exceptions: None.
        """
        if strWithSpaces == None:
            return ''
	    
        if len(strWithSpaces) == 0:
            return ''

        delimiter = '%20'
        return delimiter.join(strWithSpaces.split(' '))


    def getCgiStringFromList(self, list):
        """getCgiStringFromList returns a string based on a list with items separated by %20's

        Inputs: list - a python list
        
        Returns: a string based on list with items separated by %20's

        Affects: None.

        Exceptions: None.
        """

        i = 1
        lenList = len(list)
        returnStr = ''
        for item in list:
            returnStr = returnStr + str(item).strip()
            if i < lenList:
                returnStr = returnStr + '%20'
            i = i + 1

        return returnStr
            


    def getSpaceString(self, cgiString):
        """getSpaceString returns a string based on cgiString with all %20's replaced by spaces

        Inputs: cgiString - a string that may contain %20's replacing spaces
        
        Returns: a string based on cgiString with all %20's replaced by spaces

        Affects: None.

        Exceptions: None.
        """
	
        if cgiString == None:
            return ''
	
        if len(cgiString) == 0:
            return ''

        delimiter = ' '
        return delimiter.join(cgiString.split('%20'))
        


    def getOptionNumericTags(self, startNum, endNum, selectedNum):
        """getOptionTags outputs a series of html option tags with numerical values from startNum to endNum

        Inputs: startNum - integer value of first option tag
            endNum - integer value of last option tag
            selectedNum - integer value of which tag is selected
        
        Returns: a string containing option tags of form <option value="1">1<option value="2">2...

        Affects: None.

        Exceptions: None.
        """
        returnStr = ''
        i = startNum
        while i - 1 < endNum:
            returnStr = returnStr + '<option value=' + str(i)
            if i == selectedNum:
                returnStr = returnStr + ' selected'
            returnStr = returnStr + '>' + str(i)
            i = i + 1

        return returnStr


    def getOptionMonthTags(self, selectedMonth):
        """getOptionMonthTags outputs a series of html option tags with representing the months of the year

        Inputs: selectedMonth - an integer (1-12) representing the month to be selected
        
        Returns: a string containing option tags of form <option value="1">Jan<option value="2">Feb...

        Affects: None.

        Exceptions: None.
        """
        returnStr = ''
        i = 1
        while i < 13:
            returnStr = returnStr + '<option value=' + str(i)
            if i == selectedMonth:
                returnStr = returnStr + ' selected'
            returnStr = returnStr + '>' + calendar.month_name[i][0:3]
            i = i + 1

        
        return returnStr


    def getOptionListTags(self, optionList, selectedNumber=None):
        """getOptionListTags outputs a series of html option tags with representing the items in a list

        Inputs: optionList - a list of items to be options
            selectedNumber - the number of the item to be selected by default.  If None, none selected
        
        Returns: a string containing option tags of form <option value="<item>"><item>...

        Affects: None.

        Exceptions: None.
        """
        returnStr = ''
        i = 0
        for item in optionList:
            returnStr = returnStr + '<option value=' + str(item)
            if selectedNumber != None:
                if i == selectedNumber:
                    returnStr = returnStr + ' selected'
            returnStr = returnStr + '>' + str(item)
            i = i + 1

        
        return returnStr


    def getOptionKinstListTags(self, optionList, selectedNumber=None):
        """getOptionKinstListTags outputs a series of html option tags of form <option value="<kinst num>"><kinst description>..

        Inputs: optionList - a list of kinst numbers to be options
            selectedNumber - the number of the item to be selected by default.  If None, none selected
        
        Returns: a string containing option tags of form <option value="<kinst num>"><kinst description>...

        Affects: None.

        Exceptions: None.
        """
        instObj = madrigal.metadata.MadrigalInstrument(self.__madDB)
        
        returnStr = ''
        i = 0
        for item in optionList:
            returnStr = returnStr + '<option value=' + str(item)
            if selectedNumber != None:
                if i == selectedNumber:
                    returnStr = returnStr + ' selected'
            if instObj.getInstrumentName(int(item)) != None:
                returnStr = returnStr + '>' + str(instObj.getInstrumentName(int(item)))
            else:
                returnStr = returnStr + '>' + str(item)
            i = i + 1

        
        return returnStr



    def getOptionKindatListTags(self, optionList, selectedNumber=None):
        """getOptionKindatListTags outputs a series of html option tags of form <option value="<kindat num>"><kindat description>..

        Inputs: optionList - a list of kindat numbers to be options
            selectedNumber - the number of the item to be selected by default.  If None, none selected
        
        Returns: a string containing option tags of form <option value="<kindatt num>"><kindat description>...

        Affects: None.

        Exceptions: None.
        """
        typeObj = madrigal.metadata.MadrigalKindat(self.__madDB)
        
        returnStr = ''
        i = 0
        for item in optionList:
            returnStr = returnStr + '<option value=' + str(item)
            if selectedNumber != None:
                if i == selectedNumber:
                    returnStr = returnStr + ' selected'
            if typeObj.getKindatDescription(int(item)) != None:
                returnStr = returnStr + '>' + str(typeObj.getKindatDescription(int(item)))
            else:
                returnStr = returnStr + '>' + str(item)
            i = i + 1

        
        return returnStr
    


    def getOptionPubDirTags(self, madUserDataObj, filterOwner='', filterDir=''):
        """getOptionDirTags outputs a series of html option tags with representing the list of public directories

        Inputs: madUserDataObj - an object of type MadrigalUserData
            filterOwner - a string representing the username of the filter owner
            filterDir - a string representing the name of the directory
        
        Returns: a string containing option tags of form <option value="<userName:dirName>"><userName:dirName>...
        If a filterOwner and filterDir are passed in and found to match, then that dir will be selected.  Otherwise,
        the first row will be selected

        Affects: None.

        Exceptions: None.
        """
        
        filterOwner = filterOwner.lower().strip()
        
        returnStr = ''

        # get list of public directories
        pubDirList = madUserDataObj.getPublicDirNameList()

        # check is match found
        matchFlag = 0

        for dirName in pubDirList:
            returnStr = returnStr + '<option value="' + str(dirName) + '"'
            if dirName == str(filterOwner) + ':' + str(filterDir):
                returnStr = returnStr + ' selected'
                matchFlag = 1
            returnStr = returnStr + '>' + str(dirName)

        # if no match found, repeat with first item selected
        if matchFlag == 0:
            returnStr = ''
            for dirName in pubDirList:
                returnStr = returnStr + '<option value="' + str(dirName) + '"'
                if matchFlag == 0:
                    returnStr = returnStr + ' selected'
                    matchFlag = 1
                returnStr = returnStr + '>' + str(dirName)
 
        return returnStr


    def getFirstOwner(self, madUserDataObj):
        """getFirstOwner returns the first owner name of a public directory - used by default when a new file is loaded.

        Inputs: None
        
        Returns: the first owner name of a public directory - used when a new file is loaded.  Empty string if no
        public directories exist.

        Affects: None.

        Exceptions: None.
        """
        # get list of public directories
        pubDirList = madUserDataObj.getPublicDirNameList()

        if len(pubDirList) == 0:
            return ''

        for name in pubDirList:
            return name.split(':')[0]


    def getFirstPubDir(self, madUserDataObj):
        """ getFirstPubDir returns the first public directory name found - used by default when a new file is loaded.

        Inputs: None
        
        Returns: the first public directory name found - used by default when a new file is loaded.  Empty string if no
        public directories exist.

        Affects: None.

        Exceptions: None.
        """
        # get list of public directories
        pubDirList = madUserDataObj.getPublicDirNameList()

        if len(pubDirList) == 0:
            return ''

        for name in pubDirList:
            return name.split(':')[1]


    def getFirstUserDir(self, madUserDataObj, username):
        """ getFirstUserDir returns the first directory name found for a given user - used by default when save filter is loaded.

        Inputs: None
        
        Returns: the first directory name found owned by a given user - used by default when save filter file is loaded.  Empty string if no
        public directories exist.

        Affects: None.

        Exceptions: None.
        """
        username = username.lower().strip()

        # be sure username exists
        if not madUserDataObj.verifyUser(username, 'password'):
            madUserDataObj.addUser(username, 'password')
        
        # get list of all directories
        allDirList = madUserDataObj.getAllDirInfo()

        if len(allDirList[username]) == 0:
            return ''

        for dir in allDirList[username]:
            return dir.dirName

        

    def getOptionPrivDirTags(self, madUserDataObj, filterOwner, filterDir=''):
        """getOptionDirTags outputs a series of html option tags with representing the list of private directories

        Inputs: madUserDataObj - an object of type MadrigalUserData
            filterOwner - a string representing the username of the filter owner
            filterDir - a string representing the name of the private directory
        
        Returns: a string containing option tags of form <option value="<dirName>"><dirName>...
        If a filterOwner and filterDir are passed in and found to match, then that dir will be selected.  Otherwise,
        the first row will be selected

        Affects: None.

        Exceptions: None.
        """
        
        filterOwner = filterOwner.lower().strip()
        
        returnStr = ''

        # get list of private directories
        privDirList = madUserDataObj.getPrivateDirNameList(str(filterOwner))

        # check is match found
        matchFlag = 0

        for dirName in privDirList:
            returnStr = returnStr + '<option value="' + str(dirName) + '"'
            if dirName == str(filterDir):
                returnStr = returnStr + ' selected'
                matchFlag = 1
            returnStr = returnStr + '>' + str(dirName)

        # if no match found, repeat with first item selected
        if matchFlag == 0:
            returnStr = ''
            for dirName in privDirList:
                returnStr = returnStr + '<option value="' + str(dirName) + '"'
                if matchFlag == 0:
                    returnStr = returnStr + ' selected'
                    matchFlag = 1
                returnStr = returnStr + '>' + str(dirName)
 
        return returnStr


    def getOptionAllUserDirTags(self, madUserDataObj, filterOwner, dirName=''):
        """getOptionAllUserDirTags outputs a series of html option tags with representing the list of all dirs owned by a user

        Inputs: madUserDataObj - an object of type MadrigalUserData
            filterOwner - a string representing the username of the filter owner
        
        Returns: a string containing option tags of form <option value="<dirName> (public)"><dirName>...
        where (public) or (private) is appended to value based on dir type.  If dirName is passed in and
        found to match, then that dir will be selected.  Otherwise, the first row will be selected

        Affects: None.

        Exceptions: None.
        """
        
        filterOwner = filterOwner.lower().strip()
        
        returnStr = ''

        # get list of all directories
        allDirList = madUserDataObj.getAllDirInfo()

        # check is match found
        matchFlag = 0

        # get this users' directories
        for madDir in allDirList[filterOwner]:
            returnStr = returnStr + '<option value="' + madDir.dirName + '"'
            if madDir.dirName == dirName:
                returnStr = returnStr + ' selected'
                matchFlag = 1
            returnStr = returnStr + '>' + madDir.dirName
            if madDir.dirType == 'public':
                returnStr = returnStr + ' (public)'
            else:
                returnStr = returnStr + ' (private)'

        # if no match found, repeat with first item selected
        if matchFlag == 0:
            returnStr = ''
            for madDir in allDirList[filterOwner]:
                returnStr = returnStr + '<option value="' + madDir.dirName + '"'
                if matchFlag == 0:
                    returnStr = returnStr + ' selected'
                    matchFlag = 1
                returnStr = returnStr + '>' + madDir.dirName
                if madDir.dirType == 'public':
                    returnStr = returnStr + ' (public)'
                else:
                    returnStr = returnStr + ' (private)'
 
        return returnStr


    def getFirstPrivDir(self, madUserDataObj, username):
        """ getFirstPrivDir returns the first private directory name found - used by default when a new file is loaded.

        Inputs: None
        
        Returns: the first private directory name found - used by default when a new file is loaded.  Empty string if no
        public directories exist.

        Affects: None.

        Exceptions: None.
        """
        username = username.lower().strip()
        
        # get list of private directories
        privDirList = madUserDataObj.getPrivateDirNameList(username)

        if len(privDirList) == 0:
            return ''

        for name in privDirList:
            return name


    def getOptionFilterTags(self, madUserDataObj, filterOwner, filterDir, filterName=''):
        """getOptionFilterTags outputs a series of html option tags with representing the list of filters

        Inputs: madUserDataObj - an object of type MadrigalUserData
            filterOwner - a string representing the username of the filter owner
            filterDir - a string representing the name of the private directory
            filterName - a string representing the filter name to be selected - if blank or not found, select first
        
        Returns: a string containing option tags of form <option value="<filterName>"><filterName>...
        If a filterName is passed in and found to match, then that filter will be selected.  Otherwise,
        the first row will be selected

        Affects: None.

        Exceptions: None.
        """

        filterOwner = filterOwner.lower().strip()
        
        returnStr = ''

        # get list of filter names
        filtNameList = madUserDataObj.getFilterNameList(filterOwner, filterDir)

        # check is match found
        matchFlag = 0

        for filtName in filtNameList:
            returnStr = returnStr + '<option value="' + str(filtName) + '"'
            if filtName == str(filterName):
                returnStr = returnStr + ' selected'
                matchFlag = 1
            returnStr = returnStr + '>' + str(filtName)

        # if no match found, repeat with first item selected
        if matchFlag == 0:
            returnStr = ''
            for filtName in filtNameList:
                returnStr = returnStr + '<option value="' + str(filtName) + '"'
                if matchFlag == 0:
                    returnStr = returnStr + ' selected'
                    matchFlag = 1
                returnStr = returnStr + '>' + str(filtName)
 
        return returnStr


    def filterHtmlFormat(self, madFilter):
        """ filterHtmlFormat returns a filter formatted as an html table for display on the web.

        Inputs: a madrigal filter
        
        Returns: a filter formated as an html table for display on the web.

        Affects: None.

        Exceptions: None.
        """

        if madFilter == None:
            return '<br> No filter selected. <br>'

        # create parameter string from madFilter.parmlist if it exists
        if madFilter.parmlist != None:
            parmList = madFilter.parmlist.split()
            # need parameter object
            parmObj = madrigal.data.MadrigalParameters(self.__madDB)
            # create Mnemonic list
            parmMnemList = parmObj.getParmMnemonicList(parmList)
            # create Mnemonic string
            delimiter = ', '
            strParmList = delimiter.join(parmMnemList)
        else:
            strParmList = str(madFilter.parmlist)
        
        htmlStr = '<table cols=4 width=60%>\n'
        
        if madFilter.description != None:
            htmlStr = htmlStr + '\t<tr>\n'
            htmlStr = htmlStr + '\t\t<td>Description:</td>\n'
            htmlStr = htmlStr + '\t\t<td colspan=3>' + str(madFilter.description) + '</td>\n'
            htmlStr = htmlStr + '\t</tr><tr>\n'
        else:
            htmlStr = htmlStr + '\t<tr>\n'
        htmlStr = htmlStr + '\t\t<td>Start time:</td>\n'
        htmlStr = htmlStr + '\t\t<td id="sH">H: ' + str(madFilter.starthour) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td id="sM">M: ' + str(madFilter.startmin) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td id="sS">S: ' + str(madFilter.startsec) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>End time:</td>\n'
        htmlStr = htmlStr + '\t\t<td id="eH">H :' + str(madFilter.endhour) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td id="eM">M :' + str(madFilter.endmin) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td id="eS">S :' + str(madFilter.endsec) + '</td>\n'
        htmlStr = htmlStr + '\t</tr></div><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Min altitude:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.minalt) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max altitude:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.maxalt) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Min azimuth:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.minaz) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max azimuth:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.maxaz) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Min elevation:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.minel) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max elevation:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.maxel) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Min azimuth 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.minaz2) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max azimuth 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.maxaz2) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Min elevation 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.minel2) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max elevation 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.maxel2) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Min pulse len:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.minpl) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max pulse len:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.maxpl) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Kinst selection:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.flkinst) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Kindat selection:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.flkdat) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Mnemonic 1:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.mnemStr1) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Mnemonic 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.mnemStr2) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Lower limit 1:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.lower1) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Lower limit 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.lower2) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Upper limit 1:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.upper1) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Upper limit 2:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.upper2) + '</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td valign=top>Parameters selected:</td>\n'
        # insert commas into parmlist
        htmlStr = htmlStr + '\t\t<td colspan=3>' + strParmList + '</td>\n'
        if madFilter.header != None:
            htmlStr = htmlStr + '\t</tr><tr>\n'
            htmlStr = htmlStr + '\t\t<td colspan=4>Headers off: selected</td>\n'
        htmlStr = htmlStr + '\t</tr><tr>\n'
        htmlStr = htmlStr + '\t\t<td>Bad value:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.badval) + '</td>\n'
        htmlStr = htmlStr + '\t\t<td>Max chars:</td>\n'
        htmlStr = htmlStr + '\t\t<td>' + str(madFilter.mxchar) + '</td>\n'
        htmlStr = htmlStr + '</tr></table>\n'

        
        return htmlStr


    def outputIsprintReport(self, madFilter, madForm, filename):
        """ outputIsprintReport prints a isprint-formated maddata object given a madrigal filter.

        Works by calling madrigal._Madrec.getIsprintReport

        Inputs: 1) madFilter = the MadrigalFilter that determines all the settings.

                2) madForm - a cgi.FieldStorage object representing the submitted form
        
                2) filename  = the full path to the madrigal file to use
        
        Returns: None

        Affects: prints a isprint-formated maddata object given a madrigal filter.

        Exceptions: None.

        """
        # strings to be passed into getIsprintReport
        infoStr = ''
        parmList = ''
        displayHeaders = 1
        displaySummary = 1
        maxCharsPerLine = 0
        missingStr = 'missing'
        assumedStr = 'assumed'
        knownBadStr = 'knownBad'
        outFile = 'stdout'

        # lists to be passed into getIsprintReport
        filtTypeList = []
        filtParm1List = []
        filtParm2List = []
        filtNumRangeList = []
        filterLowList = []
        filterHighList = []

        # set up time filter
        if madForm.has_key('startYear'):
            syear = int(madForm.getvalue('startYear'))
            if madForm.has_key('startMonth'):
                smonth = int(madForm.getvalue('startMonth'))
            else:
                smonth = 0
            if madForm.has_key('startDay'):
                sday = int(madForm.getvalue('startDay'))
            else:
                sday = 0
            if madFilter.starthour == None:
                shour = 0
            else:
                shour = madFilter.starthour
            if madFilter.startmin == None:
                smin = 0
            else:
                smin = madFilter.startmin
            if madFilter.startsec == None:
                ssec = 0
            else:
                ssec = madFilter.startsec   
            ut_lower = madrigal._Madrec.getUtFromDate(syear, smonth, sday, shour, smin, ssec, 0)
        else:
            ut_lower = None

        if madForm.has_key('endYear'):
            eyear = int(madForm.getvalue('endYear'))
            if madForm.has_key('endMonth'):
                emonth = int(madForm.getvalue('endMonth'))
            else:
                emonth = 0
            if madForm.has_key('endDay'):
                eday = int(madForm.getvalue('endDay'))
            else:
                eday = 0
            if madFilter.endhour == None:
                ehour = 23
            else:
                ehour = madFilter.endhour
            if madFilter.endmin == None:
                emin = 59
            else:
                emin = madFilter.endmin
            if madFilter.endsec == None:
                esec = 59
            else:
                esec = madFilter.endsec   
            ut_upper = madrigal._Madrec.getUtFromDate(eyear, emonth, eday, ehour, emin, esec, 0)
        else:
            ut_upper = None

        if (ut_lower != None or ut_upper != None):
            filtTypeList.append('1')
            filtParm1List.append('ut1')
            filtParm2List.append(None)
            filtNumRangeList.append(1)
            filterLowList.append(ut_lower)
            filterHighList.append(ut_upper)

        # add gdalt filter if needed
        if madFilter.minalt != None or madFilter.maxalt != None:
            # add the filter
            filtTypeList.append('1')
            filtParm1List.append('gdalt')
            filtParm2List.append(None)
            filtNumRangeList.append(1)
            filterLowList.append(madFilter.minalt)
            filterHighList.append(madFilter.maxalt)

        # add azm filter if needed
        if (madFilter.minaz != None or madFilter.maxaz != None) and \
           (madFilter.minaz != -180.0 or madFilter.maxaz != 180.0):

            # check if second azm filter used
            if (madFilter.minaz2 != None or madFilter.maxaz2 != None) and \
               (madFilter.minaz2 != 0.0 or madFilter.maxaz2 != 0.0):
                use2ndAz = 1
            else:
                use2ndAz = 0

            # convert azm from 0 to 360 to -180 to 180 as required by maddata
            # no longer really needed, but can't hurt
            if madFilter.minaz == None:
                minaz = None
            elif madFilter.minaz > 180:
                minaz = madFilter.minaz - 360.0
            else:
                minaz = madFilter.minaz

            if madFilter.maxaz == None:
                maxaz = None
            elif madFilter.maxaz > 180:
                maxaz = madFilter.maxaz - 360.0
            else:
                maxaz = madFilter.maxaz

            if madFilter.minaz2 == None:
                minaz2 = None
            elif madFilter.minaz2 > 180:
                minaz2 = madFilter.minaz2 - 360.0
            else:
                minaz2 = madFilter.minaz2

            if madFilter.maxaz2 == None:
                maxaz2 = None
            elif madFilter.maxaz2 > 180:
                maxaz2 = madFilter.maxaz2 - 360.0
            else:
                maxaz2 = madFilter.maxaz2
            
            # add the filter
            filtTypeList.append('1')
            filtParm1List.append('azm')
            filtParm2List.append(None)

            # the number or ranges can be 1, 2, 3, or 4, depending
            # on whether the ranges go through 180 degrees
            if minaz != None and maxaz != None:
                if minaz > maxaz:
                    # first az range must be broken into two ranges
                    need2az = 1
                else:
                    need2az = 0
            else:
                need2az = 0
            if use2ndAz:
                if minaz2 != None and maxaz2 != None:
                    if minaz2 > maxaz2:
                        # second az range must be broken into two ranges
                        need2az2 = 1
                    else:
                        need2az2 = 0
                else:
                    need2az2 = 0

            if use2ndAz:
                filtNumRangeList.append(2 + need2az + need2az2)
            else:
                filtNumRangeList.append(1 + need2az)

            filterLowList.append(minaz)
            if need2az == 1:
                filterLowList.append(-180.0)

            filterHighList.append(maxaz)
            if need2az == 1:
                filterHighList.append(180.0)

            if use2ndAz:
                filterLowList.append(minaz2)
                if need2az2:
                    filterLowList.append(-180.0)

                filterHighList.append(maxaz2)
                if need2az2:
                    filterHighList.append(180.0)
                

        # add elm filter if needed
        if (madFilter.minel != None or madFilter.maxel != None) and \
           (madFilter.minel != 0.0 or madFilter.maxel != 90.0):

            # check if second elm filter used
            if (madFilter.minel2 != None or madFilter.maxel2 != None) and \
               (madFilter.minel2 != 0.0 or madFilter.maxel2 != 0.0):
                use2ndEl = 1
            else:
                use2ndEl = 0
                
            
            if madFilter.minel == None:
                minel = None
            else:
                minel = madFilter.minel

            if madFilter.maxel == None:
                maxel = None
            else:
                maxel = madFilter.maxel

            if madFilter.minel2 == None:
                minel2 = None
            else:
                minel2 = madFilter.minel2

            if madFilter.maxel2 == None:
                maxel2 = None
            else:
                maxel2 = madFilter.maxel2
                
            # add the filter
            filtTypeList.append('1')
            filtParm1List.append('elm')
            filtParm2List.append(None)
            if use2ndEl:
                filtNumRangeList.append(2)
            else:
                filtNumRangeList.append(1)
            filterLowList.append(minel)
            filterHighList.append(maxel)

            if use2ndEl:
                filterLowList.append(minel2)
                filterHighList.append(maxel2)

        # add pulse length filter if needed
        if madFilter.minpl != None or madFilter.maxpl != None:
            # add the filter
            filtTypeList.append('1')
            filtParm1List.append('pl')
            filtParm2List.append(None)
            filtNumRangeList.append(1)
            if madFilter.minpl == None:
                filterLowList.append(None)
            else:
                # convert from secs to microsecs
                filterLowList.append(madFilter.minpl/1.e6)
            if madFilter.maxpl == None:
                filterHighList.append(None)
            else:
                # convert from secs to microsecs
                filterHighList.append(madFilter.maxpl/1.e6)

        # add kinst filter if needed
        if madFilter.flkinst != None:

            # add the filter
            filtTypeList.append('1')
            filtParm1List.append('kinst')
            filtParm2List.append(None)
            filtNumRangeList.append(1)
            filterLowList.append(madFilter.flkinst)
            filterHighList.append(madFilter.flkinst)

        # add kindat filter if needed
        if madFilter.flkdat != None:
            # add the filter
            filtTypeList.append('1')
            filtParm1List.append('kindat')
            filtParm2List.append(None)
            filtNumRangeList.append(1)
            filterLowList.append(madFilter.flkdat)
            filterHighList.append(madFilter.flkdat)

        # add free-form filter 1 if needed
        if madFilter.mnemStr1 != None and (madFilter.lower1 != None or madFilter.upper1 != None):
            # add the filter
            mnemList = madFilter.mnemStr1.strip().split(' ')

            # check for errors that leaked through the javascript
            if len(mnemList) not in [1,3]:
                raise madrigal.admin.MadrigalError("Illegal mnemonic string: " + str(madFilter.mnemStr1), None)

            if len(mnemList) == 3:
                if mnemList[1] not in ['+', '-', '*', '/']:
                    raise madrigal.admin.MadrigalError("Illegal mnemonic operator: " + mnemList[1], None)

            if len(mnemList) == 3:
                filtTypeList.append(mnemList[1])
                filtParm1List.append(mnemList[0])
                filtParm2List.append(mnemList[2])
            else:
                filtTypeList.append('1')
                filtParm1List.append(mnemList[0])
                filtParm2List.append(None)
                
            filtNumRangeList.append(1)
            filterLowList.append(madFilter.lower1)
            filterHighList.append(madFilter.upper1)

        # add free-form filter 2 if needed
        if madFilter.mnemStr2 != None and (madFilter.lower2 != None or madFilter.upper2 != None):
            # add the filter
            mnemList = madFilter.mnemStr2.strip().split(' ')

            # check for errors that leaked through the javascript
            if len(mnemList) not in [1,3]:
                raise madrigal.admin.MadrigalError("Illegal mnemonic string: " + str(madFilter.mnemStr2), None)

            if len(mnemList) == 3:
                if mnemList[1] not in ['+', '-', '*', '/']:
                    raise madrigal.admin.MadrigalError("Illegal mnemonic operator: " + mnemList[1], None)

            if len(mnemList) == 3:
                filtTypeList.append(mnemList[1])
                filtParm1List.append(mnemList[0])
                filtParm2List.append(mnemList[2])
            else:
                filtTypeList.append('1')
                filtParm1List.append(mnemList[0])
                filtParm2List.append(None)
                
            filtNumRangeList.append(1)
            filterLowList.append(madFilter.lower2)
            filterHighList.append(madFilter.upper2)

        # displayHeaders
        if madForm.has_key('header'):
            displayHeaders = 0

        # displayHeaders
        if madFilter.mxchar != None:
            maxCharsPerLine = int(madFilter.mxchar)

        # missing
        if madFilter.badval != None:
            missingStr = str(madFilter.badval)

        # assumed
        try:
            if madFilter.assumed != None:
                assumedStr = str(madFilter.assumed)
        except:
            pass

        # knownBad
        try:
            if madFilter.knownBad != None:
                knownBadStr = str(madFilter.knownBad)
        except:
            pass
                
			
	# convert parmlist to ordered list of mnemonics 
        parmList = madFilter.parmlist.split()
        orderedParmList = []
	# need parameter object
        parmObj = madrigal.data.MadrigalParameters(self.__madDB)

        catDict = parmObj.getCategoryDict(parmList)
        keyList = catDict.keys()
        keyList.sort()
        for key in keyList:
            item = catDict[key]
            catParmList = item[1]
            for parm in catParmList:
                orderedParmList.append(parm)


        madrigal._Madrec.getIsprintReport(filename,
                                          infoStr,
                                          orderedParmList,
                                          filtTypeList,
                                          filtParm1List,
                                          filtParm2List,
                                          filtNumRangeList,
                                          filterLowList,
                                          filterHighList,
                                          displayHeaders,
                                          displaySummary,
                                          maxCharsPerLine,
                                          missingStr,
                                          assumedStr,
                                          knownBadStr,
                                          outFile)



    def getRulesOfTheRoad(self):
        """ getRulesOfTheRoad returns a string giving the rules in html formal for using madrigal data.

        Inputs: None
        
        Returns: a string giving the rules in html formal for using madrigal data

        Affects: None.

        Exceptions: None.
        """

        # get the site name
        siteObj = madrigal.metadata.MadrigalSite(self.__madDB)

        siteName = siteObj.getSiteName(self.__madDB.getSiteID())
        
        returnStr = '<h3>Please contact '

        returnStr = returnStr + '<a href=' + str(self.__madDB.getTopLevelUrl()) + '>' + \
                    str(siteName) + '</a> before using this data in a report or publication.</h3>'

        return returnStr
    
        
    def generateLogout(self, fileName, expName):
        """ generateLogout generates a java script which sends a user to the madLogin page to logout automatically.

        Inputs: fileName: the madrigal file to return to
                expName:  the experiment name of the file to return to
        
        Returns: a java script which sends a user to the madLogin page to logout automatically

        Affects: None.

        Exceptions: None.
        """

        print '<script language = "JavaScript">'
        print '\twindow.location = "madLogin?fileName=' + \
              fileName + '&expName=' + \
              expName + '&state=autoLogout"'
        print '</script>'


    def isTrusted(self):
        """ isTrusted returns 1 if browser ip matches any in the trustedIPs.txt file; 0 otherwise.

        Inputs: None
        
        Returns: 1 if browser ip matches any in the trustedIPs.txt file; 0 otherwise.  Also returns
        0 if no browser ip available or trustedIPs.txt cannot be opened.

        Affects: None.

        Exceptions: None.
        """
        if self.__isTrusted__ != None:
            return(self.__isTrusted__)
        
        try:
            trustFile = open(self.__madDB.getMadroot() + '/trustedIPs.txt', 'r')
        except:
            return 0

        # try to read env var REMOTE_ADDR and HTTP_X_FORWARDED_FOR
        userIPList = []
        if os.environ.get('REMOTE_ADDR')!= None:
            userIPList.append(os.environ.get('REMOTE_ADDR'))
        if os.environ.get('HTTP_X_FORWARDED_FOR') != None:
            ips = os.environ.get('HTTP_X_FORWARDED_FOR').split(',')
            for ip in ips:
                userIPList.append(ip.strip())
        if len(userIPList) == 0:
            self.__isTrusted__ = 0
            return 0
        if len(userIPList[0]) < 7:
            # ip address too short
            self.__isTrusted__ = 0
            return 0

        # loop through trustedIPs.txt to find a match
        ipList = trustFile.readlines()
        for userIP in userIPList:
            for ipItem in ipList:
                # match using filename matching with *
                if fnmatch.fnmatch(userIP, ipItem.strip()):
                    self.__isTrusted__ = 1
                    return 1

        # out of loop, no match found
        self.__isTrusted__ = 0
        return 0


    def logDataAccess(self, fullFilenameList, user_fullname=None, user_email=None, user_affiliation=None):
        """ logDataAccess logs queries that access low-level data.

        Records user name, email, affiliation, datetime, and full path the file(s) accessed.

        Inputs:

            fullFilenameList either a list of full filenames, or a string with one filename

            user_fullname - if None, try to read from cookie.  Also, any commas replaced by spaces.

            user_email - if None, try to read from cookie.  Also, any commas replaced by spaces.

            user_affiliation - if None, try to read from cookie.  Also, any commas replaced by spaces.
            

        Outputs: None

        Affects: Write line to log file with 5 or more comma-delimited columns.  Example:

            Bill Rideout,brideout@haystack.mit.edu,MIT Haystack,2002-12-25 00:00:00, \
            /opt/madrigal/experiments/2005/mlh/01sep05/mlh050901g.001,/opt/madrigal/experiments/2005/mlh/02sep05/mlh050902g.001

        Uses __getLock and __dropLock to ensure single users access to log file
        """

        if user_fullname == None or user_email == None or user_affiliation == None:
        
            # try to get name, email, affiliation from cookie
            cookie = Cookie.SimpleCookie()
            if os.environ.has_key('HTTP_COOKIE'):
                cookie.load(os.environ['HTTP_COOKIE'])
                try:
                    user_fullname = cookie["user_fullname"].value
                    user_email = cookie["user_email"].value
                    user_affiliation = cookie["user_affiliation"].value
                except:
                    # no way to write log
                    return

            if user_fullname == None or user_email == None or user_affiliation == None:
                return

        # strip out any commas
        user_fullname = user_fullname.replace(',', ' ')
        user_email = user_email.replace(',', ' ')
        user_affiliation = user_affiliation.replace(',', ' ')

        if type(fullFilenameList) in (types.ListType, types.TupleType):
            delimiter = ','
            fileStr = delimiter.join(fullFilenameList)
        else:
            fileStr = str(fullFilenameList)
        

        now = datetime.datetime.now()


        nowStr = now.strftime('%Y-%m-%d %H-%M-%S')

        # lock out any method that writes to log file
        self.__getLock(self.__logFile)

        f = open(self.__logFile, 'a')

        f.write('%s,%s,%s,%s,%s\n' % (user_fullname,
                                      user_email,
                                      user_affiliation,
                                      nowStr,
                                      fileStr))



        f.close()

        # done with log file - allow access to other writing calls
        self.__dropLock(self.__logFile)  


    def __getLock(self, filename):
        """__getLock is a private helper function that provides exclusive access to filename via a locking file.

        Inputs: filename = the file that exclusive access is required to.
        
        Returns: None

        Affects: Writes file filename + .LCK as a lock mechanism

        Exceptions: MadrigalError thrown if unable to write lock file

        Notes: Will sleep for 1 second at a time, for a maximum of _MaxSleep seconds (presently 10)
        if the file is not modified. After each second, it will check for the lock file to be removed
        or modified. If it was modified, it resets the count to 0 sec and starts counting again. After
        _MaxSleep counts it then assumes lock file is orphaned and returns.  Orphaned file will be
        removed when dropLock is called.
        """
        gotLock = 0
        numTries = 0
        modificationTime = 0
        
        while (not gotLock):

            try:
                file = os.open(filename + '.LCK', os.O_RDWR | os.O_CREAT | os.O_EXCL)
                os.close(file)
                gotLock = 1

            except OSError, (errno, strerror):
                # error 17 is "File exists"
                if errno != 17:
                    raise madrigal.admin.MadrigalError("Unable to open " + filename + ".LCK as locking file ", None)
                # get modification time - may throw an error if file has disappearred
                try:
                    newModTime = (os.stat(filename + '.LCK')).st_mtime
                except:
                    #file has disappeared, no need to sleep
                    continue

                # if the lock file has been modified (or if this is the first time through) set numTries = 0
                if newModTime > modificationTime:
                    modificationTime = newModTime
                    numTries = 0
                    
                time.sleep(1)
                
            numTries = numTries + 1

            if numTries > self.__MaxSleep:
                return

       
    def __dropLock(self, filename):
        """__dropLock is a private helper function that drops exclusive access to filename via a locking file.

        Inputs: filename = the file that exclusive access is required to.
        
        Returns: None

        Affects: Removes file filename + .LCK as a lock mechanism

        Exceptions: None.
        """
        try:
            os.remove(filename + '.LCK')

        except IOError:
            return


class MadrigalWebFormat:
    """MadrigalWebFormat defines the format of an web interface.

    Information about how a web page is formatted is stored in this class.  In particular,
    the possible derived parameters to display for a given format (such as Short or
    Comprehensive) are set in this class.  Edit this class to create new formats or
    modify existing ones.

    Non-standard Python modules used:
    None

    No exceptions thrown

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Oct. 29, 2001
    """

    # constants

    # Edit this data to change which parameters to display
    # or to add new formats
    #
    #                   Format          Parameters      
    #                   ------          ----------  
    __privateDict =  {'Comprehensive':  [   'year',
                                            'month',
                                            'day',
					    'bmonth',
					    'bday',
                                            'hour',
                                            'min',
                                            'sec',
                                            'md',
                                            'dayno',
                                            'bhm',
                                            'bhhmmss',
                                            'ehhmmss',
                                            'uth',
                                            'b_uth',
                                            'ut',
                                            'beg_ut',
                                            'slt',
                                            'sltc',
                                            'fyear',
                                            'sunrise_hour',
                                            'sunset_hour',
                                            'conj_sunrise_h',
                                            'conj_sunset_h',
                                            'aplt',
                                            'jdayno',
                                            'gdalt',
                                            'range',
                                            'resl',
                                            'azm',
                                            'az1',
                                            'az2',
                                            'elm',
                                            'el1',
                                            'el2',
                                            'gdlat',
                                            'glon',
                                            'szen',
                                            'szenc',
                                            'sdwht',
                                            'bn',
                                            'be',
                                            'bd',
                                            'magh',
                                            'magd',
                                            'magzu',
                                            'bmag',
                                            'bdec',
                                            'binc',
                                            'lshell',
                                            'diplat',
                                            'invlat',
                                            'aplat',
                                            'aplon',
                                            'e_reg_s_lat',
                                            'e_reg_s_lon',
                                            'e_reg_s_sdwht',
                                            'e_reg_n_lat',
                                            'e_reg_n_lon',
                                            'e_reg_n_sdwht',
                                            'magconjlat',
                                            'magconjlon',
                                            'magconjsdwht',
                                            'cgm_lat',
                                            'cgm_long',
                                            'tsyg_eq_xgsm',
                                            'tsyg_eq_ygsm',
                                            'tsyg_eq_xgse',
                                            'tsyg_eq_ygse',
                                            'paclat',
                                            'paclon',
                                            'aspect',
                                            'cxr',
                                            'cyr',
                                            'czr',
                                            'pl',
                                            'snp3',
                                            'chisq',
                                            'gfit',
                                            'mhdqc1',
                                            'systmp',
                                            'systmi',
                                            'power',
                                            'tfreq',
                                            'popl',
                                            'ne',
                                            'nel',
                                            'ti',
                                            'te',
                                            'tr',
                                            'vo',
                                            'ph+',
                                            'pm',
                                            'co',
                                            'vdopp',
                                            'dvdopp',
                                            'dco',
                                            'dpm',
                                            'dph+',
                                            'dvo',
                                            'dtr',
                                            'dte',
                                            'dti',
                                            'dpopl',
                                            'ne_model',
                                            'nel_model',
                                            'te_model',
                                            'ti_model',
                                            'ne_modeldiff',
                                            'nel_modeldiff',
                                            'te_modeldiff',
                                            'ti_modeldiff',
                                            'tn',
                                            'tnm',
                                            'tinfm',
                                            'mol',
                                            'nn2l',
                                            'no2l',
                                            'nol',
                                            'narl',
                                            'nhel',
                                            'nhl',
                                            'nn4sl',
                                            'fa',
                                            'pnrmd',
                                            'pnrmdi',
                                            'ut1',
                                            'ut2',
                                            'dut21',
                                            'kinst',
                                            'recno',
                                            'kindat',
                                            'fof2',
                                            'dfa',
                                            'dst',
                                            'kp',
                                            'ap',
                                            'ap3',
                                            'f10.7',
                                            'fbar',
                                            #'pdconl',
                                            #'hlconl',
                                            'ne_iri',
                                            'nel_iri',
                                            'tn_iri',
                                            'te_iri',
                                            'ti_iri',
                                            'po+_iri',
                                            'pno+_iri',
                                            'po2+_iri',
                                            'phe+_iri',
                                            'ph+_iri',
                                            'pn+_iri',
                                            'bxgsm',
                                            'bygsm',
                                            'bzgsm',
                                            'bimf',
                                            'bxgse',
                                            'bygse',
                                            'bzgse',
                                            'swden',
                                            'swspd',
                                            'swq'],
                      'Short':          [   'year',
                                            'md',
                                            'dayno',
                                            'uth',
                                            'b_uth',
                                            'ut',
                                            'beg_ut',
                                            'lt',
                                            'aplt',
                                            'jdayno',
                                            'gdalt',
                                            'range',
                                            'rangei',
                                            'azm',
                                            'az1',
                                            'az2',
                                            'elm',
                                            'el1',
                                            'el2',
                                            'gdlat',
                                            'glon',
                                            'popl',
                                            'nel',
                                            'ti',
                                            'te',
                                            'tr',
                                            'vo',
                                            'ph+',
                                            'pm',
                                            'co',
                                            'vdopp',
                                            'dvdopp',
                                            'dco',
                                            'dpm',
                                            'dph+',
                                            'dvo',
                                            'dtr',
                                            'dte',
                                            'dti',
                                            'dpopl',
                                            'kp',
                                            'ap',
                                            'ap3',
                                            'f10.7',
                                            'fbar']}

    def getFormat(self, formatName):
        return self.__privateDict[formatName]





if __name__ == '__main__':

    
    # test MadrigalWeb
    filepath = os.environ.get('MADROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

    test = MadrigalWeb()

    filt = madrigal.ui.userData.MadrigalFilter('test')


    filt.starthour = 13
    filt.startmin = 52
    filt.startsec = 43
    filt.endhour = 16
    filt.endmin = 0
    filt.endsec = 0
    filt.minalt = 500.0
    filt.maxalt = 1500.0
    filt.minaz = 178.0
    filt.maxaz = 180.0
    filt.minaz2 = 185.0
    filt.maxaz2 = 360.0
    filt.minel = 80.0
    filt.maxel = 90.0
    filt.minel2 = 20.0
    filt.maxel2 = 30.0
    filt.minpl = 0
    filt.maxpl = 1000.0
    filt.flkinst = 32
    filt.flkdat = 3408
    filt.parmlist = '20 21 34 120 121 125 126 130 132 133 140 -120 142 143 160 170 204 206 208 210 213 216 218 220 222 226'


    print test.getOptionNumericTags(1, 5, 4)

    print test.getOptionMonthTags(2)

    print test.getOptionListTags(['Fred', 'Barney', 'Wilma'], 1)

    print test.getIntListFromStrList(['0', '10', '-100', 200])

    print test.getCookieDateOneYearAhead()

    print test.getCookieDateOneYearAgo()

    print 'Print filter as html table:'
    print test.filterHtmlFormat(filt)

    print 'Print rules of the road as html:'
    print test.getRulesOfTheRoad()

    print 'Print kinst option list'
    kinstList = [31,32]
    print test.getOptionKinstListTags(kinstList, 1)

    
    # create MadrigalUserData obj
    madUserDataObj = madrigal.ui.userData.MadrigalUserData()

    print 'Create a list of public directories with first item selected:'
    print test.getOptionPubDirTags(madUserDataObj)

    print 'Create a list of public directories with jmh_public selected:'
    print test.getOptionPubDirTags(madUserDataObj, 'jmh', 'jmh:jmh_public')

    print 'Print the name of the first owner of a public directory:'
    print test.getFirstOwner(madUserDataObj)

    print 'Print the name of the first public directory:'
    print test.getFirstPubDir(madUserDataObj)

    print 'Create a list of private directories with first selected:'
    print test.getOptionPrivDirTags(madUserDataObj, 'brideout')

    print 'Create a list of private directories with second selected:'
    print test.getOptionPrivDirTags(madUserDataObj, 'brideout', 'brideout_private2')

    print 'Create a list of filters with first selected: '
    print test.getOptionFilterTags(madUserDataObj, 'brideout', 'brideout_private2')

    print 'Create a list of filters with second selected: '
    print test.getOptionFilterTags(madUserDataObj, 'brideout', 'brideout_private2', 'secondpriv2')

    print 'Create an option string of brideouts directories with first selected: '
    print test.getOptionAllUserDirTags(madUserDataObj, 'brideout')

    print 'Create an option string of brideouts directories with brideout_private selected: '
    print test.getOptionAllUserDirTags(madUserDataObj, 'brideout', 'brideout_private')

    print 'Get first user directory name for brideout:'
    print test.getFirstUserDir(madUserDataObj, 'brideout')

    print 'The following  should be this string in cgi form:'
    print test.getCgiString('The following  should be this string in cgi form:')

    print 'The following  should be a string in normal form:'
    print test.getSpaceString(test.getCgiString('The following  should be a string in normal form:'))

    print 'The following  should be this string in cgi form:'
    print test.getCgiStringFromList([123, ' fred ', 67.89])

    print 'Test of isTrusted:'
    if test.isTrusted():
        print 'User is trusted'
    else:
        print 'User is not trusted'

    print 'Test of logging'
    test.logDataAccess('/tmp/junk', 'Bill Rideout', 'brideout@haystack.mit.edu', 'MIT Obs')


    # test MadrigalWebFormat
    test = MadrigalWebFormat()
    print 'Short list is '
    compList = test.getFormat('Short')
    print compList
    print "Number of parameters in short is " + str(len(compList))


