"""userData is responsible for interfacing to all persisted user data on the madrigal web site.

This module is meant to hide the storage mechnaism of user data on the madrigal web site, so that
the present format (xml files) can be changed by only changing this module.  The data stored at the
moment consists of user login information, and directories (private and public) of stored filters,
where a filter is all information that determines the output of an isprint display.

This modules requires that the non-standard python module PyXML be installed (this module may become
standard in a future release of python).  See the python XML SIG for this module.

$Id: userData.py,v 1.12 2008/09/16 18:39:50 brideout Exp $
"""

# dom xml imports
import xml
from xml.dom.ext.reader.Sax2 import FromXmlFile
from xml.dom import ext
from xml.dom import implementation

# sax xml imports
from xml.sax import saxutils
from xml.sax import make_parser
from xml.sax.handler import feature_namespaces

import os
import string
import crypt
import time
import stat
import traceback
import shutil

import madrigal.metadata
import madrigal.admin
import madrigal.ui.userData


class MadrigalUserData:
    """MadrigalUserData is an object that provides access to all user data.

    The object MadrigalUserData is an object that provides read and write access to
    all persisted user information on the Madrigal web site.  For the moment this data
    consists of directories of filters used in isprint, but other information may be added
    later.  At the moment this data is stored in xml files, but the public part of this
    class should not change if another storage implementation (e.g., a database) is used

    Usage example:

        import madrigal.ui.userData
    
        test = madrigal.ui.userData.MadrigalUserData()

        print test.getUsersList()
        
    Non-standard Python modules used:
    
    xml from PyXML SIG

    MadrigalError exception thrown if:

        1. Problem reading/writing to xml files containing user data

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 11, 2001

    """

    #constants
    __userXMLDir   = "userdata"
    __userXMLFile  = "users.xml"
    __userXMLFileTemplate = "users.xml.template"
    # maximum number of seconds to wait for a lock
    __MaxSleep     = 10
    # 2 char string to use in crypt
    # Changing this will cause all stored passwords to fail
    __cryptStr     = 'md'
    

    def __init__(self, madDB = None):
        """__init__ initializes MadrigalUserData by reading from MadridalDB..

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__metaDir.

        Exceptions: None.
        """

        # get metadata dir
        if madDB == None:
            thisMadDB = madrigal.metadata.MadrigalDB()
        else:
            thisMadDB = madDB

        self.__metaDir = thisMadDB.getMetadataDir()

        # start with empty dictionary - if needed, initialized by getAllDirInfo
        self.__dirsDict = {}

        # check that users.xml exists, if not, copy users.xml.template
        if not os.path.isfile(self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFile):
            shutil.copyfile(self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFileTemplate,
                            self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFile)

        
        
    def getUsersList(self):
        """getUsersList returns a list of user names/encrypted passwords that already exist.

        Inputs: none.
        
        Returns: a list of user names/passwords.  Each item in the returned list is itself a
        list with two strings: 1) username, and 2) encrypted password.

        Usage example:

            import madrigal.ui.userData

            test = madrigal.ui.userData.MadrigalUserData()

            userlist = test.getUsersList()

            for user in userlist:

                print 'User name is ' + user[0] + ' and encrypted password is ' + user[1]

        Affects: None

        Exceptions: none
        """
        
        # update userList
        self.__loadUserData()
        
        return self.__userList


    def userExists(self, username):
        """userExists returns 1 if username (case insensitive) exists, 0 otherwise.

        Inputs: username string.
        
        Returns: 1 if username (case insensitive) exists, 0 otherwise.

        Affects: None

        Exceptions: none
        """

        # update userList
        self.__loadUserData()

        nameLowCase = username.lower().strip()
        
        for name in self.__userList:
            if nameLowCase == name[0].lower().strip():
                return 1

        #not found
        return 0


    def verifyUser(self, username, password):
        """verifyUser returns 1 if username, password okay, 0 otherwise.

        Inputs: username string, password string.
        
        Returns: 1 if username, password okay, 0 otherwise.

        Affects: None

        Exceptions: none
        """

        # update userList
        self.__loadUserData()

        nameLowCase = username.lower().strip()
        
        for name in self.__userList:
            if nameLowCase == name[0].lower().strip():
                # username found, now encrypt password
                pwdEncrypt = crypt.crypt(password, self.__cryptStr)
                # verify password
                if pwdEncrypt == name[1]:
                    return 1
                else:
                    # wrong password
                    return 0

        # username not found
        return 0


    def addUser(self, username, password):
        """addUser returns 1 if user added successfully, error string otherwise.

        Inputs: username string, password string (password is not yet encrypted).
        
        Returns: 1 if user added successfully, error string otherwise.   

        Affects: Adds new user to self.__userList, writes new empty <username>.xml file
        username is always converted and stored as lower case, so its case insensitive.

        Exceptions: MadrigalError thrown if unable to write to user xml file

        Notes:  uses getLock and dropLock to insure exclusive access to user file
        """

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFile

        if username == None:
            return "Cannot create a user with null user name."
        
        # store all usernames as lowercase
        username = username.lower().strip()

        if len(username) == 0:
            return "Cannot create a user with blank user name."

        # lock out any method that writes to file
        self.__getLock(filename)
        
        # user data loaded by userExists

        if self.userExists(username):
            # done with user file - allow access to other writing calls
            self.__dropLock(filename)
            return 'User name ' + username + ' already exists.'

        if not self.__isValidFileName(username):
            # done with user file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Username ' + username + ' contains illegal characters.'


        #encrypt password
        pwdEncrypt = crypt.crypt(password, self.__cryptStr)

        # create dom
        userDoc = FromXmlFile(filename)

        #get root element
        docRoot = userDoc.documentElement

        # create new user element
        newUserElem = userDoc.createElementNS(None, 'user')

        # now create new name element under newUserElem
        newUsernameElem = userDoc.createElementNS(None, 'name')

        # Create a text node
        newUsernameText = userDoc.createTextNode(username)

        # append text node to newUsernameElem
        newUsernameElem.appendChild(newUsernameText)

        # append name node to user node
        newUserElem.appendChild(newUsernameElem)

        #now create new password element under newUserElem
        newPasswordElem = userDoc.createElementNS(None, 'password')

        #Create a text node
        newPasswordText = userDoc.createTextNode(pwdEncrypt)

        # append text node to newPasswordElem
        newPasswordElem.appendChild(newPasswordText)

        # append password node to user node
        newUserElem.appendChild(newPasswordElem)

        #Add the new user element to the document element
        docRoot.appendChild(newUserElem)

        #output result
        outfp = open(filename, 'w')
        xml.dom.ext.PrettyPrint(userDoc, outfp)
        outfp.write("\n")
        outfp.close()

        # done with user file - allow access to other writing calls
        self.__dropLock(filename)

        # create new file <username>.xml
        outfp = open(self.__metaDir + '/' + self.__userXMLDir + '/' + username + '.xml', 'w')
        outfp.write('<?xml version=\'1.0\'?>\n')
        outfp.write('<userInfo>\n')
        outfp.write('</userInfo>\n')
        outfp.close()
                     

        return 1


    def changePassword(self, username, password):
        """changePassword returns 1 if user password changed successfully, error string otherwise.

        Inputs: username string, password string (password is not yet encrypted).
        
        Returns: 1 if password changed successfully, error string otherwise.

        Affects: Modifies password in self.__userList, writes new user.xml file

        Exceptions: MadrigalError thrown if unable to write user xml file
        """

        username = username.lower().strip()

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFile

        # lock out any method that writes to file
        self.__getLock(filename)

        # user data loaded by userExists

        #make username lower, without white space
        username = username.lower().strip()

        if not self.userExists(username):
            return 'User ' + username + ' does not exist.'

        #encrypt password
        pwdEncrypt = crypt.crypt(password, self.__cryptStr)

        # open dom
        userDoc = FromXmlFile(filename)

        #now create new password element to replace old
        newPasswordElem = userDoc.createElementNS(None, 'password')

        #Create a text node
        newPasswordText = userDoc.createTextNode(pwdEncrypt)

        # append text node to newPasswordElem
        newPasswordElem.appendChild(newPasswordText)

        # get all user elements
        userElemList = userDoc.getElementsByTagName("user")

        # loop through each user element
        for user in userElemList:
            thisUserNameEl = user.getElementsByTagName('name')[0]
            if username == self.__getText(thisUserNameEl.childNodes):
                # modify password
                thisPasswordEl = user.getElementsByTagName('password')[0]
                user.replaceChild(newPasswordElem, thisPasswordEl)

        #output result
        outfp = open(self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFile, 'w')
        xml.dom.ext.PrettyPrint(userDoc, outfp)
        outfp.write("\n")
        outfp.close()

        # done with user file - allow access to other writing calls
        self.__dropLock(filename)
                     
        return 1


    def addDirectory(self, username, dirname, dirtype):
        """addDirectory returns 1 if directory added successfully to <username>.xml, error string otherwise.

        Inputs: username string, directory name string, dirtype string (public or private).
        
        Returns: 1 if directory added successfully, error string otherwise. dirtype
        must be either public or private.  Directory names are case sensitive, so Bill and bill
        can both be directories

        Affects: Adds new directory to <username>.xml file

        Exceptions: MadrigalError thrown if unable to write to <username>.xml file

        Notes:  uses getLock and dropLock to insure exclusive access to <username>.xml file
        """

        username = username.lower().strip()

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + username + '.xml'

        dirname = dirname.strip()

        dirtype = dirtype.lower().strip()

        # lock out any other method that writes to file
        self.__getLock(filename)

        if not (dirtype == 'public' or dirtype == 'private'):
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Directory type must be public or private.'

        #check that user xml file exists, if not throw error
        if not os.access(filename, os.F_OK):
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            raise madrigal.admin.MadrigalError, "Unable to open " + filename 

        # read in dom
        userDoc = FromXmlFile(filename)

        #get root element
        docRoot = userDoc.documentElement

        #check that directory doesn't already exist - if so, drop lock and return error string
        if self.__elementExists(dirname, 'dirname', userDoc):
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Directory name ' + dirname + ' already exists for user ' + username + '.'

        # create new directory element
        newDirElem = userDoc.createElementNS(None, 'directory')

        #now create new dir name element under newDirElem
        newDirnameElem = userDoc.createElementNS(None, 'dirname')

        #Create a text node
        newDirnameText = userDoc.createTextNode(dirname)

        # append text node to newDirnameElem
        newDirnameElem.appendChild(newDirnameText)

        # append dirname node to directory node
        newDirElem.appendChild(newDirnameElem)

        #now create new dirtype element under newDirElem
        newDirtypeElem = userDoc.createElementNS(None, 'dirtype')

        #Create a text node
        newDirtypeText = userDoc.createTextNode(dirtype)

        # append text node to newDirtypeElem
        newDirtypeElem.appendChild(newDirtypeText)

        # append dirtype node to dir node
        newDirElem.appendChild(newDirtypeElem)

        #Add the new dir element to the document element
        docRoot.appendChild(newDirElem)

        #output result
        outfp = open(filename, 'w')
        xml.dom.ext.PrettyPrint(userDoc, outfp)
        outfp.write("\n")
        outfp.close()

        # done with user file - allow access to other writing calls
        self.__dropLock(filename)
                     
        return 1


    def removeDirectory(self, username, dirname):
        """removeDirectory returns 1 if a  directory is removed successfully from a directory in <username>.xml, error string otherwise.

        Inputs: username string, directory name to be removed.
        
        Returns: 1 if filter removed successfully, error string otherwise. Will not remove a directory that contains filters.

        Affects: Removes existing dirctory from <username>.xml file

        Exceptions: MadrigalError thrown if unable to write to <username).xml file

        Notes:  uses getLock and dropLock to insure exclusive access to <username>.xml file
        """

        username = username.lower().strip()

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + username + '.xml'

        dirname = dirname.strip()

        # lock out any other method that writes to file
        self.__getLock(filename)

        #check that user xml file exists, if not throw error
        if not os.access(filename, os.F_OK):
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            raise madrigal.admin.MadrigalError, "Unable to open " + filename 

        # read in dom
        userDoc = FromXmlFile(filename)

        # find directory element deleteDirElem to delete 
        deleteDirElem = None
        
        dirElemList = userDoc.getElementsByTagName('directory')

        for dirElem in dirElemList:
            # check each childnode to see if its a dirname element
            if deleteDirElem != None: break
            for elem in dirElem.childNodes:
                if elem.nodeType == elem.ELEMENT_NODE:
                    if elem.localName == ('dirname'):
                        # check if its the right dirname
                        if self.__getText(elem.childNodes) == dirname:
                            #right directory found
                            deleteDirElem = dirElem
                            break

        # if directory not found, return error sring
        if deleteDirElem == None:
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Directory name ' + dirname + ' not found in xml file for user ' + username + '.'

        # check if any filters exist under the dir, if so return a error string
        filterList = deleteDirElem.getElementsByTagName('filter')
        if len(filterList) != 0:
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Directory name ' + dirname + ' cannot be deleted because its not empty.'

        # delete directory
        parent = deleteDirElem.parentNode
        parent.removeChild(deleteDirElem)
            
        #output result
        outfp = open(filename, 'w')
        xml.dom.ext.PrettyPrint(userDoc, outfp)
        outfp.write("\n")
        outfp.close()

        # done with user file - allow access to other writing calls
        self.__dropLock(filename)
                     
        return 1


    def addFilter(self, username, dirname, filter):
        """addFilter returns 1 if a new filter is added successfully to a directory in <username>.xml, 0 otherwise.

        Inputs: username string, directory name string, MadrigalFilter object to be added.
        
        Returns: 1 if filter added successfully, error string otherwise. 

        Affects: Adds new filter to <username>.xml file

        Exceptions: MadrigalError thrown if unable to write to <username).xml file

        Notes:  uses getLock and dropLock to insure exclusive access to <username>.xml file
        """

        username = username.lower().strip()

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + username + '.xml'

        dirname = dirname.strip()

        # filtername must be at least one char
        
        if filter.name == None:
            return "Cannot create a filter with no filter name."

        filtername = filter.name.strip()

        if len(filtername) == 0:
            return "Cannot create a filter with blank filter name."

        # lock out any other method that writes to file
        self.__getLock(filename)

        #check that user xml file exists, if not throw error
        if not os.access(filename, os.F_OK):
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            raise madrigal.admin.MadrigalError, "Unable to open " + filename 

        # read in dom
        userDoc = FromXmlFile(filename)

        # create new filter element
        newFilterElem = self.__createFilterElement(userDoc, filter)

        # find directory element insertDirElem to insert in into
        insertDirElem = None
        
        dirElemList = userDoc.getElementsByTagName('directory')

        for dirElem in dirElemList:
            # check each childnode to see if its a dirname element
            if insertDirElem != None: break
            for elem in dirElem.childNodes:
                if elem.nodeType == elem.ELEMENT_NODE:
                    if elem.localName == ('dirname'):
                        # check if its the right dirname
                        if self.__getText(elem.childNodes) == dirname:
                            #right directory found
                            insertDirElem = dirElem
                            break

        # if directory not found, return error sring
        if insertDirElem == None:
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Directory name ' + dirname + ' not found in xml file for user ' + username + '.'

        #check that filtername does not already exist under that directory
        filterNameList = insertDirElem.getElementsByTagName('filtername')
        for filternameElem in filterNameList:
            if filtername == self.__getText(filternameElem.childNodes):
                # done with file - allow access to other writing calls
                self.__dropLock(filename)
                return 'Filter name ' + filtername + ' already exists in ' + \
                       dirname + ' in xml file for user ' + username + '.'

        #Add the new filter element to the correct directory element
        insertDirElem.appendChild(newFilterElem)

        #output result
        outfp = open(filename, 'w')
        xml.dom.ext.PrettyPrint(userDoc, outfp)
        outfp.write("\n")
        outfp.close()

        # done with user file - allow access to other writing calls
        self.__dropLock(filename)
                     
        return 1


    def removeFilter(self, username, dirname, filtername):
        """removeFilter returns 1 if a  filter is removed successfully from a directory in <username>.xml, error string otherwise.

        Inputs: username string, directory name string, filtername of filter to be removed.
        
        Returns: 1 if filter removed successfully, error string otherwise. 

        Affects: Removes existing filter from <username>.xml file

        Exceptions: MadrigalError thrown if unable to write to <username).xml file

        Notes:  uses getLock and dropLock to insure exclusive access to <username>.xml file
        """

        username = username.lower().strip()

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + username + '.xml'

        dirname = dirname.strip()

        # filtername must be at least one char
        
        if filtername == None:
            return 'Must pass in a non-null filter name.'
        
        filtername = filtername.strip()

        if len(filtername) == 0:
            return 'Must pass in a non-zero length filter name.'

        # lock out any other method that writes to file
        self.__getLock(filename)

        #check that user xml file exists, if not throw error
        if not os.access(filename, os.F_OK):
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            raise madrigal.admin.MadrigalError, "Unable to open " + filename 

        # read in dom
        userDoc = FromXmlFile(filename)

        # find directory element deleteDirElem to delete from
        deleteDirElem = None
        
        dirElemList = userDoc.getElementsByTagName('directory')

        for dirElem in dirElemList:
            # check each childnode to see if its a dirname element
            if deleteDirElem != None: break
            for elem in dirElem.childNodes:
                if elem.nodeType == elem.ELEMENT_NODE:
                    if elem.localName == ('dirname'):
                        # check if its the right dirname
                        if self.__getText(elem.childNodes) == dirname:
                            #right directory found
                            deleteDirElem = dirElem
                            break

        # if directory not found, return error sring
        if deleteDirElem == None:
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Directory name ' + dirname + ' not found in xml file for user ' + username + '.'

        # find filter element under that directory node and remove it
        filterFound = 0
        filterNameList = deleteDirElem.getElementsByTagName('filtername')
        for filternameElem in filterNameList:
            if filtername == self.__getText(filternameElem.childNodes):
                # found correct filter
                filterFound = 1
                deleteFilterElem = filternameElem.parentNode
                # delete filter
                deleteDirElem.removeChild(deleteFilterElem)
                break
                

        # if filter not found, return error string
        if not filterFound:
            # done with file - allow access to other writing calls
            self.__dropLock(filename)
            return 'Filter name ' + filtername + ' not found in directory ' + \
                   dirname + ' in xml file for user ' + username + '.'
            

        #output result
        outfp = open(filename, 'w')
        xml.dom.ext.PrettyPrint(userDoc, outfp)
        outfp.write("\n")
        outfp.close()

        # done with user file - allow access to other writing calls
        self.__dropLock(filename)
                     
        return 1


    def getAllDirInfo(self):
        """getAllDirInfo returns a dictionary with key=username, value=list of MadrigalDirectory objects owned by user.

        Inputs: none.
        
        Returns: a dictionary with key=username, value=list of MadrigalDirectory objects owned by user.  See this file
        for the description of the information in a MadrigalDirectory class.

        Affects: builds __dirsDict.  Other public functions that use __dirsDict will call getAllDirInfo to
        populate __dirsDict if its = None

        Exceptions: MadrigalError thrown if unable to read a user file

        Usage example:

            import madrigal.ui.userData

            test = madrigal.ui.userData.MadrigalUserData()

            userDirInfo = test.getAllDirInfo()

            # print all the directory information for user brideout
            
            for madDir in userDirInfo['brideout']:

                print 'Directory name is ' + madDir.dirName

                print 'This directory type (public or private) is ' + madDir.dirType

                print 'The filter names in this directory are:'

                for filtername in madDir.filterList:

                    print '    ' + filtername
            

        Notes: Presently implemented by the sax handler class getDirInfoParser (included in this file).
        """

        # update userList
        self.__loadUserData()
        
        # start with empty dictionary
        self.__dirsDict = {}

        # set up to user our Sax handler
        # Create a parser
        parser = make_parser()
                
        # Tell the parser we are not interested in XML namespaces
        parser.setFeature(feature_namespaces, 0)

        # loop through each users file
        for user in self.__userList:

            # name of each user xml file
            userFile = self.__metaDir + '/' + self.__userXMLDir + '/' + user[0] + '.xml'

            # create list of MadrigalDirectory objects
            userMadDirList = []

            # check that users xml file exists, if not throw error
            if not os.access(userFile, os.F_OK):
                raise madrigal.admin.MadrigalError, "Unable to open " + userFile

            # Create the handler
            dh = getDirInfoParser()

            # Tell the parser to use our handler
            parser.setContentHandler(dh)

            # parse the users xml file
            parser.parse(userFile)

            # add user's data to dictionary
            self.__dirsDict[user[0]] = dh.madDirList

            # reset the parser
            dh = None
            
            # next user file

        # done with all files, return dict
        return self.__dirsDict


    def getPublicDirNameList(self):
        """getPublicDirNameList returns a list of strings of the names of all public directories in form 'user:dirName'.

        Inputs: none.
        
        Returns: a list of strings of the names of all public directories

        Affects: Builds __dirsDict by calling getAllDirInfo if not yet populated

        Exceptions: None
        """

        # make sure __dirDict is populated
        if self.__dirsDict == {}:
            self.getAllDirInfo()
            
        __pubDirNameList = []

        # loop through each user
        for user in self.__dirsDict.keys():
            # for each user, loop through each dir
            for madDir in self.__dirsDict[user]:
                # if public, add to list
                if madDir.dirType == 'public':
                    __pubDirNameList.append(user + ':' + madDir.dirName)

        return __pubDirNameList


    def getPrivateDirNameList(self, username):
        """getPrivateDirNameList returns a list of strings of the names of all private directories for a given user.

        Inputs: username (string).
        
        Returns: a list of strings of the names of all private directories owned by that user.
        Empty list if none

        Affects: Builds __dirsDict by calling getAllDirInfo if not yet populated

        Exceptions: None
        """

        # make sure __dirDict is populated
        if self.__dirsDict == {}:
            self.getAllDirInfo()
            
        __privateDirNameList = []

        # convert username to lower case
        username = username.lower().strip()


        for madDir in self.__dirsDict[username]:
            # if private, add to list
            if madDir.dirType == 'private':
                __privateDirNameList.append(madDir.dirName)

        return __privateDirNameList


    def getFilterNameList(self, username, dirName):
        """getFilterNameList returns a list of strings of the names of all filters in a given directory of a given user.

        Inputs: username (string), directory name (string).
        
        Returns: a list of strings of the names of all filters in a given directory of a given user (empty list if none)

        Affects: Builds __dirsDict by calling getAllDirInfo if not yet populated

        Exceptions: None
        """

        # make sure __dirDict is populated
        if self.__dirsDict == {}:
            self.getAllDirInfo()
            
        __filterNameList = []

        # convert username to lower case
        username = username.lower().strip()

        # if username doesn't exist, return empty list
        if not self.__dirsDict.has_key(username):
            return __filterNameList

        for madDir in self.__dirsDict[username]:
            # find matching MadrigalDirectory object
            if madDir.dirName == dirName:
                # loop through all filters in directory
                for filtername in madDir.filterList:
                    __filterNameList.append(filtername)

        return __filterNameList
                
            

    def getFilter(self, username, dirname, filtername):
        """getFilter returns a MadrigalFilter object specified by the username, dirname, and filtername.

        Inputs: username string (case insensitive), dirname string (case sensitive), filtername string(case sensitive).
        
        Returns: a MadrigalFilter object specified by the username, dirname, and filtername.  If none matches,
        return None

        Affects: None

        Exceptions: MadrigalError thrown if unable to read a user file

        Notes: Presently implemented by the sax handler class getFilterParser (included in this file) for greater
        speed than the use of dom.
        """
        
        #make username lower, without white space
        username = username.lower().strip()
        
        # set up to user our Sax handler
        # Create a parser
        parser = make_parser()
                
        # Tell the parser we are not interested in XML namespaces
        parser.setFeature(feature_namespaces, 0)

        # name of  <username>.xml file
        userFile = self.__metaDir + '/' + self.__userXMLDir + '/' + username + '.xml'

        # check that users xml file exists, if not throw error
        if not os.access(userFile, os.F_OK):
            raise madrigal.admin.MadrigalError, "Unable to open " + userFile

        # Create the handler
        dh = getFilterParser(username, dirname, filtername)

        # Tell the parser to use our handler
        parser.setContentHandler(dh)

        # parse the file
        parser.parse(userFile)

        # return the MadrigalFilter
        return dh.filter


    ########  End public functions - rest of MadrigalUserData class is helper functions ############

    def __createFilterElement(self, userDoc, filter):
        """__createFilterElement is a private helper function that takes a MadrigalFilter python object and converts to an xml node.

        Inputs: userDoc - the xml document to be added to, filter - a MadrigalFilter object to be
        persisted as an xml node in an xml file.
        
        Returns: the xml node created

        Affects: None

        Exceptions: None

        Notes: Each attribute added to the node takes four lines of code, basically 1) create new element,
        2) create text element, 3 append text element to new element, and 4) append new element to its parent.
        Since a MadrigalFilter has at the moment 23 attributes, this function is long but simply repeats those
        4 steps above.  One dom implementaion hint:  always finish creating a node first before appending to a
        parent.  Once a node is appended to a parent, it cannot have other nodes appended to it.  So always
        append from the bottom up.
        """
        
        newFilterElem = userDoc.createElementNS(None, 'filter')

        # -- filtername

        #now create new filtername element under newFilterElem
        newFilternameElem = userDoc.createElementNS(None, 'filtername')

        #Create a text node
        newFilternameText = userDoc.createTextNode(filter.name)

        # append text node to newFilternameElem
        newFilternameElem.appendChild(newFilternameText)

        # append filtername node to filter node
        newFilterElem.appendChild(newFilternameElem)

        # -- starttime

        #now create new startime element under newFilterElem
        newStarttimeElem = userDoc.createElementNS(None, 'starttime')

        # -- starthour

        #now create new starthour element under newStarttimeElem
        newStarthourElem = userDoc.createElementNS(None, 'starthour')

        #Create a text node
        newStarthourText = userDoc.createTextNode(self.__getXmlString(filter.starthour))

        # append text node to newStarhourElem
        newStarthourElem.appendChild(newStarthourText)

        # append starthour element to newStarttimeElem
        newStarttimeElem.appendChild(newStarthourElem)

        # -- startmin

        #now create new startmin element under newStarttimeElem
        newStartminElem = userDoc.createElementNS(None, 'startmin')

        #Create a text node
        newStartminText = userDoc.createTextNode(self.__getXmlString(filter.startmin))

        # append text node to newStarminElem
        newStartminElem.appendChild(newStartminText)

        # append startmin element to newStarttimeElem
        newStarttimeElem.appendChild(newStartminElem)

        # -- startsec

        #now create new startsec element under newStarttimeElem
        newStartsecElem = userDoc.createElementNS(None, 'startsec')

        #Create a text node
        newStartsecText = userDoc.createTextNode(self.__getXmlString(filter.startsec))

        # append text node to newStartsecElem
        newStartsecElem.appendChild(newStartsecText)

        # append startsec element to newStarttimeElem
        newStarttimeElem.appendChild(newStartsecElem)

        # append starttime elem to filter elem
        newFilterElem.appendChild(newStarttimeElem)

        # -- endtime

        #now create new endtime element under newFilterElem
        newEndtimeElem = userDoc.createElementNS(None, 'endtime')

        # -- endhour

        #now create new endhour element under newEndtimeElem
        newEndhourElem = userDoc.createElementNS(None, 'endhour')

        #Create a text node
        newEndhourText = userDoc.createTextNode(self.__getXmlString(filter.endhour))

        # append text node to newStarhourElem
        newEndhourElem.appendChild(newEndhourText)

        # append endhour element to newEndtimeElem
        newEndtimeElem.appendChild(newEndhourElem)

        # -- endmin

        #now create new endmin element under newEndtimeElem
        newEndminElem = userDoc.createElementNS(None, 'endmin')

        #Create a text node
        newEndminText = userDoc.createTextNode(self.__getXmlString(filter.endmin))

        # append text node to newStarminElem
        newEndminElem.appendChild(newEndminText)

        # append endmin element to newEndtimeElem
        newEndtimeElem.appendChild(newEndminElem)

        # -- endsec

        #now create new endsec element under newEndtimeElem
        newEndsecElem = userDoc.createElementNS(None, 'endsec')

        #Create a text node
        newEndsecText = userDoc.createTextNode(self.__getXmlString(filter.endsec))

        # append text node to newEndsecElem
        newEndsecElem.appendChild(newEndsecText)

        # append endsec element to newEndtimeElem
        newEndtimeElem.appendChild(newEndsecElem)

        # append endtime elem to filter elem
        newFilterElem.appendChild(newEndtimeElem)

        # -- minimum altitude

        #now create new minalt element under newFilterElem
        new_minaltElem = userDoc.createElementNS(None, 'minalt')

        #Create a text node
        new_minaltText = userDoc.createTextNode(self.__getXmlString(filter.minalt))

        # append text node to new_minaltElem
        new_minaltElem.appendChild(new_minaltText)

        # append minalt element to newFilterElem
        newFilterElem.appendChild(new_minaltElem)

        # -- maximum altitude

        #now create new maxalt element under newFilterElem
        new_maxaltElem = userDoc.createElementNS(None, 'maxalt')

        #Create a text node
        new_maxaltText = userDoc.createTextNode(self.__getXmlString(filter.maxalt))

        # append text node to new_maxaltElem
        new_maxaltElem.appendChild(new_maxaltText)

        # append maxalt element to newFilterElem
        newFilterElem.appendChild(new_maxaltElem)

        # -- minimum azimuth

        #now create new minaz element under newFilterElem
        new_minazElem = userDoc.createElementNS(None, 'minaz')

        #Create a text node
        new_minazText = userDoc.createTextNode(self.__getXmlString(filter.minaz))

        # append text node to new_minazElem
        new_minazElem.appendChild(new_minazText)

        # append minaz element to newFilterElem
        newFilterElem.appendChild(new_minazElem)

        # -- maximum azimuth

        #now create new maxaz element under newFilterElem
        new_maxazElem = userDoc.createElementNS(None, 'maxaz')

        #Create a text node
        new_maxazText = userDoc.createTextNode(self.__getXmlString(filter.maxaz))

        # append text node to new_maxazElem
        new_maxazElem.appendChild(new_maxazText)

        # append maxaz element to newFilterElem
        newFilterElem.appendChild(new_maxazElem)

        # -- minimum azimuth - 2

        #now create new minaz2 element under newFilterElem
        new_minaz2Elem = userDoc.createElementNS(None, 'minaz2')

        #Create a text node
        new_minaz2Text = userDoc.createTextNode(self.__getXmlString(filter.minaz2))

        # append text node to new_minaz2Elem
        new_minaz2Elem.appendChild(new_minaz2Text)

        # append minaz2 element to newFilterElem
        newFilterElem.appendChild(new_minaz2Elem)

        # -- maximum azimuth - 2

        #now create new maxaz2 element under newFilterElem
        new_maxaz2Elem = userDoc.createElementNS(None, 'maxaz2')

        #Create a text node
        new_maxaz2Text = userDoc.createTextNode(self.__getXmlString(filter.maxaz2))

        # append text node to new_maxaz2Elem
        new_maxaz2Elem.appendChild(new_maxaz2Text)

        # append maxaz2 element to newFilterElem
        newFilterElem.appendChild(new_maxaz2Elem)

        # -- minimum elevation

        #now create new minel element under newFilterElem
        new_minelElem = userDoc.createElementNS(None, 'minel')

        #Create a text node
        new_minelText = userDoc.createTextNode(self.__getXmlString(filter.minel))

        # append text node to new_minelElem
        new_minelElem.appendChild(new_minelText)

        # append minel element to newFilterElem
        newFilterElem.appendChild(new_minelElem)

        # -- maximum elevation

        #now create new maxel element under newFilterElem
        new_maxelElem = userDoc.createElementNS(None, 'maxel')

        #Create a text node
        new_maxelText = userDoc.createTextNode(self.__getXmlString(filter.maxel))

        # append text node to new_maxelElem
        new_maxelElem.appendChild(new_maxelText)

        # append maxel element to newFilterElem
        newFilterElem.appendChild(new_maxelElem)

        # -- minimum elevation - 2

        #now create new minel2 element under newFilterElem
        new_minel2Elem = userDoc.createElementNS(None, 'minel2')

        #Create a text node
        new_minel2Text = userDoc.createTextNode(self.__getXmlString(filter.minel2))

        # append text node to new_minel2Elem
        new_minel2Elem.appendChild(new_minel2Text)

        # append minel2 element to newFilterElem
        newFilterElem.appendChild(new_minel2Elem)

        # -- maximum elevation - 2

        #now create new maxel2 element under newFilterElem
        new_maxel2Elem = userDoc.createElementNS(None, 'maxel2')

        #Create a text node
        new_maxel2Text = userDoc.createTextNode(self.__getXmlString(filter.maxel2))

        # append text node to new_maxel2Elem
        new_maxel2Elem.appendChild(new_maxel2Text)

        # append maxel2 element to newFilterElem
        newFilterElem.appendChild(new_maxel2Elem)

        # -- minimum pulse length

        #now create new minpl element under newFilterElem
        new_minplElem = userDoc.createElementNS(None, 'minpl')

        #Create a text node
        new_minplText = userDoc.createTextNode(self.__getXmlString(filter.minpl))

        # append text node to new_minplElem
        new_minplElem.appendChild(new_minplText)

        # append minpl element to newFilterElem
        newFilterElem.appendChild(new_minplElem)

        # -- maximum pulse length

        #now create new maxpl element under newFilterElem
        new_maxplElem = userDoc.createElementNS(None, 'maxpl')

        #Create a text node
        new_maxplText = userDoc.createTextNode(self.__getXmlString(filter.maxpl))

        # append text node to new_maxplElem
        new_maxplElem.appendChild(new_maxplText)

        # append maxpl element to newFilterElem
        newFilterElem.appendChild(new_maxplElem)

        # -- kinst filter

        #now create new kinst element under newFilterElem
        new_kinstElem = userDoc.createElementNS(None, 'kinst')

        #Create a text node
        new_kinstText = userDoc.createTextNode(self.__getXmlString(filter.flkinst))

        # append text node to new_kinstElem
        new_kinstElem.appendChild(new_kinstText)

        # append kinst element to newFilterElem
        newFilterElem.appendChild(new_kinstElem)

        # -- kindat filter

        #now create new kindat element under newFilterElem
        new_kindatElem = userDoc.createElementNS(None, 'kindat')

        #Create a text node
        new_kindatText = userDoc.createTextNode(self.__getXmlString(filter.flkdat))

        # append text node to new_kindatElem
        new_kindatElem.appendChild(new_kindatText)

        # append kindat element to newFilterElem
        newFilterElem.appendChild(new_kindatElem)

        # -- mnemStr1

        #now create new mnemStr1 element under newFilterElem
        new_mnemStr1Elem = userDoc.createElementNS(None, 'mnemStr1')

        #Create a text node
        new_mnemStr1Text = userDoc.createTextNode(self.__getXmlString(filter.mnemStr1))

        # append text node to new_mnemStr1Elem
        new_mnemStr1Elem.appendChild(new_mnemStr1Text)

        # append mnemStr1 element to newFilterElem
        newFilterElem.appendChild(new_mnemStr1Elem)

        # -- lower1

        #now create new lower1 element under newFilterElem
        new_lower1Elem = userDoc.createElementNS(None, 'lower1')

        #Create a text node
        new_lower1Text = userDoc.createTextNode(self.__getXmlString(filter.lower1))

        # append text node to new_lower1Elem
        new_lower1Elem.appendChild(new_lower1Text)

        # append lower1 element to newFilterElem
        newFilterElem.appendChild(new_lower1Elem)

        # -- upper1

        #now create new upper1 element under newFilterElem
        new_upper1Elem = userDoc.createElementNS(None, 'upper1')

        #Create a text node
        new_upper1Text = userDoc.createTextNode(self.__getXmlString(filter.upper1))

        # append text node to new_upper1Elem
        new_upper1Elem.appendChild(new_upper1Text)

        # append upper1 element to newFilterElem
        newFilterElem.appendChild(new_upper1Elem)

        # -- mnemStr2

        #now create new mnemStr2 element under newFilterElem
        new_mnemStr2Elem = userDoc.createElementNS(None, 'mnemStr2')

        #Create a text node
        new_mnemStr2Text = userDoc.createTextNode(self.__getXmlString(filter.mnemStr2))

        # append text node to new_mnemStr2Elem
        new_mnemStr2Elem.appendChild(new_mnemStr2Text)

        # append mnemStr2 element to newFilterElem
        newFilterElem.appendChild(new_mnemStr2Elem)

        # -- lower2

        #now create new lower2 element under newFilterElem
        new_lower2Elem = userDoc.createElementNS(None, 'lower2')

        #Create a text node
        new_lower2Text = userDoc.createTextNode(self.__getXmlString(filter.lower2))

        # append text node to new_lower2Elem
        new_lower2Elem.appendChild(new_lower2Text)

        # append lower2 element to newFilterElem
        newFilterElem.appendChild(new_lower2Elem)

        # -- upper2

        #now create new upper2 element under newFilterElem
        new_upper2Elem = userDoc.createElementNS(None, 'upper2')

        #Create a text node
        new_upper2Text = userDoc.createTextNode(self.__getXmlString(filter.upper2))

        # append text node to new_upper2Elem
        new_upper2Elem.appendChild(new_upper2Text)

        # append upper2 element to newFilterElem
        newFilterElem.appendChild(new_upper2Elem)

        # -- parmeters to select list

        #now create new parmlist element under newFilterElem
        new_parmlistElem = userDoc.createElementNS(None, 'parmlist')

        #Create a text node
        new_parmlistText = userDoc.createTextNode(self.__getXmlString(filter.parmlist))

        # append text node to new_parmlistElem
        new_parmlistElem.appendChild(new_parmlistText)

        # append parmlist element to newFilterElem
        newFilterElem.appendChild(new_parmlistElem)

        # -- description

        #now create new description element under newFilterElem
        new_descriptionElem = userDoc.createElementNS(None, 'description')

        #Create a text node
        new_descriptionText = userDoc.createTextNode(self.__getXmlString(filter.description))

        # append text node to new_descriptionElem
        new_descriptionElem.appendChild(new_descriptionText)

        # append description element to newFilterElem
        newFilterElem.appendChild(new_descriptionElem)

        # -- header

        #now create new header element under newFilterElem
        new_headerElem = userDoc.createElementNS(None, 'header')

        #Create a text node
        new_headerText = userDoc.createTextNode(self.__getXmlString(filter.header))

        # append text node to new_headerElem
        new_headerElem.appendChild(new_headerText)

        # append header element to newFilterElem
        newFilterElem.appendChild(new_headerElem)

        # -- badval

        #now create new badval element under newFilterElem
        new_badvalElem = userDoc.createElementNS(None, 'badval')

        #Create a text node
        new_badvalText = userDoc.createTextNode(self.__getXmlString(filter.badval))

        # append text node to new_badvalElem
        new_badvalElem.appendChild(new_badvalText)

        # append badval element to newFilterElem
        newFilterElem.appendChild(new_badvalElem)

         # -- mxchar

        #now create new mxchar element under newFilterElem
        new_mxcharElem = userDoc.createElementNS(None, 'mxchar')

        #Create a text node
        new_mxcharText = userDoc.createTextNode(self.__getXmlString(filter.mxchar))

        # append text node to new_mxcharElem
        new_mxcharElem.appendChild(new_mxcharText)

        # append mxchar element to newFilterElem
        newFilterElem.appendChild(new_mxcharElem)


        return newFilterElem
            

    def __getText(self, nodelist):
        """__getText is a private helper function that returns a string made up of all the text nodes of a node list.

        Inputs: A list of xml nodes.
        
        Returns: a string made up of all the text nodes of a node list.

        Affects: None

        Exceptions: None

        Usage:  If an xml file contains <person age=43> John Doe  </person>, and the
        element you have (elem) is the 'person' element, then
        self.__getText(elem.childNodes) will return 'John Doe', since 'John Doe'
        is a child text node of person, and __getText strips leading and trailing
        whitespace.
        """
        
        rc = ""
        for node in nodelist:
            if node.nodeType == node.TEXT_NODE:
                rc = rc + node.data
        return rc.strip()


    def __elementExists(self, elem, elemTag, doc):
        """__elementExists is a private helper function that returns 1 if the xml doc has a element elemTag with text elem, 0 otherwise.
        
        Inputs: elem - a string to match text nodes against, elemTag - a string giving the element name to search in,
        and doc is the xml document.
        
        Returns: returns 1 if the xml doc has a element elemTag with text elem, 0 otherwise.

        Affects: None

        Exceptions: None

        Usage:  If an xml file contains <person age=43> John Doe  </person>, and you call
        self.__elementExists('John Doe', 'person', doc), 1 will be returned
        """
        
        elemList = doc.getElementsByTagName(elemTag)
        for item in elemList:
            if elem == self.__getText(item.childNodes):
                return 1

        # elem not found
        return 0
            
        


    def __loadUserData(self):
        """__loadUserData is a private helper function that reads in user information from an xml file.

        Inputs: None.
        
        Returns: void

        Affects: Populates self.__userList.

        Exceptions: MadrigalError thrown  if problem parsing users.xml.

        Depends on: Existance of file in metadata dir self.__userXMLDir + '/' + self.__userXMLFile (now userdata/user.xml)

        This file must be of the form of the following format:

            <?xml version='1.0'?>
            
            <users>
            
              <user>
              
                <name>brideout</name>
                
                <password>briWy6v1L.z1E</password>
                
              </user>
              
              possibly more users...
              
            </users>

        The password is stored encrypted by crypt.crypt(password, self.__cryptStr).  Implemented via xml.dom, since it is
        only a short file, but for higher speed (and more complex code) could be implemented with sax since read only.
        
        """

        filename = self.__metaDir + '/' + self.__userXMLDir + '/' + self.__userXMLFile

        #check that user xml file exists, if not throw error
        if not os.access(filename, os.F_OK):
            raise madrigal.admin.MadrigalError("Unable to open " + filename, None)
        

        #get list of user names
        userDoc = FromXmlFile(filename)

        #userDoc is root element of users.xml

        self.__userList = []

        # get all user tags
        userElemList = userDoc.getElementsByTagName("user")
        
        for user in userElemList:
            # get name from user list
            nameEl = user.getElementsByTagName("name")[0]

            # get text node, convert from unicode to ascii
            for elem in nameEl.childNodes:
                if elem.nodeType == elem.TEXT_NODE:
                   #self.__userList.append(elem.data.encode('utf-8'))
                   tempName = (elem.data.encode('utf-8'))

            # get encrypted password from user list
            nameEl = user.getElementsByTagName("password")[0]

            # get text node, convert from unicode to ascii
            for elem in nameEl.childNodes:
                if elem.nodeType == elem.TEXT_NODE:
                    #self.__userList.append(elem.data.encode('utf-8'))
                    tempPassword = (elem.data.encode('utf-8'))

            #append this user's information to user list
            self.__userList.append([tempName, tempPassword])



    def __isValidFileName(self, name):
        """__isValidFileName is a private helper function that validates that the string name does not contain excluded characters.
        
        Inputs: name - the string to be validated.
        
        Returns: 1 if all characters are allowed, 0 otherwise.  Valid if the following characters are not found in the
        string after leading and trailing whitespace is removed: [' ', '/', '<', '>', '\']

        Affects: None

        Exceptions: None.
        """

        name = name.strip()

        invalidChars = [' ', '/', '<', '>', '\\']

        # check if any invalid characters found in name
        for char in invalidChars:
            if -1 != name.find(char):
                # invalid char found
                return 0

        return 1

    

    def __getXmlString(self, obj):
        """__getXmlString is a private helper function that extends 'str' by making it return an empty string on a null object.
        
        Inputs: obj - the object to be converted to a string.
        
        Returns: if the object == None, returns '', otherwise returns str(obj).  Normally str(None) returns "None"

        Affects: None

        Exceptions: None.
        """
        if obj == None:
            return ''
        else:
            # make sure string is printable
            tempStr = str(obj)
            newStr = ''
            for char in tempStr:
                if char in string.printable:
                    newStr += char
                else:
                    newStr += ' '
            return str(newStr)

            

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
                    newModTime = os.stat(filename + '.LCK')[stat.ST_MTIME]
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


    ########## End of MadrigalUserData class ###########################
        



class MadrigalDirectory:
    """MadrigalDirectory is a public object that provides access to information in a single user directory.

    The MadrigalDirectory object is designed to allow easy access to the information in
    one directory of user data.  At the moment this data is the directory name, the directory
    type, and a list of filternames in that directory.  Created by MadrigalUserData.getAllDirInfo()

    Usage example:

        import madrigal.ui.userData

        test = madrigal.ui.userData.MadrigalUserData()

        userDirInfo = test.getAllDirInfo()

        # print all the directory information for user brideout
            
        for madDir in userDirInfo['brideout']:

            print 'Directory name is ' + madDir.dirName

            print 'This directory type (public or private) is ' + madDir.dirType

            print 'The filter names in this directory are:'

            for filtername in madDir.filterList:

                print '    ' + filtername
        
    Non-standard Python modules used: None

    Exceptions thrown: None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 11, 2001
    """

    def __init__(self, dirName, dirType):
        """__init__ initializes MadrigalDirectory by initializing the class members from the inputs.

        Inputs: dirName - case sensitive name of directory (string), dirType = public or private (string).
        
        Returns: void

        Affects: Initializes all the class member variables.  All member variables are public.

        Exceptions: None.
        """
        
        self.dirName = dirName

        self.dirType = dirType

        self.filterList = []
        

    def toString(self):

        return 'MadrigalDirectory, name = ' + self.dirName + ', type = ' + \
              self.dirType + ', filters = ' + str(self.filterList)

    ########## End of MadrigalUserData class ###########################



############# Private sax parser classes overriding DefaultHandler   ############################
               
class getDirInfoParser(saxutils.DefaultHandler):
    """getDirInfoParser is a private Sax Parser designed to rapidly parse a <username>.xml file for directory/filter names.

    This Sax parser is designed to return a list of MadrigalDirectory classes found in a given file. This list
    is presently used to support the isprint selection web page, which needs to know about all user directories
    and filters.

    For usage information for any Python Sax parser, see "Python Sax tutorial":http://py-howto.sourceforge.net/xml-howto/SAX.html

    This parser assumes the username.xml is of the form of the following format:

        <?xml version='1.0'?>
        
        <userInfo>
        
            <directory>
          
                <dirname>jmh_public</dirname>
            
                <dirtype>public</dirtype>
            
                <filter>
            
                    <filtername>jmh_pub1</filtername>

                    more filter elements ...

                </filter>

                possibly more filters...

            </directory>

            possibly more directories...

        </userInfor>

        

    Non-standard Python modules used:
    
    PyXML

    Exceptions thrown:

        Exception possibly thrown by sax module

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 11, 2001

    """
        
    def __init__(self):
        """ Override of saxutils.DefaultHandler __init__ function to support parsing <username>.xml."""
        
        self.madDirList = []
        self.presentMadDir = None
        self.presentDirName = None
        self.presentDirType = None
        self.presentFilterName = None
        self.inDirNameContent = 0
        self.inDirTypeContent = 0
        self.inFilterContent  = 0
           
    
    def startElement(self, name, attrs):
        """ Override of saxutils.DefaultHandler startElement function to support parsing <username>.xml."""
        
        # check if new directory found
        if name.lower().strip() == 'directory':
            self.presentMadDir = None
            self.presentDirName = ''
            self.presentDirType = ''
            self.presentFilterName = ''

        # check if new dirname found
        elif name.strip() == 'dirname':
            self.inDirNameContent = 1

        # check if new dirtype found
        elif name.lower().strip() == 'dirtype':
            self.inDirTypeContent = 1

        # check if new filter found
        elif name.strip() == 'filtername':
            self.inFilterContent = 1
            self.presentFilterName = ''
            # if no MadrigalDirectory object exists yet, create one
            if self.presentMadDir == None:
                self.presentMadDir = MadrigalDirectory(self.presentDirName, self.presentDirType)

        # ignore all other elements


    def characters(self, ch):
        """ Override of saxutils.DefaultHandler characters function to support parsing <username>.xml."""
        
        if self.inDirNameContent:
            self.presentDirName = self.presentDirName + ch.encode('utf-8').strip()
        elif self.inDirTypeContent:
            self.presentDirType = self.presentDirType + ch.encode('utf-8').strip()
        elif self.inFilterContent:
            self.presentFilterName = self.presentFilterName + ch.encode('utf-8').strip()

    def endElement(self, name):
        """ Override of saxutils.DefaultHandler endElement function to support parsing <username>.xml."""
        
        if name.strip() == 'dirname':
            self.inDirNameContent = 0
        elif name.lower().strip() == 'dirtype':
            self.inDirTypeContent = 0
        elif name.strip() == 'filtername':
            self.inFilterContent = 0
            # add filter to self.presentMadDir
            self.presentMadDir.filterList.append(self.presentFilterName)
        elif name.lower().strip() == 'directory':
            # add previous directory (if any) to list
            if self.presentMadDir != None:
                self.madDirList.append(self.presentMadDir)
            else:
                if self.presentDirName != None and self.presentDirType != None:
                    self.madDirList.append(MadrigalDirectory(self.presentDirName, self.presentDirType))



class getFilterParser(saxutils.DefaultHandler):
    """getFilterParser is a private Sax Parser designed to rapidly parse a <username>.xml file for one filter.

    This Sax parser is designed to return a list of MadrigalDirectory classes found in a given file. This list
    is presently used to support the isprint selection web page, which needs to know about all user directories
    and filters.

    For usage information for any Python Sax parser, see "Python Sax tutorial":http://py-howto.sourceforge.net/xml-howto/SAX.html

    This parser assumes the <username>.xml is of the form of the following format:


        <?xml version='1.0'?\>
        
        <userInfo>
        
            <directory>
          
                <dirname>jmh_public</dirname>
            
                <dirtype>public</dirtype>
            
                <filter>
            
                    <filtername>jmh_pub1</filtername>

                    more filter elements ...

                </filter>

                possibly more filters...

            </directory>

            possibly more directories...

        </userInfor>

        

    Non-standard Python modules used:
    
    xml.sax

    Exceptions thrown:

        Exception possibly thrown by sax module

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 11, 2001

    """
        
    def __init__(self, username, dirname, filtername):
        """ Override of saxutils.DefaultHandler startElement function to support parsing <username>.xml."""
        
        self.username        = username
        self.dirname         = dirname
        self.filtername      = filtername
        self.isRightDir      = 0
        self.isRightFilter   = 0
        self.checkDirName    = 0
        self.checkFilter     = 0
        self.done            = 0
        self.presentDirName  = ''
        self.presentFilter   = ''
        self.filter          = None

        # flag for every filter item
        self.checkstarthour  = 0
        self.checkstartmin   = 0
        self.checkstartsec   = 0
        self.checkendhour    = 0
        self.checkendmin     = 0
        self.checkendsec     = 0
        self.checkminalt     = 0
        self.checkmaxalt     = 0
        self.checkminaz      = 0
        self.checkmaxaz      = 0
        self.checkminaz2     = 0
        self.checkmaxaz2     = 0
        self.checkminel      = 0
        self.checkmaxel      = 0
        self.checkminel2     = 0
        self.checkmaxel2     = 0
        self.checkminpl      = 0
        self.checkmaxpl      = 0
        self.checkkinst      = 0
        self.checkkindat     = 0
        self.checkmnem1      = 0
        self.checklower1     = 0
        self.checkupper1     = 0
        self.checkmnem2      = 0
        self.checklower2     = 0
        self.checkupper2     = 0
        self.checkparmlist   = 0
        self.checkdescription = 0
        self.checkheader     = 0
        self.checkbadval     = 0
        self.checkmxchar     = 0

        # string for every filter item
        self.strstarthour  = ''
        self.strstartmin   = ''
        self.strstartsec   = ''
        self.strendhour    = ''
        self.strendmin     = ''
        self.strendsec     = ''
        self.strminalt     = ''
        self.strmaxalt     = ''
        self.strminaz      = ''
        self.strmaxaz      = ''
        self.strminaz2     = ''
        self.strmaxaz2     = ''
        self.strminel      = ''
        self.strmaxel      = ''
        self.strminel2     = ''
        self.strmaxel2     = ''
        self.strminpl      = ''
        self.strmaxpl      = ''
        self.strkinst      = ''
        self.strkindat     = ''
        self.strmnem1      = ''
        self.strlower1     = ''
        self.strupper1     = ''
        self.strmnem2      = ''
        self.strlower2     = ''
        self.strupper2     = ''
        self.strparmlist   = ''
        self.strdescription = ''
        self.strheader     = ''
        self.strbadval     = ''
        self.strmxchar     = ''

    def startElement(self, name, attrs):
        """ Override of saxutils.DefaultHandler startElement function to support parsing <username>.xml."""
        
        if self.done:
            return
        name = name.strip()
        
        # check if new dirname found
        if name == 'dirname':
            self.checkDirName  = 1
        elif name == 'filtername':
            self.checkFilter   = 1
        elif not self.isRightFilter:
            return
        # right filter found
        elif name == 'starthour':
            self.checkstarthour  = 1
        elif name == 'startmin':
            self.checkstartmin   = 1
        elif name == 'startsec':
            self.checkstartsec   = 1
        elif name == 'endhour':
            self.checkendhour    = 1
        elif name == 'endmin':
            self.checkendmin     = 1
        elif name == 'endsec':
            self.checkendsec     = 1
        elif name == 'minalt':
            self.checkminalt     = 1
        elif name == 'maxalt':
            self.checkmaxalt     = 1
        elif name == 'minaz':
            self.checkminaz      = 1
        elif name == 'maxaz':
            self.checkmaxaz      = 1
        elif name == 'minaz2':
            self.checkminaz2     = 1
        elif name == 'maxaz2':
            self.checkmaxaz2     = 1
        elif name == 'minel':
            self.checkminel      = 1
        elif name == 'maxel':
            self.checkmaxel      = 1
        elif name == 'minel2':
            self.checkminel2     = 1
        elif name == 'maxel2':
            self.checkmaxel2     = 1
        elif name == 'minpl':
            self.checkminpl      = 1
        elif name == 'maxpl':
            self.checkmaxpl      = 1
        elif name == 'kinst':
            self.checkkinst      = 1
        elif name == 'kindat':
            self.checkkindat     = 1
        elif name == 'mnemStr1':
            self.checkmnem1      = 1
        elif name == 'lower1':
            self.checklower1     = 1
        elif name == 'upper1':
            self.checkupper1     = 1
        elif name == 'mnemStr2':
            self.checkmnem2      = 1
        elif name == 'lower2':
            self.checklower2     = 1
        elif name == 'upper2':
            self.checkupper2     = 1
        elif name == 'parmlist':
            self.checkparmlist   = 1
        elif name == 'description':
            self.checkdescription   = 1
        elif name == 'header':
            self.checkheader     = 1
        elif name == 'badval':
            self.checkbadval     = 1
        elif name == 'mxchar':
            self.checkmxchar     = 1

    def characters(self, ch):
        """ Override of saxutils.DefaultHandler characters function to support parsing <username>.xml."""
        
        if self.done:
            return
        if self.checkDirName:
            self.presentDirName = self.presentDirName + ch.encode('utf-8').strip()
        elif self.checkFilter:
            self.presentFilter = self.presentFilter + ch.encode('utf-8').strip()
        elif not self.isRightFilter:
            return
        # right filter found
        elif self.checkstarthour:
            self.strstarthour  = self.strstarthour + ch.encode('utf-8').strip()
        elif self.checkstartmin:
            self.strstartmin   = self.strstartmin + ch.encode('utf-8').strip()
        elif self.checkstartsec:
            self.strstartsec   = self.strstartsec + ch.encode('utf-8').strip()
        elif self.checkendhour:
            self.strendhour    = self.strendhour + ch.encode('utf-8').strip()
        elif self.checkendmin:
            self.strendmin     = self.strendmin + ch.encode('utf-8').strip()
        elif self.checkendsec:
            self.strendsec     = self.strendsec + ch.encode('utf-8').strip()
        elif self.checkminalt:
            self.strminalt     = self.strminalt + ch.encode('utf-8').strip()
        elif self.checkmaxalt:
            self.strmaxalt     = self.strmaxalt + ch.encode('utf-8').strip()
        elif self.checkminaz:
            self.strminaz      = self.strminaz + ch.encode('utf-8').strip()
        elif self.checkmaxaz:
            self.strmaxaz      = self.strmaxaz + ch.encode('utf-8').strip()
        elif self.checkminaz2:
            self.strminaz2     = self.strminaz2 + ch.encode('utf-8').strip()
        elif self.checkmaxaz2:
            self.strmaxaz2     = self.strmaxaz2 + ch.encode('utf-8').strip()
        elif self.checkminel:
            self.strminel      = self.strminel + ch.encode('utf-8').strip()
        elif self.checkmaxel:
            self.strmaxel      = self.strmaxel + ch.encode('utf-8').strip()
        elif self.checkminel2:
            self.strminel2     = self.strminel2 + ch.encode('utf-8').strip()
        elif self.checkmaxel2:
            self.strmaxel2     = self.strmaxel2 + ch.encode('utf-8').strip()
        elif self.checkminpl:
            self.strminpl      = self.strminpl + ch.encode('utf-8').strip()
        elif self.checkmaxpl:
            self.strmaxpl      = self.strmaxpl + ch.encode('utf-8').strip()
        elif self.checkkinst:
            self.strkinst      = self.strkinst + ch.encode('utf-8').strip()
        elif self.checkkindat:
            self.strkindat     = self.strkindat + ch.encode('utf-8').strip()
        elif self.checkmnem1:
            self.strmnem1      = self.strmnem1 + ch.encode('utf-8').strip()
        elif self.checklower1:
            self.strlower1     = self.strlower1 + ch.encode('utf-8').strip()
        elif self.checkupper1:
            self.strupper1     = self.strupper1 + ch.encode('utf-8').strip()
        elif self.checkmnem2:
            self.strmnem2      = self.strmnem2 + ch.encode('utf-8').strip()
        elif self.checklower2:
            self.strlower2     = self.strlower2 + ch.encode('utf-8').strip()
        elif self.checkupper2:
            self.strupper2     = self.strupper2 + ch.encode('utf-8').strip()
        elif self.checkparmlist:
            self.strparmlist   = self.strparmlist + ch.encode('utf-8').strip()
        elif self.checkdescription:
            self.strdescription   = self.strdescription + ch.encode('utf-8').strip()
        elif self.checkheader:
            self.strheader     = self.strheader + ch.encode('utf-8').strip()
        elif self.checkbadval:
            self.strbadval     = self.strbadval + ch.encode('utf-8').strip()
        elif self.checkmxchar:
            self.strmxchar     = self.strmxchar + ch.encode('utf-8').strip()


    def endElement(self, name):
        """ Override of saxutils.DefaultHandler endElement function to support parsing <username>.xml."""
        
        if self.done:
            return

        name = name.strip()

        try:
        
            if name == 'dirname':
                self.checkDirName = 0
                if self.presentDirName == self.dirname:
                    # right directory found
                    self.isRightDir = 1
                else:
                    # not right directory
                    self.isRightDir = 0
                    self.presentDirName = ''
                    
            elif name == 'filtername':
                self.checkFilter = 0
                if self.presentFilter == self.filtername:
                    if self.isRightDir:
                        # right filter found'
                        self.isRightFilter = 1
                        # create MadrigalFilter
                        self.filter = madrigal.ui.userData.MadrigalFilter(self.presentFilter)
                else:
                    # not right filter
                    self.isRightFilter = 0
                    self.presentFilter = ''
                    
            elif not self.isRightFilter:
                return
            
            # right filter found
            elif name == 'filter':
                #done reading filter
                self.done = 1
                return
            elif name == 'starthour':
                if len(self.strstarthour):
                    self.filter.starthour  = int(self.strstarthour)
                self.checkstarthour = 0
            elif name == 'startmin':
                if len(self.strstartmin):
                    self.filter.startmin   = int(self.strstartmin)
                self.checkstartmin = 0
            elif name == 'startsec':
                if len(self.strstartsec):
                    self.filter.startsec   = int(self.strstartsec)
                self.checkstartsec = 0
            elif name == 'endhour':
                if len(self.strendhour):
                    self.filter.endhour    = int(self.strendhour)
                self.checkendhour = 0
            elif name == 'endmin':
                if len(self.strendmin):
                    self.filter.endmin     = int(self.strendmin)
                self.checkendmin = 0
            elif name == 'endsec':
                if len(self.strendsec):
                    self.filter.endsec     = int(self.strendsec)
                self.checkendsec = 0
            elif name == 'minalt':
                if len(self.strminalt):
                     self.filter.minalt     = float(self.strminalt)
                self.checkminalt = 0
            elif name == 'maxalt':
                if len(self.strmaxalt):
                    self.filter.maxalt     = float(self.strmaxalt)
                self.checkmaxalt = 0
            elif name == 'minaz':
                if len(self.strminaz):
                    self.filter.minaz      = float(self.strminaz)
                self.checkminaz = 0
            elif name == 'maxaz':
                if len(self.strmaxaz):
                    self.filter.maxaz      = float(self.strmaxaz)
                self.checkmaxaz = 0
            elif name == 'minaz2':
                if len(self.strminaz2):
                    self.filter.minaz2     = float(self.strminaz2)
                self.checkminaz2 = 0
            elif name == 'maxaz2':
                if len(self.strmaxaz2):
                    self.filter.maxaz2     = float(self.strmaxaz2)
                self.checkmaxaz2 = 0
            elif name == 'minel':
                if len(self.strminel):
                    self.filter.minel      = float(self.strminel)
                self.checkminel = 0
            elif name == 'maxel':
                if len(self.strmaxel):
                    self.filter.maxel      = float(self.strmaxel)
                self.checkmaxel = 0
            elif name == 'minel2':
                if len(self.strminel2):
                    self.filter.minel2     = float(self.strminel2)
                self.checkminel2 = 0
            elif name == 'maxel2':
                if len(self.strmaxel2):
                    self.filter.maxel2     = float(self.strmaxel2)
                self.checkmaxel2 = 0
            elif name == 'minpl':
                if len(self.strminpl):
                    self.filter.minpl      = float(self.strminpl)
                self.checkminpl = 0
            elif name == 'maxpl':
                if len(self.strmaxpl):
                    self.filter.maxpl      = float(self.strmaxpl)
                self.checkmaxpl = 0
            elif name == 'kinst':
                if len(self.strkinst):
                    self.filter.flkinst      = int(self.strkinst)
                self.checkkinst = 0
            elif name == 'kindat':
                if len(self.strkindat):
                    self.filter.flkdat     = int(self.strkindat)
                self.checkkindat = 0
            elif name == 'mnemStr1':
                if len(self.strmnem1):
                    self.filter.mnemStr1   = self.strmnem1
                self.checkmnem1 = 0
            elif name == 'lower1':
                if len(self.strlower1):
                    self.filter.lower1      = float(self.strlower1)
                self.checklower1 = 0
            elif name == 'upper1':
                if len(self.strupper1):
                    self.filter.upper1      = float(self.strupper1)
                self.checkupper1 = 0
            elif name == 'mnemStr2':
                if len(self.strmnem2):
                    self.filter.mnemStr2   = self.strmnem2
                self.checkmnem2 = 0
            elif name == 'lower2':
                if len(self.strlower2):
                    self.filter.lower2      = float(self.strlower2)
                self.checklower2 = 0
            elif name == 'upper2':
                if len(self.strupper2):
                    self.filter.upper2      = float(self.strupper2)
                self.checkupper2 = 0
            elif name == 'parmlist':
                if len(self.strparmlist):
                    self.filter.parmlist   = self.strparmlist
                self.checkparmlist = 0
            elif name == 'description':
                if len(self.strdescription):
                    self.filter.description   = self.strdescription
                self.checkdescription = 0
            elif name == 'header':
                if len(self.strheader):
                    self.filter.header      = self.strheader
                self.checkheader = 0
            elif name == 'badval':
                if len(self.strbadval):
                    self.filter.badval     = self.strbadval
                self.checkbadval = 0
            elif name == 'mxchar':
                if len(self.strmxchar):
                    self.filter.mxchar      = int(self.strmxchar)
                self.checkmxchar = 0

        except:
            raise madrigal.admin.MadrigalError("Unable to read filter: " + self.filtername + \
                                                       ' in directory: ' + self.dirname + ' in file: ' + \
                                                       self.username + '.xml',
                                                       traceback.format_exception(sys.exc_info()[0],
                                                                        sys.exc_info()[1],
                                                                        sys.exc_info()[2]))
                                                       

class MadrigalFilter:
    """MadrigalFilter is an object used to filter Madrigal data.

    This object provides access to all parameters used to filter Madrigal data for display.
    All its data members are public.


    Non-standard Python modules used:
    None

    Exceptions thrown: None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 10, 2001

    """

    def __init__(self, name = None,
                 startyear = None, startmonth = None, startday = None,
                 endyear = None, endmonth = None, endday = None,
                 starthour = None, startmin = None, startsec = None,
                 endhour = None, endmin = None, endsec = None,
                 minalt = None, maxalt = None,
                 minaz = None, maxaz = None,
                 minaz2 = None, maxaz2 = None,
                 minel = None, maxel = None,
                 minel2 = None, maxel2 = None,
                 minpl = None, maxpl = None,
                 mnemStr1 = None, lower1 = None, upper1 = None,
                 mnemStr2 = None, lower2 = None, upper2 = None,
                 flkinst = None, flkdat = None,
                 parmlist = None, description = None,
                 header = None, badval = None,
                 mxchar = None, assumed=None,
                 knownBad=None):
        """__init__ initializes MadrigalFilter by reading in arguments if any. All default to None.

        Inputs: name        filter name - string

                startyear   starting year - integer
                
                startmonth  starting month - integer
                
                startday    starting day - integer
                
                endyear     ending year - integer
                
                endmonth    ending month - integer
                
                endday      ending day - integer
        
                starthour   starting hour - integer
                
                startmin    starting minute - integer
                
                startsec    starting second - integer
                
                endhour     ending hour - integer
                
                endmin      ending minute - integer
                
                endsec      ending second - integer
                
                minalt      minimum altitude in km - float
                
                maxalt      maximum altitude in km - float
                
                minaz       minimum azimuth in degrees - float
                
                maxaz       maximum azimuth in degrees - float
                
                minaz2      minimum azimuth for second range in degrees - float
                
                maxaz2      maximum azimuth for second range in degrees - float
                
                minel       minimum elevation in degrees - float
                
                maxel       maximum elevation in degrees - float
                
                minel2      minimum elevation for second range in degrees - float
                
                maxel2      maximum elevation for second range in degrees - float
                
                minpl       minimum pulse length in microseconds - float
                
                maxpl       maximum pulse length in microseconds - float

                mnemStr1    a general mnemonic or two mnemonics separated by +-*/ - str

                lower1      lower limit (if any) for mnemStr1 - float

                upper1      upper limit (if any) for mnemStr1 - float

                mnemStr2    a general mnemonic or two mnemonics separated by +-*/ - str

                lower2      lower limit (if any) for mnemStr2 - float

                upper2      upper limit (if any) for mnemStr2 - float
                
                flkinst     the kind of instrument filter (used only if more than one in file) - integer
                
                flkdat      the kind of data filter (used only if more than one in file) - integer
                
                parmlist    a python string holding the mnemonics representing the parameters to display
                            (space delimited)
                
                description a python string holders the users description

                header      a python string = 'checked'

                badval      a python string representing a missing value in isprint output

                mxchar      represents the maximum characters per column to print - integer

                assumed     a python string representing an assumed value in isprint output

                knownBad    a python string representing a known bad value in isprint output
        
        Returns: void

        Affects: Initializes any class member variables with data passed in.

        Exceptions: none
        """

        self.name       =   name
        self.startyear  =   startyear
        self.startmonth =   startmonth
        self.startday   =   startday
        self.endyear    =   endyear
        self.endmonth   =   endmonth
        self.endday     =   endday
        self.starthour  =   starthour
        self.startmin   =   startmin
        self.startsec   =   startsec
        self.endhour    =   endhour
        self.endmin     =   endmin
        self.endsec     =   endsec
        self.minalt     =   minalt
        self.maxalt     =   maxalt
        self.minaz      =   minaz
        self.maxaz      =   maxaz 
        self.minaz2     =   minaz2
        self.maxaz2     =   maxaz2
        self.minel      =   minel
        self.maxel      =   maxel
        self.minel2     =   minel2
        self.maxel2     =   maxel2
        self.minpl      =   minpl
        self.maxpl      =   maxpl
        self.flkinst    =   flkinst
        self.flkdat     =   flkdat
        self.mnemStr1   =   mnemStr1
        self.lower1     =   lower1
        self.upper1     =   upper1
        self.mnemStr2   =   mnemStr2
        self.lower2     =   lower2
        self.upper2     =   upper2
        self.parmlist   =   parmlist
        self.description =  description
        self.header     =   header
        self.badval     =   badval
        self.mxchar     =   mxchar
        self.assumed    =   assumed
        self.knownBad   =   knownBad
        

    def toString(self):

        filtStr = ''
        filtStr = filtStr +  'name       ' + str(self.name) + '\n'
        filtStr = filtStr +  'startyear  ' + str(self.startyear) + '\n'
        filtStr = filtStr +  'startmonth ' + str(self.startmonth) + '\n'
        filtStr = filtStr +  'startday   ' + str(self.startday) + '\n'
        filtStr = filtStr +  'endyear    ' + str(self.endyear) + '\n'
        filtStr = filtStr +  'endmonth   ' + str(self.endmonth) + '\n'
        filtStr = filtStr +  'endday     ' + str(self.endday) + '\n'
        filtStr = filtStr +  'starthour  ' + str(self.starthour) + '\n'
        filtStr = filtStr +  'startmin   ' + str(self.startmin) + '\n'
        filtStr = filtStr +  'startsec   ' + str(self.startsec) + '\n'
        filtStr = filtStr +  'endhour    ' + str(self.endhour) + '\n'
        filtStr = filtStr +  'endmin     ' + str(self.endmin) + '\n'
        filtStr = filtStr +  'endsec     ' + str(self.endsec) + '\n'
        filtStr = filtStr +  'minalt     ' + str(self.minalt) + '\n'
        filtStr = filtStr +  'maxalt     ' + str(self.maxalt) + '\n'
        filtStr = filtStr +  'minaz      ' + str(self.minaz) + '\n'
        filtStr = filtStr +  'maxaz      ' + str(self.maxaz) + '\n'
        filtStr = filtStr +  'minaz2     ' + str(self.minaz2) + '\n'
        filtStr = filtStr +  'maxaz2     ' + str(self.maxaz2) + '\n'
        filtStr = filtStr +  'minel      ' + str(self.minel) + '\n'
        filtStr = filtStr +  'maxel      ' + str(self.maxel) + '\n'
        filtStr = filtStr +  'minel2     ' + str(self.minel2) + '\n'
        filtStr = filtStr +  'maxel2     ' + str(self.maxel2) + '\n'
        filtStr = filtStr +  'minpl      ' + str(self.minpl) + '\n'
        filtStr = filtStr +  'maxpl      ' + str(self.maxpl) + '\n'
        filtStr = filtStr +  'flkinst    ' + str(self.flkinst) + '\n'
        filtStr = filtStr +  'flkdat     ' + str(self.flkdat) + '\n'
        filtStr = filtStr +  'mnemStr1   ' + str(self.mnemStr1) + '\n'
        filtStr = filtStr +  'lower1     ' + str(self.lower1) + '\n'
        filtStr = filtStr +  'upper1     ' + str(self.upper1) + '\n'
        filtStr = filtStr +  'mnemStr2   ' + str(self.mnemStr2) + '\n'
        filtStr = filtStr +  'lower2     ' + str(self.lower2) + '\n'
        filtStr = filtStr +  'upper2     ' + str(self.upper2) + '\n'
        filtStr = filtStr +  'parmlist   ' + str(self.parmlist) + '\n'
        filtStr = filtStr +  'description ' + str(self.description) + '\n'
        filtStr = filtStr +  'header     ' + str(self.header) + '\n'
        filtStr = filtStr +  'badval     ' + str(self.badval) + '\n'
        filtStr = filtStr +  'mxchar     ' + str(self.mxchar) + '\n'

        return filtStr

            

if __name__ == '__main__':

    #test MadrigalUserData
    dB = madrigal.metadata.MadrigalDB()

    test = MadrigalUserData(dB)

    userlist = test.getUsersList()

    for user in userlist:

        print 'User name is ' + user[0] + ' and encrypted password is ' + user[1]

    print "brideout exists = " + str(test.userExists('BRideout'))

    print "JFoster exists = " + str(test.userExists(' JFoster '))

    print "JMH exists = " + str(test.userExists(' JMH '))

    print "brideout's password is brideout = " + str(test.verifyUser('BRideout', 'brideout'))

    print "brideout's password is BRideout = " + str(test.verifyUser('BRideout', 'BRideout'))

    print "JFoster's password is brideout = " + str(test.verifyUser('JFoster', 'brideout'))

    print "JMH's password is jmh = " + str(test.verifyUser('JMH ', 'jmh'))

    print "Adding user = junk, pw = junk, returned " + str(test.addUser('junk', 'junk'))

    print "Adding user = junk2, pw = junk2, returned " + str(test.addUser('junk2', 'junk2'))

    print "Adding user = junk3, pw = junk3, returned " + str(test.addUser('junk3', 'junk3'))

    print "Adding user = Bill Rideout, pw = brideout, returned " + str(test.addUser('Bill Rideout', 'brideout'))

    print "Adding user = junk4, pw = blank, returned " + str(test.addUser('junk4', ''))

    print "junk4's password is blank = " + str(test.verifyUser('junk4', ''))

    print "List of all directory info : "

    

    dictDirs = test.getAllDirInfo()

    # print length of dictDirs
    print 'Length of dict created is: ' + str(len(dictDirs))

    for user in dictDirs.keys():
        print user
        for madDir in dictDirs[user]:
            print madDir.toString()

    print 'Changing junks password, returns ' + str(test.changePassword('junk', 'brideout'))

    newFilter = test.getFilter('brideout', 'brideout_public', 'second')

    print

    print ' Get Filter second from brideout.xml directory brideout_public'

    print 'New filters name is: ' + str(newFilter.name)

    print newFilter.toString()

    print

    print ' Get Filter second from brideout.xml directory brideout_private'

    try:

        newFilter = test.getFilter('brideout', 'brideout_private', 'secondpriv')

        print 'New filters name is: ' + str(newFilter.name)

        print newFilter.toString()

    except:

        print 'filter not found'

    print

    print ' Get non-existant Filter third from brideout.xml directory brideout_private'

    newFilter = test.getFilter('brideout', 'brideout_private', 'third')

    print 'newFilter is ' + str(newFilter)

    print

    print ' Get filter second from brideout.xml non-existant directory brideout_nonesuch'

    newFilter = test.getFilter('brideout', 'brideout_nonesuch', 'second')

    print 'newFilter is ' + str(newFilter)

    print

    print ' Get non-existent filter second from junk.xml non-existant directory brideout_nonesuch'

    newFilter = test.getFilter('junk', 'brideout_nonesuch', 'second')

    print 'newFilter is ' + str(newFilter)

    print 'Result of add dir to brideout is ' + str(test.addDirectory('brideout', 'newDirectory', 'public'))

    print 'Result of add dir to brideout with illegal dirtype is ' + \
          str(test.addDirectory('brideout', 'anotherDirectory', 'medium'))

    madFilter = madrigal.ui.userData.MadrigalFilter('newFilter', 15, 16, 17, 18)
    madFilter.minalt = 100
    madFilter.maxel2 = 50.345
    madFilter.flkdat = 1008
    madFilter.parmlist = '10 100 -700'
    madFilter.mnemStr1 = 'gdalt - sdwht'

    print madFilter.toString()

    print 'Result of adding filter newFilter to user brideout, dirname brideout_public is ' + \
          str(test.addFilter('brideout', 'brideout_public', madFilter))

    """

    print 'Result of removing filter newFilter from user brideout, dirname brideout_private is ' + \
          str(test.removeFilter('brideout', 'brideout_private', 'newFilter'))

    print 'Result of removing non-existant filter oldFilter from user brideout, dirname brideout_private is ' + \
          str(test.removeFilter('brideout', 'brideout_private', 'oldFilter'))

    print 'Result of removing  filter newFilter from user brideout, wrong dirname brideout_public is ' + \
          str(test.removeFilter('brideout', 'brideout_public', 'newFilter'))

    print 'Result of removing filter newFilter from user brideout, dirname brideout_private is ' + \
          str(test.removeFilter('brideout', 'brideout_private', 'newFilter'))

    print 'Result of remove dir newDirectory from brideout is ' + str(test.removeDirectory('brideout', 'newDirectory'))

    print 'Result of remove dir brideout_public (not empty) from brideout is: ' + str(test.removeDirectory('brideout', 'brideout_public'))

    testFilt = test.getFilter('brideout', 'brideout_public', 'standard')

    print 'The following is the filter standard, from brideout_public:'
    print testFilt.toString()

    print 'Now printing public dir name list'
    print test.getPublicDirNameList()

    print 'Printing list of filters in dir brideout_private, user brideout:'
    print test.getFilterNameList('brideout', 'brideout_private')

    print 'Printing list of private directories for user brideout:'
    print test.getPrivateDirNameList('brideout')


    #test MadrigalFilter
    test = MadrigalFilter('Bills filter', 1997, 12, 31, 1998, 1, 15, 10, 23, 24, 11, 20, None, 0, 2000)

    test.flkinst = 31

    test.header = 'checked'

    test.badval = -10000

    print 'The name is: ' + str(test.name)

    print 'The endhour is: ' + str(test.endhour)

    print 'The endsec is: ' + str(test.endsec)

    test.parmlist = '120 550'

    print 'The parmlist is: ' + str(test.parmlist)

    print test.toString()

    """
