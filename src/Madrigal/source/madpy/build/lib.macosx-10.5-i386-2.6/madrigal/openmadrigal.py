"""The openmadrigal module provides access to all OpenMadrigal installations via http and to OpenMadrigal CVS.

$Id: openmadrigal.py,v 1.6 2009/01/30 16:48:04 brideout Exp $
"""


import os, os.path, sys
import traceback
import urllib2
import re


import madrigal.admin
import madrigal.metadata


class OpenMadrigal:
    """OpenMadrigal is an object that provides access to all Open Madrigal installations via http and to OpenMadrigal CVS.


    Usage example::

        import madrigal.openmadrigal
    
        try:
        
            test =  madrigal.openmadrigal.OpenMadrigal()

            # get the metadata file fileTab.txt from Madrigal site with id = 1

            test.getFileTab(1)
            
        except madrigal.admin.MadrigalError, e:
        
            print e.getExceptionStr()
            


    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Aug. 14, 2002

    """


    #constants
    __webProtocol  = "http"
    """ Change the above string to use another web protocol such as https. """
    __openMadrigalUrl = 'http://www.haystack.mit.edu/madrigal/distributionFiles/'

    # the following constants are used by cgi script getMetadata
    __expTab     = 0
    __fileTab    = 1
    __dataTab    = 2
    __instTab    = 3
    __parcods    = 4
    __siteTab    = 5
    __typeTab    = 6
    __instKindat = 7
    __instParm   = 8
    __madCat     = 9
    __instType   = 10

    # the url to the cvs madrigal repository
    __cvsUrl  = 'http://www.haystack.mit.edu/cgi-bin/madrigal_viewcvs.cgi/'



    def __init__(self, madDB=None):
        """__init__ initializes OpenMadrigal by setting or creating a MadrigalDB object.

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: None.
        """
        
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        # create the needed MadrigalSite object
        self.__madSite = madrigal.metadata.MadrigalSite(self.__madDB)


    def __getMetadata(self, siteId, metadataType):
        """ __getMetadata is a private helper function called to get metadata files via the web.
        
        Inputs: siteId - integer identifying Madrigal site.
            metadataType - constant defined by getMetadata cgi script
        
        Returns: The desired metadata file as a string, or None if not successful

        Affects: None.

        Exceptions: None.
        """
        # get site server name
        serverName = self.__madSite.getSiteServer(siteId)
        if serverName == None:
            return None

        # get site relative cgi path
        relativeCgi = self.__madSite.getSiteRelativeCGI(siteId)
        if relativeCgi == None:
            return None

        # create url string
        urlStr = self.__webProtocol + '://' + serverName + '/' + \
                 relativeCgi + '/getMetadata?fileType=' + str(metadataType)
        
        try:
            file = urllib2.urlopen(urlStr)
            fileStr = file.read()
            return fileStr
        
        except:
            return None

    

    # public methods

    def getMetadataFromOpenMadrigal(self, filename):
        """getMetadataFromOpenMadrigal returns a metadata file from OpenMadrigal server
        as a string

        
        Inputs:

            filename - metadata file to download, relative to metadata
        
        Returns: File contents as a string

        Affects: Nothing
        """
        metadataDir = 'metadata'

        url = os.path.join(self.__openMadrigalUrl, metadataDir, filename)
        f = urllib2.urlopen(url)
        result = f.read()
        f.close()
        return(result)

        
    

    def getExpMetadata(self, siteId):
        """getExpMetadata returns the expTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the expTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__expTab)

        return fileStr


    def getFileMetadata(self, siteId):
        """getFileMetadata returns the fileTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the fileTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__fileTab)

        return fileStr


    def getDataMetadata(self, siteId):
        """getDataMetadata returns the dataTab.txt file from siteId as a string.

        This file is deprecated with Madrigal 2.5 and may not exist.

        
        Inputs: None
        
        Returns: the dataTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__dataTab)

        return fileStr


    def getInstMetadata(self, siteId):
        """getInstMetadata returns the instTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the instTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__instTab)

        return fileStr


    def getParcodsMetadata(self, siteId):
        """getParcodsMetadata returns the parcods.tab file from siteId as a string.

        
        Inputs: None
        
        Returns: the parcods.tab file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__parcods)

        return fileStr


    def getSiteMetadata(self, siteId):
        """getSiteMetadata returns the siteTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the siteTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__siteTab)

        return fileStr


    def getTypeMetadata(self, siteId):
        """getTypeMetadata returns the typeTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the typeTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__typeTab)

        return fileStr


    def getMadCatMetadata(self, siteId):
        """getMadCatMetadata returns the madCatTab.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the madCatTab.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__madCat)

        return fileStr


    def getInstTypeMetadata(self, siteId):
        """getInstTypeMetadata returns the instType.txt file from siteId as a string.

        
        Inputs: None
        
        Returns: the instType.txt file from siteId as a string, or None if not found

        Affects: Nothing

        Exceptions: None
        """

        fileStr = self.__getMetadata(siteId, self.__instType)

        return fileStr


    def getLatestCvsVersion(self, fullPath):
        """getLatestCvsVersion returns the latest cvs version of the file given by fullPath as a string.

        Inputs: fullPath - full path to the Madrigal file in cvs
        (example: 'madroot/metadata/siteTab.txt')
        
        Returns: the latest cvs version of the file given by fullPath as a string,
        or None if not found

        Affects: Nothing

        Exceptions: None
        """
        try:
            # the first step is to find the latest revision from the selection page
            urlStr = self.__cvsUrl + fullPath

            f = urllib2.urlopen(urlStr)
            fileStr = f.read()
            f.close()

            # find first occurance of string: '<a name="rev'
            firstOcc = fileStr.find('<a name="rev')
            if firstOcc == -1:
                return None
            else:
                firstOcc += len('<a name="rev')

            # next find the next ", what's in between will be the revision number
            lastOcc = fileStr.find('"', firstOcc)
            if lastOcc == -1:
                return None

            revStr = fileStr[firstOcc:lastOcc]

            # now with the revision number, create final url
            finalUrl = urlStr + '?rev=' + revStr + '&content-type=text/plain'
            finalFile = urllib2.urlopen(finalUrl)
            text = finalFile.read()
            finalFile.close()
            return(text)


        except:
            return None


    def getAllRevisionNumbers(self, fullPath):
        """getAllRevisionNumbers a list of all revision numbers for a given file in CVS in
        order from latest to earliest.

        Inputs: fullPath - full path to the Madrigal file in cvs
        (example: 'madroot/metadata/siteTab.txt')
        
        Returns: a list of all revision numbers for a given file in CVS in
        order from latest to earliest.  Empty list if file not found

        Affects: Nothing

        Exceptions: None
        """
        try:
            # the first step is to read the the selection page
            urlStr = self.__cvsUrl + fullPath

            f = urllib2.urlopen(urlStr)
            fileStr = f.read()
            f.close()

            # find all occurances of string: '<a name="rev'
            reStr = r'<a name="rev[0-9]+\.[0-9]+">'
            reList = re.findall(reStr, fileStr)

            retList = []
            for item in reList:
                retList.append(item[12:-2])

            return(retList)

        except:
            return []


    def getCvsVersion(self, fullPath, revision):
        """getCvsVersion returns the a cvs version of the file given by fullPath and revision.

        Inputs:

            fullPath - full path to the Madrigal file in cvs
            (example: 'madroot/metadata/siteTab.txt')

            revision - revision string (example '1.45')
        
        Returns: the cvs version of the file given by fullPath and revision,
        or None if not found

        Affects: Nothing

        Exceptions: None
        """
        try:
            urlStr = self.__cvsUrl + fullPath

            # now with the revision number, create final url
            finalUrl = urlStr + '?rev=' + revision + '&content-type=text/plain'
            finalFile = urllib2.urlopen(finalUrl)
            text = finalFile.read()
            finalFile.close()
            return(text)

        except:
            return None

        
        

if __name__ == '__main__':

    test = OpenMadrigal()

    print 'This is expTab.txt:'
    
    print test.getExpMetadata(54) + '\n'

    print 'This is fileTab.txt:'
    
    print test.getFileMetadata(54) + '\n'

    print 'This is dataTab.txt:'
    
    print test.getDataMetadata(54) + '\n'

    print 'This is instTab.txt:'
    
    print test.getInstMetadata(54) + '\n'

    print 'This is parcods.tab:'
    
    print test.getParcodsMetadata(54) + '\n'

    print 'This is siteTab.txt:'
    
    print test.getSiteMetadata(54) + '\n'

    print 'This is typeTab.txt:'
    
    print test.getTypeMetadata(54) + '\n'

    print test.getLatestCvsVersion('madroot/metadata/siteTab.txt')
