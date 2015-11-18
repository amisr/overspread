"""The madrigalWeb module provides access to all Madrigal data via web services.

"""

# $Id: madrigalWeb.py,v 1.24 2008/11/06 17:19:22 brideout Exp $


import os, os.path, sys
import traceback
import urllib2
import types
import re
import urlparse
import datetime


class MadrigalData:
    """MadrigalData is a class that aquires data from a particular Madrigal site.


    Usage example::

        import madrigalWeb.madrigalWeb
    
        test =  madrigalWeb.madrigalWeb.MadrigalData('http://www.haystack.mit.edu/madrigal')

        instList = test.getInstrumentList()
            


    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 10, 2004

    """
    def __init__(self, url):
        """__init__ initializes a MadrigalData object.

        Inputs::

            url - (string) url of main page of madrigal site. Example: 'http://www.haystack.mit.edu/madrigal'

        Affects: Converts main page to cgi url, and stores that.

        Also stores self.siteDict, with key = site id, value= site url, and self.siteId

        Exceptions: If url not found.
        """
        cgiName = 'accessData.cgi'

        regExp = re.compile('".*' + cgiName)

        # get base of url
        urlParts = urlparse.urlparse(url)

        urlBase = urlParts[0] + '://' + urlParts[1]

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)

        page = mainUrl.read()

        mainUrl.close()

        result = regExp.search(page)

        # check for success
        if result == None:
            raise ValueError, 'invalid url: ' + str(url)

        result = result.group()

        if type(result) != types.StringType:
            result = result[0]

        self.cgiurl = urlBase + result[1:(-1*len(cgiName))]

        self.siteDict = self.__getSiteDict()

        self.siteId = self.__getSiteId()


    def __getSiteDict(self):
        """__getSiteDict returns a dictionary with key = site id, value= site url.

        Uses getMetadata cgi script
        """
        url = os.path.join(self.cgiurl, 'getMetadata?fileType=5')

        f = urllib2.urlopen(url)

        page = f.read()

        f.close()

        lines = page.split('\n')

        siteDict = {}

        for line in lines:
            items = line.split(',')
            if len(items) < 4:
                continue
            site = int(items[0])
            thisUrl = 'http://%s/%s' % (items[2], items[3])
            siteDict[site] = thisUrl

        return siteDict


    def __getSiteId(self):
        """__getSiteId returns the local site id

        Uses getMetadata cgi script
        """
        url = os.path.join(self.cgiurl, 'getMetadata?fileType=0')

        f = urllib2.urlopen(url)

        page = f.read()

        f.close()

        lines = page.split('\n')

        for line in lines:
            items = line.split(',')
            if len(items) < 4:
                continue
            siteId = int(items[3])
            return(siteId)

        raise IOError, 'No siteId found'
            
        



    def getAllInstruments(self):
        """ returns a list of all MadrigalInstruments at the given Madrigal site"""

        scriptName = 'getInstrumentsService.py'

        url = self.cgiurl + scriptName

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)

        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            raise ValueError, 'No data found at url' + str(url)

        # check that html was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []

        for line in page:
            items = line.split(',')
            result.append(MadrigalInstrument(items[0],
                                             items[1],
                                             items[2],
                                             items[3],
                                             items[4],
                                             items[5]))

        return result


    
    def getExperiments(self,
                       code,
                       startyear,
                       startmonth,
                       startday,
                       starthour,
                       startmin,
                       startsec,
                       endyear,
                       endmonth,
                       endday,
                       endhour,
                       endmin,
                       endsec,
                       local=1):
        """ returns a list of all MadrigalExperiments that meet criteria at the given Madrigal site

        Inputs:

           code - int or list of ints representing instrument code(s). Special value of 0 selects all instruments.
           
           startyear - int or string convertable to int
           
           startmonth - int or string convertable to int
           
           startday - int or string convertable to int
           
           starthour - int or string convertable to int
           
           startmin - int or string convertable to int
           
           startsec - int or string convertable to int
           
           endyear - int or string convertable to int
           
           endmonth - int or string convertable to int
           
           endday - int or string convertable to int
           
           endhour - int or string convertable to int
           
           endmin - int or string convertable to int
           
           endsec - int or string convertable to int

           local - 0 if all sites desired, 1 (default) if only local experiments desired

        Outputs:

            List of MadrigalExperiment objects that meet the criteria


        """

        scriptName = 'getExperimentsService.py'

        url = self.cgiurl + scriptName + '?'

        # first append code(s)
        if type(code) == types.ListType:
            for item in code:
                url += 'code=%i&' % (int(item))
        else:
            url += 'code=%i&' % (int(code))

        # append times
        url += 'startyear=%i&' % (int(startyear))
        url += 'startmonth=%i&' % (int(startmonth))
        url += 'startday=%i&' % (int(startday))
        url += 'starthour=%i&' % (int(starthour))
        url += 'startmin=%i&' % (int(startmin))
        url += 'startsec=%i&' % (int(startsec))
        url += 'endyear=%i&' % (int(endyear))
        url += 'endmonth=%i&' % (int(endmonth))
        url += 'endday=%i&' % (int(endday))
        url += 'endhour=%i&' % (int(endhour))
        url += 'endmin=%i&' % (int(endmin))
        url += 'endsec=%i&' % (int(endsec))
        url += 'local=%i'% (int(local))
        


        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                

        

        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            return []

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []


        for line in page:
            items = line.split(',')
            # calculate isLocal
            if int(items[3]) == self.siteId:
                isLocal = True
            else:
                isLocal = False
            result.append(MadrigalExperiment(items[0],
                                             items[1],
                                             items[2],
                                             items[3],
                                             items[4],
                                             items[5],
                                             items[6],
                                             items[7],
                                             items[8],
                                             items[9],
                                             items[10],
                                             items[11],
                                             items[12],
                                             items[13],
                                             items[14],
                                             items[15],
                                             items[16],
                                             items[17],
                                             items[18],
                                             isLocal,
                                             self.siteDict[int(items[3])]))
            

        return result


    def getExperimentFiles(self, id, getNonDefault=False):
        """ returns a list of all default MadrigalExperimentFiles for a given experiment id

        Inputs:

           id - Experiment id.

           getNonDefault - if False (the default), only get default files, or realtime
                           files if no default files found.  If True, get all files

        Outputs:

            List of MadrigalExperimentFile objects for that experiment id


        """

        scriptName = 'getExperimentFilesService.py'

        url = self.cgiurl + scriptName + '?id=%i' % (int(id))


        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                

        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            return []

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []

        # find out if no default files.  If so, return realtime also
        hasDefault = False
        for line in page:
            items = line.split(',')
            if int(items[3]) == 1:
                hasDefault = True
                break

        for line in page:
            items = line.split(',')
            category = int(items[3])
            if hasDefault and category != 1 and not getNonDefault:
                continue
            if not hasDefault and category != 4 and not getNonDefault:
                continue
            result.append(MadrigalExperimentFile(items[0],
                                                 items[1],
                                                 items[2],
                                                 items[3],
                                                 items[4],
                                                 items[5],
                                                 id))
            

        return result


    def getExperimentFileParameters(self,fullFilename):
        """ getExperimentFileParameters returns a list of all measured and derivable parameters in file

        Inputs:

           fullFilename - full path to experiment file as returned by getExperimentFiles.

        Outputs:

            List of MadrigalParameter objects for that fullFilename.  Includes both measured
            and derivable parameters in file.


        """

        scriptName = 'getParametersService.py'

        url = self.cgiurl + scriptName + '?filename=%s' % (str(fullFilename))


        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                

        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            return []

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []



        for line in page:
            items = line.split('\\')
            # with Madrigal 2.5, isAddIncrement was added as 8th column
            try:
                isAddIncrement = int(items[7])
            except:
                isAddIncrement = -1
            result.append(MadrigalParameter(items[0],
                                            items[1],
                                            int(items[2]),
                                            items[3],
                                            int(items[4]),
                                            items[5],
                                            int(items[6]),
                                            isAddIncrement))
            

        return result


    def simplePrint(self, filename, user_fullname, user_email, user_affiliation):
        """simplePrint prints the data in the given file is a simple ascii format.

        simplePrint prints only the parameters in the file, without filters or derived
        parameters.  To choose which parameters to print, to print derived parameters, or
        to filter the data, use isprint instead.

        Inputs:

            filename - The absolute filename to be printed.  Returned by getExperimentFiles.

            user_fullname - full name of user making request

            user_email - email address of user making request

            user_affiliation - affiliation of user making request

        Returns: string representing all data in the file in ascii, space-delimited form.
                 The first line if the list of parameters printed.  The first six parameters will
                 always be year, month, day, hour, min, sec, representing the middle time of
                 the measurment.
        """
        parms = self.getExperimentFileParameters(filename)

        parmStr = 'year,month,day,hour,min,sec'
        labelStr = 'YEAR     MONTH       DAY      HOUR       MIN       SEC        '

        for parm in parms:
            if parm.isMeasured and parm.isAddIncrement != 1:
                parmStr += ',%s' % (parm.mnemonic)
                thisLabel = parm.mnemonic[:11].upper()
                labelStr += '%s%s' % (thisLabel, ' '*(11-len(thisLabel)))

        retStr = '%s\n' % (labelStr)

        retStr += self.isprint(filename, parmStr, '', user_fullname, user_email, user_affiliation)

        return(retStr)

        

    def isprint(self, file, parms, filters, user_fullname, user_email, user_affiliation):
        """returns as a string the isprint output given file, parms, filters without headers or summary.

        Inputs:

            file - The absolute filename to be analyzed by isprint.

            parms - Comma delimited string listing requested parameters (no spaces allowed).

            filters - Space delimited string listing filters desired, as in isprint command

            user_fullname - full name of user making request

            user_email - email address of user making request

            user_affiliation - affiliation of user making request

        Returns:
        
            a string holding the isprint output
        """
        
        scriptName = 'isprintService.py'

        # build the complete cgi string, replacing characters as required by cgi standard

        url = self.cgiurl + scriptName + '?'

        
        url += 'file=%s&' % (file.replace('/', '%2F'))
        parms = parms.replace('+','%2B')
        parms = parms.replace(',','+')
        url += 'parms=%s&' % (parms)
        filters = filters.replace('=','%3D')
        filters = filters.replace(',','%2C')
        filters = filters.replace('/','%2F')
        filters = filters.replace('+','%2B')
        filters = filters.replace(' ','+')
        url += 'filters=%s&' % (filters)
        user_fullname = user_fullname.replace(' ','+').strip()
        url += 'user_fullname=%s&' % (user_fullname)
        user_email = user_email.strip()
        url += 'user_email=%s&' % (user_email)
        user_affiliation = user_affiliation.replace(' ','+').strip()
        url += 'user_affiliation=%s' % (user_affiliation)

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                

        page = mainUrl.read()

        mainUrl.close()

        if page.find('Error occurred') != -1:
            raise ValueError, 'error raised using url ' + str(url)

        return page


    def madCalculator(self,
                      year,
                      month,
                      day,
                      hour,
                      min,
                      sec,
                      startLat,
                      endLat,
                      stepLat,
                      startLong,
                      endLong,
                      stepLong,
                      startAlt,
                      endAlt,
                      stepAlt,
                      parms,
                      oneDParmList=[],
                      oneDParmValues=[]):
        """

        Input arguments:

            1. year - int 

            2. month - int 

            3. day - int
            
            4. hour - int 

            5. min - int 

            6. sec - int 

            7. startLat - Starting geodetic latitude, -90 to 90 (float)

            8. endLat - Ending geodetic latitude, -90 to 90 (float)

            9. stepLat - Latitude step (0.1 to 90) (float)

            10. startLong - Starting geodetic longitude, -180 to 180  (float)

            11. endLong - Ending geodetic longitude, -180 to 180 (float)

            12. stepLong - Longitude step (0.1 to 180) (float)

            13. startAlt - Starting geodetic altitude, >= 0 (float)

            14. endAlt - Ending geodetic altitude, > 0 (float)

            15. stepAlt - Altitude step (>= 0.1) (float)

            16. parms - comma delimited string of Madrigal parameters desired

            17. oneDParmList - a list of one-D parameters whose values should
                               be set for the calculation.  Can be codes or mnemonics.
                               Defaults to empty list.

            18. oneDParmValues - a list of values (doubles) associated with the one-D
                                 parameters specified in oneDParmList. Defaults to empty list.

        Returns:

            A list of lists of doubles, where each list contains 3 + number of parameters doubles.
            The first three doubles are the input latitude, longitude, and altitude.  The rest of the
            doubles are the values of each of the calculated values.  If the value cannot be calculated,
            it will be set to nan.

            Example:

                result = testData.madCalculator(1999,2,15,12,30,0,45,55,5,-170,-150,10,200,200,0,'bmag,bn')

                result = [  [45.0, -170.0, 200.0, 4.1315700000000002e-05, 2.1013500000000001e-05]
                            [45.0, -160.0, 200.0, 4.2336899999999998e-05, 2.03685e-05]
                            [45.0, -150.0, 200.0, 4.3856400000000002e-05, 1.97411e-05]
                            [50.0, -170.0, 200.0, 4.3913599999999999e-05, 1.9639999999999998e-05]
                            [50.0, -160.0, 200.0, 4.4890099999999999e-05, 1.8870999999999999e-05]
                            [50.0, -150.0, 200.0, 4.6337800000000002e-05, 1.80077e-05]
                            [55.0, -170.0, 200.0, 4.6397899999999998e-05, 1.78115e-05]
                            [55.0, -160.0, 200.0, 4.7265400000000003e-05, 1.6932500000000001e-05]
                            [55.0, -150.0, 200.0, 4.85495e-05,            1.5865399999999999e-05] ]

                Columns:     gdlat  glon    gdalt  bmag                    bn

	"""

        scriptName = 'madCalculatorService.py'

        url = self.cgiurl + scriptName + '?year'

        if len(oneDParmList) != len(oneDParmValues):
            raise ValueError, 'len(oneDParmList) != len(oneDParmValues)'

        # append arguments
        url += '=%i&month' % (int(year))
        url += '=%i&day' % (int(month))
        url += '=%i&hour' % (int(day))
        url += '=%i&min' % (int(hour))
        url += '=%i&sec' % (int(min))
        url += '=%i&startLat' % (int(sec))
        url += '=%f&endLat' % (float(startLat))
        url += '=%f&stepLat' % (float(endLat))
        url += '=%f&startLong' % (float(stepLat))
        url += '=%f&endLong' % (float(startLong))
        url += '=%f&stepLong' % (float(endLong))
        url += '=%f&startAlt' % (float(stepLong))
        url += '=%f&endAlt' % (float(startAlt))
        url += '=%f&stepAlt' % (float(endAlt))
        url += '=%f&parms' % (float(stepAlt))
        url += '=%s' % (parms)

        for i in range(len(oneDParmList)):
            url += '&oneD=%s,%s' % (str(oneDParmList[i]), str(oneDParmValues[i]))

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                

        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            raise ValueError, 'No data found at url' + str(url)

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []

        # parse output
        for line in page:
            items = line.split()
            if len(items) < 3:
                # blank line
                continue
            newList = []
            for item in items:
                try:
                    newList.append(float(item))
                except:
                    newList.append(str(item))
            result.append(newList)

        return result


    def madTimeCalculator(self,
                          startyear,
                          startmonth,
                          startday,
                          starthour,
                          startmin,
                          startsec,
                          endyear,
                          endmonth,
                          endday,
                          endhour,
                          endmin,
                          endsec,
                          stephours,
                          parms):
        """

        Input arguments:

            1. startyear - int 

            2. startmonth - int 

            3. startday - int
            
            4. starthour - int 

            5. startmin - int 

            6. startsec - int

            7. endyear - int 

            8. endmonth - int 

            9. endday - int
            
            10. endhour - int 

            11. endmin - int 

            12. endsec - int

            13. stephours - float - number of hours per time step

            16. parms - comma delimited string of Madrigal parameters desired (must not depend on location)

        Returns:

            A list of lists, where each list contains 6 ints (year, month, day, hour, min, sec)  + number
            of parameters.  If the value cannot be calculated, it will be set to nan.

            Example:

                result = testData.madTestCalculator(1999,2,15,12,30,0,
                                                    1999,2,20,12,30,0,
                                                    24.0, 'kp,dst')

                result = [[1999.0, 2.0, 15.0, 12.0, 30.0, 0.0, 3.0, -9.0]
                          [1999.0, 2.0, 16.0, 12.0, 30.0, 0.0, 1.0, -6.0]
                          [1999.0, 2.0, 17.0, 12.0, 30.0, 0.0, 4.0, -31.0]
                          [1999.0, 2.0, 18.0, 12.0, 30.0, 0.0, 6.7000000000000002, -93.0]
                          [1999.0, 2.0, 19.0, 12.0, 30.0, 0.0, 5.2999999999999998, -75.0]]

                Columns:     year, month, day, hour, min, sec, kp, dst

	"""

        scriptName = 'madTimeCalculatorService.py'

        url = self.cgiurl + scriptName + '?startyear'

        # append arguments
        url += '=%i&startmonth' % (int(startyear))
        url += '=%i&startday' % (int(startmonth))
        url += '=%i&starthour' % (int(startday))
        url += '=%i&startmin' % (int(starthour))
        url += '=%i&startsec' % (int(startmin))
        url += '=%i&endyear' % (int(startsec))
        url += '=%i&endmonth' % (int(endyear))
        url += '=%i&endday' % (int(endmonth))
        url += '=%i&endhour' % (int(endday))
        url += '=%i&endmin' % (int(endhour))
        url += '=%i&endsec' % (int(endmin))
        url += '=%i&stephours' % (int(endsec))
        url += '=%f&parms' % (float(stephours))
        url += '=%s' % (parms)

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                

        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            raise ValueError, 'No data found at url' + str(url)

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []

        # parse output
        for line in page:
            items = line.split()
            if len(items) < 3:
                # blank line
                continue
            newList = []
            for item in items:
                try:
                    newList.append(float(item))
                except:
                    newList.append(str(item))
            result.append(newList)

        return result



    def radarToGeodetic(self,
                        slatgd,
                        slon,
                        saltgd,
                        az,
                        el,
                        radarRange):
        """radarToGeodetic converts arrays of az, el, and ranges to geodetic locations.

        Input arguments:

            1. slatgd - radar geodetic latitude 

            2. slon - radar longitude 

            3. saltgd - radar altitude
            
            4. az - either a single azimuth, or a list of azimuths 

            5. el - either a single elevation, or a list of elevations.  If so, len(el)
                    must = len(az)

            6. radarRange - either a single range, or a list of ranges.  If so, len(radarRange)
                            must = len(az)


        Returns:

            A list of lists, where each list contains 3 floats (gdlat, glon, and gdalt)
        """
        scriptName = 'radarToGeodeticService.py'

        url = self.cgiurl + scriptName + '?slatgd'

        # append arguments
        url += '=%f&slon' % (float(slatgd))
        url += '=%f&saltgd' % (float(slon))
        url += '=%f&' % (float(saltgd))

        if type(az) == types.ListType or type(az) == types.TupleType:
            if len(az) != len(el) or len(az) != len(radarRange):
                raise ValueError, 'all lists most have same length'
            for i in range(len(az)):
                if i == 0:
                    arg = str(az[i])
                else:
                    arg += ',' + str(az[i])
            url += 'az=%s&' % (arg)

            for i in range(len(el)):
                if i == 0:
                    arg = str(el[i])
                else:
                    arg += ',' + str(el[i])
            url += 'el=%s&' % (arg)

            for i in range(len(radarRange)):
                if i == 0:
                    arg = str(radarRange[i])
                else:
                    arg += ',' + str(radarRange[i])
            url += 'range=%s' % (arg)

        else:
            url += 'az=%f&' % (az)
            url += 'el=%f&' % (el)
            url += 'range=%f&' % (radarRange)

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                
        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            raise ValueError, 'No data found at url' + str(url)

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []

        # parse output
        for line in page:
            items = line.split(',')
            if len(items) < 3:
                # blank line
                continue
            newList = []
            for item in items:
                try:
                    newList.append(float(item))
                except:
                    newList.append(str(item))
            result.append(newList)

        return result


    def geodeticToRadar(self,
                        slatgd,
                        slon,
                        saltgd,
                        gdlat,
                        glon,
                        gdalt):
        """geodeticToRadar converts arrays of points in space to az, el, and range.

        Input arguments:

            1. slatgd - radar geodetic latitude 

            2. slon - radar longitude 

            3. saltgd - radar altitude
            
            4. gdlat - either a single geodetic latitude, or a list of geodetic latitudes 

            5. glon - either a single longitude, or a list of longitudes.  If so, len(gdlat)
                      must = len(glon)

            6. gdalt - either a single deodetic altitude, or a list of geodetic altitudes.
                       If so, len(gdalt) must = len(gdlat)


        Returns:

            A list of lists, where each list contains 3 floats (az, el, and range)
        """
        scriptName = 'geodeticToRadarService.py'

        url = self.cgiurl + scriptName + '?slatgd'

        # append arguments
        url += '=%f&slon' % (float(slatgd))
        url += '=%f&saltgd' % (float(slon))
        url += '=%f&' % (float(saltgd))

        if type(gdlat) == types.ListType or type(gdlat) == types.TupleType:
            if len(gdlat) != len(glon) or len(gdlat) != len(gdalt):
                raise ValueError, 'all lists most have same length'
            for i in range(len(gdlat)):
                if i == 0:
                    arg = str(gdlat[i])
                else:
                    arg += ',' + str(gdlat[i])
            url += 'gdlat=%s&' % (arg)

            for i in range(len(glon)):
                if i == 0:
                    arg = str(glon[i])
                else:
                    arg += ',' + str(glon[i])
            url += 'glon=%s&' % (arg)

            for i in range(len(gdalt)):
                if i == 0:
                    arg = str(gdalt[i])
                else:
                    arg += ',' + str(gdalt[i])
            url += 'gdalt=%s' % (arg)

        else:
            url += 'gdlat=%f&' % (gdlat)
            url += 'glon=%f&' % (glon)
            url += 'gdalt=%f&' % (gdalt)

        # read main url
        try:
            mainUrl = urllib2.urlopen(url)
        except:
            raise ValueError, 'unable to open url ' + str(url)
                
        page = mainUrl.readlines()

        mainUrl.close()

        # parse the result
        if len(page) == 0:
            raise ValueError, 'No data found at url' + str(url)

        # check that error was not returned
        for line in page:
            if line.find('Error occurred') != -1:
                raise ValueError, 'error raised using url ' + str(url) + ' ' + str(page)

        result = []

        # parse output
        for line in page:
            items = line.split(',')
            if len(items) < 3:
                # blank line
                continue
            newList = []
            for item in items:
                try:
                    newList.append(float(item))
                except:
                    newList.append(str(item))
            result.append(newList)

        return result


    def downloadFile(self, filename, destination, format='madrigal'):
        """downloadFile will download a Cedar file in the specified format.

        Inputs:

            filename - The absolute filename to as returned via getExperimentFiles.

            destination - where the file is to be stored

            format - file format desired.  May be 'madrigal', 'blockedBinary', 'ncar',
                     'unblockedBinary', 'ascii'.  Default is 'madrigal'

        """
        fileType = 0
        if format not in ('madrigal', 'blockedBinary', 'ncar', 'unblockedBinary', 'ascii'):
            raise ValueError, 'Illegal format specified: %s' % (str(format))
        if format == 'blockedBinary':
            fileType = 1
        elif format == 'ncar':
            fileType = 2
        elif format == 'unblockedBinary':
            fileType = 3
        else:
            fileType = 4



        
        url = os.path.join(self.cgiurl,'getMadfile.cgi?fileName=%s&fileType=%i' % (filename, fileType))

        urlFile = urllib2.urlopen(url)

        data = urlFile.read()

        urlFile.close()

        f = open(destination, 'w')

        f.write(data)

        f.close()

        
    

class MadrigalInstrument:
    """MadrigalInstrument is a class that encapsulates information about a Madrigal Instrument.


    Attributes::

      name (string) Example: 'Millstone Hill Incoherent Scatter Radar'
      
      code (int) Example: 30
      
      mnemonic (3 char string) Example: 'mlh'
      
      latitude (double) Example: 45.0
      
      longitude (double) Example: 110.0
      
      altitude (double)  Example: 0.015 (km)
            


    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 10, 2004

    """
    def __init__(self, name, code, mnemonic, latitude, longitude, altitude):
        """__init__ initializes a MadrigalInstrument.

        Inputs::

            name - (string) Example: 'Millstone Hill Incoherent Scatter Radar'
          
            code - (int, or string that can be converted) Example: 30
          
            mnemonic - (3 char string) Example: 'mlh'
          
            latitude - (double, or string that can be converted) Example: 45.0
          
            longitude (double, or string that can be converted) Example: 110.0
          
            altitude (double, or string that can be converted)  Example: 0.015 (km)
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: If illegal argument passed in.
        """

        if type(name) != types.StringType:
            raise ValueError, 'In MadrigalInstrument, name not string type: %s' % (str(name))

        self.name = name

        self.code = int(code)

        if type(mnemonic) != types.StringType:
            raise ValueError, 'In MadrigalInstrument, mnemonic not string type: %s' % (str(mnemonic))

        if len(mnemonic) != 3:
            raise ValueError, 'In MadrigalInstrument, mnemonic not three characters: %s' % (str(mnemonic))

        self.mnemonic = mnemonic.lower()

        self.latitude = float(latitude)

        self.longitude = float(longitude)

        self.altitude = float(altitude)

    def __str__(self):
        """return a readible form of this object"""
        retStr = ''
        retStr += 'name: %s\n' % (str(self.name))
        retStr += 'code: %s\n' % (str(self.code))
        retStr += 'mnemonic: %s\n' % (str(self.mnemonic))
        retStr += 'latitude: %s\n' % (str(self.latitude))
        retStr += 'longitude: %s\n' % (str(self.longitude))
        retStr += 'altitude: %s\n'%  (str(self.altitude))
        return retStr



class MadrigalExperiment:
    """MadrigalExperiment is a class that encapsulates information about a Madrigal Experiment.


    Attributes::

        id (int) Example: 10000111.  Uniquely identifies experiment.
        
        url (string) Example: 'http://www.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97'
        
        name (string) Example: 'Wide Latitude Substorm Study'
        
        siteid (int) Example: 1
        
        sitename (string) Example: 'Millstone Hill Observatory'
        
        instcode (int) Code of instrument. Example: 30
        
        instname (string) Instrument name. Example: 'Millstone Hill Incoherent Scatter Radar'
        
        startyear - int
           
        startmonth - int
       
        startday - int
       
        starthour - int
       
        startmin - int
       
        startsec - int
       
        endyear - int
       
        endmonth - int
       
        endday - int
       
        endhour - int
       
        endmin - int
       
        endsec - int

        isLocal - True if a local experiment, False if not

        madrigalUrl - url of Madrigal site.  Used if not a local experiment.
            

    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 10, 2004

    """
    def __init__(self,
                 id,
                 url,
                 name,
                 siteid,
                 sitename,
                 instcode,
                 instname,
                 startyear,
                 startmonth,
                 startday,
                 starthour,
                 startmin,
                 startsec,
                 endyear,
                 endmonth,
                 endday,
                 endhour,
                 endmin,
                 endsec,
                 isLocal,
                 madrigalUrl):
        """__init__ initializes a MadrigalExperiment.

        Inputs::

            id (int, or string that can be converted) Example: 10000111.  Uniquely identifies experiment.
        
            url (string) Example: 'http://www.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97'
            
            name (string) Example: 'Wide Latitude Substorm Study'
            
            siteid (int, or string that can be converted) Example: 1
            
            sitename (string) Example: 'Millstone Hill Observatory'
            
            instcode (int, or string that can be converted) Code of instrument. Example: 30
            
            instname (string) Instrument name. Example: 'Millstone Hill Incoherent Scatter Radar'

            startyear - int, or string that can be converted
           
            startmonth - int, or string that can be converted
       
            startday - int, or string that can be converted
       
            starthour - int, or string that can be converted
       
            startmin - int, or string that can be converted
       
            startsec - int, or string that can be converted
       
            endyear - int, or string that can be converted
       
            endmonth - int, or string that can be converted
       
            endday - int, or string that can be converted
       
            endhour - int, or string that can be converted
       
            endmin - int, or string that can be converted
       
            endsec - int, or string that can be converted
            
            isLocal - True if a local experiment, False if not

            madrigalUrl - url of Madrigal site.  Used if not a local experiment.
        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: If illegal argument passed in.
        """

        self.id = int(id)

        if type(url) != types.StringType:
            raise ValueError, 'In MadrigalExperiment, url not string type: %s' % (str(url))

        self.url = url

        if type(name) != types.StringType:
            raise ValueError, 'In MadrigalExperiment, name not string type: %s' % (str(name))

        self.name = name

        self.siteid = int(siteid)

        if type(sitename) != types.StringType:
            raise ValueError, 'In MadrigalExperiment, sitename not string type: %s' % (str(sitename))

        self.sitename = sitename

        self.instcode = int(instcode)

        if type(instname) != types.StringType:
            raise ValueError, 'In MadrigalExperiment, instname not string type: %s' % (str(instname))

        self.instname = instname

        self.startyear = int(startyear)

        self.startmonth = int(startmonth)

        self.startday = int(startday)

        self.starthour = int(starthour)

        self.startmin = int(startmin)

        self.startsec = int(startsec)

        self.endyear = int(endyear)

        self.endmonth = int(endmonth)

        self.endday = int(endday)

        self.endhour = int(endhour)

        self.endmin = int(endmin)

        self.endsec = int(endsec)

        if isLocal not in (True, False):
            raise ValueError, 'In MadrigalExperiment, isLocal not boolean: %s' % (str(isLocal))

        self.isLocal = isLocal

        if type(madrigalUrl) != types.StringType:
            raise ValueError, 'In MadrigalExperiment, madrigalUrl not string type: %s' % (str(madrigalUrl))

        self.madrigalUrl = madrigalUrl
        

    def __str__(self):
        """return a readible form of this object"""
        retStr = ''
        retStr += 'id: %s\n' % (str(self.id))
        retStr += 'url: %s\n' % (str(self.url))
        retStr += 'name: %s\n' % (str(self.name))
        retStr += 'siteid: %s\n' % (str(self.siteid))
        retStr += 'sitename: %s\n' % (str(self.sitename))
        retStr += 'instcode: %s\n'%  (str(self.instcode))
        retStr += 'instname: %s\n' % (str(self.instname))
        retStr += 'startyear: %s\n' % (str(self.startyear))
        retStr += 'startmonth: %s\n'%  (str(self.startmonth))
        retStr += 'startday: %s\n' % (str(self.startday))
        retStr += 'starthour: %s\n'%  (str(self.starthour))
        retStr += 'startmin: %s\n' % (str(self.startmin))
        retStr += 'startsec: %s\n'%  (str(self.startsec))
        retStr += 'endyear: %s\n' % (str(self.endyear))
        retStr += 'endmonth: %s\n'%  (str(self.endmonth))
        retStr += 'endday: %s\n' % (str(self.endday))
        retStr += 'endhour: %s\n'%  (str(self.endhour))
        retStr += 'endmin: %s\n' % (str(self.endmin))
        retStr += 'endsec: %s\n'%  (str(self.endsec))
        retStr += 'isLocal: %s\n' % (str(self.isLocal))
        retStr += 'madrigalUrl: %s\n'%  (str(self.madrigalUrl))
        return retStr

    def __cmp__(self, other):
        """ __cmp__ compares two MadrigalExperiment objects.

        Compared by start time, then by end time.
        """
        dt1 = datetime.datetime(self.startyear, self.startmonth, self.startday,
                                self.starthour, self.startmin, self.startsec)
        dt2 = datetime.datetime(other.startyear, other.startmonth, other.startday,
                                other.starthour, other.startmin, other.startsec)
        result = cmp(dt1, dt2)
        if result != 0:
            return(result)

        
        dt1 = datetime.datetime(self.endyear, self.endmonth, self.endday,
                                self.endhour, self.endmin, self.endsec)
        dt2 = datetime.datetime(other.endyear, other.endmonth, other.endday,
                                other.endhour, other.endmin, other.endsec)
        return(cmp(dt1, dt2))                       
        
        


class MadrigalExperimentFile:
    """MadrigalExperimentFile is a class that encapsulates information about a Madrigal ExperimentFile.


    Attributes::

        name (string) Example '/opt/mdarigal/blah/mlh980120g.001'
        
        kindat (int) Kindat code.  Example: 3001
        
        kindatdesc (string) Kindat description: Example 'Basic Derived Parameters'
        
        category (int) (1=default, 2=variant, 3=history, 4=real-time)
        
        status (string)('preliminary', 'final', or any other description)
        
        permission (int)  0 for public, 1 for private

        expId - experiment id of the experiment this MadrigalExperimentFile belongs in
            

    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 10, 2004

    """
    def __init__(self, name, kindat, kindatdesc, category, status, permission, expId = None):
        """__init__ initializes a MadrigalExperimentFile.

        Inputs::

            name - (string) Example '/opt/mdarigal/blah/mlh980120g.001'
        
            kindat - (int, or string that can be converted) Kindat code.  Example: 3001
        
            kindatdesc - (string) Kindat description: Example 'Basic Derived Parameters'
        
            category - (int, or string that can be converted) (1=default, 2=variant, 3=history, 4=real-time)
        
            status - (string)('preliminary', 'final', or any other description)
        
            permission - (int, or string that can be converted)  0 for public, 1 for private

            expId - experiment id of the experiment this MadrigalExperimentFile belongs in

        
        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: If illegal argument passed in.
        """

        if type(name) != types.StringType:
            raise ValueError, 'In MadrigalExperimentFile, name not string type: %s' % (str(name))

        self.name = name

        self.kindat = int(kindat)

        if type(kindatdesc) != types.StringType:
            raise ValueError, 'In MadrigalExperimentFile, kindatdesc not string type: %s' % (str(kindatdesc))

        self.kindatdesc = kindatdesc

        self.category = int(category)

        if type(status) != types.StringType:
            raise ValueError, 'In MadrigalExperimentFile, status not string type: %s' % (str(status))

        self.status = status

        self.permission = int(permission)

        if expId == None:
            self.expId = None
        else:
            self.expId = int(expId)
            
        

    def __str__(self):
        """return a readible form of this object"""
        retStr = ''
        retStr += 'name: %s\n' % (str(self.name))
        retStr += 'kindat: %s\n' % (str(self.kindat))
        retStr += 'kindatdesc: %s\n' % (str(self.kindatdesc))
        retStr += 'category: %s\n' % (str(self.category))
        retStr += 'status: %s\n' % (str(self.status))
        retStr += 'permission: %s\n'%  (str(self.permission))
        retStr += 'expId: %s\n'%  (str(self.expId))
        return retStr


class MadrigalParameter:
    """MadrigalParameter is a class that encapsulates information about a Madrigal Parameter.


    Attributes::

        mnemonic (string) Example 'dti'

        description (string) Example: "F10.7 Multiday average observed (Ott)"

        isError (int) 1 if error parameter, 0 if not

        units (string) Example "W/m2/Hz"

        isMeasured (int) 1 if measured, 0 if derivable

        category (string) Example: "Time Related Parameter"

        isSure (int) - 1 if parameter can be found for every record, 0 if can only be found for some

        isAddIncrement - 1 if additional increment, 0 if normal, -1 if unknown (this capability
                         only added with Madrigal 2.5)


    Non-standard Python modules used: None


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Aug. 8, 2005

    """
    def __init__(self, mnemonic, description, isError, units, isMeasured, category, isSure, isAddIncrement):
        """__init__ initializes a MadrigalParameter.

        Inputs::

            mnemonic (string) Example 'dti'

            description (string) Example: "F10.7 Multiday average observed (Ott)"

            isError (int) 1 if error parameter, 0 if not

            units (string) Example "W/m2/Hz"

            isMeasured (int) 1 if measured, 0 if derivable

            category (string) Example: "Time Related Parameter"

            isSure (int) - 1 if parameter can be found for every record, 0 if can only be found for some

            isAddIncrement - 1 if additional increment, 0 if normal, -1 if unknown (this capability
                             only added with Madrigal 2.5)
 

        Returns: void

        Affects: Initializes all the class member variables.

        Exceptions: If illegal argument passed in.
        """

        if type(mnemonic) != types.StringType:
            raise ValueError, 'In MadrigalParameter, mnemonic not string type: %s' % (str(mnemonic))

        self.mnemonic = mnemonic

        if type(description) != types.StringType:
            raise ValueError, 'In MadrigalParameter, description not string type: %s' % (str(description))

        self.description = description

        self.isError = int(isError)

        if type(units) != types.StringType:
            raise ValueError, 'In MadrigalParameter, units not string type: %s' % (str(units))

        self.units = units

        self.isMeasured = int(isMeasured)

        if type(category) != types.StringType:
            raise ValueError, 'In MadrigalParameter, category not string type: %s' % (str(category))

        self.category = category
            
        self.isSure = int(isSure)

        self.isAddIncrement = int(isAddIncrement)
        

    def __str__(self):
        """return a readible form of this object"""
        retStr = ''
        retStr += 'mnemonic: %s\n' % (str(self.mnemonic))
        retStr += 'description: %s\n' % (str(self.description))
        retStr += 'isError: %s\n' % (str(self.isError))
        retStr += 'units: %s\n' % (str(self.units))
        retStr += 'isMeasured: %s\n' % (str(self.isMeasured))
        retStr += 'category: %s\n'%  (str(self.category))
        retStr += 'isSure: %s\n'%  (str(self.isSure))
        retStr += 'isAddIncrement: %s\n'%  (str(self.isAddIncrement))
        return retStr     



if __name__ == '__main__':

    testInst  = MadrigalInstrument("Millstone Hill AMISR Radar", '33', 'Mlh', 45.0, 110.0, '0.015')
    print testInst

    
    testExp  = MadrigalExperiment('10000111',
                                  'http://www.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97',
                                  "World day",
                                  '33',
                                  'Westford',
                                  30,
                                  'SuperDarn',
                                  1993,1,2,3,4,5,
                                  '1993','1','2','3','4','5',
                                  True,
                                  'http://www.haystack.mit.edu/madrigal')
    print testExp


    testExpFile  = MadrigalExperimentFile("/opt/madrigal/blah/mlh980120g.001", '33', 'Blah blah', '2', 'final', '0')
    print testExpFile

    testData = MadrigalData('http://grail/madrigal/')

    # save a file
    testData.downloadFile('/home/grail/brideout/madroot/experiments/1998/mlh/20jan98/mil980120g.001',
                          '/tmp/junk.001')

    instList = testData.getAllInstruments()

    for inst in instList:
        print inst

    expList = testData.getExperiments(20, 1960,1,1,0,0,0,2005,1,1,0,0,0)

    for exp in expList:
        print exp

    fileList = testData.getExperimentFiles(expList[0].id)

    for file in fileList:
        print file

    print 'Parameter list for %s' % (file.name)
    parmList = testData.getExperimentFileParameters(file.name)
    for parm in parmList:
        print str(parm) + '\n'

    testData.simplePrint('/home/grail/brideout/madroot/experiments/1998/mlh/20jan98/mlh980120g.001',
                               'Bill Rideout', 'brideout@haystack.mit.edu', 'MIT Haystack')

    print testData.isprint('/home/grail/brideout/madroot/experiments/1998/mlh/20jan98/mlh980120g.001',
                           'gdalt,ti', 'filter=gdalt,500,600 filter=ti,1000,2000','Bill Rideout',
                           'brideout@haystack.mit.edu', 'MIT Haystack')

    result = testData.madCalculator(1999,2,15,12,30,0,45,55,5,-170,-150,10,200,200,0,'bmag,bn')

    for line in result:
        print line

    result = testData.madTimeCalculator(1999,2,15,12,30,0,
                                        1999,2,20,12,30,0,
                                        24.0, 'kp,dst')

    for line in result:
        print line

    print 'test of radarToGeodetic with arg 42,-70,0.1,(0,90),(45,45),(1000,1000):'
    result = testData.radarToGeodetic(42,-70,0.1,(0,90),(45,45),(1000,1000))
    print result

    print 'test of radarToGeodetic with arg 42,-70,0.1,90, 45, 1000:'
    result = testData.radarToGeodetic(42,-70,0.1,90,45,1000)
    print result

    print 'test of geodeticToRadar with arg 42,-70,0.1,(50,60),(-70,-70),(1000,1000):'
    result = testData.geodeticToRadar(42,-70,0.1,(50,60),(-70,-70),(1000,1000))
    print result

    print 'test of geodeticToRadar with arg 42,-70,0.1,50, -70, 1000:'
    result = testData.geodeticToRadar(42,-70,0.1,50,-70,1000)
    print result

