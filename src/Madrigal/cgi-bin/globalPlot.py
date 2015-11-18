#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: globalPlot.py,v 1.17 2009/04/23 16:12:42 brideout Exp $

import sys, os, traceback
import cgi, Cookie
import time, datetime
import types
import os.path
import Queue
import threading

import madrigal.metadata
import madrigal._Madrec
import madrigal.data
import madrigal.ui.web
import madrigal.ui.madrigalPlot
import madrigalWeb.madrigalWeb

class globalPlot:
    """globalPlot is the class that produces the globalPlot page.
    
    globalPlot has the following structure:  the entire cgi is contained in one class, with a
    main function at the end which serves simply to call the __init__  function of the class.  This
    __init__ function is responsible for calling all other class methods. It is made up of a single
    try block, with the purpose of reporting all exceptions in well-formatted html to both the user
    and the administrator. The __init__ function first makes sure the pythonlib can be found.  It
    then calls setScriptState to determine from any cgi arguments what the script is supposed to do.
    The script state is always set in self.state.  The particular values allowed for globalPlot
    are discussed below.

    The __init__ function then calls createObjects to create whatever python api objects are required
    to complete the script.  If the user has made a request that may succeed or may fail, that request is
    then processed.  The script thereafter calls outputHead to output the header section and any required
    javascript.  Finally, __init__ calls a few functions for each of the main sections of the body.

    If any uncaught exception is thrown, its caught by the __init__ try block.  If it's a MadrigalError,
    additional information is available.  The catch blocks attempt to display the error message on the screen
    by backing out of of large number of possible tags, which might prevent its display (in any case, the error
    message will always be available in the page source.  The formatted error message is also sent to the email
    address given in the siteTab.txt metadata file.

    Every attempt is made to generate easy to read source html, since it is often an easy starting point for
    analyzing the script.  Table structure is indicated by indentation, as is javascript code structure.

    List of cgi arguments that this script uses:

    stationName - instrument id of instruments to be plotted.  More than one allowed.

    plotFile - full path to previously created plot. More than one allowed.

    plotHeader - instrument id of instrument plotted in plotFile. More than one allowed.

    startYear, startMonth, startDay, startHour, startMin, startSec - starting time of plot to create

    endYear, endMonth, endDay, endHour, endMin, endSec - ending time of plot to create

    plotType - either "scatter" or "pcolor"

    parmSelect - mnemonic of parameter to plot

    parmLower - (optional) - lower value filter for parameter value

    parmUpper - (optional) - upper value filter for parameter value

    gdaltLower - (optional) - lower value filter for altitude - only if plotType == pcolor

    parmUpper - (optional) - upper value filter for titude - only if plotType == pcolor

    filterParm - (optional) - a parmeter other than the one selected that can be used to filter data

    filterLower - (optional) - lower value for filterParm parameter

    filterUpper - (optional) - upper value for filterParm parameter

    filterParm2 - (optional) - a 2nd parmeter other than the one selected that can be used to filter data

    filterLower2 - (optional) - lower value for filterParm2 parameter

    filterUpper2 - (optional) - upper value for filterParm2 parameter

    filterParm3 - (optional) - a 3rd parmeter other than the one selected that can be used to filter data

    filterLower3 - (optional) - lower value for filterParm3 parameter

    filterUpper3 - (optional) - upper value for filterParm3 parameter
  

    Change history:

    Written by "Bill Rideout":mailto:brideout@haystack.mit.edu  July 6, 2005
    """

    # constants
    __scriptName = 'globalPlot.py'


   
    def __init__(self):
        """__init__ run the entire globalPlot script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script globalPlot.

        Exceptions: None.
        """


        # catch any exception, and write an appropriate message to user and to admin
        try:

            # check if pythonlibpath env variable exists
            # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
            temp = os.environ.get('PYTHON' + 'LIBPATH')
            if temp != None:
                    sys.path.append(temp)
		    
            # append path madroot/lib (needed only if python not installed by setup)
            sys.path.append('/Users/mnicolls/Documents/Work/Madrigal/lib/python')

            # prepare to handle MadrigalError
            import madrigal.admin
            
        except ImportError:
	    
            # Fatal error - madpy library not found
            print "Content-Type: text/html"
            print
            print "Unable to import the madrigal python library - please alert the sys admin!"
            sys.exit(0)
	    
        try:

            # set flag as to whether script headers have been written
            self.scriptHeaders = 0
            
            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()
            
            # output html

            #print header
            self.outputHead('Madrigal plots')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printHeading()

            self.printDescription()

            self.printHiddenElements()

            self.printPlots()

            self.printEndPage()

            

        except madrigal.admin.MadrigalError, e:
            # handle a MadrigalError

            # back out of any tag so error message appears
            if self.scriptHeaders != 0:
                print '</script></select></td></tr></table></td></tr></table>'
                
            errStr = '<h1> Error occurred in script ' + self.__scriptName + '.</h1>'

            errStr = errStr + e.getExceptionHtml()
            
            err = traceback.format_exception(sys.exc_info()[0],
                                             sys.exc_info()[1],
                                             sys.exc_info()[2])

            for errItem in err:
                errStr = errStr + '<br>\n' + str(errItem)

        
            # add info about called form:
            if self.madForm != None:
                errStr = errStr + '<h3>Form elements</h3>\n'
                for key in self.madForm.keys():
                    errStr = errStr + '<br>\n' + str(key)
                    errStr = errStr + ' = ' + str(self.madForm.getvalue(key))

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/html"
                print
                
            print errStr + '<BR>'

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('<html>\n' + errStr + '</html>',
                                 'Error running ' + self.__scriptName)


            print '<br><b>Your system administrator has been notified.<b>'

        except SystemExit:
            sys.exit(0)

        except:
            # handle a normal error
            
            # back out of any tag so error message appears
            if self.scriptHeaders != 0:
                print '</script></select></td></tr></table></td></tr></table>'
                
            errStr = '<h1> Error occurred in script ' + self.__scriptName + '.</h1>'

            
            err = traceback.format_exception(sys.exc_info()[0],
                                             sys.exc_info()[1],
                                             sys.exc_info()[2])

            for errItem in err:
                errStr = errStr + '<br>\n' + str(errItem)

        
            # add info about called form:
            if self.madForm != None:
                errStr = errStr + '<h3>Form elements</h3>\n'
                for key in self.madForm.keys():
                    errStr = errStr + '<br>\n' + str(key)
                    errStr = errStr + ' = ' + str(self.madForm.getvalue(key))

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/html"
                print
                
            print errStr + '<BR>'

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('<html>\n' + errStr + '</html>',
                                 'Error running ' + self.__scriptName)


            print '<br><b>Your system administrator has been notified.<b>'

        # end __init__


    def setScriptState(self):
        
        #create a form object
        self.madForm = cgi.FieldStorage()

        # get name, email, affiliation from cookie is possible
        self.cookie = Cookie.SimpleCookie()
        if os.environ.has_key('HTTP_COOKIE'):
            self.cookie.load(os.environ['HTTP_COOKIE'])
            try:
                self.user_fullname = self.cookie["user_fullname"].value
                self.user_email = self.cookie["user_email"].value
                self.user_affiliation = self.cookie["user_affiliation"].value
            except:
                self.user_fullname = 'unknown'
                self.user_email = 'unknown'
                self.user_affiliation = 'unknown'
                    

        else:
            # no cookie exists
            self.user_fullname = 'unknown'
            self.user_email = 'unknown'
            self.user_affiliation = 'unknown'
  

    def createObjects(self):

        # all states require a MadrigalDB object
        self.madDBObj = madrigal.metadata.MadrigalDB()


        # create object for MadrigalInstrument
        self.madInstrumentObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

        # create list of selected kinst values
        self.madKinstList = []
        stationName = self.madForm.getvalue("stationName")
        # checking for lists or strings and changing as appropriate
        if type(stationName) == types.ListType:
            stationNameList = stationName
        else:
            stationNameList = [stationName]
            
        for item in stationNameList:
            self.madKinstList.append(int(item))

        # get list of old plots
        if self.madForm.has_key('plotFile'):
            self.plotFile = self.madForm.getvalue('plotFile')
            self.plotHeader = self.madForm.getvalue('plotHeader')
            if type(self.plotFile) == types.StringType:
                self.plotFile = [self.plotFile]
                self.plotHeader = [self.plotHeader]
        else:
            self.plotFile = []
            self.plotHeader = []

        # create starting and ending datetimes
        self.startDatetime = datetime.datetime(int(self.madForm.getvalue("startYear")),
                                               int(self.madForm.getvalue("startMonth")),
                                               int(self.madForm.getvalue("startDay")),
                                               int(self.madForm.getvalue("startHour")),
                                               int(self.madForm.getvalue("startMin")),
                                               int(self.madForm.getvalue("startSec")))

        self.endDatetime  =  datetime.datetime(int(self.madForm.getvalue("endYear")),
                                               int(self.madForm.getvalue("endMonth")),
                                               int(self.madForm.getvalue("endDay")),
                                               int(self.madForm.getvalue("endHour")),
                                               int(self.madForm.getvalue("endMin")),
                                               int(self.madForm.getvalue("endSec")))

        # acquire plotting parameters
        self.plotType = self.madForm.getvalue("plotType")
        self.parmSelect = self.madForm.getvalue("parmSelect")
        if self.madForm.has_key('parmLower'):
            self.parmLower = float(self.madForm.getvalue("parmLower"))
        else:
            self.parmLower = None
        if self.madForm.has_key('parmUpper'):
            self.parmUpper = float(self.madForm.getvalue("parmUpper"))
        else:
            self.parmUpper = None
        if self.madForm.has_key('gdaltLower'):
            self.gdaltLower = float(self.madForm.getvalue("gdaltLower"))
        else:
            self.gdaltLower = None
        if self.madForm.has_key('gdaltUpper'):
            self.gdaltUpper = float(self.madForm.getvalue("gdaltUpper"))
        else:
            self.gdaltUpper = None

        if self.madForm.has_key('filterParm'):
            self.filterParm = self.madForm.getvalue("filterParm")
            self.filterParm = self.filterParm.strip()
        else:
            self.filterParm = None

        if self.madForm.has_key('filterLower'):
            self.filterLower = float(self.madForm.getvalue("filterLower"))
        else:
            self.filterLower = None
        if self.madForm.has_key('filterUpper'):
            self.filterUpper = float(self.madForm.getvalue("filterUpper"))
        else:
            self.filterUpper = None

        if self.madForm.has_key('filterParm2'):
            self.filterParm2 = self.madForm.getvalue("filterParm2")
            self.filterParm2 = self.filterParm2.strip()
        else:
            self.filterParm2 = None

        if self.madForm.has_key('filterLower2'):
            self.filterLower2 = float(self.madForm.getvalue("filterLower2"))
        else:
            self.filterLower2 = None
        if self.madForm.has_key('filterUpper2'):
            self.filterUpper2 = float(self.madForm.getvalue("filterUpper2"))
        else:
            self.filterUpper2 = None

        if self.madForm.has_key('filterParm3'):
            self.filterParm3 = self.madForm.getvalue("filterParm3")
            self.filterParm3 = self.filterParm3.strip()
        else:
            self.filterParm3 = None

        if self.madForm.has_key('filterLower3'):
            self.filterLower3 = float(self.madForm.getvalue("filterLower3"))
        else:
            self.filterLower3 = None
        if self.madForm.has_key('filterUpper3'):
            self.filterUpper3 = float(self.madForm.getvalue("filterUpper3"))
        else:
            self.filterUpper3 = None


        # create object for MadrigalParameters
        self.madParametersObj = madrigal.data.MadrigalParameters(self.madDBObj)

        self.madSiteObj = madrigal.metadata.MadrigalSite(self.madDBObj)

        # create MadrigalExperiments object for local Madrigal site
        self.madExperimentsObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)

        # create a MadrigalWeb object
        self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)
        self.madWebFormatObj = madrigal.ui.web.MadrigalWebFormat()

        
    def outputHead(self, title):

        print "Content-Type: text/html"
        print                               # blank line
        self.scriptHeaders = 1
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        print '\t<style type="text/css">.lb {background: #eeeeff}</style>'
        self.printJavaScript()
        print '</head>'


    def printJavaScript(self):

        print '<script language = "JavaScript">'
        self.printAddGraph()
        self.printStartOver()
        print '</script>'


    def printAddGraph(self):

        print '\tfunction addGraph(madForm)'
        print '\t{'
        print '\t\tmadForm.action="plotInstrumentsSelect.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printStartOver(self):

        print '\tfunction startOver()'
        print '\t{'
        print '\t\ttop.location = "plotInstrumentsSelect.py"'
        print '\t}\n'

    # end javascript
    

    def printHeading(self):


        print '<center><h1>Madrigal plots</h1></center>'
        
       
    def printDescription(self):

        print '<form method=get enctype="application/x-www-form-urlencoded">'          #begin form>'
        
        
    def printHiddenElements(self):

        # now print all received post elements from plotInstrumentsSelect.py as hidden elements
        # if form just loaded
        print '<input type=hidden name=callingpage value=globalPlot.py>'
        for key in self.madForm.keys():
            if key in ('callingpage', 'parmSelect'):
                continue
            if type(self.madForm.getvalue(key)) == types.ListType:
                for value in self.madForm.getvalue(key):
                    print '<input type=hidden name=' + str(key) + \
                          ' value=' + value + '>'
            else:
                print '<input type=hidden name=' + str(key) + \
                          ' value="' + str(cgi.escape(self.madForm.getvalue(key))) + '">'



    def printPlots(self):

        # the first task is to figure out which site to query for each instrument.  Use
        # local metadata for this task
        kinstDict = {}
        for kinst in self.madKinstList:
            # find which madrigal site has most experiments with that instrument in that time
            # in the case of tie, use lowest site id
            siteDict = {}
            index = -1
            while 1:
                index += 1
                thisKinst = self.madExperimentsObj.getKinstByPosition(index)
                if thisKinst == None:
                    break
                if thisKinst != kinst:
                    continue
                # see if this experiment overlaps at all
                thisStartTime = self.madExperimentsObj.getExpStartDateTimeByPosition(index)
                thisStartDateTime = datetime.datetime(thisStartTime[0],
                                                      thisStartTime[1],
                                                      thisStartTime[2],
                                                      thisStartTime[3],
                                                      thisStartTime[4],
                                                      thisStartTime[5])
                if thisStartDateTime > self.endDatetime:
                    # experiment started after time period
                    continue
                
                thisEndTime = self.madExperimentsObj.getExpEndDateTimeByPosition(index)
                
                thisEndDateTime = datetime.datetime(thisEndTime[0],
                                                    thisEndTime[1],
                                                    thisEndTime[2],
                                                    thisEndTime[3],
                                                    thisEndTime[4],
                                                    thisEndTime[5])
                
                if thisEndDateTime < self.startDatetime:
                    # experiment ended before time period
                    continue

                # this experiment needs to be included - add it to siteDict
                thisSite = self.madExperimentsObj.getExpSiteIdByPosition(index)
                if thisSite not in siteDict.keys():
                    siteDict[thisSite] = [index]
                else:
                    siteDict[thisSite].append(index)

            if len(siteDict.keys()) == 0:
                print '<p>No data exists for instrument %s between %s and %s.</p>' % (self.madInstrumentObj.getInstrumentName(kinst),
                                                                                      str(self.startDatetime),
                                                                                      str(self.endDatetime))
                continue

            # find correct site
            thisSite = None
            numExp = 0
            for site in siteDict.keys():
                if len(siteDict[site]) > numExp:
                    thisSite = site
                    numExp = len(siteDict[site])

            # add key to kinstDict - value is tuple containing site id and Queue to talk with thread
            kinstDict[kinst] = (thisSite, Queue.Queue())

        # now we need to retrieve data for each instrument.  Since this data may be on separate
        # Madrigal sites, it makes sense to request all the data at once using threads.  Each thread
        # will return a single string via a queue representing all the data in the form of a headerless
        # isprint string
        for kinst in kinstDict.keys():
            thisSite, thisQueue = kinstDict[kinst]

            thisThread = CreateIsprintStringThread(thisSite,
                                                   kinst,
                                                   thisQueue,
                                                   self.madDBObj,
                                                   self.plotType,
                                                   self.parmSelect,
                                                   self.startDatetime,
                                                   self.endDatetime,
                                                   self.parmLower,
                                                   self.parmUpper,
                                                   self.gdaltLower,
                                                   self.gdaltUpper,
                                                   self.filterParm,
                                                   self.filterLower,
                                                   self.filterUpper,
                                                   self.filterParm2,
                                                   self.filterLower2,
                                                   self.filterUpper2,
                                                   self.filterParm3,
                                                   self.filterLower3,
                                                   self.filterUpper3,
                                                   self.user_fullname,
                                                   self.user_email,
                                                   self.user_affiliation)

            thisThread.start()

        self.plotList = [] # self.plotList holds a list of tuples where each tuple contains the relative url to
                           # the plot, and the plot header giving the site id

        # loop through the Queue until all return data
        queueLength = len(kinstDict.keys())
        queuesRead = 0
        while queuesRead < queueLength:
            for kinst in kinstDict.keys():
                thisSite, thisQueue = kinstDict[kinst]
                try:
                    # read from queue, but don't block
                    thisIsprintStr = thisQueue.get_nowait()
                    queuesRead += 1
                    if len(thisIsprintStr) > 2:
                        try:
                            plotInfo = self.plotIsprintStr(thisIsprintStr, kinst, thisSite)
                            self.plotList.append(plotInfo)
                            # write new plots as hidden form element
                            print '<input type=hidden name=plotFile value=%s>' % (plotInfo[0])
                            print '<input type=hidden name=plotHeader value=%s>' % (str(plotInfo[1]))
                        except ValueError:
                            print '<p>No valid data exists for parameter %s and instrument %s in the selected time period with the given filters.</p>' % (self.parmSelect,
                                                                                                                                                          self.madInstrumentObj.getInstrumentName(kinst))
                    else:
                        print '<p>No data exists for instrument %s between %s and %s with the given filters.</p>' % (self.madInstrumentObj.getInstrumentName(kinst),
                                                                                                                     str(self.startDatetime),
                                                                                                                     str(self.endDatetime))

                except Queue.Empty:
                    pass

            time.sleep(1)

        # show old plots
        for index in range(len(self.plotFile)):
            print '<p>&nbsp;&nbsp;</p><p>%s</p>' % (self.getHeaderStr(int(self.plotHeader[index])))
            print '<IMG SRC="%s" BORDER=0 ALT="[Madrigal data plot]">' % (self.plotFile[index])

        
        # show new plots
        for index in range(len(self.plotList)):
            print '<p>&nbsp;&nbsp;</p><p>%s</p>' % (self.getHeaderStr(self.plotList[index][1]))
            print '<IMG SRC="%s" BORDER=0 ALT="[Madrigal data plot]">' % (self.plotList[index][0])



    def getHeaderStr(self, siteID):
        """getHeaderStr takes a siteID and generates a descriptive string
        """
        
        rootUrl = os.path.join('http://',
                               self.madSiteObj.getSiteServer(siteID),
                               self.madSiteObj.getSiteDocRoot(siteID))

        siteName = self.madSiteObj.getSiteName(siteID)

        return """The plot below is from Madrigal database at <a href="%s">%s</a>.
                  See their site for rules for data usage.""" % (rootUrl, siteName)

        


    def plotIsprintStr(self, thisIsprintStr, kinst, thisSite):
        """plotIsprintStr creates a plot, and save it to file.

        Inputs:

            thisIsprintStr - isprint string

            kinst - instrument id

            thisSite - site id where data came from

        Returns: a tuple consisting of 1. relative url to file, and 2. thisSite
        """
        numDays = 0.5 # plots are deleted after 1/2 day
        self.__tmpDir = 'tempReports'

        MAX_NUM_TIMES = 500
        MAX_NUM_ALT = 100

        # get startTime and endTime as seconds since 1/1/1950
        thisStartTime = madrigal._Madrec.getUtFromDate(self.startDatetime.year,
                                                       self.startDatetime.month,
                                                       self.startDatetime.day,
                                                       self.startDatetime.hour,
                                                       self.startDatetime.minute,
                                                       self.startDatetime.second,
                                                       0)
        
        thisEndTime = madrigal._Madrec.getUtFromDate(self.endDatetime.year,
                                                     self.endDatetime.month,
                                                     self.endDatetime.day,
                                                     self.endDatetime.hour,
                                                     self.endDatetime.minute,
                                                     self.endDatetime.second,
                                                     0)
        
        # create a unique filename
        filename = '%i_%i_%i.png' % (kinst, thisSite, time.time())

        tmpDir = os.path.join(self.madDBObj.getDocDirPath(), self.__tmpDir)

        fullFilename = os.path.join(tmpDir, filename)

        relUrl = '/' + os.path.join(self.madDBObj.getRelativeTopLevel(),
                                    self.__tmpDir,
                                    filename)

        # create plot file
        if self.plotType == 'scatter':
            plotTitle = 'Scatter plot of %s for %s'% (self.parmSelect,
                                                      self.madInstrumentObj.getInstrumentName(kinst))
            if self.filterParm != None:
                plotTitle += ' (%s %s:%s)' % (self.filterParm,
                                              str(self.filterLower),
                                              str(self.filterUpper))
            if self.filterParm2 != None:
                plotTitle += ' (%s %s:%s)' % (self.filterParm2,
                                              str(self.filterLower2),
                                              str(self.filterUpper2))
            if self.filterParm3 != None:
                plotTitle += ' (%s %s:%s)' % (self.filterParm3,
                                              str(self.filterLower3),
                                              str(self.filterUpper3))
                
            madrigal.ui.madrigalPlot.madScatterPlot(thisIsprintStr,
                                                    plotTitle,
                                                    'UT Time (from %s to %s)' % (self.startDatetime.strftime('%Y-%m-%d %H:%M:%S'),
                                                                                 self.endDatetime.strftime('%Y-%m-%d %H:%M:%S')),
                                                    '%s (%s)' % (self.parmSelect,
                                                                 self.madParametersObj.getParmUnits(self.parmSelect)),
                                                    fullFilename,
                                                    size = 'wide',
                                                    useAbsoluteTime = True,
                                                    startTime = thisStartTime,
                                                    endTime = thisEndTime,
                                                    maxNumPoints = 10000)

        elif self.plotType == 'pcolor':
            plotTitle = 'Pcolor plot of %s (%s) for %s' % (self.parmSelect,
                                                           self.madParametersObj.getParmUnits(self.parmSelect),
                                                           self.madInstrumentObj.getInstrumentName(kinst))
            if self.filterParm != None:
                plotTitle += ' (%s %s:%s)' % (self.filterParm,
                                              str(self.filterLower),
                                              str(self.filterUpper))
            if self.filterParm2 != None:
                plotTitle += ' (%s %s:%s)' % (self.filterParm2,
                                              str(self.filterLower2),
                                              str(self.filterUpper2))
            if self.filterParm3 != None:
                plotTitle += ' (%s %s:%s)' % (self.filterParm3,
                                              str(self.filterLower3),
                                              str(self.filterUpper3))
                
            madrigal.ui.madrigalPlot.madPcolorPlot(thisIsprintStr,
                                                   plotTitle,
                                                   'UT Time (from %s to %s)' % (self.startDatetime.strftime('%Y-%m-%d %H:%M:%S'),
                                                                                self.endDatetime.strftime('%Y-%m-%d %H:%M:%S')),
                                                   'Altitude (km)',
                                                   fullFilename,
                                                   size = 'wide',
                                                   minColormap = self.parmLower,
                                                   maxColormap = self.parmUpper,
                                                   smoothAltitude = True,
                                                   insertDataGap = 5,
                                                   useAbsoluteTime = True,
                                                   startTime = thisStartTime,
                                                   endTime = thisEndTime,
                                                   sortTimeFlag = True,
                                                   maxNumTimes = MAX_NUM_TIMES,
                                                   maxNumAlt = MAX_NUM_ALT,
                                                   truncateIsprint = True)

        # remove old plots
        # get present local time
        now = time.time()
        # get dir listing of temp folder
        fileList = os.listdir(self.madDBObj.getDocDirPath() + '/' + self.__tmpDir)
        for name in fileList:
            if len(name) > 7:
                # must be at least seven characters of the form '*.png'
                if name[-3:] == 'png':
                    # get its creation time
                    createTime = os.stat(self.madDBObj.getDocDirPath() + '/' + self.__tmpDir + '/' + name)[9]
                    # if its too old, delete it
                    if now > createTime + numDays*24*60*60.0:
                        os.remove(self.madDBObj.getDocDirPath() + '/' + self.__tmpDir + '/' + name)


        return (relUrl, thisSite)
            

        
    
    def printEndPage(self):

        self.printButtons()
        print '</form></body></html>'       # end form, body, and html 


    def printButtons(self):

        print '<center><p>'
        print '<input class=lb type=button value="Create a new set of plots" onClick=startOver()>'
        print '<input class=lb type=button value="Add another plot below" onClick=addGraph(this.form)>'
        print '</p></center>'


class CreateIsprintStringThread(threading.Thread):
    """CreateIsprintStringThread is an private implementation class used to run a thread to create isprint strings.

    Non-standard Python modules used: none

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 7, 2005

    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Aug. 8, 2005 to select the first default file
    that has the requested parameter parmSelect.
    """
    
    def __init__(self,
                 thisSite,
                 kinst,
                 thisQueue,
                 madDBObj,
                 plotType,
                 parmSelect,
                 startDatetime,
                 endDatetime,
                 parmLower,
                 parmUpper,
                 gdaltLower,
                 gdaltUpper,
                 filterParm,
                 filterLower,
                 filterUpper,
                 filterParm2,
                 filterLower2,
                 filterUpper2,
                 filterParm3,
                 filterLower3,
                 filterUpper3,
                 user_fullname,
                 user_email,
                 user_affiliation):
        """
        __init__ queries a Madrigal server via madrigalWeb module to create the needed isprint string

        Inputs:

           thisSite - site id that holds the data

           kinst - instrument id

           thisQueue - queue to place isprint string on when complete

           madDBObj - MadrigalDB object

           plotType - either 'scatter' or 'pcolor'

           parmSelect - mnemonic of parameter selected

           startDatetime - datetime to start plot

           endDatetime - datetime to end plot

           parmLower - lower limit of parameter, or None if no lower limit

           parmUpper - upper limit of parameter, or None if no upper limit

           gdaltLower - lower limit of altitude, or None if no lower limit

           gdaltUpper - upper limit of altitude, or None if no upper limit

           filterParm - 1st additional parameter to filter data, or None

           filterLower - lower limit of filterParm, or None if no lower limit

           filterUpper - upper limit of filterParm, or None if no upper limit

           filterParm2 - 2nd additional parameter to filter data, or None

           filterLower2 - lower limit of filterParm2, or None if no lower limit

           filterUpper2 - upper limit of filterParm2, or None if no upper limit

           filterParm3 - 3rd additional parameter to filter data, or None

           filterLower3 - lower limit of filterParm3, or None if no lower limit

           filterUpper3 - upper limit of filterParm3, or None if no upper limit

           user_fullname - user full name (string)

           user_email - user email (string)

           user_affiliation - user affiliation (string)
           
        """
        # set up thread
        threading.Thread.__init__(self)

        self.thisSite = thisSite
        self.kinst = kinst
        self.thisQueue = thisQueue
        self.madDBObj = madDBObj
        self.startDatetime = startDatetime
        self.endDatetime = endDatetime
        self.parmSelect = parmSelect
        self.user_fullname = user_fullname
        self.user_email = user_email
        self.user_affiliation = user_affiliation

        
        # create isprint parms string
        if plotType == 'scatter':
            self.isprintParms = 'ut1,%s' % parmSelect.strip().lower()
        elif plotType == 'pcolor':
            self.isprintParms = 'ut1,gdalt,%s' % parmSelect.strip().lower()
        else:
            raise ValueError, 'Unknown plot type %s' % (str(plotType))


        # create isprint filter string
        self.isprintFilter = 'date1=%i/%i/%i time1=%i:%i:%i date2=%i/%i/%i time2=%i:%i:%i ' % (startDatetime.month,
                                                                                          startDatetime.day,
                                                                                          startDatetime.year,
                                                                                          startDatetime.hour,
                                                                                          startDatetime.minute,
                                                                                          startDatetime.second,
                                                                                          endDatetime.month,
                                                                                          endDatetime.day,
                                                                                          endDatetime.year,
                                                                                          endDatetime.hour,
                                                                                          endDatetime.minute,
                                                                                          endDatetime.second)
        
        if parmLower != None or parmUpper != None:
            if parmLower != None and parmUpper != None:
                self.isprintFilter += 'filter=%s,%g,%g ' % (parmSelect.strip().lower(), parmLower, parmUpper)
            elif parmLower != None:
                self.isprintFilter += 'filter=%s,%g, ' % (parmSelect.strip().lower(), parmLower)
            else:
                self.isprintFilter += 'filter=%s,,%g ' % (parmSelect.strip().lower(), parmUpper)
                
        if gdaltLower != None or gdaltUpper != None:
            if gdaltLower != None and gdaltUpper != None:
                self.isprintFilter += 'filter=gdalt,%g,%g ' % (gdaltLower, gdaltUpper)
            elif gdaltLower != None:
                self.isprintFilter += 'filter=gdalt,%g, ' % (gdaltLower)
            else:
                self.isprintFilter += 'filter=gdalt,,%g ' % (gdaltUpper)
        elif plotType == 'pcolor':
            self.isprintFilter += 'filter=gdalt,, '

        if filterParm != None and (filterLower != None or filterUpper != None):
            if filterLower != None and filterUpper != None:
                self.isprintFilter += 'filter=%s,%g,%g ' % (filterParm.strip().lower(), filterLower, filterUpper)
            elif filterLower != None:
                self.isprintFilter += 'filter=%s,%g, ' % (filterParm.strip().lower(), filterLower)
            else:
                self.isprintFilter += 'filter=%s,,%g ' % (filterParm.strip().lower(), filterUpper)

        if filterParm2 != None and (filterLower2 != None or filterUpper2 != None):
            if filterLower2 != None and filterUpper2 != None:
                self.isprintFilter += 'filter=%s,%g,%g ' % (filterParm2.strip().lower(), filterLower2, filterUpper2)
            elif filterLower2 != None:
                self.isprintFilter += 'filter=%s,%g, ' % (filterParm2.strip().lower(), filterLower2)
            else:
                self.isprintFilter += 'filter=%s,,%g ' % (filterParm2.strip().lower(), filterUpper2)

        if filterParm3 != None and (filterLower3 != None or filterUpper3 != None):
            if filterLower3 != None and filterUpper3 != None:
                self.isprintFilter += 'filter=%s,%g,%g ' % (filterParm3.strip().lower(), filterLower3, filterUpper3)
            elif filterLower3 != None:
                self.isprintFilter += 'filter=%s,%g, ' % (filterParm3.strip().lower(), filterLower3)
            else:
                self.isprintFilter += 'filter=%s,,%g ' % (filterParm3.strip().lower(), filterUpper3)
        

        # get the main madrigal url of the given site
        madSiteObj = madrigal.metadata.MadrigalSite(madDBObj)
        self.rootUrl = os.path.join('http://',
                               madSiteObj.getSiteServer(thisSite),
                               madSiteObj.getSiteDocRoot(thisSite))
            


    def run(self):
        """ all the real work is done in run, including communication with remote Madrigal sites """
        isprintStr = '' # string to be returned

        madWebObj = madrigalWeb.madrigalWeb.MadrigalData(self.rootUrl)

        # get a list of madrigalWeb.MadrigalExperiment objects from the server itself.  Here we don't trust the local
        # metadata, in case its out of date
        madExpList = madWebObj.getExperiments(self.kinst,
                                              self.startDatetime.year,
                                              self.startDatetime.month,
                                              self.startDatetime.day,
                                              self.startDatetime.hour,
                                              self.startDatetime.minute,
                                              self.startDatetime.second,
                                              self.endDatetime.year,
                                              self.endDatetime.month,
                                              self.endDatetime.day,
                                              self.endDatetime.hour,
                                              self.endDatetime.minute,
                                              self.endDatetime.second)

        for exp in madExpList:
            # get list of default files
            madExpFileList = madWebObj.getExperimentFiles(exp.id)
            # find the first default file that can display the requested parameter
            madExpFile = None
            for thisMadExpFile in madExpFileList:
                if madExpFile != None:
                    break
                # get a list of parameters this file has (measured and derived)
                thisMadParmList = madWebObj.getExperimentFileParameters(thisMadExpFile.name)
                for thisParm in thisMadParmList:
                    if self.parmSelect.lower() == thisParm.mnemonic.lower():
                        madExpFile = thisMadExpFile
                        break
                        

            if madExpFile != None:
                # get thisIsprintString
                thisIsprintString = madWebObj.isprint(madExpFile.name, self.isprintParms, self.isprintFilter,
                                                      self.user_fullname, self.user_email, self.user_affiliation)

                if thisIsprintString.find('No records') != -1:
                    thisIsprintString = ''

            else:
                thisIsprintString = ''

            isprintStr += thisIsprintString


        # put result for this instrument on the Queue, and exit thread
        self.thisQueue.put(isprintStr)
        return

if __name__ == '__main__':

    # Script madParmList
    # This script only calls the init function of the class globalPlot
    # All work is done by the init function
    globalPlot()
