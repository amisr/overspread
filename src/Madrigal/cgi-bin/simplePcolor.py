#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, os.path
import traceback
import cgi, Cookie
import time, datetime
import types



class simplePcolor:
    """simplePcolor is the class that produces the simplePcolor page.
    
    Like all my python cgi scripts, simplePcolor has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for simplePcolor are discussed below.

    The __init__ function then calls createObjects to create whatever python api objects are required
    to complete the script.  If the user has made a request that may succeed or may fail, that request is
    then processed, and self.success is set to either 'true' or 'false', and self.result is set to either
    1 (if success) or to the error message from the madpy library if failure.  The output html will then
    be determined by self.success.  The script then calls outputHead to output the header section and any required
    javascript.  Finally, __init__ calls a few functions for each of the main sections of the body.

    If any uncaught exception is thrown, its caught by the __init__ try block.  If its an MadrigalError,
    additional information is available.  The catch blocks attempt to display the error message on the screen
    by backing out of of large number of possible tags, which might prevent its display (in any case, the error
    message will always be available in the page source.  The formatted error message is also sent to the email
    address given in the siteTab.txt metadata file.

    Every attempt is made to generate easy to read source html, since it is often an easy starting point for analyzing
    the script.  Table structure is indicated by indentation, as is javascript code structure.

    The names of all form elements used by simplePcolor are listed below:

    selectInstrument:	local instrument selected - id is instrument id

    selectExperiments:  list of experiment ids selected

    selectParm: mnemonic of parameter to plot
    
    selectYAxis: one of Geodetic, Range, Latitude, Longitude, PACE_Magnetic_Latitude, or
                 PACE_Magnetic_Longitude

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 14, 2005

    $Id: simplePcolor.py,v 1.13 2008/09/17 14:31:27 brideout Exp $
    """

    # constants
    __scriptName = 'simplePcolor.py'
    __MAX_NUM_TIMES = 500
    __MAX_NUM_ALT = 100
    __MAX_STR_SIZE = 300000000 # can't handle strings greater than 300 MB
    __MAX_SCATTER_POINTS = 10000 # maximum number of points in scatter plot


    def __init__(self):
        """__init__ run the entire simplePcolor script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script simplePcolor.

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

            # determine from form arguments and cookies which script state to use
            self.setScriptState()

            # create needed Madrigal objects
            self.createObjects()

            # output html

            #print header
            self.outputHead('Simple Plot Data')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            # in this script, we want to give the user some feedback that the
            # script will take some time
            self.printWaitMessage()


            # now create the graph
            self.createGraph()

            self.printBody()

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
                    errStr = errStr + ' = ' + str(self.madForm.getlist(key))

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
                    errStr = errStr + ' = ' + str(self.madForm.getlist(key))

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

        if not self.madForm.has_key('selectInstrument'):
            if  self.scriptHeaders == 0:
                print "Content-Type: text/html\n"

            print '<h3> This cgi script was called without the proper arguments.</h3>' + \
                  'Since this script uses post, you cannot bookmark this page. ' + \
                  'Please contact your site administrator with any questions.'

            sys.exit(0)
            
        else:
            self.instrumentKinst = int(self.madForm.getvalue('selectInstrument'))
            self.experimentList = self.madForm.getlist('selectExperiments')
            self.selectParm = self.madForm.getvalue('selectParm')
            self.selectYAxis = self.madForm.getvalue('selectYAxis')


    def createObjects(self):


        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

        # create a MadrigalInstrument object
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

	# get the name of the selected instrument
        self.instrumentName = self.madInstObj.getInstrumentName(self.instrumentKinst)
	
        # create a MadrigalExperiment object
        self.madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)

        # create a MadrigalParameters object
        self.madParmObj = madrigal.data.MadrigalParameters(self.madDBObj)



    def printWaitMessage(self):

        print """<p><center><i>Please wait.  Creation of graphs takes between 10 seconds and 2
        minutes per day of data requested, depending on the instrument..."""
        sys.stdout.flush()



    def createGraph(self):

	# create a list of list of all default files, one list for each date
        import madrigalWeb.madrigalWeb
        self.madWebObj = madrigalWeb.madrigalWeb.MadrigalData(self.madDBObj.getTopLevelUrl())

        self.allExpFiles = []
        for exp in self.experimentList:
            expIdList = self.madWebObj.getExperiments(self.instrumentKinst,
                                                      int(exp[0:4]),
                                                      int(exp[5:7]),
                                                      int(exp[8:]),
                                                      0,
                                                      0,
                                                      0,
                                                      int(exp[0:4]),
                                                      int(exp[5:7]),
                                                      int(exp[8:]),
                                                      23,
                                                      59,
                                                      59)

            self.allExpFiles.append(self.madWebObj.getExperimentFiles(expIdList[0].id))
            if exp == self.experimentList[-1]:
                self.startTime = (int(exp[0:4]),
                                  int(exp[5:7]),
                                  int(exp[8:]),
                                  0,
                                  0,
                                  0)
            if exp == self.experimentList[0]:
                self.endTime = (int(exp[0:4]),
                                int(exp[5:7]),
                                int(exp[8:]),
                                23,
                                59,
                                59)

        # get user info for logging purposes
        # try to get name, email, affiliation from cookie
        cookie = Cookie.SimpleCookie()
        if os.environ.has_key('HTTP_COOKIE'):
            cookie.load(os.environ['HTTP_COOKIE'])
            try:
                self.user_fullname = cookie["user_fullname"].value
                self.user_email = cookie["user_email"].value
                self.user_affiliation = cookie["user_affiliation"].value
            except:
                self.user_fullname = 'Unknown'
                self.user_email = 'Unknown'
                self.user_affiliation ='Unknown'
        else:
            self.user_fullname = 'Unknown'
            self.user_email = 'Unknown'
            self.user_affiliation ='Unknown'

        # create isprint string to plot
        isprintStr = self.createIsprintStr()

        # figure out limits for data
        if isprintStr[:len('WARNING')] != 'WARNING':
            try:
                self.dmin, self.dmax = self.getPlotLimits(isprintStr)
            except:
                isprintStr = 'WARNING: No valid data found in the day requested.'

        self.plotUrl = self.plotIsprintStr(isprintStr)



    def createIsprintStr(self):
        """createIsprintStr returns one string containing all isprint outputs in chronological order.

        If something goes wrong, returns a string that begins WARNING
        """
        import madrigal.data
        
        retStr = ''


        # create comma-delimited parm list
        if self.selectYAxis == 'Altitude':
            yaxis = 'gdalt'
        elif self.selectYAxis == 'Scatter':
            yaxis = 'scatter'
        elif self.selectYAxis == 'Range':
            yaxis = 'range'
        elif self.selectYAxis == 'Latitude':
            yaxis = 'gdlat'
        elif self.selectYAxis == 'Longitude':
            yaxis = 'glon'
        elif self.selectYAxis == 'PACE_Magnetic_Latitude':
            yaxis = 'paclat'
        elif self.selectYAxis == 'PACE_Magnetic_Longitude':
            yaxis = 'paclon'
        else:
            raise 'Illegal yaxis %s' % (str(self.selectYAxis))
            
        if yaxis != 'scatter':
            isprintParmStr = 'ut1,%s,%s' % (yaxis, self.selectParm)
        else:
            isprintParmStr = 'ut1,ut2,%s' % (self.selectParm)

        self.pcolorOkay = False # set to True as soon as some file is found that can derive both yaxis
                                # and self.selectParm, as long as not already scatter plot

        # to increase speed, check whether self.allExpFiles contains the same single file each time,
        # and all the dates are contiguous
        isContiguous = True
        if len(self.allExpFiles) > 1:
            previousDate = None
            for i in range(len(self.allExpFiles)):
                if len(self.allExpFiles[i]) > 1:
                    isContiguous = False
                    break
                if i > 0:
                    if self.allExpFiles[i][0].name != self.allExpFiles[i-1][0].name:
                        isContiguous = False
                        break
                thisDateStr = self.experimentList[i]
                thisDate = datetime.date(int(thisDateStr[0:4]), int(thisDateStr[5:7]), int(thisDateStr[8:]))
                if previousDate != None:
                    timeDelta = previousDate - thisDate

                    if timeDelta.days != 1:
                        isContiguous = False
                        break
                previousDate = thisDate
        else:
            isContiguous = False
              
        
        for i in range(len(self.allExpFiles)-1,-1,-1):
            if not isContiguous:
                thisDate = self.experimentList[i]
                thisFilter = 'date1=%s/%s/%s time1=00:00:00 date2=%s/%s/%s time2=23:59:59' % (thisDate[5:7],
                                                                                              thisDate[8:],
                                                                                              thisDate[0:4],
                                                                                              thisDate[5:7],
                                                                                              thisDate[8:],
                                                                                              thisDate[0:4])
            else:
                # we only need to run isprint once to save time
                if i != len(self.allExpFiles)-1:
                    break
                firstDate = self.experimentList[-1]
                lastDate = self.experimentList[0]
                thisFilter = 'date1=%s/%s/%s time1=00:00:00 date2=%s/%s/%s time2=23:59:59' % (firstDate[5:7],
                                                                                              firstDate[8:],
                                                                                              firstDate[0:4],
                                                                                              lastDate[5:7],
                                                                                              lastDate[8:],
                                                                                              lastDate[0:4])
            if yaxis != 'scatter':
                thisFilter += ' filter=%s,, ' % (yaxis)
            
            for thisFile in self.allExpFiles[i]:
                
                thisFilename = thisFile.name
                thisFileObj = madrigal.data.MadrigalFile(thisFilename, self.madDBObj)
                # lists needed to call getMeasDervBothParmLists
                parmList = (yaxis, self.selectParm)
                ml = []
                dl = []
                al = []
                sl = []
                parmsLists = thisFileObj.getMeasDervBothParmLists(parmList,ml,dl,al,sl)
                
                # check that its worth calling isprint
                if (not isContiguous) and (yaxis != 'scatter') and (yaxis.upper() not in al or self.selectParm.upper() not in al):
                    if thisFile != self.allExpFiles[i][-1]:
                        continue
                else:
                    self.pcolorOkay = True

                thisIsprintString = self.madWebObj.isprint(thisFilename, isprintParmStr, thisFilter,
                                                           self.user_fullname, self.user_email, self.user_affiliation)

                # check that retStr not too long
                if len(thisIsprintString) + len(retStr) > self.__MAX_STR_SIZE:
                    return('WARNING: too much data selected to plot. Please choose fewer days.')
                
                retStr += thisIsprintString

        
        if self.selectYAxis == 'Scatter':
            return(self.truncateIsprint(retStr, (self.__MAX_SCATTER_POINTS)))
        else:
            return(self.truncateIsprint(retStr, (self.__MAX_NUM_TIMES * self.__MAX_NUM_ALT)))


    def getPlotLimits(self, isprintStr):
        """getPlotLimits returns a list of plot limits

        Input: isprintStr - isprint str, with three columns time, yaxis value, data point

        Output: list with two items: 1. data min limit 2. data max limit

        Effects: If no valid y axis data found, sets self.pcolorOkay = False

        Algorithm: sort values, find 10%, 90% values.  Total range is 2.5 times the
        distance from 25% to 75%
        """
        yaxisOkay = False
        dataList = []
        isprintList = isprintStr.split()
        for i in range(len(isprintList)):
            if (i % 3) == 2:
                try:
                    dataList.append(float(isprintList[i]))
                except ValueError:
                    pass
            if (not yaxisOkay) and (i % 3) == 1:
                try:
                    float(isprintList[i])
                    yaxisOkay = True
                except ValueError:
                    pass

        if not yaxisOkay:
           self.pcolorOkay = False 

        dataList.sort()

        d10 = dataList[int(len(dataList)*0.10)]
        d90 = dataList[int(len(dataList)*0.90)]

        dmin = d10 - (d90-d10) * 0.75
        dmax = d90 + (d90-d10) * 0.75

        return((dmin,dmax))
            

    def plotIsprintStr(self, thisIsprintStr):
        """plotIsprintStr creates a plot, and save it to file.

        Inputs:

            thisIsprintStr - isprint string


        Returns: relative url to file (string)
        """
        numDays = 0.5 # plots are deleted after 1/2 day
        self.__tmpDir = 'tempReports'

        if thisIsprintStr[:len('WARNING')] == 'WARNING':
            return thisIsprintStr

        # get startTime and endTime as seconds since 1/1/1950
        import madrigal._Madrec
        import madrigal.ui.madrigalPlot
        thisStartTime = madrigal._Madrec.getUtFromDate(self.startTime[0],
                                                       self.startTime[1],
                                                       self.startTime[2],
                                                       self.startTime[3],
                                                       self.startTime[4],
                                                       self.startTime[5],
                                                       0)
        
        thisEndTime = madrigal._Madrec.getUtFromDate(self.endTime[0],
                                                     self.endTime[1],
                                                     self.endTime[2],
                                                     self.endTime[3],
                                                     self.endTime[4],
                                                     self.endTime[5],
                                                     0)
        
        # create a unique filename
        filename = '%i_%i.png' % (self.instrumentKinst, time.time())

        tmpDir = os.path.join(self.madDBObj.getDocDirPath(), self.__tmpDir)

        fullFilename = os.path.join(tmpDir, filename)

        relUrl = '/' + os.path.join(self.madDBObj.getRelativeTopLevel(),
                                    self.__tmpDir,
                                    filename)

        # create a pcolor plot if desired
        if self.selectYAxis != 'Scatter' and self.pcolorOkay:

            # create plot file
            plotTitle = 'Pcolor plot of %s (%s) for %s' % (self.selectParm,
                                                           self.madParmObj.getParmUnits(self.selectParm),
                                                           self.madInstObj.getInstrumentName(self.instrumentKinst))

                
            try:
                madrigal.ui.madrigalPlot.madPcolorPlot(thisIsprintStr,
                                                       plotTitle,
                                                       'UT Time (from %04i-%02i-%02i %02i:%02i:%02i to %04i-%02i-%02i %02i:%02i:%02i)' % (self.startTime[0],
                                                                                                                                          self.startTime[1],
                                                                                                                                          self.startTime[2],
                                                                                                                                          self.startTime[3],
                                                                                                                                          self.startTime[4],
                                                                                                                                          self.startTime[5],
                                                                                                                                          self.endTime[0],
                                                                                                                                          self.endTime[1],
                                                                                                                                          self.endTime[2],
                                                                                                                                          self.endTime[3],
                                                                                                                                          self.endTime[4],
                                                                                                                                          self.endTime[5]),
                                                       self.selectYAxis,
                                                       fullFilename,
                                                       'wide',
                                                       self.dmin,
                                                       self.dmax,
                                                       True,
                                                       5,
                                                       True,
                                                       thisStartTime,
                                                       thisEndTime,
                                                       True,
                                                       self.__MAX_NUM_TIMES,
                                                       self.__MAX_NUM_ALT)
            except:
                self.pcolorOkay = False

        # create scatter plot if needed
        if self.selectYAxis == 'Scatter' or self.pcolorOkay == False:

            # create plot file
            plotTitle = 'Scatter plot of %s (%s) for %s' % (self.selectParm,
                                                           self.madParmObj.getParmUnits(self.selectParm),
                                                           self.madInstObj.getInstrumentName(self.instrumentKinst))

            # convert isprint string to have only two parameters
            newIsprintStr = ''
            isprintList = thisIsprintStr.split()
            for i in range(len(isprintList)):
                if i % 3 == 0:
                    try:
                        float(isprintList[i+2])
                    except:
                        continue
                    newIsprintStr += isprintList[i] + ' '
                if i % 3 == 2:
                    try:
                        float(isprintList[i])
                    except:
                        continue
                    newIsprintStr += isprintList[i] + '\n'
                
            try:
                madrigal.ui.madrigalPlot.madScatterPlot(newIsprintStr,
                                                        plotTitle,
                                                        'UT Time (from %04i-%02i-%02i %02i:%02i:%02i to %04i-%02i-%02i %02i:%02i:%02i)' % (self.startTime[0],
                                                                                                                                      self.startTime[1],
                                                                                                                                      self.startTime[2],
                                                                                                                                      self.startTime[3],
                                                                                                                                      self.startTime[4],
                                                                                                                                      self.startTime[5],
                                                                                                                                      self.endTime[0],
                                                                                                                                      self.endTime[1],
                                                                                                                                      self.endTime[2],
                                                                                                                                      self.endTime[3],
                                                                                                                                      self.endTime[4],
                                                                                                                                      self.endTime[5]),
                                                        self.selectParm,
                                                        fullFilename,
                                                        'wide',
                                                        True,
                                                        thisStartTime,
                                                        thisEndTime,
                                                        None)
            except:
                return 'WARNING: error creating scatter plot'

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

        return relUrl


    def truncateIsprint(self, isprintText, maxLines):
        """truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText



    def outputHead(self, title):

        print "Content-Type: text/html"
        print                               # blank line, end of headers
        self.scriptHeaders = 1
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        cssAddress = os.path.join(self.madDBObj.getRelativeTopLevel(), 'madrigal.css')
        print '\t<link href="/%s" rel="stylesheet" type="text/css" />' % (cssAddress)
        self.printJavaScript()
        print '</head>'


    def printJavaScript(self):

        print '<script language = "JavaScript">'
        self.printDiffInst()
        self.printDiffExp()
        self.printDiffPlot()
        print '</script>'


    def printDiffInst(self):

        print '\tfunction diffInst(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simpleChooseInstrument.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'

    def printDiffExp(self):

        print '\tfunction diffExp(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simpleChooseExperiments.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printDiffPlot(self):

        print '\tfunction diffPlot(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simplePlotData.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'

    

    def printHiddenElements(self):

        for key in self.madForm.keys():
            if key in ('selectInstrument','selectExperiments'):
                if type(self.madForm.getvalue(key)) == types.ListType:
                    for value in self.madForm.getvalue(key):
                        print '<input type=hidden name=' + str(key) + \
                              ' value=' + value + '>'
                else:
                    print '<input type=hidden name=' + str(key) + \
                          ' value="' + str(cgi.escape(self.madForm.getvalue(key))) + '">'


    def printBody(self):
        print'<table width="100%"  border="1" class="nav_cgi">'
        print'  <tr>'
        print'    <td><div align="center">Simple Madrigal data access - select coordinates and print data...</div></td>'
        print'  </tr>'
        print'</table>'
        print'<form name="form1" method="post" action="simpleChooseExperiments.py">'
        self.printHiddenElements()
        print'<table width="100%"  border="1">'
        print'  <tr>'
        print'    <td valign="top"><p>Selected Instrument: </p>'
        print'        <ul>'
        print'          <li><em>%s</em></li>' % (self.instrumentName)
        print'        </ul>'
        print'        <p>'
        print'          <input type="button" name="diffInstButton" value="Choose different instrument" onClick="diffInst(this.form)">'
        print'      </p></td>'
        print'    <td><p>Selected dates: </p>'
        print'        <ul>'
        
        for index in range(len(self.experimentList)-1,-1,-1):
            print'          <li>%s</li>' % (self.experimentList[index])

        print'        </ul>'
        print'        <p>'
        print'          <input name="timeButton" type="button" id="timeButton3" value="Choose different dates" onClick="diffExp(this.form)">'
        print'      </p></td>'
        print'    <td valign="top"><p>Plotting Parameters: </p>'
        print'        <ul>'
        print'          <li>Parameter: %s</li>' % (self.selectParm)
        print'          <li>Y axis: %s</li>' % (self.selectYAxis)
        print'        </ul>'
        print'        <p>'
        print'          <input type="button" name="diffPlotButton" value="Modify plot" onClick="diffPlot(this.form)">'
        print'      </p></td>'
        print'  </tr>'
        print'</table>'
        print'<h3 align="center">Plot data</h3>'
        
        if self.plotUrl[:len('WARNING')] == 'WARNING':
            print '<center><p>%s</p></center>' % (self.plotUrl)
            print '<center><p><i>This plot cannot be generated.</i></p></center>'
        else:
            if self.selectYAxis != 'Scatter' and self.pcolorOkay == False:
                print '<center><p><i>Unable to create this plot using %s as y axis, so creating scatterplot instead.</i></p></center>'  % (self.selectYAxis)
            print '<center><IMG SRC="%s" BORDER=0 ALT="[Madrigal data plot]"></center>' % (self.plotUrl)
        print'</form>'
        print'<table width="100%"  border="1" class="nav_cgi">'
        print'  <tr>'
        print'    <td><a href="/%s/wt_simple.html">Tutorial</a> on this page</td>' % (self.madDBObj.getRelativeTopLevel())
        print'    <td><a href="accessData.cgi">Return to Access Data page</a></td>'
        print'    <td><a href="/%s">Return to Madrigal home page </a></td>' % (self.madDBObj.getRelativeTopLevel())
        print'    <td><a href="/%s/simpleDifferences.html">How is the simple data access different?</a></td>' % (self.madDBObj.getRelativeTopLevel())
        print'  </tr>'
        print'</table>'
        # print feedback link
        print '<p><hr><i>Please send any comments or suggestions to the <a href="mailto:openmadrigal-users@openmadrigal.org">' + \
	      'Open Madrigal Users Mailing List.</a></i>'
        print'<p>&nbsp;</p>'
   

    def printEndPage(self):
        print '</body></html>'


   
   
        
            

if __name__ == '__main__':

    # Script simplePcolor.py
    # This script only calls the init function of the class simplePcolor
    # All work is done by the init function
    simplePcolor()
