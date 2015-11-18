#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, traceback
import cgi, Cookie
import time, datetime
import types





class plotInstrumentsSelect:
    """plotInstrumentsSelect is the class that produces the plotInstrumentsSelect page.
    
    plotInstrumentsSelect has the following structure:  the entire cgi is contained in one class, with a
    main function at the end which serves simply to call the __init__  function of the class.  This
    __init__ function is responsible for calling all other class methods. It is made up of a single
    try block, with the purpose of reporting all exceptions in well-formatted html to both the user
    and the administrator. The __init__ function first makes sure the pythonlib can be found.  It
    then calls setScriptState to determine from any cgi arguments what the script is supposed to do.
    The script state is always set in self.state.  The particular values allowed for plotInstrumentsSelect
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

    Allowed values of self.state:

    'default':          Default when page is called.

    'reload':           User is reloading the page.


    All the python cgi scripts maintain state through form elements, either visible on the page, or as hidden elements.
    The names of all form elements used by madSearch are listed below:

    stationName - instrument id of instruments to be plotted.  More than one allowed.

    startYear, startMonth, startDay, startHour, startMin, startSec - starting time of plot to create

    endYear, endMonth, endDay, endHour, endMin, endSec - ending time of plot to create

    plotType - either "scatter" or "pcolor"
                           
    
    Change history:

    Written by "Bill Rideout":mailto:brideout@haystack.mit.edu  July 6, 2005

    $Id: plotInstrumentsSelect.py,v 1.8 2009/03/17 13:19:40 brideout Exp $
    """

    
    # constants
    __scriptName = 'plotInstrumentsSelect'
    
    # states the script can be entered

    def __init__(self):
        """__init__ run the entire plotInstrumentsSelect script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script plotInstrumentsSelect.

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
            self.outputHead('Select instruments to plot')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printHeading()

            self.printForm()

            

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

        if not self.madForm.has_key('stationName'):
            self.state = 'default'
        else:
            self.state = 'reload'


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()
        self.topLevelUrl = self.madDBObj.getTopLevelUrl()

        # create object for MadrigalInstrument
        self.madInstrumentObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

        # create a MadrigalWeb object
        import madrigal.ui.web
        self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)

        
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
        self.printTrim()
        self.printIsInt()
        self.printInRange()
        self.printIsEmpty()
        self.printValidateDates()
        self.printValidateFilter()
        self.printListExperiments()
        self.printGetParameters()
        print '</script>'


    def printTrim(self):
        
        print '\tfunction trim(strText)'
        print '\t{'
        print '\t\t// this will get rid of leading spaces'
        print '\t\twhile (strText.substring(0,1) == \' \')'
        print '\t\t\tstrText = strText.substring(1, strText.length);'
        print '\t\t// this will get rid of trailing spaces'
        print '\t\twhile (strText.substring(strText.length-1,strText.length) == \' \')'
        print '\t\t\tstrText = strText.substring(0, strText.length-1);'
        print '\t\treturn strText;'
        print '\t}\n'

    def printIsInt(self):
        
        print '\tfunction isInt(textObj)'
        print '\t{'
        print '\t\tvar newValue = trim(textObj.value);'
        print '\t\tvar newLength = newValue.length;'
        print '\t\tfor (var i = 0; i != newLength; i++)'
        print '\t\t{'
        print '\t\t\taChar = newValue.substring(i,i+1);'
        print '\t\t\tif(aChar < "0" || aChar > "9")'
        print '\t\t\t{'
        print '\t\t\t\treturn false;'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\treturn true;'
        print '\t}\n'


    def printInRange(self):
        
        print '\tfunction inRange(num, lowerLimit, upperLimit)'
        print '\t{'
        print '\t\tif(num >= lowerLimit && num <= upperLimit)'
        print '\t\t{'
        print '\t\t\treturn true;'
        print '\t\t}'
        print '\t\tif(typeof num != "number")'
        print '\t\t{'
        print '\t\t\treturn true;'
        print '\t\t}'
        print '\t\treturn false;'
        print '\t}\n'


    def printIsEmpty(self):
        
        print '\tfunction isEmpty(textObj)'
        print '\t{'
        print '\t\tvar newValue = trim(textObj.value);'
        print '\t\tvar newLength = newValue.length;'
        print '\t\tif(newLength == 0)'
        print '\t\t{'
        print '\t\t\treturn true;'
        print '\t\t}'
        print '\t\treturn false;'
        print '\t}\n'

    def printValidateDates(self):
        # check that all dates are valid, and second is later than first
        print '\tfunction validateDates(madForm)'
        print '\t{'
        print '\t\ttry'
        print '\t\t{'
        print '\t\t\tvar firstDate = new Date();'
        print '\t\t\tfirstDate.setFullYear(madForm.startYear.value);'
        print '\t\t\tfirstDate.setMonth(madForm.startMonth.value - 1);'
        print '\t\t\tfirstDate.setDate(madForm.startDay.value);'
        print '\t\t\tfirstDate.setHours(madForm.startHour.value);'
        print '\t\t\tfirstDate.setMinutes(madForm.startMin.value);'
        print '\t\t\tfirstDate.setSeconds(madForm.startSec.value);'
        print '\t\t\tif (firstDate.getMonth() != (madForm.startMonth.value - 1))'
        print '\t\t\t{'
        print '\t\t\t\talert("Illegal starting date");'
        print '\t\t\t\treturn false;'
        print '\t\t\t}'
        print '\t\t}catch(e){'
        print '\t\t\talert("Illegal starting date");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\ttry'
        print '\t\t{'
        print '\t\t\tvar lastDate = new Date();'
        print '\t\t\tlastDate.setFullYear(madForm.endYear.value);'
        print '\t\t\tlastDate.setMonth(madForm.endMonth.value - 1);'
        print '\t\t\tlastDate.setDate(madForm.endDay.value);'
        print '\t\t\tlastDate.setHours(madForm.endHour.value);'
        print '\t\t\tlastDate.setMinutes(madForm.endMin.value);'
        print '\t\t\tlastDate.setSeconds(madForm.endSec.value);'
        print '\t\t\tif (lastDate.getMonth() != (madForm.endMonth.value - 1))'
        print '\t\t\t{'
        print '\t\t\t\talert("Illegal ending date");'
        print '\t\t\t\treturn false;'
        print '\t\t\t}'
        print '\t\t}catch(e){'
        print '\t\t\talert("Illegal ending date");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (parseFloat(firstDate.getTime()) > parseFloat(lastDate.getTime()))'
        print '\t\t{'
        print '\t\t\talert("Start date after end date! " + firstDate.toString() + " after " + lastDate.toString());'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\treturn true;'
        print '\t}\n'

    def printValidateFilter(self):
        
        # check that all filter values are valid via javascript before form is submitted
        print '\tfunction validateFilter(madForm)'
        print '\t{'
        print '\t\tif (!isInt(madForm.startYear) || isEmpty(madForm.startYear))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start year.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.startYear.value), 1950, 3000) && madForm.startYear.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start year.  Must not be less than 1950.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.startMonth) || isEmpty(madForm.startMonth))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start month.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.startMonth.value), 1, 12) && madForm.startMonth.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start month.  Must be between 1 and 12.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.startDay) || isEmpty(madForm.startDay))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start day.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.startDay.value), 1, 31) && madForm.startDay.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start day.  Must be between 1 and 31.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.startHour) || isEmpty(madForm.startHour))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start hour.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.startHour.value), 0, 24) && madForm.startHour.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start hour.  Must be between 0 and 24.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.startMin) || isEmpty(madForm.startMin))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start minute.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.startMin.value), 0, 59) && madForm.startMin.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start minute.  Must be between 0 and 59.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.startSec) || isEmpty(madForm.startSec))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start second.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.startSec.value), 0, 61) && madForm.startSec.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start second.  Must be between 0 and 61.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.endYear) || isEmpty(madForm.endYear))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end year.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.endYear.value), 1950, 3000) && madForm.endYear.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end year.  Must not be less than 1950.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.endMonth) || isEmpty(madForm.endMonth))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end month.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.endMonth.value), 1, 12) && madForm.endMonth.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end month.  Must be between 1 and 12.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.endDay) || isEmpty(madForm.endDay))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end day.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.endDay.value), 1, 31) && madForm.endDay.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end day.  Must be between 1 and 31.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.endHour) || isEmpty(madForm.endHour))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end hour.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.endHour.value), 0, 24) && madForm.endHour.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end hour.  Must be between 0 and 24.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.endMin) || isEmpty(madForm.endMin))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end minute.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.endMin.value), 0, 59) && madForm.endMin.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end minute.  Must be between 0 and 59.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.endSec) || isEmpty(madForm.endSec))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end second.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.endSec.value), 0, 61) && madForm.endSec.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end second.  Must be between 0 and 61.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!validateDates(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (madForm.stationName.selectedIndex == -1)'
        print '\t\t{'
        print '\t\t\talert("Please select at least one instrument.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\treturn true;'
        print '\t}\n'

    
    def printListExperiments(self):
    
        print '\tfunction listExperiments(madForm)'
        print '\t{'
        print '\t\tif (!validateFilter(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tmadForm.action="listExperiments.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printGetParameters(self):
    
        print '\tfunction getParameters(madForm)'
        print '\t{'
        print '\t\tif (!validateFilter(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tmadForm.action="plotParametersSelect.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printHeading(self):
        
        print '<center><p></p><h1><b>Add new plot(s)<br></b></h1></center>'


    def printForm(self):
        
        print '<form action=plotParametersSelect.py method=get name=form1>'
        self.printIntroduction()
        self.printHiddenElements()
        self.printFormTable()
        self.printEndPage()


    def printIntroduction(self):
        print """<center><p>This page allows you to add plots to make a collection
                 of stacked plots, all with the same time scale.&nbsp; If you want to
                 add a series of plots with the same parameters from more than one
                 instrument, just select more than one instrument.&nbsp; If you want to
                 stack plots with different parameters, just add one plot at a
                 time.&nbsp; Each time you add a new plot, you will have the chance to
                 view the ones you have created, and either add more, or start over.</p>
                 <table width="80%%"><tr>
                 <td>Return to <a href="/%s">main Madrigal page</a></td>
                 <td><a href="/%s/wt_plotData.html">Tutorial</a> on this page</td>
                 </tr></table><center>
        """ % (self.madDBObj.getRelativeTopLevel(),self.madDBObj.getRelativeTopLevel())

    def printFormTable(self):
        print """<br><p>
                 <table style="width: 815px; height: 521px;" border="0" cellpadding="10">
                 <tbody>
                 <tr valign="top">
                    <td style="width: 50%; text-align: center;">
                       <p><b><font size="5">Select instrument(s)</font></b></p>
                    </td>
                    <td align="center" valign="top" width="50%">
                       <p><b><font size="5">Select time period<br>
                       </font></b></p>
                    </td>
                 </tr>
                 """
        self.printInstrumentTable()
        self.printTimeSelection()
        self.printPlotSelection()
        print '</tbody></table>'


    def printInstrumentTable(self):
        print """<tr valign="top"><td align="center" width="50%">
                 <p class="hanging-indent">
                 <select name="stationName" size="13" multiple="multiple">"""
        # print station names
        InstrumentList, categoryDict = \
            self.madInstrumentObj.getOrderedInstrumentListWithData(self.madWebObj.isTrusted(),
                                                                   localOnly=True, allowArchive=True)

        # while search page is loaded initially, print list of instruments and highlight "All Instruments"
        if self.state == 'default':
            for item in InstrumentList:
                print '<OPTION VALUE="%i">%s %i-%i' % (item[5], item[0], item[1], item[2])
        else:
            # reloading -- while an instrument has been chosen, get the numeric value of that instrument
            stationNameStr = self.madForm.getvalue("stationName")
            # checking for lists or strings and changing as appropriate
            if type(stationNameStr) == types.ListType:
                stationNameList = stationNameStr
            elif type(stationNameStr) == types.StringType:
                stationNameList = [stationNameStr]
            else:
                stationNameList = []
            stationNameIntList = self.madWebObj.getIntListFromStrList(stationNameList)
            # searching for selected instruments
            for item in InstrumentList:         
                if item[5] in stationNameIntList:
                    #highlights only selected instruments
                    print '<OPTION VALUE="%i" SELECTED>%s %i-%i' % (item[5], item[0], item[1], item[2])
                else:
                    print '<OPTION VALUE="%i">%s %i-%i' % (item[5], item[0], item[1], item[2])
                    
        print '</select></p></td>'

    def printTimeSelection(self):
        print """<td style="text-align: left;" valign="top" width="50%">
                 <div style="text-align: left;"><br>
                 <input name="expList" class="lb"
                 value="Show ALL times data available for these instruments"
                 onclick="listExperiments(this.form)" type="button"><br>
                 </div>
                 <p>Select time range to plot data:</p>
                    <table style="width: 100%; text-align: left;" border="1"
                        cellpadding="2" cellspacing="2">
                    <tbody>
                       <tr>"""
        # set values to insert
        if self.state == 'default':
            d = datetime.datetime.now()
            startYear = d.year
            startMonth = d.month
            startDay = d.day
            startHour = 0
            startMin = 0
            startSec = 0
            endYear = d.year
            endMonth = d.month
            endDay = d.day
            endHour = 23
            endMin = 59
            endSec = 59
        else:
            startYear = int(self.madForm.getvalue('startYear'))
            startMonth = int(self.madForm.getvalue('startMonth'))
            startDay = int(self.madForm.getvalue('startDay'))
            startHour = int(self.madForm.getvalue('startHour'))
            startMin = int(self.madForm.getvalue('startMin'))
            startSec = int(self.madForm.getvalue('startSec'))
            endYear = int(self.madForm.getvalue('endYear'))
            endMonth = int(self.madForm.getvalue('endMonth'))
            endDay = int(self.madForm.getvalue('endDay'))
            endHour = int(self.madForm.getvalue('endHour'))
            endMin = int(self.madForm.getvalue('endMin'))
            endSec = int(self.madForm.getvalue('endSec'))

        print """<td style="vertical-align: top;">Start:<br>
                 </td>
                 <td style="vertical-align: top;">Year<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="4"
                 size="4" name="startYear"></td>
                 <td style="vertical-align: top;">Month<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="startMonth"></td>
                 <td style="vertical-align: top;">Day<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="startDay"></td>
                 </tr>
                 <tr>
                 <td style="vertical-align: top;"><br>
                 </td>
                 <td style="vertical-align: top;">Hour<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="startHour"></td>
                 <td style="vertical-align: top;">Min<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="startMin"></td>
                 <td style="vertical-align: top;">Sec<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="startSec"></td>
                 </tr>
                 <tr>
                 <td style="vertical-align: top;">End<br>
                 </td>
                 <td style="vertical-align: top;">Year<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="4"
                 size="4" name="endYear"></td>
                 <td style="vertical-align: top;">Month<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="endMonth"></td>
                 <td style="vertical-align: top;">Day<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="endDay"></td>
                 </tr>
                 <tr>
                 <td style="vertical-align: top;"><br>
                 </td>
                 <td style="vertical-align: top;">Hour<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="endHour"></td>
                 <td style="vertical-align: top;">Min<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="endMin"></td>
                 <td style="vertical-align: top;">Sec<br>
                 </td>
                 <td style="vertical-align: top;"><input value="%i" maxlength="2"
                 size="2" name="endSec"></td>
                 </tr>
                 </tbody>
                 </table>
                 <br>
                 </td>
                 </tr>""" % (startYear, startMonth, startDay,
                             startHour, startMin, startSec,
                             endYear, endMonth, endDay,
                             endHour, endMin, endSec)
        
    def printPlotSelection(self):
        # first, determine which plot to select
        if self.state == 'default':
            select = 'scatter'
        else:
            if self.madForm.getvalue('plotType') == 'scatter':
                select = 'scatter'
            else:
                select = 'pcolor'
        if select == 'scatter':
            checkedStr1 = 'checked="checked"'
            checkedStr2 = ''
        else:
            checkedStr1 = ''
            checkedStr2 = 'checked="checked"'
        htmlStr = """</tr><tr valign="top">
                 <td align="center" width="50%%">
                    <input %s name="plotType" value="scatter"
                      type="radio">&nbsp;&nbsp; Scatter plot&nbsp; <br>
                 <img alt="Scatter plot" src="%s/icons/scatter.png"
                    style="width: 216px; height: 108px;"><br>
                 <br>
                 Use a scatter plot to plot a single parameter versus time.<br>
                 </td>
                 <td align="center" valign="top" width="50%%">
                     <input %s name="plotType" value="pcolor"
                       type="radio">&nbsp;&nbsp; Pcolor plot <br>
                       &nbsp; <img alt="Pcolor plot" src="%s/icons/pcolor.png"
                       style="width: 216px; height: 126px;"><br>
                 Use a pcolor plot to plot altitude versus time for a given parameter.<br>
                 </td>
            </tr>""" % (checkedStr1, self.topLevelUrl, checkedStr2, self.topLevelUrl)
        print htmlStr
        
    def printHiddenElements(self):
        # now print all received post elements not used in this form as hidden elements
        print '<input type=hidden name=callingpage value=plotInstrumentsSelect.py>'
        print '<input type=hidden name=local value=1>'
        for key in self.madForm.keys():
            if str(key) in ('stationName', 'startYear', 'startMonth', 'startDay',
                            'startHour', 'startMin', 'startSec',
                            'endYear', 'endMonth', 'endDay',
                            'endHour', 'endMin', 'endSec',
                            'callingpage', 'plotType', 'local'):
                continue
            if type(self.madForm.getvalue(key)) == types.ListType:
                for value in self.madForm.getvalue(key):
                    print '<input type=hidden name=' + str(key) + \
                          ' value=' + value + '>'
            else:
                print '<input type=hidden name=' + str(key) + \
                      ' value="' + cgi.escape(str(self.madForm.getvalue(key))) + '">'
                


                
    def printEndPage(self):
        print """<br></p><center><p>
                 <input value="Clear" type="reset">
                 <input name="plot" class="lb"
                     value="Choose parameter to plot" onclick="getParameters(this.form)" type="button"></p>
                 </center>
                 <p></p>
                 <hr><i>Please send any comments or suggestions to the <a
                     href="mailto:openmadrigal-users@openmadrigal.org">Open Madrigal Users
                     Mailing List.</a></i>"""

        print '</form></body></html>'       # end body, form, and html 


    
          

if __name__ == '__main__':

    # Script plotInstrumentsSelect
    # This script only calls the init function of the class plotInstrumentsSelect
    # All work is done by the init function
    plotInstrumentsSelect()
