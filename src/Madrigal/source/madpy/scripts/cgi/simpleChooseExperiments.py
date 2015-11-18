#!PYTHONEXE

import sys, os, os.path
import traceback
import cgi, Cookie
import time, datetime



class simpleChooseExperiments:
    """simpleChooseExperiments is the class that produces the simpleChooseExperiments page.
    
    Like all my python cgi scripts, simpleChooseExperiments has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for simpleChooseExperiments are discussed below.

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

    The names of all form elements used by simpleChooseExperiments are listed below:

    selectInstrument:	local instrument selected - id is instrument id


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 14, 2005

    $Id: simpleChooseExperiments.py,v 1.12 2009/03/24 19:17:35 brideout Exp $
    """

    # constants
    __scriptName = 'simpleChooseExperiments.py'


    def __init__(self):
        """__init__ run the entire simpleChooseExperiments script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script simpleChooseExperiments.

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
            sys.path.append('MADROOT/lib/python')

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
            self.outputHead('Simple Time Choice')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.createDates()

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

        # create web obj to test whether the user is trusted
        import madrigal.ui.web
        self.webObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)


    def createDates(self):

        import madrigal.metadata
        tenYears = datetime.timedelta(365*10)

        # create a list of all expId, where the experiment has a data file
        expWithFileList = []
        fileTabObj = madrigal.metadata.MadrigalMetaFile(self.madDBObj)
        index = 0
        while 1:
            thisExpId = fileTabObj.getExpIdByPosition(index)
            if thisExpId == None:
                break
            thisSecurity = fileTabObj.getAccessByPosition(index)
            if thisSecurity != 0 and not self.webObj.isTrusted():
                index += 1
                continue
            if thisExpId not in expWithFileList:
                expWithFileList.append(thisExpId)
            index += 1

	# create a list of all experiment dates for this instrument, in form
	# ('YYYY-MM-DD')
        self.allExperiments = []
        index = 0
        printMessage = True
        while 1:
            startTime = self.madExpObj.getExpStartDateTimeByPosition(index)
            endTime = self.madExpObj.getExpEndDateTimeByPosition(index)
            kinst = self.madExpObj.getKinstByPosition(index)
            expId = self.madExpObj.getExpIdByPosition(index)
            thisSecurity = self.madExpObj.getSecurityByPosition(index)
            if expId == None:
                break
            if expId not in expWithFileList:
                index += 1
                continue
            if thisSecurity == -1:
                index += 1
                continue
            if thisSecurity in (1,3) and not self.webObj.isTrusted():
                index += 1
                continue
            if kinst != self.instrumentKinst:
                index += 1
                continue
            startDatetime = datetime.datetime(startTime[0],startTime[1],startTime[2],0,0,0)
            endDatetime = datetime.datetime(endTime[0],endTime[1],endTime[2],23,59,59)
            # do not add next day if experiment terminates at midnight
            if endTime[3] == 0 and endTime[4] == 0 and endTime[5] == 0:
                endDatetime -= datetime.timedelta(1)
            
            if endDatetime-startDatetime > tenYears and printMessage:
                # a lot of dates to print - warn user
                print """<p><center><i>Please wait about 10-30 seconds.  This instrument contains
                         a large number of days of data...</i></center></p>"""
                sys.stdout.flush()
                printMessage = False
            

            # loop through all days during experiment and add them
            while startDatetime < endDatetime:
                dateStr = startDatetime.strftime('%Y-%m-%d')
                if dateStr not in self.allExperiments:
                    self.allExperiments.append(dateStr)
                startDatetime += datetime.timedelta(1)
            
            index += 1
            
        self.allExperiments.sort()


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
        self.printPrintData()
        self.printPlotData()
        print '</script>'


    def printDiffInst(self):

        print '\tfunction diffInst(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simpleChooseInstrument.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printPrintData(self):

        print '\tfunction printData(madForm)'
        print '\t{'
        print '\t\tif (madForm.selectExperiments.selectedIndex == -1)'
        print '\t\t{'
        print '\t\t\talert("Please select at least one date.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tmadForm.action="simplePrintData.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printPlotData(self):

        print '\tfunction plotData(madForm)'
        print '\t{'
        print '\t\tif (madForm.selectExperiments.selectedIndex == -1)'
        print '\t\t{'
        print '\t\t\talert("Please select at least one date.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tmadForm.action="simplePlotData.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'
        

    def printHiddenElements(self):

        for key in self.madForm.keys():
            if key in ('selectInstrument',):
                print '<input type=hidden name=' + str(key) + \
                          ' value="' + str(cgi.escape(self.madForm.getvalue(key))) + '">'


    def printBody(self):
        print'<table width="100%"  border="1" class="nav_cgi">'
        print'  <tr>'
        print'    <td><div align="center">Simple Madrigal data access - select dates...</div></td>'
        print'  </tr>'
        print'</table>'
        print'<form name="form1" method="post" action="simpleChooseExperiments.py">'
        self.printHiddenElements()
        print'<p>Selected Instrument: <em>%s</em></p>' % (self.instrumentName)
        print'<p><em>'
        print'    <input type="button" name="diffInstButton" value="Choose different instrument" onClick="diffInst(this.form)">'
        print'</em></p>'
        print'<h3 align="center">Click on  one or more dates you want data or plots from:</h3>'
        print'<p align="center">(Windows users: Hold down <em>Control</em> key to select more than one date) </p>'
        print'  <div align="center">'
        print'    <p>'
        print'      <select name="selectExperiments" size="10" multiple id="selectExperiments">'
        
        # print dates in reverse order
        for i in range(len(self.allExperiments)):
            print'      <option value="%s" >%s' % (self.allExperiments[i],
                                                   self.allExperiments[i])
        print'      </select>'
        print'</p><p>'
        print'      <input type="button" name="printDataButton" value="Print data" onClick="printData(this.form)">'
        print'      &nbsp;&nbsp;'
        print'      <input type="submit" name="plotDataButton" value="Plot data" onClick="plotData(this.form)">'
        print'      &nbsp;&nbsp;'
        print'    </p>'
        print'  </div>'
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

    # Script simpleChooseExperiments.py
    # This script only calls the init function of the class simpleChooseExperiments
    # All work is done by the init function
    simpleChooseExperiments()
