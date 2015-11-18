#!PYTHONEXE

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

class listInstruments:
    """listInstruments is the class that produces the listInstruments page.
    
    listInstruments has the following structure:  the entire cgi is contained in one class, with a
    main function at the end which serves simply to call the __init__  function of the class.  This
    __init__ function is responsible for calling all other class methods. It is made up of a single
    try block, with the purpose of reporting all exceptions in well-formatted html to both the user
    and the administrator. The __init__ function first makes sure the pythonlib can be found.  It
    then calls setScriptState to determine from any cgi arguments what the script is supposed to do.
    The script state is always set in self.state.  The particular values allowed for listInstruments
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

    stationName - instrument id of instruments whose data is to be listed.  More than one allowed.

    local - if local, show only local Experiments
  

    Change history:

    Written by "Bill Rideout":mailto:brideout@haystack.mit.edu  July 11, 2005

    $Id: listExperiments.py,v 1.6 2008/11/18 20:13:13 brideout Exp $
    """

    # constants
    __scriptName = 'listInstruments.py'


   
    def __init__(self):
        """__init__ run the entire listInstruments script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script listInstruments.

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
            traceback
            sys.exit(0)
	    
        try:

            # set flag as to whether script headers have been written
            self.scriptHeaders = 0
            
            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()
            
            # output html

            #print header
            self.outputHead('List Experiments')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printHeading()

            self.printDescription()

            self.printHiddenElements()

            self.printButtons()

            self.printExperimentList()

            self.printButtons()

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
  

    def createObjects(self):

        # all states require a MadrigalDB object
        self.madDBObj = madrigal.metadata.MadrigalDB()


        # create object for MadrigalInstrument
        self.madInstrumentObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

        # create object for MadrigalSite
        self.madSiteObj = madrigal.metadata.MadrigalSite(self.madDBObj)

        # create list of selected kinst values
        self.madKinstList = []
        stationName = self.madForm.getvalue("stationName")
        # checking for lists or strings and changing as appropriate
        if type(stationName) == types.ListType:
            stationNameList = stationName
        else:
            stationNameList = [stationName]
            
        for item in stationNameList:
            if item == None:
                continue
            self.madKinstList.append(int(item))

        self.isLocal = False
        if self.madForm.getfirst('local') != None:
            self.isLocal = True

        # create madrigalWeb.madrigalWeb object
        self.remoteMadWeb = madrigalWeb.madrigalWeb.MadrigalData(self.madDBObj.getTopLevelUrl())


        # create MadrigalExperiments object for all Madrigal sites
        now = datetime.datetime.utcnow()
        if self.isLocal:
            localFlag = 1
        else:
            localFlag = 0
            
        self.madExperimentsObj = self.remoteMadWeb.getExperiments(0,1950,1,1,0,0,0,
                                                                  now.year, now.month, now.day,
                                                                  now.hour, now.minute, now.second,
                                                                  localFlag)
        self.madExperimentsObj.sort()
        
        # check whether "All Instruments" selected
        if 0 in self.madKinstList:
            # reset self.madKinstList to include all instruments with data
            self.madKinstList = []
            for exp in self.madExperimentsObj:
                if exp.instcode not in self.madKinstList:
                    self.madKinstList.append(exp.instcode)

        # create a MadrigalWeb object
        self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)
        self.madWebFormatObj = madrigal.ui.web.MadrigalWebFormat()

        # find out where to return to
        self.callBack = 'plotInstrumentsSelect.py'
        try:
            callingpage = self.madForm.getvalue("callingpage")
            if callingpage in ('madInvent.cgi',):
                self.callBack = callingpage
        except:
            pass

        
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
        self.printClosePage()
        print '</script>'


    def printClosePage(self):

        print '\tfunction ClosePage(madForm)'
        print '\t{'
        print '\t\tmadForm.action="%s"' % (self.callBack)
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'

    # end javascript
    

    def printHeading(self):

        print '<center><h2>The following is a listing of ***all*** experiments for the selected instruments</h2></center>'
        print 'You may have reached this page because no experiments were found.  If so, please '
        print 'check the list below to find out when experiments are available, and then hit the Return button<br>'

       
    def printDescription(self):

        print '<form method=get enctype="application/x-www-form-urlencoded">'          #begin form>'
        
        
    def printHiddenElements(self):

        # now print all received post elements from plotInstrumentsSelect.py as hidden elements
        # if form just loaded
        print '<input type=hidden name=callingpage value=listInstruments.py>'
        for key in self.madForm.keys():
            if self.callBack != 'plotInstrumentsSelect.py':
                continue
            if key in ('callingpage'):
                continue
            if type(self.madForm.getvalue(key)) == types.ListType:
                for value in self.madForm.getvalue(key):
                    print '<input type=hidden name=' + str(key) + \
                          ' value=' + value + '>'
            else:
                print '<input type=hidden name=' + str(key) + \
                          ' value="' + str(cgi.escape(self.madForm.getvalue(key))) + '">'



    def printExperimentList(self):

        for kinst in self.madKinstList:
            print '<h3>Experiment listing for %s</h3>' % (self.madInstrumentObj.getInstrumentName(kinst))
            print '\t<ul>'
            position = 0

            for exp in self.madExperimentsObj:
                if exp.instcode != kinst:
                    continue
                
                if self.isLocal:
                    url = exp.url.split('/madtoc/')[1]
                    link = '%s/madExperiment.cgi?exp=%s&displayLevel=0&expTitle=%s' % (self.madDBObj.getCGIHomeBase(),
                                                                                       url,
                                                                                       exp.name.replace(' ', '%20'))
                                                                                       
                    print '\t<li>Start: %04i-%02i-%02i End: %04i-%02i-%02i ' % (exp.startyear,
                                                                                    exp.startmonth,
                                                                                    exp.startday,
                                                                                    exp.endyear,
                                                                                    exp.endmonth,
                                                                                    exp.endday)
                    print '<a href="%s">Link</a></li>' % (link)
                else:
                    print '\t<li>Start: %04i-%02i-%02i End: %04i-%02i-%02i - Site: %s </li>' % (exp.startyear,
                                                                                                exp.startmonth,
                                                                                                exp.startday,
                                                                                                exp.endyear,
                                                                                                exp.endmonth,
                                                                                                exp.endday,
                                                                                                exp.sitename)
                                                                                                         
            print '\t</ul>'
        
    
    def printEndPage(self):

        print '</form></body></html>'       # end form, body, and html 


    def printButtons(self):

        print '<center><p>'
        print '<input class=lb type=button value="Return" onClick=ClosePage(this.form)>'
        print '</p></center>'




if __name__ == '__main__':

    # Script madParmList
    # This script only calls the init function of the class listInstruments
    # All work is done by the init function
    listInstruments()
