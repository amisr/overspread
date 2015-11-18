#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, os.path
import traceback
import cgi, Cookie
import time



class simpleChooseInstrument:
    """simpleChooseInstrument is the class that produces the simpleChooseInstrument page.
    
    Like all my python cgi scripts, simpleChooseInstrument has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for simpleChooseInstrument are discussed below.

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

    The names of all form elements used by simpleChooseInstrument are listed below:

    selectInstument:	list of local instruments to choose from - id is instrument id


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 14, 2005

    $Id: simpleChooseInstrument.py,v 1.8 2009/03/17 13:20:56 brideout Exp $
    """

    # constants
    __scriptName = 'simpleChooseInstrument.py'


    def __init__(self):
        """__init__ run the entire simpleChooseInstrument script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script simpleChooseInstrument.

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
            
            # create needed Madrigal objects
            self.createObjects()

            # output html

            #print header
            self.outputHead('Simple Instrument Choice')

            #print body tag
            print self.madDBObj.getHtmlStyle()

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

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/html"
                print
                
            print errStr + '<BR>'

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('<html>\n' + errStr + '</html>',
                                 'Error running ' + self.__scriptName)


            print '<br><b>Your system administrator has been notified.<b>'

        # end __init__


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

        # create a MadrigalInstrument object
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

        # create a MadrigalSite object
        self.madSiteObj = madrigal.metadata.MadrigalSite(self.madDBObj)
        self.siteName = self.madSiteObj.getSiteName(self.madDBObj.getSiteID())

        # create web obj to test whether the user is trusted
        import madrigal.ui.web
        self.webObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)

	# create a list of all instruments for which there is data
        self.allInst, categoryDict = \
            self.madInstObj.getOrderedInstrumentListWithData(self.webObj.isTrusted(), True,
                                                             allowArchive=True)

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
        self.printSubmit()
        print '</script>'


    def printSubmit(self):

        print '\tfunction submit(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simpleChooseExperiments.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printBody(self):
        print'<table width="100%"  border="1" class="nav_cgi">'
        print'  <tr>'
        print'    <td><div align="center">Simple Madrigal data access - select an instrument... </div></td>'
        print'  </tr>'
        print'</table>'
        print'<p>&nbsp;</p>'
        print'<h3 align="center">Click on  the instrument in the %s database you want to get data or plots from:</h3>' % (self.siteName)
        print'<form name="form1" method="post" action="simpleChooseExperiments.py">'
        print'  <div align="center">'
        print'    <p>'
        print'      <select name="selectInstrument" size="10" id="selectInstrument" onChange="submit(this.form)">'
        print'         <option value="0" selected>Select an instrument'
        
        # loop through all instuments, finding ones with local data
        for inst in self.allInst:
            print'         <option value="%i">%s %i-%i' % (inst[5], inst[0], inst[1], inst[2])

        print'      </select>'
        print'    </p>'
        print'  </div>'
        print'</form>'
        print'<p>&nbsp;</p>'
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

    # Script simpleChooseInstrument.py
    # This script only calls the init function of the class simpleChooseInstrument
    # All work is done by the init function
    simpleChooseInstrument()
