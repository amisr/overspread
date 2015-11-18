#!PYTHONEXE

import sys, os, os.path
import traceback
import cgi, Cookie
import time



class MadrigalDisplayCatHead:
    """MadrigalDisplayCatHead is the class that produces the MadrigalDispalyCatHead page.
    
    Like all my python cgi scripts, MadrigalDisplayCatHead has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for MadrigalDisplayCatHead are discussed below.

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

    The names of all form elements used by MadrigalDisplayCatHead are listed below:

    fileName:		gives the base name of the file being analyzed

    expName:	        gives the YYYY/3 letter instrument code/DDmonYY path the the file from madroot/experiments.

    expTitle:           gives the experiment name

    displayLevel:       used to return to madExperiments.cgi script


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Mar. 10, 2005

    $Id: displayCatHead.py,v 1.6 2008/07/25 18:57:19 brideout Exp $
    """

    # constants
    __scriptName = 'displayCatHead.py'


    def __init__(self):
        """__init__ run the entire MadrigalDisplayCatHead script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script MadrigalDisplayCatHead.

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
            self.outputHead('Display Catalog and Header Records')

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

	if not self.madForm.has_key('fileName') or \
           not self.madForm.has_key('expName') or \
           not self.madForm.has_key('displayLevel'):
            if  self.scriptHeaders == 0:
                print "Content-Type: text/html\n"

            print '<h3> This cgi script was called without the proper arguments.</h3>' + \
                  'Since this script uses post, you cannot bookmark this page. ' + \
                  'Please contact your site administrator with any questions.'

            sys.exit(0)
            
        else:
            self.fileName = self.madForm.getvalue('fileName')
            self.expName = self.madForm.getvalue('expName')
            if self.madForm.has_key('expTitle'):
                self.expTitle = self.madForm.getvalue('expTitle')
            else:
                self.expTitle = 'Unknown'
            self.displayLevel = self.madForm.getvalue('displayLevel')


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # if madroot not set, set it now
	if os.environ.get('MAD' + 'ROOT') == None:
	    os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

        # create a MadrigalWeb object
	import madrigal.ui.web
	self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)

        fullFilePath = os.path.join(self.madDBObj.getMadroot(), 'experiments', self.expName, self.fileName)
        self.madFile = madrigal.data.MadrigalFile(fullFilePath, self.madWebObj)

    def outputHead(self, title):

        print "Content-Type: text/html"
        print                               # blank line, end of headers
        self.scriptHeaders = 1
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        print '\t<style type="text/css">.lb {background: #ADD8E6}</style>'
        print '</head>'


    def printBody(self):
        cgiExpTitle = self.madWebObj.getCgiString(self.expTitle)
        cgiExpName = self.expName.replace('/', '%2f')
        returnLink = 'madExperiment.cgi?exp=%s&displayLevel=%s&expTitle=%s' % (cgiExpName, self.displayLevel, cgiExpTitle)

        print '<h3>Catalog/Header information for %s</h3>' % (self.fileName)
        print '<ul>'
        print '    <li>Experiment title: %s</li>' % (self.expTitle)
        print '    <li>Experiment dir: %s</li>' % (self.expName)
        print '    <li><a href="%s">Return to experiment selection page</a></li>' % (returnLink)
        print '</ul>'
        print '<pre>'
        print self.madFile.getCatalogHeaderStr()
        print '</pre>'
   

    def printEndPage(self):
        print '</body></html>'


   
   
        
            

if __name__ == '__main__':

    # Script displayDatHead.py
    # This script only calls the init function of the class MadrigalLogin
    # All work is done by the init function
    MadrigalDisplayCatHead()
