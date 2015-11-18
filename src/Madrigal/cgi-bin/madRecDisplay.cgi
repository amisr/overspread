#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, traceback
import cgi
import time






class MadrigalRecDisplay:
    """MadrigalRecDisplay is the class that produces the Madrigal looker display page.

    Like all my python cgi scripts, MadrigalRecDisplay has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for MadrigalRecDisplay are discussed below.

    The __init__ function then calls createObjects to create whatever python api objects are required
    to complete the script.  It then calls outputHead to output the header section and any required
    javascript.  Finally, __init__ calls a few functions for each of the main sections of the body.

    If any uncaught exception is thrown, its caught by the __init__ try block.  If its an MadrigalError,
    additional information is available.  The catch blocks attempt to display the error message on the screen
    by backing out of of large number of possible tags, which might prevent its display (in any case, the error
    message will always be available in the page source.  The formatted error message is also sent to the email
    address given in the siteTab.txt metadata file.

    Every attempt is made to generate easy to read source html, since it is often an easy starting point for analyzing
    the script.  Table structure is indicated by indentation, as is javascript code structure.

    Allowed values of self.state:
    
    Not used


    All the python cgi scripts maintain state through form elements, either visible on the page, or as hidden elements.
    The names of all form elements used by madRecDisplay are listed below:

    start_lat:		beginning latitude (in degrees).

    stop_lat:		ending latitude (in degrees).

    step_lat:		latitude step (in degrees).

    start_lon:		beginning longitude (in degrees).

    stop_lon:		ending longitude (in degrees).

    step_lon:		longitude step (in degrees).

    start_alt:		beginning altitude (in km).

    stop_alt:		ending altitude (in km).

    step_alt:		altitude step (in km).

    year

    month

    day

    hour

    min

    sec


    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Apr. 9, 2003

    $Id: madRecDisplay.cgi,v 1.7 2009/02/26 21:52:37 brideout Exp $
    """

    # constants
    __scriptName = 'madRecDisplay'

    # states the script can be entered: none

    def __init__(self):
        """__init__ runs the entire MadrigalRecDisplay script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script MadrigalRecDisplay.

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
            
            # determine from form arguments and cookies which script state to use
            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()


            # output html

            #print header
            self.outputHead('Looker Results')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printHeading()

            self.printDisplay()

            self.printEndPage()

            

        except madrigal.admin.MadrigalError, e:
            # handle a MadrigalError

            # back out of any tag so error message appears
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

        # create a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # create a MadrigalReport object
	import madrigal.ui.report
	self.madReportObj = madrigal.ui.report.MadrigalReport(self.madDBObj)

	self.populateParmlist()



    def populateParmlist(self):
        
        # making parm list from all names with key = "pList"
        self.parmList = self.madForm.getlist('pList')
        

    def outputHead(self, title):

        print "Content-Type: text/html"
        print
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        print '\t<style type="text/css">.lb {background: #ADD8E6}</style>'
        print '</head>'



    def printHeading(self):
        print '<h1>Looker output</h1>'

    def printDisplay(self):
        # get date in nice format
        print time.asctime((int(self.madForm.getvalue('year')),
               int(self.madForm.getvalue('month')),
               int(self.madForm.getvalue('day')),
               int(self.madForm.getvalue('hour')),
               int(self.madForm.getvalue('min')),
                int(self.madForm.getvalue('min')),0,0,0))
        print '<hr><pre>'
        
        self.madReportObj.looker(self.parmList,
               float(self.madForm.getvalue('start_lat')),
               float(self.madForm.getvalue('stop_lat')),
               float(self.madForm.getvalue('step_lat')),
               float(self.madForm.getvalue('start_lon')),
               float(self.madForm.getvalue('stop_lon')),
               float(self.madForm.getvalue('step_lon')),
               float(self.madForm.getvalue('start_alt')),
               float(self.madForm.getvalue('stop_alt')),
               float(self.madForm.getvalue('step_alt')),
               int(self.madForm.getvalue('year')),
               int(self.madForm.getvalue('month')),
               int(self.madForm.getvalue('day')),
               int(self.madForm.getvalue('hour')),
               int(self.madForm.getvalue('min')),
               int(self.madForm.getvalue('sec')))
        
        print '<hr></pre>'


    def printEndPage(self):
        print '</body></html>'


   
   
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class MadrigalLogin
    # All work is done by the init function
    MadrigalRecDisplay()
