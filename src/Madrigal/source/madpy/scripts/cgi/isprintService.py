#!PYTHONEXE

import sys, os, traceback
import cgi, Cookie
import time

# $Id: isprintService.py,v 1.9 2008/07/25 18:57:18 brideout Exp $




class IsprintService:
    """IsprintService is the class that produces allows remote access to "isprint":../../ug_commandLine.html#isprint functionality.
    
    Like all my python cgi scripts, IsprintService has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    text to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to validate the the cgi arguments, which are simply the
    arguments for the isprint command.

    If any uncaught exception is thrown, its caught by the __init__ try block.  If its an MadrigalError,
    additional information is available.  The catch blocks attempt to display the error message on the screen
    by backing out of of large number of possible tags, which might prevent its display (in any case, the error
    message will always be available in the page source.  The formatted error message is also sent to the email
    address given in the siteTab.txt metadata file.

    This script is not meant to be used directly by a user, and thus is named Service.  It is meant to be used by
    scripting languages such as Matlab that want to call isprint via the web

    Input cgi arguments (see "isprint":../../ug_commandLine.html#isprint command for details):
    
    'file':	        The file to be analyzed by isprint.

    'parms':	        Space delimited list of requested parameters.

    'filters':          Space delimited list of filters desired, as in isprint command

    'header':           y for headers, n for no header.  Defaults to no header

    'user_fullname'     user name - if given, allows logging, but not required

    'user_email'        user email - if given, allows logging, but not required

    'user_affiliation'  user affiliation - if given, allows logging, but not required

    Returns "isprint":../../ug_commandLine.html#isprint output summary.  If error, returns error description.


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 13, 2003
    """

    # constants
    __scriptName = 'isprintService'
    

    def __init__(self):
        """__init__ run the entire IsprintService script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs isprint data as a service.

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


            # output isprint
            self.outputReport()

            

        except madrigal.admin.MadrigalError, e:
            # handle a MadrigalError

                
            errStr = 'Error occurred in script ' + self.__scriptName + '.'

            
            err = traceback.format_exception(sys.exc_info()[0],
                                             sys.exc_info()[1],
                                             sys.exc_info()[2])

            for errItem in err:
                errStr = errStr + '\n' + str(errItem)

        
            # add info about called form:
            if self.madForm != None:
                errStr = errStr + 'Form elements\n'
                for key in self.madForm.keys():
                    errStr = errStr + '\n' + str(key)
                    errStr = errStr + ' = ' + str(self.madForm.getvalue(key))

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/plain"
                print
                
            print errStr

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('\n' + errStr,
                                 'Error running ' + self.__scriptName)


            print 'Your system administrator has been notified.'

        except SystemExit:
            sys.exit(0)

        except:
            # handle a normal error
            
                
            errStr = 'Error occurred in script ' + self.__scriptName + '.'

            
            err = traceback.format_exception(sys.exc_info()[0],
                                             sys.exc_info()[1],
                                             sys.exc_info()[2])

            for errItem in err:
                errStr = errStr + '\n' + str(errItem)

        
            # add info about called form:
            if self.madForm != None:
                errStr = errStr + 'Form elements\n'
                for key in self.madForm.keys():
                    errStr = errStr + '\n' + str(key)
                    errStr = errStr + ' = ' + str(self.madForm.getvalue(key))

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/plain"
                print
                
            print errStr

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('\n' + errStr,
                                 'Error running ' + self.__scriptName)


            print 'Your system administrator has been notified.'

        # end __init__


    def setScriptState(self):
        
        #create a form object
        self.madForm = cgi.FieldStorage()

        print "Content-Type: text/plain\n"
        sys.stdout.flush()


        if not self.madForm.has_key('file'):

            print 'This cgi script was called without the proper arguments.\n' + \
                  'Since this script uses post, you cannot bookmark this page. ' + \
                  'Please contact your site administrator with any questions.'

            sys.exit(0)
            
        else:
            self.file = self.madForm.getvalue('file')
            self.parms = self.madForm.getvalue('parms')
            self.filters = self.madForm.getvalue('filters')
            self.header = self.madForm.getvalue('header')

	# try to get info needed for logging, but no error if not available
        self.canLog = False
        try:
            self.user_fullname = self.madForm.getvalue('user_fullname')
            self.user_email = self.madForm.getvalue('user_email')
            self.user_affiliation = self.madForm.getvalue('user_affiliation')
            self.canLog = True
        except:
            pass



    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

	# create a MadrigalWeb object and log if possible
        if self.canLog:
            import madrigal.ui.web
            self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)
            self.madWebObj.logDataAccess(self.file,
                                         self.user_fullname,
                                         self.user_email,
                                         self.user_affiliation)



    def outputReport(self):

        # create isprint cmd
        cmd = os.environ['MAD' + 'ROOT'] + '/bin/isprint '
        cmd += 'file=' + self.file + ' '
        if self.parms != None:
            cmd += self.parms + ' '
        if self.filters != None:
            cmd += self.filters + ' '
	# never need summary
        cmd += 'summary=f '
        if self.header == None:
            cmd += 'header=f '
        elif self.header[0] in ['n', 'N']:
            cmd += 'header=f '
        else:
            cmd += 'header=t '
        os.system(cmd)
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class IsprintService
    # All work is done by the init function
    IsprintService()
