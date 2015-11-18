#!PYTHONEXE

import sys, os, traceback
import cgi
import time



class getExperimentFilesService:
    """getExperimentFilesService is the class that allows remote access to the "getExperimentFiles.py":../scripts/getExperimentFiles.py.html script.
    
    Like all my python cgi scripts, getExperimentFilesService has the following structure:  the entire cgi is
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

    This script calls madrigal.ui.web.ui.isTrusted() to determine if user is trusted and should see private files.

    This script is not meant to be used directly by a user, and thus is named Service.  It is meant to be used by
    scripting languages such as Matlab that want to call getExperimentFiles via the web

    Input cgi arguments:

	1. id (int) - experiment id

    Returns comma-delimited data, one line for each experiment file, with the following fields:

        1. file.name (string) Example '/opt/mdarigal/blah/mlh980120g.001'
        
        2. file.kindat (int) Kindat code.  Example: 3001
        
        3. file.kindat desc (string) Kindat description: Example 'Basic Derived Parameters'
        
        4. file.category (int) (1=default, 2=variant, 3=history, 4=real-time)
        
        5. file.status (string)('preliminary', 'final', or any other description)
        
        6. file.permission (int)  0 for public, 1 for private.  For now will not return private files.


    Calls script "getExperimentFiles.py":../scripts/getExperimentFiles.py.html.

    If error, returns error description.

    


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 16, 2003

    $Id: getExperimentFilesService.py,v 1.11 2008/07/25 18:57:19 brideout Exp $
    """

    # constants
    __scriptName = 'getExperimentFilesService'
    

    def __init__(self):
        """__init__ run the entire getExperimentFilesService script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs getInstrument data as a service.

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

            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()


            # output getExperimentFiles
            self.getExperimentFiles()

            

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

	if not self.madForm.has_key('id'):

            print 'This cgi script was called without the proper arguments.\n'

            sys.exit(0)
            
        else:
            self.id = self.madForm.getvalue('id')


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # create web obj to test whether the user is trusted
        import madrigal.ui.web
        self.webObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)


    def getExperimentFiles(self):

        # create cmd
        cmd = self.madDBObj.getMadroot() + '/bin/getExperimentFiles.py '

	# append id
	cmd += ' --id=%i ' % (int(self.id))

	# if trusted, add another argument
	if self.webObj.isTrusted():
            cmd += ' --trusted=1 '

        os.system(cmd)
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class getExperimentFilesService
    # All work is done by the init function
    getExperimentFilesService()
