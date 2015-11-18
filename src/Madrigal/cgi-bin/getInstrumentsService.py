#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, traceback
import cgi
import time



class getInstrumentsService:
    """getInstrumentsService is the class that allows remote access to the "getInstruments.py":../scripts/getInstruments.py.html script.
    
    Like all my python cgi scripts, getInstrumentsService has the following structure:  the entire cgi is
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
    scripting languages such as Matlab that want to call getInstruments via the web

    Input cgi arguments (None)

    Returns comma-delimited data, one line for each experiment, with the following fields:

        1. instrument.name  Example: 'Millstone Hill Incoherent Scatter Radar'

        2. instrument.code Example: 30

        3. instrument.mnemonic (3 char string) Example: 'mlh'

        4. instrument.latitude  Example: 45.0

        5. instrument.longitude  Example: 110.0

        6. instrument.altitude   Example: 0.015 (km) 

    Calls script "getInstruments.py":../scripts/getInstruments.py.html.

    If error, returns error description


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 11, 2003

    $Id: getInstrumentsService.py,v 1.10 2008/07/25 18:57:19 brideout Exp $
    """

    # constants
    __scriptName = 'getInstrumentsService'
    

    def __init__(self):
        """__init__ run the entire getInstrumentsService script.  All other functions are private and called by __init__.

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


            # output getInstruments
            self.getInstruments()

            

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

        print "Content-Type: text/plain\n"
        sys.stdout.flush()


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()


    def getInstruments(self):

        # create cmd
        cmd = self.madDBObj.getMadroot() + '/bin/getInstruments.py '
        os.system(cmd)
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class getInstrumentsService
    # All work is done by the init function
    getInstrumentsService()
