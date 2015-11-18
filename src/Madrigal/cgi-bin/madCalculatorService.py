#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, traceback
import cgi
import time




class madCalculatorService:
    """madCalculatorService is the class that allows remote access to the "madCalculator.py":../scripts/madCalculator.py.html script.
    
    Like all my python cgi scripts, madCalculatorService has the following structure:  the entire cgi is
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
    scripting languages such as Matlab that want to call madCalculator via the web

    Input cgi arguments:


        1. year - int (required)

        2. month - int (required)

        3. day - int (required)

        4. hour - int (required)

        5. min - int (required)

        6. sec - int (required)

        7. startLat - Starting geodetic latitude, -90 to 90 (required)

        8. endLat - Ending geodetic latitude, -90 to 90 (required)

        9. stepLat - Latitude step (0.1 to 90) (required)

        10. startLong - Starting geodetic longitude, -180 to 180 (required)

        11. endLong - Ending geodetic longitude, -180 to 180 (required)

        12. stepLong - Longitude step (0.1 to 180) (required)

        13. startAlt - Starting geodetic altitude, >= 0 (required)

        14. endAlt - Ending geodetic altitude, > 0 (required)

        15. stepAlt - Altitude step (>= 0.1) (required)

        16. parms - comma delimited string of Madrigal parameters desired (required)

        17. oneD - string in form <parm>,<value> This argument allows the user to
                            set any number of one-D parameters to be used in the calculation.
                            Value must be parameter name, comma, value as double.
                            Example:  &oneD=kinst,31.0&oneD=elm,45.0
                            (optional - 0 or more allowed)

    Returns comma-delimited data, one line for each combination of lat, long, and alt,
    with the following fields:

        1. latitude
        
        2. longitude
        
        3. altitude
        
        4. Values for each Madrigal parameter listed in argument parms, separated by whitespace


    Calls script "madCalculator.py":../scripts/madCalculator.py.html.

    If error, returns error description


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 6, 2004

    $Id: madCalculatorService.py,v 1.11 2008/10/03 19:34:55 brideout Exp $
    """

    # constants
    __scriptName = 'madCalculatorService'
    

    def __init__(self):
        """__init__ run the entire madCalculatorService script.  All other functions are private and called by __init__.

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


            # output madCalculator
            self.madCalculator()

            

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

        if not self.madForm.has_key('year'):

            print 'This cgi script was called without the proper arguments.\n'

            sys.exit(0)
            
        else:
            # get start time
            self.year = self.madForm.getvalue('year')
            self.month = self.madForm.getvalue('month')
            self.day = self.madForm.getvalue('day')
            self.hour = self.madForm.getvalue('hour')
            self.min = self.madForm.getvalue('min')
            self.sec = self.madForm.getvalue('sec')
            
            # get geometric limits
            self.startLat = self.madForm.getvalue('startLat')
            self.endLat = self.madForm.getvalue('endLat')
            self.stepLat = self.madForm.getvalue('stepLat')
            self.startLong = self.madForm.getvalue('startLong')
            self.endLong = self.madForm.getvalue('endLong')
            self.stepLong = self.madForm.getvalue('stepLong')
            self.startAlt = self.madForm.getvalue('startAlt')
            self.endAlt = self.madForm.getvalue('endAlt')
            self.stepAlt = self.madForm.getvalue('stepAlt')

            # get parms
            self.parms = self.madForm.getvalue('parms')

            # get oneD list
            self.oneDList = self.madForm.getlist('oneD')


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()


    def madCalculator(self):

        # create cmd
        cmd = self.madDBObj.getMadroot() + '/bin/madCalculator.py '


        # append time
        dateStr = '%i/%i/%i' % (int(self.month), int(self.day), int(self.year))
        timeStr = '%i:%i:%i' % (int(self.hour), int(self.min), int(self.sec))

        cmd += ' --date=%s ' % (dateStr)
        cmd += ' --time=%s ' % (timeStr)

        # append geometric data
        cmd += ' --startLat=%f ' % (float(self.startLat))
        cmd += ' --endLat=%f ' % (float(self.endLat))
        cmd += ' --stepLat=%f ' % (float(self.stepLat))
        cmd += ' --startLong=%f ' % (float(self.startLong))
        cmd += ' --endLong=%f ' % (float(self.endLong))
        cmd += ' --stepLong=%f ' % (float(self.stepLong))
        cmd += ' --startAlt=%f ' % (float(self.startAlt))
        cmd += ' --endAlt=%f ' % (float(self.endAlt))
        cmd += ' --stepAlt=%f ' % (float(self.stepAlt))

        # append parms
        cmd += ' --parms=%s ' % (self.parms)

        # append oneD
        for item in self.oneDList:
            cmd += ' --oneD=%s ' % (item)
        
        os.system(cmd)
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class madCalculatorService
    # All work is done by the init function
    madCalculatorService()
