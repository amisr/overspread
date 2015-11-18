#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, traceback
import cgi
import time


class madTimeCalculatorService:
    """madTimeCalculatorService is the class that allows remote access to the "madTimeCalculator.py":../scripts/madTimeCalculator.py.html script.
    
    Like all my python cgi scripts, madTimeCalculatorService has the following structure:  the entire cgi is
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
    scripting languages such as Matlab that want to call madTimeCalculator via the web

    Input cgi arguments:


        1. startyear - int (required)

        2. startmonth - int (required)

        3. startday - int (required)

        4. starthour - int (required)

        5. startmin - int (required)

        6. startsec - int (required)

        7. endyear - int (required)

        8. endmonth - int (required)

        9. endday - int (required)

        10. endhour - int (required)

        11. endmin - int (required)

        12. endsec - int (required)

        13. stephours - double - number of hours between each measurement (required)

        13. parms - comma delimited string of Madrigal parameters desired (required)

    Returns comma-delimited data, one line for time,
    with the following fields:

        1. year
        
        2. month
        
        3. day

        4. hour

        5. min

        6. sec
        
        7. Values for each Madrigal parameter listed in argument parms, separated by whitespace


    Calls script "madTimeCalculator.py":../scripts/madTimeCalculator.py.html.

    If error, returns error description


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  July. 12, 2004

    $Id: madTimeCalculatorService.py,v 1.9 2008/07/25 18:55:29 brideout Exp $
    """

    # constants
    __scriptName = 'madTimeCalculatorService'
    

    def __init__(self):
        """__init__ run the entire madTimeCalculatorService script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs madTimeCalculator data as a service.

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


            # output madTimeCalculator
            self.madTimeCalculator()

            

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

        if not self.madForm.has_key('startyear'):

            print 'This cgi script was called without the proper arguments.\n'

            sys.exit(0)
            
        else:
            # get start time
            self.startyear = self.madForm.getvalue('startyear')
            self.startmonth = self.madForm.getvalue('startmonth')
            self.startday = self.madForm.getvalue('startday')
            self.starthour = self.madForm.getvalue('starthour')
            self.startmin = self.madForm.getvalue('startmin')
            self.startsec = self.madForm.getvalue('startsec')
            
            # get end time
            self.endyear = self.madForm.getvalue('endyear')
            self.endmonth = self.madForm.getvalue('endmonth')
            self.endday = self.madForm.getvalue('endday')
            self.endhour = self.madForm.getvalue('endhour')
            self.endmin = self.madForm.getvalue('endmin')
            self.endsec = self.madForm.getvalue('endsec')

            # get stephour
            self.stephours = self.madForm.getvalue('stephours')

            # get parms
            self.parms = self.madForm.getvalue('parms')


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()


    def madTimeCalculator(self):

        # create cmd
        cmd = self.madDBObj.getMadroot() + '/bin/madTimeCalculator.py '


        # append start time
        dateStartStr = '%i/%i/%i' % (int(self.startmonth), int(self.startday), int(self.startyear))
        timeStartStr = '%i:%i:%i' % (int(self.starthour), int(self.startmin), int(self.startsec))

        cmd += ' --date1=%s ' % (dateStartStr)
        cmd += ' --time1=%s ' % (timeStartStr)

        # append end time
        dateEndStr = '%i/%i/%i' % (int(self.endmonth), int(self.endday), int(self.endyear))
        timeEndStr = '%i:%i:%i' % (int(self.endhour), int(self.endmin), int(self.endsec))

        cmd += ' --date2=%s ' % (dateEndStr)
        cmd += ' --time2=%s ' % (timeEndStr)

        # append stephours

        cmd += ' --stepHours=%s ' % (self.stephours)

        # append parms
        cmd += ' --parms=%s ' % (self.parms)
        
        os.system(cmd)
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class madTimeCalculatorService
    # All work is done by the init function
    madTimeCalculatorService()
