#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, traceback
import cgi
import time



class getExperimentsService:
    """getExperimentsService is the class that allows remote access to the "getExperiments.py":../scripts/getExperiments.py.html script.
    
    Like all my python cgi scripts, getExperimentsService has the following structure:  the entire cgi is
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
    scripting languages such as Matlab that want to call getExperiments via the web

    Input cgi arguments:


	1. code - int representing instrument code.  Special value of 0 selects all instruments. More than one allowed.

	2. startyear - int

	3. startmonth - int

	4. startday - int

	5. starthour - int

	6. startmin - int

	7. startsec - int

	8. endyear - int

	9. endmonth - int

	10. endday - int

	11. endhour - int

	12. endmin - int

	13. endsec - int

	14. local - 1 if local experiments only, 0 if all experiments

    Returns comma-delimited data, one line for each experiment, with the following fields:

        1. experiment.id (int) Example: 10000111
        
        2. experiment.url (string) Example: 'http://www.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97'
        
        3. experiment.name (string) Example: 'Wide Latitude Substorm Study'
        
        4. experiment.siteid (int) Example: 1
        
        5. experiment.sitename (string) Example: 'Millstone Hill Observatory'
        
        6. experiment.instcode (int) Code of instrument. Example: 30
        
        7. experiment.instname (string) Instrument name. Example: 'Millstone Hill Incoherent Scatter Radar'
        
        8. experiment.start year (int) year of experiment start
        
        9. experiment.start month (int) month of experiment start
        
        10. experiment.start day (int) day of experiment start
        
        11. experiment.start hour (int) hour of experiment start
        
        12. experiment.start minute (int) min of experiment start
        
        13. experiment.start second (int) sec of experiment start
        
        14. experiment.end year (int) year of experiment end
        
        15. experiment.end month (int) month of experiment end
        
        16. experiment.end day (int) day of experiment end
        
        17. experiment.end hour (int) hour of experiment end
        
        18. experiment.end minute (int) min of experiment end
        
        19. experiment.end second (int) sec of experiment end
        
        20. experiment.isLocal (int) 1 if local, 0 if not


    Call script "getExperiments.py":../scripts/getExperiments.py.html.

    If error, returns error description


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 11, 2003

    $Id: getExperimentsService.py,v 1.12 2008/07/25 18:57:19 brideout Exp $
    """

    # constants
    __scriptName = 'getExperimentsService'
    

    def __init__(self):
        """__init__ run the entire getExperimentsService script.  All other functions are private and called by __init__.

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


            # output getExperiments
            self.getExperiments()

            

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

        if not self.madForm.has_key('code'):

            print 'This cgi script was called without the proper arguments.\n'

            sys.exit(0)
            
        else:
            self.code = self.madForm.getvalue('code')
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
            
            self.local = self.madForm.getvalue('local')


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # create web obj to test whether the user is trusted
        import madrigal.ui.web
        self.webObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)


    def getExperiments(self):

        # create cmd
        cmd = self.madDBObj.getMadroot() + '/bin/getExperiments.py '

        # append code
        if isinstance(self.code, list):
            for code in self.code:
                cmd += ' --code=%i ' % (int(code))
        else:
           cmd += ' --code=%i ' % (int(self.code))

        # append start time
        cmd += ' --startyear=%i ' % (int(self.startyear))
        cmd += ' --startmonth=%i ' % (int(self.startmonth))
        cmd += ' --startday=%i ' % (int(self.startday))
        cmd += ' --starthour=%i ' % (int(self.starthour))
        cmd += ' --startmin=%i ' % (int(self.startmin))
        cmd += ' --startsec=%i ' % (int(self.startsec))

        # append end time
        cmd += ' --endyear=%i ' % (int(self.endyear))
        cmd += ' --endmonth=%i ' % (int(self.endmonth))
        cmd += ' --endday=%i ' % (int(self.endday))
        cmd += ' --endhour=%i ' % (int(self.endhour))
        cmd += ' --endmin=%i ' % (int(self.endmin))
        cmd += ' --endsec=%i ' % (int(self.endsec))

        # append local
        cmd += ' --local=%i ' % (int(self.local))

        # if trusted, add another argument
        if self.webObj.isTrusted():
            cmd += ' --trusted=1 '
        
        os.system(cmd)
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class getExperimentsService
    # All work is done by the init function
    getExperimentsService()
