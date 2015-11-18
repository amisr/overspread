#!PYTHONEXE

import sys, os, traceback
import cgi
import time


class traceMagneticFieldService:
    """traceMagneticFieldService is web service that allows access to the madrigal._Madrec.pyTraceMagneticField method.
    
    Like all my python cgi scripts, traceMagneticFieldService has the following structure:  the entire cgi is
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

    Input cgi arguments::

        year

        month

        day

        hour

        min

        sec

        inputType (0 for geodetic, 1 for GSM)

        outputType (0 for geodetic, 1 for GSM)
        
            The following parameter depend on inputType:
            
        in1 - a comma-separated list of geodetic altitudes or ZGSMs of starting point
        
        in2 - a comma-separated list of geodetic latitudes or XGSMs of starting point
        
        in3 - a comma-separated list of longitude or YGSM of starting point

            Length of all three lists must be the same
        
        model - 0 for Tsyganenko, 1 for IGRF
        
        qualifier - 0 for conjugate, 1 for north_alt, 2 for south_alt, 3 for apex, 4 for GSM XY plane
        
        stopAlt - altitude in km to stop trace at, if qualifier is north_alt or south_alt.
        If other qualifier, this parameter is not required.

    Returns comma-delimited data, one line for point in in lists:

        1. geodetic altitude or ZGSM of ending point

        2. geodetic latitude or XGSM of ending point

        3. longitude or YGSM of ending point


    If error, returns error description


    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 10, 2004

    $Id: traceMagneticFieldService.py,v 1.7 2008/07/25 18:48:45 brideout Exp $
    """

    # constants
    __scriptName = 'traceMagneticFieldService'
    

    def __init__(self):
        """__init__ run the entire traceMagneticFieldService script.  All other functions are private and called by __init__.

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

            # output list on end points
            self.traceMagneticField()

            

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
        
        #create a form object
        self.madForm = cgi.FieldStorage()

        if not self.madForm.has_key('year'):

            print 'This cgi script was called without the proper arguments.\n'

            sys.exit(0)
            
        else:
            self.year = int(self.madForm.getvalue('year'))
            self.month = int(self.madForm.getvalue('month'))
            self.day = int(self.madForm.getvalue('day'))
            self.hour = int(self.madForm.getvalue('hour'))
            self.min = int(self.madForm.getvalue('min'))
            self.sec = int(self.madForm.getvalue('sec'))
            self.inputType = int(self.madForm.getvalue('inputType'))
            self.outputType = int(self.madForm.getvalue('outputType'))
            in1 = self.madForm.getvalue('in1')
            self.in1 = []
            delimiter = ','
            items = delimiter.split(in1)
            for item in items:
                self.in1.append(float(item))
            in2 = self.madForm.getvalue('in2')
            self.in2 = []
            items = delimiter.split(in2)
            for item in items:
                self.in2.append(float(item))
            in3 = self.madForm.getvalue('in3')
            self.in3 = []
            items = delimiter.split(in3)
            for item in items:
                self.in3.append(float(item))
            # check that all three input lists have the same length
            if len(self.in1) != len(self.in2) or len(self.in1) != len(self.in3):
                raise 'length of three comman-delimited input lists must be equal'
            self.model = int(self.madForm.getvalue('model'))
            self.qualifier = int(self.madForm.getvalue('qualifier'))
            if self.madForm.has_key('stopAlt'):
                self.stopAlt = float(self.madForm.getvalue('stopAlt'))
            else:
                self.stopAlt = 0.0


        
    def traceMagneticField(self):

        import madrigal._Madrec

        # raise error if qualifier == 4 and model == 1
        if self.qualifier == 4 and self.model == 1:
            raise 'Present IGRF line trace code cannot trace to GSM XY plane'


        for i in range(len(self.in1)):
            result = madrigal._Madrec.pyTraceMagneticField(self.year,
                                                           self.month,
                                                           self.day,
                                                           self.hour,
                                                           self.min,
                                                           self.sec,
                                                           self.inputType,
                                                           self.outputType,
                                                           self.in1[i],
                                                           self.in2[i],
                                                           self.in3[i],
                                                           self.model,
                                                           self.qualifier,
                                                           self.stopAlt)
            if result[0] < 1.1e-38 and result[0] > 0.9e-38:
                print 'missing,missing,missing'
            else:
                print '%f,%f,%f' % (result[0], result[1], result[2])
        
            

if __name__ == '__main__':

    # Script madLogin
    # This script only calls the init function of the class traceMagneticFieldService
    # All work is done by the init function
    traceMagneticFieldService()
