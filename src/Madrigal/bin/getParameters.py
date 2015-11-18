#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

"""getParameters.py is a script run to return a text output of parameter information for a given file.

It is presently used by the madmatlab methods getParameters, and via the cgi script
"getParametersService.py":../services/getParametersService.py.html
the madmatlab method getParametersWeb.  It has the following input argument:

     --filename=<full path to data file>


Returns backslash-delimited data, one for each parameter either measured or derivable, with the following fields:

    1. parameter.mnemonic (string) Example 'dti'
    
    2. parameter.description (string) Example:
        "F10.7 Multiday average observed (Ott)"
        
    3. parameter.isError (int) 1 if error parameter, 0 if not
    
    4. parameter.units (string) Example "W/m2/Hz"
    
    5. parameter.isMeasured (int) 1 if measured, 0 if derivable
    
    6. parameter.category (string) Example: "Time Related Parameter"
    
    7. parameter.isSure (int) - 1 if parameter can be found for every record, 0 if can only be found for some

    8. parameter.isAddIncrement - 1 if additional increment, 0 if normal (Added in Madrigal 2.5)

Returns empty string if filename not found, and returns status -1.

Uses backslash-delimited since some strings contain commas.

This script, and the corresponding cgi script
"getParametersService.py":../services/getParametersService.py.html are available to any scripting
language that wants to access Madrigal parameter info.
"""

import sys
import os
import datetime
import traceback
import getopt


# catch any exception, and write an appropriate message admin
try:
    # check if pythonlibpath env variable exists
    # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
    temp = os.environ.get('PYTHON' + 'LIBPATH')
    if temp != None:
        sys.path.append(temp)
        
    # append path madroot/lib (needed only if python not installed by setup)
    sys.path.append('/Users/mnicolls/Documents/Work/Madrigal/lib/python')

    # prepare to handle MadrigalError
    from madrigal.admin import *

except ImportError:
    
    # Fatal error - madpy library not found
    print "Unable to import the madrigal python library - please alert the sys admin!"
    sys.exit(-1)

# try to run script, and report all errors to Madrigal sys admin
try:

    import madrigal.metadata
    import madrigal.data

    # parse command line
    arglist = ''
    longarglist = ['filename=']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    filename = None

    for opt in optlist:
        if opt[0] == '--filename':
            filename = opt[1]
        else:
            raise 'Illegal option %s' % (opt[0])

    # raise exception if no filename
    if filename == None:
        raise 'getParameters requires --filename argument'
        
    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # create Madrigal File object 
    madFileObj = madrigal.data.MadrigalFile(filename,madDBObj)

    # create Madrigal Parameter object
    madParmObj = madrigal.data.MadrigalParameters(madDBObj)
    
    # create Madrigal web object 
    madWebObj = madrigal.ui.web.MadrigalWebFormat()
    

    # create lists of parameters
    measParmList = []
    derivedParmList = []
    allParmList = []
    sureParmList = []

    # use the comprehensive list of parameters to check if derivable
    parmList = madWebObj.getFormat('Comprehensive')

    # populate lists
    madFileObj.getMeasDervBothParmLists(parmList,
                                        measParmList,
                                        derivedParmList,
                                        allParmList,
                                        sureParmList)

    # loop through allParmList and output results
    for parm in allParmList:
        description = madParmObj.getSimpleParmDescription(parm)
        isNorm = madParmObj.getParmType(parm)
        if isNorm == 1:
            isError = 0
        else:
            isError = 1
        units = madParmObj.getParmUnits(parm)
        if parm in measParmList:
            isMeasured = 1
        else:
            isMeasured = 0
        if parm in sureParmList:
            isSure = 1
        else:
            isSure = 0
        category = madParmObj.getParmCategory(parm)
        if madParmObj.isAddIncrement(parm):
            isAddIncrement = 1
        else:
            isAddIncrement = 0
        # print out this parm
        print '%s\%s\%i\%s\%i\%s\%i\%i' % (parm,
                                           description,
                                           isError,
                                           units,
                                           isMeasured,
                                           category,
                                           isSure,
                                           isAddIncrement)
        

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by getParameters')


except MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in getParameters.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(datetime.datetime.today().isoformat()) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getParameters.py' )

    sys.exit(-1)

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running getParameters.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(datetime.datetime.today().isoformat()) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getParameters.py')

    sys.exit(-1)


# end script

