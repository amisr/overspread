#!PYTHONEXE

"""getInstruments.py is a script run to return a text output of all instrument data.

It is presently used by the madmatlab methods getInstruments, and via the cgi script
"getInstrumentsService.py":../services/getInstrumentsService.py.html
the madmatlab method getInstrumentsWeb.  It has no input arguments.

Returns comma-delimited data, one line for each experiment, with the following fields:

    1. instrument.name  Example: 'Millstone Hill Incoherent Scatter Radar'

    2. instrument.code Example: 30

    3. instrument.mnemonic (3 char string) Example: 'mlh'

    4. instrument.latitude  Example: 45.0

    5. instrument.longitude  Example: 110.0

    6. instrument.altitude   Example: 0.015 (km) 

This script, and the corresponding cgi script
"getInstrumentsService.py":../services/getInstrumentsService.py.html are available to any scripting
language that wants to access Madrigal metadata.
"""

import sys
import os
import time
import traceback


# catch any exception, and write an appropriate message admin
try:
    # check if pythonlibpath env variable exists
    # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
    temp = os.environ.get('PYTHON' + 'LIBPATH')
    if temp != None:
        sys.path.append(temp)
        
    # append path madroot/lib (needed only if python not installed by setup)
    sys.path.append('MADROOT/lib/python')

    # prepare to handle MadrigalError
    from madrigal.admin import *

except ImportError:
    
    # Fatal error - madpy library not found
    print "Unable to import the madrigal python library - please alert the sys admin!"
    sys.exit(-1)

# try to run script, and report all errors to Madrigal sys admin
try:

    import madrigal.metadata

    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # create MadrigalInstument object
    madInst = madrigal.metadata.MadrigalInstrument(madDBObj)

    # get instrument list
    instList = madInst.getInstrumentList()

    # loop through each instrument
    for inst in instList:
        name = inst[0]
        code = inst[2]
        mnemonic = inst[1]
        latitude = madInst.getLatitude(code)
        if latitude == None:
            latitude = 0.0
        longitude = madInst.getLongitude(code)
        if longitude == None:
            longitude = 0.0
        altitude = madInst.getAltitude(code)
        if altitude == None:
            altitude = 0.0
        # print data
        instStr = '%s,%i,%s,%f,%f,%f' % (name,
                                         code,
                                         mnemonic,
                                         latitude,
                                         longitude,
                                         altitude)
        print instStr

    

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by getInstruments')


except MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in getInstruments.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getInstruments.py' )

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running getInstruments.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getInstruments.py')


# end script

