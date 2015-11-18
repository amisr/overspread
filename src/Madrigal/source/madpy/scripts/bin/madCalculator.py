#!PYTHONEXE

"""madCalculator.py is a script run to return a text output of derived Madrigal parameters for a time and a range of gdlat, glon, and gdalt.

It is presently used by the madmatlab methods madCalculator, and via the cgi script
"madCalculatorService.py":../services/madCalculatorService.py.html, the madmatlab method madCalculatorWeb.

It has the following input arguments:

     --date=<MM/DD/YYYY> (required)

     --time=<HH:MM:SS>  (optional - if no HH:MM:SS given, 00:00:00 assumed)

     --startLat=<latitude>  Starting geodetic latitude, -90 to 90 (required)

     --endLat=<latitude>  Ending geodetic latitude, -90 to 90 (required)

     --stepLat=<latitude step>  Latitude step (0.1 to 90) (required)

     --startLong=<longitude>  Starting geodetic longitude, -180 to 180 (required)

     --endLong=<longitude>  Ending geodetic longitude, -180 to 180 (required)

     --stepLong=<longitude step>  Longitude step (0.1 to 180) (required)

     --startAlt=<altitude>  Starting geodetic altitude, >= 0 (required)

     --endAlt=<altitude>  Ending geodetic altitude, > 0 (required)

     --stepAlt=<altitude step>  Altitude step (>= 0.1) (required)

     --parms=<comma delimited string of Madrigal parameters desired> (required)

     --showHeader (optional)  Prints header line before data

     --oneD=<parm>,<value>  (optional - 0 or more allowed) This argument allows the user to
                            set any number of one-D parameters to be used in the calculation.
                            Value must be parameter name, comma, value as double.
                            Example:  --oneD=kinst,31.0  --oneD=elm=45.0

Returns comma-delimited data, one line for each combination of lat, long, and alt,
with the following fields:

1. latitude
2. longitude
3. altitude
4. Values for each Madrigal parameter listed in argument parms, separated by whitespace

If --showHeader given, also prints header line before data

This script, and the corresponding cgi script
"madCalculatorService.py":../services/madCalculatorService.py.html are available to any scripting
language that wants to access Madrigal derived parameters.
"""

import sys
import os
import time
import traceback
import getopt

def isnan( x ):
    rx = repr(float(x))
    if rx.lower().find('nan') != -1:
        return 1
    else:
        return 0

usage = """madCalculator.py is a script run to return a text output of derived Madrigal parameters for a time and a range of gdlat, glon, and gdalt.

It has the following input arguments:
     --date=<MM/DD/YYYY> (required)
     --time=<HH:MM:SS>  (optional - if no HH:MM:SS given, 00:00:00 assumed)
     --startLat=<latitude>  Starting geodetic latitude, -90 to 90 (required)
     --endLat=<latitude>  Ending geodetic latitude, -90 to 90 (required) If = startLat, no steps
     --stepLat=<latitude step>  Latitude step ( (required)
     --startLong=<longitude>  Starting geodetic longitude, -180 to 180 (required)
     --endLong=<longitude>  Ending geodetic longitude, -180 to 180 (required) If = startLong, no steps
     --stepLong=<longitude step>  Longitude step (0.1 to 180) (required)
     --startAlt=<altitude>  Starting geodetic altitude, >= 0 (required)
     --endAlt=<altitude>  Ending geodetic altitude, > 0 (required) If = startLong, no steps
     --stepAlt=<altitude step>  Altitude step (>= 0.1) (required)
     --parms=<comma delimited string of Madrigal parameters desired> (required)
     --showHeader (optional)  Prints header line before data

Returns comma-delimited data, one line for each combination of lat, long, and alt,
with the following fields:

1. latitude
2. longitude
3. altitude
4. Values for each Madrigal parameter listed in argument parms, separated by whitespace

If --showHeader given, also prints header line before data
"""


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

# try to run script, and report all errors to Madrigal sys admin, and also print
try:

    import madrigal.ui.report
    import madrigal.data

    parmObj = madrigal.data.MadrigalParameters()

    # parse command line
    arglist = ''
    longarglist = ['date=',
                   'time=',
                   'startLat=',
                   'endLat=',
                   'stepLat=',
                   'startLong=',
                   'endLong=',
                   'stepLong=',
                   'startAlt=',
                   'endAlt=',
                   'stepAlt=',
                   'parms=',
                   'showHeader',
                   'oneD=']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    date = None
    timeStr = None
    startLat = None
    endLat = None
    stepLat = None
    startLong = None
    endLong = None
    stepLong = None
    startAlt = None
    endAlt = None
    stepAlt = None
    parms = None
    showHeader = 0
    oneDParmList = []
    oneDParmValues = []

    for opt in optlist:
        if opt[0] == '--date':
            date = opt[1]
        elif opt[0] == '--time':
            timeStr = opt[1]
        elif opt[0] == '--startLat':
            startLat = float(opt[1])
        elif opt[0] == '--endLat':
            endLat = float(opt[1])
        elif opt[0] == '--stepLat':
            stepLat = float(opt[1])
        elif opt[0] == '--startLong':
            startLong = float(opt[1])
        elif opt[0] == '--endLong':
            endLong = float(opt[1])
        elif opt[0] == '--stepLong':
            stepLong = float(opt[1])
        elif opt[0] == '--startAlt':
            startAlt = float(opt[1])
        elif opt[0] == '--endAlt':
            endAlt = float(opt[1])
        elif opt[0] == '--stepAlt':
            stepAlt = float(opt[1])
        elif opt[0] == '--parms':
            parms = opt[1]
        elif opt[0] == '--showHeader':
            showHeader = 1
        elif opt[0] == '--oneD':
            items = opt[1].split(',')
            if len(items) != 2:
                raise ValueError, 'oneD must be in form --oneD=<parm>,<value>, not %s' % (opt[1])
            oneDParmList.append(parmObj.getParmMnemonic(items[0]))
            oneDParmValues.append(float(items[1]))
    
        else:
            raise ValueError, 'Illegal option %s\n%s' % (opt[0], usage)

    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # verify input values
    if date == None:
        raise 'Date argument required!\n%s' % (usage)

    dateFields = date.split('/')
    if len(dateFields) != 3:
        raise 'Date argument must be in form MM/DD/YYYY: %s' % (date)
    month = int(dateFields[0])
    day = int(dateFields[1])
    year = int(dateFields[2])

    if month < 1 or month > 12:
        raise 'Date argument must be in form MM/DD/YYYY: %s' % (date)

    if day < 1 or day > 31:
        raise 'Date argument must be in form MM/DD/YYYY: %s' % (date)

    if timeStr != None:

        timeFields = timeStr.split(':')
        if len(timeFields) != 3:
            raise 'time argument must be in form HH:MM:SS: %s' % (timeStr)
        hour = int(timeFields[0])
        min = int(timeFields[1])
        sec = int(timeFields[2])

        if hour < 0 or hour > 24:
            raise 'Time argument must be in form HH:MM:SS: %s' % (timeStr)

        if min < 0 or min > 60:
            raise 'Time argument must be in form HH:MM:SS: %s' % (timeStr)

        if sec < 0 or sec > 61:
            raise 'Time argument must be in form HH:MM:SS: %s' % (timeStr)

    else:
        # set default times
        hour = 0
        min = 0
        sec = 0

    if startLat == None or isnan(startLat):
        raise 'valid startLat argument required!\n%s' % (usage)
    elif startLat < -90 or startLat > 90:
        raise 'startLat must be between -90 and 90: %f' % (startLat)

    if endLat == None or isnan(endLat):
        raise 'valid endLat argument required!\n%s' % (usage)
    elif endLat < -90 or endLat > 90:
        raise 'endLat must be between -90 and 90: %f' % (endLat)

    if stepLat == None or isnan(stepLat):
        raise 'valid stepLat argument required!\n%s' % (usage)

    if startLong == None or isnan(startLong):
        raise 'valid startLong argument required!\n%s' % (usage)
    elif startLong < -180 or startLong > 180:
        raise 'startLong must be between -180 and 180: %f' % (startLong)

    if endLong == None or isnan(endLong):
        raise 'valid endLong argument required!\n%s' % (usage)
    elif endLong < -180 or endLong > 180:
        raise 'valid endLong must be between -180 and 180: %f' % (endLong)

    if stepLong == None or isnan(stepLong):
        raise 'valid stepLong argument required!\n%s' % (usage)

    if startAlt == None or isnan(startAlt):
        raise 'valid startAlt argument required!\n%s' % (usage)
    elif startAlt < 0.0:
        raise 'valid startAlt must be positive: %f' % (startAlt)

    if endAlt == None or isnan(endAlt):
        raise 'valid endAlt argument required!\n%s' % (usage)
    elif endAlt < 0.0:
        raise 'valid endAlt must be positive: %f' % (endAlt)

    if stepAlt == None or isnan(stepAlt):
        raise 'valid stepAlt argument required!\n%s' % (usage)

    if parms == None:
        raise 'parms argument required!\n%s' % (usage)

    reportObj = madrigal.ui.report.MadrigalReport(madDBObj)

    parmList = parms.split(',')

    reportObj.looker(parmList,
                     startLat,
                     endLat,
                     stepLat,
                     startLong,
                     endLong,
                     stepLong,
                     startAlt,
                     endAlt,
                     stepAlt,
                     year,
                     month,
                     day,
                     hour,
                     min,
                     sec,
                     showHeader,
                     oneDParmList,
                     oneDParmValues)

    

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by madCalculator')


except MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in madCalculator.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    print errStr

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running madCalculator.py' )

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running madCalculator.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    print errStr

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running madCalculator.py')


# end script

