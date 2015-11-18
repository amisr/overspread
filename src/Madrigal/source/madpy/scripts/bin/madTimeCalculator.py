#!PYTHONEXE

"""madTimeCalculator.py is a script run to return a text output of derived Madrigal parameters for parameters that depend only on time.

It is presently used by the cgi script
"madTimeCalculatorService.py":../services/madTimeCalculatorService.py.html.

It has the following input arguments:

     --date1=<MM/DD/YYYY> (required)

     --time1=<HH:MM:SS>  (required)

     --date2=<MM/DD/YYYY> (required)

     --time2=<HH:MM:SS>  (required)

     --stepHours=<step hours between each calculation (double)> (required)

     --parms=<comma delimited string of Madrigal parameters desired> (required)

     --showHeader (optional)  Prints header line before data


Returns comma-delimited data, one line for each time
with the following fields:

    1. year
    
    2. month

    3. day

    4. hour

    5. min

    6. sec

    7. Values for each Madrigal parameter listed in argument parms, separated by whitespace

If --showHeader given, also prints header line before data

This script, and the corresponding cgi script
"madTimeCalculatorService.py":../services/madTimeCalculatorService.py.html are available to any scripting
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

usage = """madTimeCalculator.py is a script run to return a text output of derived Madrigal parameters for parameters that depend only on time.

It has the following input arguments:

     --date1=<MM/DD/YYYY> (required)
     
     --time1=<HH:MM:SS>  (required)
     
     --date2=<MM/DD/YYYY> (required)
     
     --time2=<HH:MM:SS>  (required)
     
     --stepHours=<step hours between each calculation (double)> (required)
     
     --parms=<comma delimited string of Madrigal parameters desired> (required)
     
     --showHeader (optional)  Prints header line before data

Returns comma-delimited data, one line for each time
with the following fields:

    1. year
    
    2. month

    3. day

    4. hour

    5. min

    6. sec

    7. Values for each Madrigal parameter listed in argument parms, separated by whitespace

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
    import madrigal._Madrec

    # parse command line
    arglist = ''
    longarglist = ['date1=',
                   'time1=',
                   'date2=',
                   'time2=',
                   'stepHours=',
                   'parms=',
                   'showHeader']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    date1 = None
    time1 = None
    date2 = None
    time2 = None
    stepHours = None
    parms = None
    showHeader = 0

    for opt in optlist:
        if opt[0] == '--date1':
            date1 = opt[1]
        elif opt[0] == '--time1':
            time1 = opt[1]
        elif opt[0] == '--date2':
            date2 = opt[1]
        elif opt[0] == '--time2':
            time2 = opt[1]
        elif opt[0] == '--stepHours':
            stepHours = float(opt[1])
        elif opt[0] == '--parms':
            parms = opt[1]
        elif opt[0] == '--showHeader':
            showHeader = 1
    
        else:
            raise IOError, 'Illegal option %s\n%s' % (opt[0], usage)

    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # verify input values
    if date1 == None:
        raise IOError, 'Date1 argument required!\n%s' % (usage)

    dateFields1 = date1.split('/')
    if len(dateFields1) != 3:
        raise IOError, 'Date1 argument must be in form MM/DD/YYYY: %s' % (date1)
    month1 = int(dateFields1[0])
    day1 = int(dateFields1[1])
    year1 = int(dateFields1[2])

    if month1 < 1 or month1 > 12:
        raise IOError, 'Date1 argument must be in form MM/DD/YYYY: %s' % (date1)

    if day1 < 1 or day1 > 31:
        raise IOError, 'Date1 argument must be in form MM/DD/YYYY: %s' % (date1)

    if date2 == None:
        raise IOError, 'Date2 argument required!\n%s' % (usage)

    dateFields2 = date2.split('/')
    if len(dateFields2) != 3:
        raise IOError, 'Date2 argument must be in form MM/DD/YYYY: %s' % (date2)
    month2 = int(dateFields2[0])
    day2 = int(dateFields2[1])
    year2 = int(dateFields2[2])

    if month2 < 1 or month2 > 12:
        raise IOError, 'Date2 argument must be in form MM/DD/YYYY: %s' % (date2)

    if day2 < 1 or day2 > 31:
        raise IOError, 'Date2 argument must be in form MM/DD/YYYY: %s' % (date2)

    if time1 == None:
        raise IOError, 'Time1 argument required!\n%s' % (usage)
    
    timeFields1 = time1.split(':')
    if len(timeFields1) != 3:
        raise IOError, 'time1 argument must be in form HH:MM:SS: %s' % (time1)
    hour1 = int(timeFields1[0])
    min1 = int(timeFields1[1])
    sec1 = int(timeFields1[2])

    if hour1 < 0 or hour1 > 24:
        raise IOError, 'Time1 argument must be in form HH:MM:SS: %s' % (time1)

    if min1 < 0 or min1 > 60:
        raise IOError, 'Time1 argument must be in form HH:MM:SS: %s' % (time1)

    if sec1 < 0 or sec1 > 61:
        raise IOError, 'Time1 argument must be in form HH:MM:SS: %s' % (time1)

    if time2 == None:
        raise IOError, 'Time2 argument required!\n%s' % (usage)
    
    timeFields2 = time2.split(':')
    if len(timeFields2) != 3:
        raise IOError, 'time2 argument must be in form HH:MM:SS: %s' % (time2)
    hour2 = int(timeFields2[0])
    min2 = int(timeFields2[1])
    sec2 = int(timeFields2[2])

    if hour2 < 0 or hour2 > 24:
        raise IOError, 'Time2 argument must be in form HH:MM:SS: %s' % (time2)

    if min2 < 0 or min2 > 60:
        raise IOError, 'Time2 argument must be in form HH:MM:SS: %s' % (time2)

    if sec2 < 0 or sec2 > 61:
        raise IOError, 'Time2 argument must be in form HH:MM:SS: %s' % (time2)


    if stepHours == None or stepHours <= 0.0:
        raise IOError, 'valid stepHours argument required!\n%s' % (usage)

    if parms == None:
        raise IOError, 'parms argument required!\n%s' % (usage)
    else:
        parms = parms.split(',')
    

    reportObj = madrigal.ui.report.MadrigalReport(madDBObj)

    # loop through the times
    startTime = madrigal._Madrec.getUtFromDate(year1,month1,day1,hour1,min1,sec1,0)
    endTime = madrigal._Madrec.getUtFromDate(year2,month2,day2,hour2,min2,sec2,0)

    if endTime < startTime:
        raise IOError, 'end time must be after start time.'

    thisTime = startTime

    while thisTime < endTime:

        madrigal._Madrec.getParmsAtTime(parms,
                                        thisTime,
                                        showHeader)

        thisTime += stepHours * 3600.0

    

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by madTimeCalculator')


except MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in madTimeCalculator.py</h1>'

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
                         'Error running madTimeCalculator.py' )

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running madTimeCalculator.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    print errStr

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running madTimeCalculator.py')


# end script

