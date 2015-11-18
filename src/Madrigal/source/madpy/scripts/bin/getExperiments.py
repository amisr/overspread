#!PYTHONEXE

"""getExperiments.py is a script run to return a text output of selected experiments data.

It is presently used by the madmatlab methods getExperiments, and via the cgi script
"getExperimentsService.py":../services/getExperimentsService.py.html
the madmatlab method getExperimentsWeb.  It has the following input arguments:

     --code=<instrument code> (integer) - more than one allowed; 0 indicates all instruments (default)
     
     --startyear=<year>
     
     --startmonth=<month> (1-12) (defaults to 1)
     
     --startday=<day> (1-31) (defaults to 1)
     
     --starthour=<hour> (0-24)(defaults to 0)
     
     --startmin=<min> (0-59)(defaults to 0)
     
     --startsec=<sec> (0-61)(defaults to 0)
     
     --endyear=<year>
     
     --endmonth=<month> (1-12) (defaults to 12)
     
     --endday=<day> (1-31) (defaults to 31)
     
     --endhour=<hour> (0-24) (defaults to 23)
     
     --endmin=<min> (0-59) (defaults to 59)
     
     --endsec=<sec> (0-61) (defaults to 59)
     
     --local=<1 or 0> - if 1, return local experiments only, otherwise, return all. Defaults to local.

     --trusted=<0 if not, 1 if is> (default to not trusted).  If not trusted, skips private experiments

If no code given, all instruments assumed. If no startyear given, starttime filter not used.  If no
endyear given, no endtime filter used.

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

This script, and the corresponding cgi script
"getExperimentsService.py":../services/getExperimentsService.py.html are available to any scripting
language that wants to access Madrigal metadata.
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

    # parse command line
    arglist = ''
    longarglist = ['code=',
                   'startyear=',
                   'startmonth=',
                   'startday=',
                   'starthour=',
                   'startmin=',
                   'startsec=',
                   'endyear=',
                   'endmonth=',
                   'endday=',
                   'endhour=',
                   'endmin=',
                   'endsec=',
                   'local=',
                   'trusted=']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    codeList = []
    startyear = None
    startmonth = 1
    startday = 1
    starthour = 0
    startmin = 0
    startsec = 0
    endyear = None
    endmonth = 12
    endday = 31
    endhour = 23
    endmin = 59
    endsec = 59
    local = 1
    trusted = 0

    for opt in optlist:
        if opt[0] == '--code':
            codeList.append(int(opt[1]))
        elif opt[0] == '--startyear':
            startyear = int(opt[1])
        elif opt[0] == '--startmonth':
            startmonth = int(opt[1])
        elif opt[0] == '--startday':
            startday = int(opt[1])
        elif opt[0] == '--starthour':
            starthour = int(opt[1])
        elif opt[0] == '--startmin':
            startmin = int(opt[1])
        elif opt[0] == '--startsec':
            startsec = int(opt[1])
        elif opt[0] == '--endyear':
            endyear = int(opt[1])
        elif opt[0] == '--endmonth':
            endmonth = int(opt[1])
        elif opt[0] == '--endday':
            endday = int(opt[1])
        elif opt[0] == '--endhour':
            endhour = int(opt[1])
        elif opt[0] == '--endmin':
            endmin = int(opt[1])
        elif opt[0] == '--endsec':
            endsec = int(opt[1])
        elif opt[0] == '--local':
            local = int(opt[1])
        elif opt[0] == '--trusted':
            trusted = int(opt[1])
        else:
            raise 'Illegal option %s' % (opt[0])

    # if startsec or endsec in (60, 61), handle correctly
    if startsec in (60, 61):
        tmpTime = datetime.datetime(startyear,
                                    startmonth,
                                    startday,
                                    starthour,
                                    startmin,
                                    59)
        tmpTime += datetime.timedelta(0, startsec - 59)
        startyear = tmpTime.year
        startmonth = tmpTime.month
        startday = tmpTime.day
        starthour = tmpTime.hour
        startmin = tmpTime.minute
        startsec = tmpTime.second

    if endsec in (60, 61):
        tmpTime = datetime.datetime(endyear,
                                    endmonth,
                                    endday,
                                    endhour,
                                    endmin,
                                    59)
        tmpTime += datetime.timedelta(0, endsec - 59)
        endyear = tmpTime.year
        endmonth = tmpTime.month
        endday = tmpTime.day
        endhour = tmpTime.hour
        endmin = tmpTime.minute
        endsec = tmpTime.second
        
    # if codeList is empty or contains 0, change it to only contain 0
    if len(codeList) == 0 or 0 in codeList:
        codeList = [0]

    if trusted not in (0,1):
        raise ValueError, '--trusted must be 0 or 1, not %i' % (trusted)
        
    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # get the local site id
    localSiteId = madDBObj.getSiteID()

    # create MadrigalInstrument obj to convert kinst to instrument names
    madInstObj = madrigal.metadata.MadrigalInstrument(madDBObj)

    # create MadrigalSite obj to convert site id to site name
    madSiteObj = madrigal.metadata.MadrigalSite(madDBObj)

    # create starttime for filter, if possible
    if startyear != None:
        startTimeFilter = datetime.datetime(startyear,
					    startmonth,
					    startday,
					    starthour,
					    startmin,
					    startsec) 
    else:
        startTimeFilter = None

    # create endtime for filter, if possible
    if endyear != None:
        endTimeFilter = datetime.datetime(endyear,
				          endmonth,
					  endday,
					  endhour,
					  endmin,
					  endsec) 
    else:
        endTimeFilter = None

    # create MadrigalExperiments for local or all files
    if local == 1:
        madExpObj = madrigal.metadata.MadrigalExperiment(madDBObj)
    else:
        # use file expTabAll.txt to get all experiments
        filename = madDBObj.getMadroot()
        if filename[-1] != '/':
            filename += '/'
        filename += 'metadata/expTabAll.txt'
        madExpObj = madrigal.metadata.MadrigalExperiment(madDBObj, filename)


    # loop through the data
    position = 0
    while 1:
        thisId = madExpObj.getExpIdByPosition(position)
        # check for end
        if thisId == None:
            break
        thisUrl = madExpObj.getExpUrlByPosition(position)
        thisName = madExpObj.getExpNameByPosition(position)
        thisSiteId = madExpObj.getExpSiteIdByPosition(position)
        thisSiteName = madSiteObj.getSiteName(thisSiteId)
        thisInstCode = madExpObj.getKinstByPosition(position)
        thisInstName =madInstObj.getInstrumentName(thisInstCode)
        thisStart = madExpObj.getExpStartDateTimeByPosition(position)
        thisEnd = madExpObj.getExpEndDateTimeByPosition(position)
        thisSecurity = madExpObj.getSecurityByPosition(position)
        if thisSiteId == localSiteId:
            thisLocal = 1
        else:
            thisLocal = 0
        position += 1

        # some experiments set the end of the day to 24:00:00 - not
        # technically correct - reset to 23:59:59
        
        if (thisStart[3] == 24 and thisStart[4] == 0 and thisStart[5] == 0):
            thisStart[3] = 23
            thisStart[4] = 59
            thisStart[5] = 59

        if (thisEnd[3] == 24 and thisEnd[4] == 0 and thisEnd[5] == 0):
            thisEnd[3] = 23
            thisEnd[4] = 59
            thisEnd[5] = 59
        
        # apply filters
        
        # first apply instrument code filter
        if codeList[0] != 0:
            if thisInstCode not in codeList:
                continue

        # apply starttime and endtime filters
        thisStartTime = datetime.datetime(thisStart[0],
                                          thisStart[1],
                                          thisStart[2],
                                          thisStart[3],
                                          thisStart[4],
                                          thisStart[5])

        thisEndTime = datetime.datetime(thisEnd[0],
                                        thisEnd[1],
                                        thisEnd[2],
                                        thisEnd[3],
                                        thisEnd[4],
                                        thisEnd[5])
        
        if startTimeFilter != None:
            if thisEndTime < startTimeFilter:
                continue

        if endTimeFilter != None:
            if thisStartTime > endTimeFilter:
                continue

        # apply local filer
        if local == 1 and thisLocal == 0:
            continue

        # apply security filter
        if trusted == 0 and thisSecurity not in (0,2):
            continue

        # print this experiment
        print('%i,%s,%s,%i,%s,%i,%s,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i') % \
                (thisId,
                thisUrl,
                thisName,
                thisSiteId,
                thisSiteName,
                thisInstCode,
                thisInstName,
                thisStart[0],
                thisStart[1],
                thisStart[2],
                thisStart[3],
                thisStart[4],
                thisStart[5],
                thisEnd[0],
                thisEnd[1],
                thisEnd[2],
                thisEnd[3],
                thisEnd[4],
                thisEnd[5],
                thisLocal)
    

    

    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by getExperiments')


except MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in getExperiments.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(datetime.datetime.today().isoformat()) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getExperiments.py' )

    sys.exit(-1)

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running getExperiments.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(datetime.datetime.today().isoformat()) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getExperiments.py')

    sys.exit(-1)


# end script

