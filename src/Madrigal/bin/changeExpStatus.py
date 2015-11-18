#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: changeExpStatus.py,v 1.3 2009/03/17 13:15:41 brideout Exp $

usage = """
changeExpStatus.py is a script used to change the status of an
existing Madrigal experiment.  The following attributes can be changed:

    expUrl
    experiment name
    siteID
    start date
    start time
    end date
    end time
    instrument code
    security (public, private. ignore)

Required argument: 

    --expDir - full path to experiment directory. Example:
               "/opt/madrigal/experiments/1998/mlh/20jan98"

Optional arguments - set these to change an experiment attribute:
    
    --expUrl - must be in form <cgi base>/madtoc/YYYY/<3 letter lower case inst code>/<expDir>
               example: http://www.haystack.mit.edu/cgi-bin/madtoc/1997/mlh/03dec97g

    --expName - experiment name.  Quotes required if contains spaces.  Example: "World Day"

    --siteID - Madrigal siteID of where data will be stored.  Error raised if not the siteID
               of the local Madrigal site. Example: 4

    --startDate - new start date of experiment (UT).  In form YYYY-MM-DD.  Example: 1998-01-20

    --startTime - new start time of experiment (UT).  In form HH:MM:DD.  Example: 12:30:00

    --endDate - new end date of experiment (UT).  In form YYYY-MM-DD.  Example: 1998-01-21

    --endTime - new end time of experiment (UT).  In form HH:MM:DD.  Example: 23:30:00

    --inst - new instrument code.  Example: 30

    --security - new security code.  Allowed values are 0 for public, 1 for private (limited IP range access)
                -1 for ignore, 2 for archived experiment, 3 for private (limited IP range access) archived
                experiment.
"""

import sys
import os, os.path
import getopt
import traceback
import datetime

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['expDir=',
               'expUrl=',
               'expName=',
               'siteID=',
               'startDate=',
               'startTime=',
               'endDate=',
               'endTime=',
               'inst=',
               'security=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
expDir = None
expUrl = None
expName = None
siteID = None
startDate = None
startTime = None
endDate = None
endTime = None
inst = None
security = None


for opt in optlist:
    if opt[0] == '--expDir':
        expDir = opt[1]
    elif opt[0] == '--expUrl':
        expUrl = opt[1]
    elif opt[0] == '--expName':
        expName = opt[1]
    elif opt[0] == '--siteID':
        siteID = int(opt[1])
    elif opt[0] == '--startDate':
        startDate = opt[1]
    elif opt[0] == '--startTime':
        startTime = opt[1]
    elif opt[0] == '--endDate':
        endDate = opt[1]
    elif opt[0] == '--endTime':
        endTime = opt[1]
    elif opt[0] == '--inst':
        inst = int(opt[1])
    elif opt[0] == '--security':
        security = opt[1]
        
    else:
        print usage
        raise 'Illegal option %s\n%s' % (opt[0], opt[1])
    

# check that all required arguments passed in
if expDir == None:
    print '--expDir argument required - must be full path to experiment directory'
    print usage
    sys.exit(0)

if expUrl == None and expName == None and siteID == None and \
   startDate == None and startTime == None and endDate == None and \
   endTime == None and inst == None and security == None:
    print 'No experiment attributes are being changed'
    print usage
    sys.exit(0)


# create startDatetime
if startDate == None and startTime == None:
    startDatetime = None
else:
    if startDate == None or startTime == None:
        raise ValueError, 'Both startDate and startTime must be set if either set'
    items = startDate.split('-')
    year = int(items[0])
    month = int(items[1])
    day = int(items[2])
    items = startTime.split(':')
    hour = int(items[0])
    minute = int(items[1])
    second = int(items[2])
    startDatetime = datetime.datetime(year, month, day, hour, minute, second)

    
# create endDatetime
if endDate == None and endTime == None:
    endDatetime = None
else:
    if endDate == None or endTime == None:
        raise ValueError, 'Both endDate and endTime must be set if either set'
    items = endDate.split('-')
    year = int(items[0])
    month = int(items[1])
    day = int(items[2])
    items = endTime.split(':')
    hour = int(items[0])
    minute = int(items[1])
    second = int(items[2])
    endDatetime = datetime.datetime(year, month, day, hour, minute, second)
    

adminObj = madrigal.admin.MadrigalDBAdmin()

adminObj.changeExpStatus(expDir,
                         expUrl,
                         expName,
                         siteID,
                         startDatetime,
                         endDatetime,
                         inst,
                         security)


print 'Status of experiment %s changed in local metadata - run updateMaster to register' % (expDir)


