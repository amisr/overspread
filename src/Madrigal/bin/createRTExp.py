#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: createRTExp.py,v 1.5 2008/05/08 18:34:54 brideout Exp $

usage = """
createRTExpWithFile.py is a script used to create a new Madrigal experiment
that will contain real-time files.  These real-time files are assumed not to exist
yet.

Required arguments::

    --startDate - experiment start date in form YYYY-MM-DD

    --inst - instrument code or 3-letter Madrigal mnenonic

    --expTitle - experiment title. Use quotes if title contains spaces.
    
    --rtFiles - comma-separated list of realtime file basenames to be created

    --kindats - comma-separated list of ints or single int of kindats for each realtime file.
                The length and order must be the same as rtFiles.  If only one
                given, it is assumed that all rtFiles have the same kindat.

    --fileDescs - comma-separated list of file descriptions. If the file description contains spaces,
                  quotes must be used.

Optional argument::

    --numDays - number of days the experiment is estimated to run - if not given, and no start
                and end times specified, defaults to one day.  Error raised if endDate and endTime
                also specified.

    --startTime - start time in form HH:MM:DD.  Defaults to 00:00:00

    --endDate - end day in form YYYY-MM-DD. endTime must also be specified

    --endTime - end time in form HH:MM:DD.  endDate must also be specified
    
    --permissions - comma-separated list of 0 for public, 1 for private (restricted to certain IP range).
                   If only one given, it is assumed it applied to all.  If this argument is not
                   given, it defaults to 0 (public)

    --dirName  - directory name to use for experiment.  If not given, the directory
                 name will be the default name DDmmmYY[optChar].  Cannot contain "/"

    --optChar  - optional character to be added to experiment directory if no dirName
                 given.  If dirName argument given, this argument ignored.  optChar
                 is used if the default directory name DDmmmYY is used for
                 more than one experiment created for a given instrument on a given day.
                 For example, if --optChar=h for a MLH experiment on September 12, 2005,
                 then the experiment directory created would be experiments/2005/mlh/12sep05h.

    --security - overall experiment access.  0 for public, 1 for private, -1 for ignore.
                 Defaults to public (0)

Example::

    createRTExp.py --startDate=2002-10-01 --numDays=2 --inst=mlh --expTitle="test experiment"
    --rtFiles=mlh021001a.000,mlh021001b.000 --kindats=3410
    --fileDescs="preliminary - single pulse,preliminary - alternating code"
"""

import sys
import os, os.path
import getopt
import traceback
import time, datetime

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['startDate=',
               'numDays=',
               'inst=',
               'expTitle=',
               'rtFiles=',
               'kindats=',
               'fileDescs=',
               'startTime=',
               'endDate=',
               'endTime=',
               'permissions=',
               'dirName=',
               'optChar=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
startDate = None
numDays = None
inst = None
expTitle = None
rtFiles = None
kindats = None
startDate = None
startTime = None
endDate = None
endTime = None
fileDescs = None
permissions = None
dirName = None
security = 0
optChar = ''


for opt in optlist:
    if opt[0] == '--startDate':
        startDate = opt[1]
    elif opt[0] == '--numDays':
        numDays = int(opt[1])
    elif opt[0] == '--inst':
        inst = opt[1]
    elif opt[0] == '--expTitle':
        expTitle = opt[1]
    elif opt[0] == '--rtFiles':
        rtFiles = opt[1]
    elif opt[0] == '--kindats':
        kindats = opt[1]
    elif opt[0] == '--startTime':
        startTime = opt[1]
    elif opt[0] == '--endDate':
        endDate = opt[1]
    elif opt[0] == '--endTime':
        endTime = opt[1]
    elif opt[0] == '--fileDescs':
        fileDescs = opt[1]
    elif opt[0] == '--permissions':
        permissions = opt[1]
    elif opt[0] == '--dirName':
        dirName = opt[1]
    elif opt[0] == '--security':
        security = int(opt[1])
    elif opt[0] == '--optChar':
        optChar = opt[1]
        if len(optChar) != 1:
            raise 'optChar argument must contain exactly one character, not %s' % (optChar)
        
    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

# check that all required arguments passed in
if startDate == None:
    print '--startDate argument required - must be experiment start date in form YYYY-MM-DD'
    print usage
    sys.exit(0)

if inst == None:
    print '--inst argument required - must be instrument code or 3-letter Madrigal mnenonic'
    sys.exit(0)

if expTitle == None:
    print '--expTitle argument required - must be experiment title (use quotes if needed)'
    sys.exit(0)

if expTitle.find('\n') != -1:
    print '--expTitle argument cannot contain a new line character'
    sys.exit(0)

if rtFiles == None:
    print '--rtFiles argument required - must be one or comma-separated list of base file names'
    sys.exit(0)
else:
    rtFiles = rtFiles.split(',')
    count = len(rtFiles)
    for rtFile in rtFiles:
        if rtFile.find('\n') != -1:
            print '--rtFiles argument cannot contain a new line character'
            sys.exit(0)

if kindats == None:
    print '--kindats argument required - must be one or comma-separated list kindat codes'
    sys.exit(0)
else:
    tempList = kindats.split(',')
    kindats = []
    if len(tempList) == 1 and count > 1:
        for i in range(count):
            kindats.append(int(tempList[0]))
    elif len(tempList) != count:
        print 'number of kindats must equal number of realtime file names'
        sys.exit(0)
    else:
        for i in range(count):
            kindats.append(int(tempList[i]))

if fileDescs == None:
    print '--fileDescs argument required - must be comma-separated list of file descriptions'
    sys.exit(0)
else:
    fileDescs = fileDescs.split(',')
    for fileDesc in fileDescs:
        if fileDesc.find('\n') != -1:
            print '--fileDescs argument cannot contain a new line character'
            sys.exit(0)



# deal with time arguments
if startTime == None:
    startDate = datetime.datetime(int(startDate[0:4]),
                                  int(startDate[5:7]),
                                  int(startDate[8:]),0,0,0)
else:
    hour = int(startTime[0:2])
    minute = int(startTime[3:5])
    second = int(startTime[6:])
    startDate = datetime.datetime(int(startDate[0:4]),
                                  int(startDate[5:7]),
                                  int(startDate[8:]),
                                  hour,
                                  minute,
                                  second)

if numDays == None and endDate == None and endTime == None:
    # default is 1 day
    endDate = startDate + datetime.timedelta(1)

if endDate != None or endTime != None:
    hour = int(endTime[0:2])
    minute = int(endTime[3:5])
    second = int(endTime[6:])
    endDate = datetime.datetime(int(endDate[0:4]),
                                int(endDate[5:7]),
                                int(endDate[8:]),
                                hour,
                                minute,
                                second)
    
if numDays != None and endDate == None and endTime == None:
    if numDays < 1:
        print '--numDays must be greater than zero, not %s' % (str(numDays))
        sys.exit(0)
    endDate = startDate + datetime.timedelta(numDays)

if permissions == None:
    permissions = []
    for i in range(count):
        permissions.append(0)
else:
    tempList = premissions.split(',')
    premissions = []
    if len(tempList) == 1 and count > 1:
        for i in range(count):
            permissions.append(int(tempList[0]))
    elif len(tempList) != count:
        print 'number of permissions must equal number of realtime file names'
        sys.exit(0)
    else:
        for i in range(count):
            permissions.append(int(tempList[i]))


adminObj = madrigal.admin.MadrigalDBAdmin()

expDir = adminObj.createRTExperiment(startDate,
                                     numDays,
                                     inst,
                                     expTitle,
                                     rtFiles,
                                     kindats,
                                     permissions,
                                     fileDescs,
                                     optChar,
                                     endDate,
                                     security,
                                     dirName)

print 'New realtime experiment successfully created at %s with file(s) %s - run updateMaster to register' % (expDir,
                                                                              str(rtFiles))
