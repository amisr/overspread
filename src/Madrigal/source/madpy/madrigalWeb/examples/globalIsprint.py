#!/usr/bin/env python


"""This script runs a global search through Madrigal data from a given URL.

    This script is a stand-alone application, and can be run from anywhere with a connection to the internet.
    It runs on either unix or windows.  It requires only the MadrigalWeb python module to be installed.
    
Usage:

        globalIsprint --url=<Madrigal url> --parms=<Madrigal parms> --output=<output file> \
            --user_fullname=<user fullname> --user_email=<user email> \
            --user_affiliation=<user affiliation> [options]

        where:

        --url=<Madrigal url> - url to homepage of site to be searched
                                  (ie, http://www.haystack.mit.edu/madrigal/)
                                  This is required.

        --parms=<Madrigal parms> - a comma delimited string listing the desired Madrigal parameters
                                    in mnemonic form.  (Example: gdalt,dte,te).  Data will be returned
                                    in the same order as given in this string. See
                                    http://madrigal.haystack.mit.edu/cgi-bin/madrigal/getMetadata and
                                    choose "Parameter code table" for all possible parameters

        --output=<output file name> - the file name to store the resulting data.

        --user_fullname=<user fullname> - the full user name (probably in quotes unless your name is
                                          Sting or Madonna)

        --user_email=<user email>

        --user_affiliation=<user affiliation> - user affiliation.  Use quotes if it contains spaces.

        and options are:

        --startDate=<MM/DD/YYY> - start date to filter experiments before.  Defaults to allow all experiments.

        --endDate=<MM/DD/YYY> - end date to filter experiments after.  Defaults to allow all experiments.

        --inst=<instrument list> - comma separated list of instrument codes or names.  See Madrigal documentation
                                   for this list.  Defaults to allow all instruments. If names are given, the
                                   argument must be enclosed in double quotes.  An asterick will perform matching as
                                   in glob.  Examples: (--inst=10,30 or --inst="Jicamarca IS Radar,Arecibo*")

        --expName  - filter experiments by the experiment name.  Give all or part of the experiment name. Matching
                     is case insensitive.  Default is no filtering by experiment name.

        --kindat=<kind of data list> - comma separated list of kind of data codes.  See Madrigal documentation
                                       for this list.  Defaults to allow all kinds of data.  If names are given, the
                                       argument must be enclosed in double quotes.  An asterick will perform matching as
                                       in glob. Examples: (--kindat=3001,13201 or 
                                       --kindat="INSCAL Basic Derived Parameters,*efwind*,2001")

        --filter=<[mnemonic] or [mnemonic1,[+-*/]mnemonic2]>,<lower limit1>,<upper limit1>[or<lower limit2>,<upper limit2>...]
                            a filter using any measured or derived Madrigal parameter, or two Madrigal parameters either added,
                            subtracted, multiplied or divided.  Each filter has one or more allowed ranges.  The filter accepts
                            data that is in any allowed range.  If the Madrigal parameter value is missing, the filter will always
                            reject that data.  Multiple filter arguments are allowed on the command line.  To skip either a lower
                            limit or an upper limit, leave it blank.  Examples: (--filter=ti,500,1000  (Accept when 500 <= Ti <= 1000)
                            or --filter=gdalt,-,sdwht,0,  (Accept when gdalt > shadowheight - that is, point in direct sunlight)
                            or  --filter=gdalt,200,300or1000,1200 (Accept when 200 <= gdalt <= 300 OR 1000 <= gdalt <= 1200))

        --seasonalStartDate=<MM/DD> - seasonal start date to filter experiments before.  Use this to select only part of the
                                year to collect data.  Defaults to Jan 1.  Example:
                                (--seasonalStartDate=07/01) would only allow experiments after July 1st from each year.

        
        --seasonalEndDate=<MM/DD> - seasonal end date to filter experiments after.  Use this to select only part of the
                                    year to collect data.  Defaults to Dec 31.  Example: 
                                    (--seasonalEndDate=10/31) would only allow experiments before Oct 31 of each year.

        --showFiles - if given, show file names.  Default is to not show file names.

        --showSummary - if given, summarize all arguments at the beginning.  Default is to not show summary.
        
        --includeNonDefault - if given, include all files, including history.  Default is to search only default files.

        --missing=<missing string> (defaults to "missing")

        --assumed=<assumed string> (defaults to "assumed")

        --knownbad=<knownbad string> (defaults to "knownbad")

        --verbose - if given, print each file processed info to stdout.  Default is to run silently.

$Id: globalIsprint.py,v 1.9 2009/05/01 19:11:26 brideout Exp $
"""

usage = """
        Usage:

        globalIsprint --url=<Madrigal url> --parms=<Madrigal parms> --output=<output file> \
            --user_fullname=<user fullname> --user_email=<user email> \
            --user_affiliation=<user affiliation> [options]

        where:

        --url=<Madrigal url> - url to homepage of site to be searched
                                  (ie, http://www.haystack.mit.edu/madrigal/)
                                  This is required.

        --parms=<Madrigal parms> - a comma delimited string listing the desired Madrigal parameters
                                    in mnemonic form.  (Example: gdalt,dte,te).  Data will be returned
                                    in the same order as given in this string. See
                                    http://madrigal.haystack.mit.edu/cgi-bin/madrigal/getMetadata and
                                    choose "Parameter code table" for all possible parameters

        --output=<output file name> - the file name to store the resulting data.

        --user_fullname=<user fullname> - the full user name (probably in quotes unless your name is
                                          Sting or Madonna)

        --user_email=<user email>

        --user_affiliation=<user affiliation> - user affiliation.  Use quotes if it contains spaces.

        and options are:

        --startDate=<MM/DD/YYY> - start date to filter experiments before.  Defaults to allow all experiments.

        --endDate=<MM/DD/YYY> - end date to filter experiments after.  Defaults to allow all experiments.

        --inst=<instrument list> - comma separated list of instrument codes or names.  See Madrigal documentation
                                   for this list.  Defaults to allow all instruments. If names are given, the
                                   argument must be enclosed in double quotes.  An asterick will perform matching as
                                   in glob.  For example:
                                   
           --inst=10,30
           
           --inst="Jicamarca IS Radar,Arecibo*"

        --expName  - filter experiments by the experiment name.  Give all or part of the experiment name. Matching
                     is case insensitive.  Default is no filtering by experiment name.

        --kindat=<kind of data list> - comma separated list of kind of data codes.  See Madrigal documentation
                                       for this list.  Defaults to allow all kinds of data.  If names are given, the
                                       argument must be enclosed in double quotes.  An asterick will perform matching as
                                       in glob. For example:
                                   
            --kindat=3001,13201
        
            --kindat="INSCAL Basic Derived Parameters,*efwind*,2001"

        --filter=<[mnemonic] or [mnemonic1,[+-*/]mnemonic2]>,<lower limit1>,<upper limit1>[or<lower limit2>,<upper limit2>...]
                            a filter using any measured or derived Madrigal parameter, or two Madrigal parameters either added,
                            subtracted, multiplied or divided.  Each filter has one or more allowed ranges.  The filter accepts
                            data that is in any allowed range.  If the Madrigal parameter value is missing, the filter will always
                            reject that data.  Multiple filter arguments are allowed on the command line.  To skip either a lower
                            limit or an upper limit, leave it blank.  For example:

            filter=ti,500,1000  (Accept when 500 <= Ti <= 1000)

            filter=gdalt,-,sdwht,0,  (Accept when gdalt > shadowheight - that is, point in direct sunlight)

            filter=gdalt,200,300or1000,1200 (Accept when 200 <= gdalt <= 300 OR 1000 <= gdalt <= 1200)

        --seasonalStartDate=<MM/DD> - seasonal start date to filter experiments before.  Use this to select only part of the
                                year to collect data.  Defaults to Jan 1.  Example:  
                                
            --seasonalStartDate=07/01 would only allow experiments after July 1st from each year.

        
        --seasonalEndDate=<MM/DD> - seasonal end date to filter experiments after.  Use this to select only part of the
                                    year to collect data.  Defaults to Dec 31.  Example:  
                                    
            --seasonalEndDate=10/31 would only allow experiments before Oct 31 of each year.

        --showFiles - if given, show file names.  Default is to not show file names.

        --showSummary - if given, summarize all arguments at the beginning.  Default is to not show summary.
        
        --includeNonDefault - if given, include all files, including history.  Default is to search only default files.

        --missing=<missing string> (defaults to "missing")

        --assumed=<assumed string> (defaults to "assumed")

        --knownbad=<knownbad string> (defaults to "knownbad")

        --verbose - if given, print each file processed info to stdout.  Default is to run silently.
                                    
"""

import sys
import os
import time
import traceback
import getopt
import re
import datetime
import fnmatch

import madrigalWeb.madrigalWeb

def getInstrumentList(inst, server):
    """getInstrumentList takes the user argument inst and coverts it into a list of instrument codes.

    Inputs:

        inst - a string containing a comma separated list of instrument codes or names.  If names are given,
                the argument must be enclosed in double quotes.  An asterick will perform matching as in glob.
                Both names and codes may be mixed together.

        server - the active MadrigalData object to get information from

    Returns:

        a list of instrument codes (int).  Instrument code 0 means all instruments
    """
    if inst == '0':
        return [0]

    retList = []
            
    # make a list
    stringList = inst.split(',')
    # see if any are names
    nameFound = 0
    for item in stringList:
        try:
            int(item)
        except:
            nameFound = 1
            break
        
    if nameFound == 0:
        # all codes
        for item in stringList:
            retList.append(int(item))
        return retList


    # at least one name found - get a list of all instruments
    allInst = server.getAllInstruments()
    # loop through each inst
    for item in stringList:
        # if its an int, just add it
        try:
            code = int(item)
            if code not in retList:
                retList.append(code)
            continue
        except:
            pass
        # its a name if it made it here
        # see if its an exact match or a regular expression with *
        if item.find('*') == -1:
            # exact match (case insensitive)
            instFound = 0
            for thisInst in allInst:
                if item.lower() == thisInst.name.lower():
                    retList.append(thisInst.code)
                    instFound = 1
                    break
            # print warning if none found
            if instFound == 0:
                print 'Warning: unable to find instrument ' + str(item)
        else:
            # use regular expression matching
            instFound = 0
            reObj = re.compile(item.replace('*', '.*'))
            for thisInst in allInst:
                m = reObj.search(thisInst.name)
                if m != None:
                    retList.append(thisInst.code)
                    instFound = 1
        
            # print warning if none found
            if instFound == 0:
                print 'Warning: unable to find instrument ' + str(item)

    return retList


def filterExperimentsUsingSeason(expList, seasonalStartDate, seasonalEndDate):
    """filterExperimentsUsingSeason returns a subset of the experiments in expList whose date is within the given season.

    Input:

        expList - a list of MadrigalExperiment objects to be filtered

        seasonalStartDate - in form MM/DD - seasonal start date to filter experiments before

        seasonalEndDate - in form MM/DD - seasonal end date to filter experiments after

    Returns:

        a subset of expList whose times are accepted
    """
    # parse seasonalStartDate and seasonalEndDate
    dateList = seasonalStartDate.split('/')
    if len(dateList) != 2:
        raise 'seasonalStartDate must be in form MM/DD: ' + str(seasonalStartDate)
    try:
        startmonth = int(dateList[0])
        startday = int(dateList[1])
    except:
        raise 'seasonalStartDate must be in form MM/DD: ' + str(seasonalStartDate)

    if startmonth < 1 or startmonth > 12 or startday < 1 or startday > 31:
        raise 'seasonalStartDate must be in form MM/DD: ' + str(seasonalStartDate)

    dateList = seasonalEndDate.split('/')
    if len(dateList) != 2:
        raise 'seasonalEndDate must be in form MM/DD: ' + str(seasonalEndDate)
    try:
        endmonth = int(dateList[0])
        endday = int(dateList[1])
    except:
        raise 'seasonalEndDate must be in form MM/DD: ' + str(seasonalEndDate)

    if endmonth < 1 or endmonth > 12 or endday < 1 or endday > 31:
        raise 'seasonalEndDate must be in form MM/DD: ' + str(seasonalEndDate)

    retList = []

    # now loop through all experiments and add those that pass
    for exp in expList:
        if exp.startmonth < startmonth:
            continue
        elif exp.startmonth == startmonth and exp.startday < startday:
            continue
        if exp.endmonth > endmonth:
            continue
        elif exp.endmonth == endmonth and exp.endday > endday:
            continue
        # accept
        retList.append(exp)

    return retList


def filterExperimentsUsingExpName(expList, expName):
    """filterExperimentsUsingExpName returns a subset of the experiments in expList whose name matches.

    Input:

        expList - a list of MadrigalExperiment objects to be filtered

        expName  - filter experiments by the experiment name.  Can be all or part of the experiment name. Matching
                     is case insensitive.

    Returns:

        a subset of expList whose names are accepted
    """
    retList = []
    expNameArg = '*%s*' % (expName.replace(' ', '_')) # since we are using fnmatch

    # now loop through all experiments and add those that pass
    for exp in expList:
        try:
            thisExpName = exp.name.replace(' ', '_')
        except:
            continue

        if not fnmatch.fnmatch(thisExpName.lower(), expNameArg.lower()):
            continue

        # accept
        retList.append(exp)

    return retList


def getExperimentFileList(server, expList):
    """getExperimentFileList returns a list of MadrigalExperimentFile objects given an experiment list.

    Inputs::

        server - the active MadrigalData object to get information from
        
        expList - the list of desired MadrigalExperiment objects

    Returns:

        a list of MadrigalExperimentFile objects
    """
    retList = []

    for exp in expList:
        try:
            theseExpFiles = server.getExperimentFiles(exp.id)
        except:
            # skip experiments with no files
            continue
        for expFile in theseExpFiles:
            retList.append(expFile)

    return retList


def filterExperimentFilesUsingKindat(expFileList, kindat):
    """filterExperimentFilesUsingKindat returns a subset of the experiment files in expFileList whose kindat is found in kindat argument.

    Input:

        expFileList - a list of MadrigalExperimentFile objects to be filtered

        kindat - the kindat argument passed in by the user - comma separated list of kind of data codes.  If names are given, the
                argument must be enclosed in double quotes.  An asterick will perform matching as in glob.

    Returns:

        a subset of expFileList whose kindat values are accepted
    """
    strList = kindat.split(',')

    # create lists of kindat ints, kindat names, and kindat regular expressions
    kindatCodeList = []
    kindatNameList = []

    for item in strList:
        try:
            value = int(item)
            kindatCodeList.append(value)
            continue
        except:
            pass
        # a non-integer found
        testName = '*' + item.lower().replace(' ', '_') + '*'
        kindatNameList.append(testName)

    # now loop through each experiment file, and add it to a new list if its accepted
    retList = []
    for expFile in expFileList:
        # code match
        if expFile.kindat in kindatCodeList:
            retList.append(expFile)
            continue
        # description match
        try:
            kindatDesc = expFile.kindatdesc.lower()
        except:
            continue
        kindatDesc = kindatDesc.replace(' ', '_')
        for kindatName in kindatNameList:
            if fnmatch.fnmatch(kindatDesc, kindatName):
                retList.append(expFile)
                break

    return retList



def filterExperimentFilesUsingStatus(expFileList):
    """filterExperimentFilesUsingStatus returns a subset of the experiment files in expFileList with default status.

    Input:

        expFileList - a list of MadrigalExperimentFile objects to be filtered.

    Returns:

        a subset of expFileList with default status
    """

    retList = []
    for expFile in expFileList:
        if expFile.category == 1:
            retList.append(expFile)

    return retList

def getTimesOfExperiment(expList, expId):
    """getTimesOfExperiment returns a list of the start and end time of the experiment given expId.

    Input:

        expList - the list of MadrigalExperiment objects

        expId - the experiment id

    Returns:

        a list of:
            (startyear,
            startmonth,
            startday,
            starthour,
            startmin,
            startsec,
            endyear,
            endmonth,
            endday,
            endhour,
            endmin,
            endsec)
    """

    retList = None
    for exp in expList:
        if exp.id == expId:
            retList = (exp.startyear,
                       exp.startmonth,
                       exp.startday,
                       exp.starthour,
                       exp.startmin,
                       exp.startsec,
                       exp.endyear,
                       exp.endmonth,
                       exp.endday,
                       exp.endhour,
                       exp.endmin,
                       exp.endsec)

    return retList


def getTimeParms(expTimeList, numIter, j):
    """getTimeParms creates arguments to be passed to isprint to get only a slice of an experiment's data

        Input:

            expTimeList: a list of experiment start and end times:startyear, startmonth, startday, starthour,
                startmin, startsec, endyear, endmonth, endday, endhour, endmin, endsec

            numIter - the number of pieces to break the experiment into

            j - this iteration

        Returns - a string in the form ' date1=01/20/1998 time1=09:00:00 date2=01/20/1998 time2=10:30:00 ' that
        will cause isprint to only examine a slice of the data.
    """
    expStartTime = time.mktime((expTimeList[0],
                                expTimeList[1],
                                expTimeList[2],
                                expTimeList[3],
                                expTimeList[4],
                                expTimeList[5],0,0,-1))
    expEndTime = time.mktime((expTimeList[6],
                             expTimeList[7],
                             expTimeList[8],
                             expTimeList[9],
                             expTimeList[10],
                             expTimeList[11],0,0,-1))
    totalExpTime = expEndTime - expStartTime
    begSliceTime = int(((j/float(numIter)) * totalExpTime) + expStartTime)
    endSliceTime = int((((j+1)/float(numIter)) * totalExpTime) + expStartTime)

    begTimeList = time.localtime(begSliceTime)
    endTimeList = time.localtime(endSliceTime)

    
    return ' date1=%i/%i/%i time1=%02i:%02i:%02i date2=%i/%i/%i time2=%02i:%02i:%02i ' % (begTimeList[1],
                                                                                          begTimeList[2],
                                                                                          begTimeList[0],
                                                                                          begTimeList[3],
                                                                                          begTimeList[4],
                                                                                          begTimeList[5],
                                                                                          endTimeList[1],
                                                                                          endTimeList[2],
                                                                                          endTimeList[0],
                                                                                          endTimeList[3],
                                                                                          endTimeList[4],
                                                                                          endTimeList[5])
                                                                                          

    



# parse command line
arglist = ''
longarglist = ['url=',
               'parms=',
               'output=',
               'user_fullname=',
               'user_email=',
               'user_affiliation=',
               'startDate=',
               'endDate=',
               'inst=',
               'kindat=',
               'filter=',
               'seasonalStartDate=',
               'seasonalEndDate=',
               'showFiles',
               'showSummary',
               'includeNonDefault',
               'missing=',
               'assumed=',
               'knownbad=',
               'verbose',
               'expName=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
url = None
parms = None
output = None
user_fullname=None
user_email=None
user_affiliation=None
startDate = None
endDate = None
inst = '0'
kindat = '0'
filterList = []  # since more than one allowed
seasonalStartDate = '01/01'
seasonalEndDate = '12/31'
showFiles = 0
includeNonDefault = 0
showSummary = 0
missing = 'missing'
assumed = 'assumed'
knownbad = 'knownbad'
verbose = 0
expName = None

# check if none passed in
if len(optlist) == 0:
    print usage
    sys.exit(0)
    

for opt in optlist:
    if opt[0] == '--url':
        url = opt[1]
    elif opt[0] == '--parms':
        parms = opt[1]
    elif opt[0] == '--output':
        output = opt[1]
    elif opt[0] == '--user_fullname':
        user_fullname = opt[1]
    elif opt[0] == '--user_email':
        user_email = opt[1]
    elif opt[0] == '--user_affiliation':
        user_affiliation = opt[1]
    elif opt[0] == '--startDate':
        startDate = opt[1]
    elif opt[0] == '--endDate':
        endDate = opt[1]
    elif opt[0] == '--inst':
        inst = opt[1]
    elif opt[0] == '--expName':
        expName = opt[1]
    elif opt[0] == '--kindat':
        kindat = opt[1]
    elif opt[0] == '--filter':
        filterList.append('filter=' + opt[1])
    elif opt[0] == '--seasonalStartDate':
        seasonalStartDate = opt[1]
    elif opt[0] == '--seasonalEndDate':
        seasonalEndDate = opt[1]
    elif opt[0] == '--showFiles':
        showFiles = 1
    elif opt[0] == '--includeNonDefault':
        includeNonDefault = 1
    elif opt[0] == '--showSummary':
        showSummary = 1
    elif opt[0] == '--missing':
        missing = opt[1]
    elif opt[0] == '--assumed':
       assumed  = opt[1]
    elif opt[0] == '--knownbad':
        knownbad = opt[1]
    elif opt[0] == '--verbose':
        verbose = 1

    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

# check that all required arguments passed in
if url == None:
    print '--url argument required - must be the url of the main page of a Madrigal site'
    sys.exit(0)

if parms == None:
    print '--parms argument required - must be a comma-separated list of Madrigal mnemonics'
    sys.exit(0)

if output == None:
    print '--output argument required - must be a valid, writable file path'
    sys.exit(0)

if user_fullname == None:
    print '--user_fullname argument required - must your name'
    sys.exit(0)

if user_email == None:
    print '--user_email argument required - must your email address'
    sys.exit(0)

if user_affiliation == None:
    print '--user_affiliation argument required - must your affiliation'
    sys.exit(0)

# set startDate
if startDate == None:
    startyear = 1950
    startmonth = 1
    startday = 1
else:
    dateList = startDate.split('/')
    if len(dateList) != 3:
        print '--startDate must be in the form MM/DD/YYYY: ' + str(startDate)
        sys.exit(0)
    startmonth = int(dateList[0])
    startday = int(dateList[1])
    startyear = int(dateList[2])
    if startmonth < 1 or startmonth > 12 or startday < 1 or startday > 31:
        print '--startDate must be in the form MM/DD/YYYY: ' + str(startDate)
        sys.exit(0)
    try:
        datetime.datetime(startyear, startmonth, startday)
    except:
        print 'Invalid startDate <%s>' % (str(startDate))
        sys.exit(0)

# set endDate
if endDate == None:
    # chose one year from today
    nextYear = time.time() + 365*24*60*60
    nextYear = time.gmtime(nextYear)
    endyear = nextYear[0]
    endmonth = nextYear[1]
    endday = nextYear[2]
else:
    dateList = endDate.split('/')
    if len(dateList) != 3:
        print '--endDate must be in the form MM/DD/YYYY: ' + str(endDate)
        sys.exit(0)
    endmonth = int(dateList[0])
    endday = int(dateList[1])
    endyear = int(dateList[2])
    if endmonth < 1 or endmonth > 12 or endday < 1 or endday > 31:
        print '--endDate must be in the form MM/DD/YYYY: ' + str(endDate)
        sys.exit(0)
    try:
        datetime.datetime(endyear, endmonth, endday)
    except:
        print 'Invalid endDate <%s>' % (str(endDate))
        sys.exit(0)

    

# verify the url is valid
server = madrigalWeb.madrigalWeb.MadrigalData(url)

# now, create a list of instrument codes desired from the inst argument
instList = getInstrumentList(inst, server)

# get the list of all experiments for the given instruments and time range
expList = server.getExperiments(instList,
                                startyear,
                                startmonth,
                                startday,
                                0,
                                0,
                                0,
                                endyear,
                                endmonth,
                                endday,
                                23,
                                59,
                                59)


# filter experiments using seasonal filter if needed
if seasonalStartDate != '01/01' or seasonalEndDate != '12/31':
    expList = filterExperimentsUsingSeason(expList, seasonalStartDate, seasonalEndDate)

# filter experiments using expName if needed
if expName != None:
    expList = filterExperimentsUsingExpName(expList, expName)


# get list of all experiment files given the expList
expFileList = getExperimentFileList(server, expList)


# filter expFileList using kindat filter if needed
if kindat != '0':
    expFileList = filterExperimentFilesUsingKindat(expFileList, kindat)

# filter using file status if needed
if includeNonDefault == 0:
    expFileList = filterExperimentFilesUsingStatus(expFileList)

# print error if no files selected
if len(expFileList) == 0:
    print 'No files selected with these arguments'
    sys.exit(0)

# open output file
outputFile = open(output, 'w')

# print summary if desired:
if showSummary:
    summary = 'global isprint run %s with the following arguments:\n' % (time.asctime())
    for arg in sys.argv[1:]:
        summary += '%s\n' % (str(arg))
    outputFile.write(summary)

# print header in all cases
delimiter = '  '
header = delimiter.join(parms.split(','))
outputFile.write(header + '\n')

# print message if verbose
numFiles = len(expFileList)
if verbose:
    print "%i files being analyzed" % (numFiles)

# isprint to output file from each expFileList
delimiter = ' '
filterStr = delimiter.join(filterList)
for i in range(numFiles):
    if verbose:
        print  'Analyzing file %i of %i: %s' % (i+1, numFiles, expFileList[i].name)
    if showFiles:
        outputFile.write('%s\n' % (expFileList[i].name))
    try:
        data = server.isprint(expFileList[i].name,
                              parms,
                              filterStr,
                              user_fullname,
                              user_email,
                              user_affiliation)
        # modify any special value
        if missing != 'missing':
            data = data.replace('missing', missing)
        if assumed != 'assumed':
            data = data.replace('assumed', assumed)
        if missing != 'knownbad':
            data = data.replace('knownbad', knownbad)

        # skip error message if No records selected
        if showFiles == 0:
            if data.find('No records') != -1:
                continue
        
        outputFile.write(data[1:]) # skip leading space
        
    except:
        # assume isprint timed out - try again by breaking the experiment into pieces
        expTimeList = getTimesOfExperiment(expList, expFileList[i].expId)
        numIter = 50 # number of pieces to break exp into
        for j in range(numIter):
            newParms = getTimeParms(expTimeList, numIter, j)
            try:
                data = server.isprint(expFileList[i].name,
                              parms,
                              filterStr + newParms,
                              user_fullname,
                              user_email,
                              user_affiliation) 

                # modify any special value
                if missing != 'missing':
                    data = data.replace('missing', missing)
                if assumed != 'assumed':
                    data = data.replace('assumed', assumed)
                if missing != 'knownbad':
                    data = data.replace('knownbad', knownbad)

                # skip error message if No records selected
                if showFiles == 0:
                    if data.find('No records') != -1:
                        continue
                
                outputFile.write(data[1:]) # skip leading space
            except:
                if verbose:
                    print  'Failure analyzing file %s with slice %s' % (expFileList[i].name, newParms)
                continue


outputFile.close()




