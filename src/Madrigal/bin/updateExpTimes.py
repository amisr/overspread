#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: updateExpTimes.py,v 1.2 2008/05/14 21:09:04 brideout Exp $

usage = """
updateExpTimes.py [experiment_directory]

updateExpTimes.py is a script that updates experiment start and end times in the metadata
file expTab.txt based on data found in the default files.  If optional argument
experiment_directory given, then it will only update experiments found in that directory.
Default is to update the entire database.
"""

import sys
import os, os.path
import traceback
import datetime

import madrigal.metadata
import madrigal.data
import madrigal.admin

madDB = madrigal.metadata.MadrigalDB()
expRoot = os.path.join(madDB.getMadroot(), 'experiments')

if len(sys.argv) == 2:
    expDir = sys.argv[1]
    if not os.access(expDir, os.R_OK):
        print 'Unable to access directory %s' % (expDir)
        print usage
    if expDir.find(expRoot) == -1:
        print 'Specified directory must be under root directory %s, not %s' % (expRoot, expDir)
        print usage

elif len(sys.argv) == 1:
    expDir = expRoot

else:
    print 'Too many arguments: %s' % (str(sys.argv[1:]))
    print usage


# loop through every experiment
madExpObj = madrigal.metadata.MadrigalExperiment(madDB)
madFileObj = madrigal.metadata.MadrigalMetaFile(madDB)

expModifiedCount = 0
expSkippedCount = 0

for i in range(madExpObj.getExpCount()):
    thisExpDir = madExpObj.getExpDirByPosition(i)
    
    # check whether we need to skip it
    if thisExpDir.find(expDir) == -1:
        continue

    expId = madExpObj.getExpIdByPosition(i)
    
    sy, sm, sd, sh, sM, ss, v1, v2, v3 = madExpObj.getExpStartDateTimeByPosition(i)
    expStartTime = datetime.datetime(sy, sm, sd, sh, sM, ss)
    ey, em, ed, eh, eM, es, v1, v2, v3 = madExpObj.getExpEndDateTimeByPosition(i)
    expEndTime = datetime.datetime(ey, em, ed, eh, eM, es)
    

    # loop through every default experiment file
    startTime = None
    endTime = None
    
    for j in range(madFileObj.getFileCount()):
        if madFileObj.getExpIdByPosition(j) != expId:
            continue
        if madFileObj.getCategoryByPosition(j) not in (1, 4):
            continue
        
        filename = os.path.join(thisExpDir, madFileObj.getFilenameByPosition(j))

        # get start and end time of file
        try:
            fileObj = madrigal.data.MadrigalFile(filename, madDB)
        except:
            print 'Corrupt file %s - skipping' % (filename)
            continue
        
        sy, sm, sd, sh, sM, ss = fileObj.getEarliestTime()
        ey, em, ed, eh, eM, es = fileObj.getLatestTime()

        thisStartTime = datetime.datetime(sy, sm, sd, sh, sM, ss)
        thisEndTime = datetime.datetime(ey, em, ed, eh, eM, es)

        if startTime == None:
            startTime = thisStartTime
            endTime = thisEndTime
            continue

        if thisStartTime < startTime:
            startTime = thisStartTime

        if thisEndTime > endTime:
            endTime = thisEndTime

    if startTime == None:
        print 'No default or realtime files found in experiment %s - not setting times' % (thisExpDir)
        expSkippedCount += 1
        continue

    # see if dates need resetting
    if startTime != expStartTime or endTime != expEndTime:
        print 'Resetting dates from %s - %s to %s - %s in exp %s' % (expStartTime.strftime('%Y-%m-%d %H:%M:%S'),
                                                                     expEndTime.strftime('%Y-%m-%d %H:%M:%S'),
                                                                     startTime.strftime('%Y-%m-%d %H:%M:%S'),
                                                                     endTime.strftime('%Y-%m-%d %H:%M:%S'),
                                                                     thisExpDir)

        expInfo = madrigal.metadata.MadrigalExperiment(madDB, os.path.join(thisExpDir, 'expTab.txt'))
        expInfo.setExpStartDateTimeByPosition(startTime)
        expInfo.setExpEndDateTimeByPosition(endTime)
        expInfo.writeMetadata()
        expModifiedCount += 1

print '%i experiments modified, %i experiments skipped due to no default or realtime files' % (expModifiedCount,
                                                                                               expSkippedCount)
        
                                                                 
        
        
        
