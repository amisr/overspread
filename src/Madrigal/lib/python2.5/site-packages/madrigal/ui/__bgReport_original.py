"""__bgReport is the module that runs in a separate process to create a background report.

    __bgReport contains a number of constants that can be modified to change the rules for
    generating background reports.  These constants are:

    tempDir - the name of the temporary directory on the web server where the reports are saved.
    This directory is located directly beneath the html directory defined in madrigal.cfg as MAD + SERVERDOCABS.

    numDays - the number of days a temporary report is kept on the web server before being deleted
    by this script when it runs.  This script will delete all reports older than numDays each time it
    runs.


$Id: __bgReport_original.py,v 1.11 2009/03/06 21:37:12 brideout Exp $
"""
import os
import os.path
import pickle
import sys
import smtplib
import traceback
import random
import time
import glob
import copy
# use gzip module or gzip application
try:
    import gzip
except:
    pass




def __getLock(filename):
    """__getLock is a private helper function that provides exclusive access to filename via a locking file.

    Inputs: filename = the file that exclusive access is required to.
    
    Returns: None

    Affects: Writes file filename + .LCK as a lock mechanism

    Exceptions: MadrigalError thrown if unable to write lock file

    Notes: Will sleep for 1 second at a time, for a maximum of 10 seconds 
    if the file is not modified. After each second, it will check for the lock file to be removed
    or modified. If it was modified, it resets the count to 0 sec and starts counting again. After
    _MaxSleep counts it then assumes lock file is orphaned and returns.  Orphaned file will be
    removed when dropLock is called.
    """
    gotLock = 0
    numTries = 0
    modificationTime = 0
    
    while (not gotLock):

        try:
            file = os.open(filename + '.LCK', os.O_RDWR | os.O_CREAT | os.O_EXCL)
            os.close(file)
            gotLock = 1

        except OSError, (errno, strerror):
            # error 17 is "File exists"
            if errno != 17:
                raise madrigal.admin.MadrigalError("Unable to open " + filename + ".LCK as locking file ", None)
            # get modification time - may throw an error if file has disappearred
            try:
                newModTime = os.stat(filename + '.LCK')[stat.ST_MTIME]
            except:
                #file has disappeared, no need to sleep
                continue

            # if the lock file has been modified (or if this is the first time through) set numTries = 0
            if newModTime > modificationTime:
                modificationTime = newModTime
                numTries = 0
                
            time.sleep(1)
            
        
        numTries = numTries + 1

        if numTries > 10:
            return

   
def __dropLock(filename):
    """__dropLock is a private helper function that drops exclusive access to filename via a locking file.

    Inputs: filename = the file that exclusive access is required to.
    
    Returns: None

    Affects: Removes file filename + .LCK as a lock mechanism

    Exceptions: None.
    """
    try:
        os.remove(filename + '.LCK')

    except IOError:
        return


def __getFilterStrList(filterList):
    """__getFilterStrList is a private helper function that creates 6 list describing filters needed by _Madrec.getIsprintReport.

    Inputs: filterList - list of filter strings as passed in by report.py (See report.py for description).
    
    Returns: List of Six lists as follows:

        1. List giving filter types, len = # filters, items are '1','*','/','+', or '-'
        
        2. List with mnemonics of filter parameter 1 (items = # filters)

        3. List with mnemonics of filter parameter 2 (items = # filters) (may be None)
        
        4. List with number of ranges per filter (items = # filters)
        
        5. List with doubles of filter lower limits (items = sum of number of ranges above)
        
        6. List with doubles of filter upper limits (items = sum of number of ranges above)

    Exceptions: None.
    """

    # create list of lists
    retList = [[],[],[],[],[],[]]

    # loop through filters
    for filter in filterList:
        # skip empty strings
        if len(filter) == 0:
            continue
        tempList = filter.split(',')
        # see if it has two mnemonics
        if tempList[1].strip() in ('*', '/', '+', '-'):
            retList[0].append(tempList[1].strip())
            retList[1].append(tempList[0].strip())
            retList[2].append(tempList[2].strip())
            # get number of filters
            numFilters = (len(tempList) - 3)/2
            retList[3].append(numFilters)
            for i in range(numFilters):
                retList[4].append(tempList[3 + 2*i].strip())
                retList[5].append(tempList[4 + 2*i].strip())
        else:
            # only one menmonic given
            retList[0].append('1')
            retList[1].append(tempList[0].strip())
            retList[2].append(None)
            # get number of filters
            numFilters = (len(tempList) - 1)/2
            retList[3].append(numFilters)
            for i in range(numFilters):
                retList[4].append(tempList[1 + 2*i].strip())
                retList[5].append(tempList[2 + 2*i].strip())
            

    return retList
    

# Name of temp url directory
tempDir = 'tempReports'
# number of days to keep temp files
numDays = 10
# name of file holding presently running global query pid's
__queryPidFile = 'queryPid.txt'

# status indicator - used in heading of email
status = 'Success'

# if any exception is thrown, email both the user (if possible)
# and the madrigal administrator

email = None

# check if pythonlibpath env variable exists
# written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
temp = os.environ.get('PYTHON' + 'LIBPATH')
if temp != None:
        sys.path.append(temp)
        
# append path madroot/lib (needed only if python not installed by setup)
sys.path.append('MADROOT/lib/python')

import madrigal.metadata
import madrigal._Madrec
import madrigal.admin

# create needed MadrigalDB object
madDB = madrigal.metadata.MadrigalDB()


try:

    # parse arguments passed in as pickle strings
    # argv[0] is name of python exectuable and is ignored
    # argv[1] is the name of this file and is ignored


    # get email address
    email = pickle.loads(sys.argv[1])

    # get mailserver
    mailserver = pickle.loads(sys.argv[2])

    # get filenameList
    filenameList = pickle.loads(sys.argv[3])

    # get parmList
    parmList = pickle.loads(sys.argv[4])

    # get filterList
    filterList = pickle.loads(sys.argv[5])

    # get headerStr
    headerStr = pickle.loads(sys.argv[6])

    # get summaryLevelIndicator
    summaryLevelIndicator = pickle.loads(sys.argv[7])

    # for now we never display headers
    displayHeaders = 0

    # get max temp reports
    maxTempReports = madDB.getMaxTempReports()

    # remove any files older than numDays
    # get present local time
    now = time.time()
    # get dir listing of temp folder
    fileList = os.listdir(madDB.getDocDirPath() + '/' + tempDir)
    for name in fileList:
        if len(name) > 7:
            # must be at least seven characters of the form 'mad*'
            if name[0:3] == 'mad':
                # get its creation time
                createTime = os.stat(madDB.getDocDirPath() + '/' + tempDir + '/' + name)[8]
                # if its too old, delete it
                if now > createTime + numDays*24*60*60.0:
                    os.remove(madDB.getDocDirPath() + '/' + tempDir + '/' + name)
  

    # create needed MadrigalMetaFile object
    metaFileObj = madrigal.metadata.MadrigalMetaFile(madDB)

    # create a random file name
    urlName = 'mad' + str(random.randrange(1,10000000)) + '.txt'

    # write header info to file
    urlPath = madDB.getDocDirPath() + '/' + tempDir + '/' + urlName
    urlFile = open(urlPath, 'w')
    urlFile.write('Global search results\n')
    urlFile.write(headerStr)
    urlFile.write('\n')
    urlFile.close()

    # convert arguments in filterList to form required by _Madrec.getIsprintReport
    filtStrList = __getFilterStrList(filterList)


    # loop through each file and generate report
    isFirst = 1
    isOkay = True

    for madFile in filenameList:

        # verify enough room in tempDir
        compressedFiles = glob.glob(madDB.getDocDirPath() + '/' + tempDir + '/*txt*')
        totalBytes = 0
        for f in compressedFiles:
            totalBytes += os.stat(f)[6]
        if maxTempReports * 1.0e9 < totalBytes:
            isOkay = False
            break
        

        # open the output file to write file summary info:
        urlFile = open(urlPath, 'a')
        # get file metadata summary
        filename = os.path.basename(madFile)
        fileMetadataSummary = metaFileObj.getMetadataSummaryByFilename(filename)
        if summaryLevelIndicator == 0 or isFirst == 1:
            urlFile.write('\n' + madFile + ':\n')
            urlFile.write(fileMetadataSummary + '\n')
            displaySummary = 1
        else:
            displaySummary =0
        isFirst = 0
        urlFile.close()
    
        madrigal._Madrec.getIsprintReport(madFile,
                                          '',
                                          parmList,
                                          filtStrList[0],
                                          filtStrList[1],
                                          filtStrList[2],
                                          filtStrList[3],
                                          filtStrList[4],
                                          filtStrList[5],
                                          displayHeaders,
                                          displaySummary,
                                          0,
                                          'missing',
                                          'assumed',
                                          'knownBad',
                                          urlPath)




    # check we didn't run out of room
    if not isOkay:
        status = 'Failure - ran out of disk space'
        f = open(urlPath, 'a')
        f.write('\n\t****Failure - ran out of disk quota****\n')
        f.close()

        
    # check that at least one file was passed in
    if len(filenameList) == 0:
        status = 'Failure - no files selected'

    # compress urlPath file if needed
    size = (os.stat(urlPath))[6]
    if size > 20000000:
        try:
            f = open(urlPath)
            gzipFile = gzip.GzipFile(urlPath + '.gz', 'w')
            gzipFile.write(f.read())
            gzipFile.close()
            f.close()
            os.remove(urlPath)
            urlPath += '.gz'
            urlName += '.gz'
        except:
            try:
                f.close()
            except:
                pass
            cmd = 'gzip ' + urlPath
            os.system(cmd)
            # verify success
            if os.access(urlPath + '.gz', os.R_OK):
                urlPath += '.gz'
                urlName += '.gz'


    # create subject line
    subjectStr = 'Madrigal Global Query on ' + time.strftime('%d %b %Y',time.localtime()) + \
                 ': ' + status

    # create from_addr
    from_addr = madDB.getContactEmail().split(',')[0]


    # email the user with the url
    messBody = 'The report you requested is now available at the following url:\n\n' + \
              madDB.getTopLevelUrl() + '/' + tempDir + '/' + urlName + '\n\n' + \
              'It will be available for at least ' + str(numDays) + ' days.\n\n' + \
              'This report was generated using the following criteria:\n\n' + headerStr + '\n\n' + \
              'Local rules of the road for data access:\n' + madDB.getLocalRulesOfRoad()

    if not isOkay:
        messBody = '\n\t****Failure - ran out of disk quota****\n\n' + messBody
    
    message = 'From: Background_Madrigal_Query\n' + \
              'To: ' + email + '\n' + \
              'Subject: ' + subjectStr + '\n' + \
              'Content-type: text/plain\n\n' + messBody

    server = smtplib.SMTP(mailserver)
    server.sendmail(from_addr, email, message)
    server.quit()

    # remove this pid from the queryPid.txt file
    pid = os.getpid()
    # lock access to queryPid.txt
    queryFileName = madDB.getMetadataDir() + '/userdata/' + __queryPidFile
    __getLock(queryFileName)
    # if file exists
    if os.access(queryFileName, os.R_OK):
        queryFile = open(queryFileName, 'r')
        pidList = queryFile.read().split()
        queryFile.close()

        # loop through pid list, removing this one if there
        for item in pidList:
            if item == str(pid):
               pidList.remove(item)
               break

        # if no remaining processes, remove file and return 0
        if len(pidList) == 0:
            try:
                os.remove(queryFileName)
            except:
                pass

        # otherwise rewrite queryPid.txt file
        else:
            queryFile = open(queryFileName, 'w')
            delimiter = ' '
            queryFile.write(delimiter.join(pidList))
            queryFile.close()
        
    __dropLock(queryFileName)
        

    # raise an exception if not okay
    if not isOkay:
        raise 'Warning: Madrigal temporary report directory %s over disk quota of %s GB' % (madDB.getDocDirPath() + '/' + tempDir,
                                                                                         str(maxTempReports))
        
        

except:
    # some unexpected exception occurred - let user and administrator know
    
    # create errorMessage
    delimiter = ' '
    errorMessage = 'An exception occurred running madrigal.ui.__bgReport.\n\n' + \
                   delimiter.join(traceback.format_exception(sys.exc_info()[0],
                                                             sys.exc_info()[1],
                                                             sys.exc_info()[2]))

    # email it first to the administrator (most important)
    adminObj = madrigal.admin.MadrigalNotify()
    adminObj.sendAlert(errorMessage, 'Error running Madrigal background report')

    # now email the same message to the user if email != None
    from_addr = madDB.getContactEmail().split(',')[0]
    if email != None:
        # set up message
        message = 'From: Background_Madrigal_Query\n' + \
                  'To: ' + email + '\n' + \
                  'Subject: BackgroundMadrigalQuery Failed!\n' + \
                  'Content-type: text/plain\n\n' + errorMessage

        server = smtplib.SMTP(mailserver)
        server.sendmail(from_addr, email, errorMessage)
        server.quit()    
    
              

    
