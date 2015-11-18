"""report is the module that creates reports on Madrigal data.

$Id: report.py,v 1.19 2009/02/26 21:38:30 brideout Exp $
"""
import pickle
import os, sys, stat
import time

import madrigal.metadata
import madrigal.admin
import madrigal.data
import madrigal._Madrec

class MadrigalReport:
    """MadrigalReport is the class that produces reports on Madrigal data.


    Non-standard Python modules used:
    None

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  May. 23, 2002
    
    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Feb. 6, 2003
    to use the C Maddata module
    
    Modified by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Apr. 9, 2003
    to add looker reports
    
    """

    # constants

    # maximum number of seconds to wait for a lock
    __MaxSleep     = 10

    # name of file holding presently running global query pid's
    __queryPidFile = 'queryPid.txt'


    def __init__(self, madDB = None):
        """__init__ initializes MadrigalWeb by reading from MadridalDB..

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__metaDir.

        Exceptions: None.
        """

        if madDB == None:
            self.__MadDB = madrigal.metadata.MadrigalDB()
        else:
            self.__MadDB = madDB


    def genBackgroundReport(self,
                            email,
                            filenameList,
                            parmList,
                            filterList,
                            headerStr = '',
                            summaryLevelIndicator = 0):
        
        """ genBackgroundReport starts a separate process that emails a report.

        Inputs:

            email - the email address to send the complete report to

            filenameList - a list of full file paths to collect data from. (May be generated
            by MadrigalDB.getFileList).  The order of the filenames will be the order they
            are listed in the report.

            parmList - list of Mnemonics (may be in the form of integer strings) to be displayed.

            filterList - list of filters to select data.  Each filter is a string of the form
            "mnem1 [,[+, -, /, *],mnem2],[lower limit 1],[upper limit 1][other limits]

            Examples::

                "gdalt,-,sdwht,0,10000"

                "azm,170,180,-180,-170"

            headerStr - a String to insert at the beginning of the report.  (For example,
            can include iformation on the parameters used to select the data.) Defaults to ''

            summaryLevelIndicator:
                0 - data level (the default), include data, and file info.
                1 - same as 0 (data level), but with no file names and parameter labels between data.
                2 - record level, include only record headers and file info.
                3 - cycle level (not yet implemented, for now acts just like experiment level)
                4 - experiment level, include only file info.
        
        Returns: Process id (integer) of background process created.  If too many processes are already
        running, returns -1 and does not generate a report.

        Affects: Generates a separate process to email a report.
        """

        # make sure parmList contains only mnemonics
        parmObj = madrigal.data.MadrigalParameters(self.__MadDB)
        if parmList == None:
            parmList = []
        else:
            newParmList = parmObj.getParmMnemonicList(parmList)

        # make sure too many real queries are not already running
        if self.__getNumQueriesRunning() >= self.__MadDB.getMaxGlobalQueries():
            return -1

        # create arguments for executable
        argList = []
        argList.append(pickle.dumps(email))
        argList.append(pickle.dumps(self.__MadDB.getMailserver()))
        argList.append(pickle.dumps(filenameList))
        argList.append(pickle.dumps(newParmList))
        argList.append(pickle.dumps(filterList))
        argList.append(pickle.dumps(headerStr))
        argList.append(pickle.dumps(summaryLevelIndicator))

        # get dir of this module
        localDir = os.path.dirname(sys.modules['madrigal.ui.web'].__file__)
        

        # run __bgReport
        ret = os.spawnl(os.P_NOWAIT, sys.executable, sys.executable,
                         localDir + '/__bgReport.py',
                         argList[0], argList[1], argList[2], argList[3],
                         argList[4], argList[5], argList[6])

        # add new process id to queryPid.txt
        self.__addPid(ret)


        # return process id
        return ret


    def looker(self,
               parmList,
               start_lat,
               stop_lat,
               step_lat,
               start_lon,
               stop_lon,
               step_lon,
               start_alt,
               stop_alt,
               step_alt,
               year,
               month,
               day,
               hour,
               min,
               sec,
               printHeaderFlag=1,
               oneDParmList = [],
               oneDParmValues = []):
        
        """ looker prints a Madrecord for a range of lat, lon, and alt.

        Inputs:

            parmList - a list of requested parameter mnemonics.
            Do not include gdlat, glon, or gdalt - included by default
            
            start_lat - start latitude in degrees
            
            stop_lat - end latitude in degrees
            
            step_lat - number of degrees in lat to step
            
            start_lon - start longitude in degrees
            
            stop_lon - end longitude in degrees
            
            step_lon - number of longitude in lat to step
            
            start_alt - start altitude in km
            
            stop_alt - stop altitude in km
            
            step_alt - number of km in alt to step
            
            year
            
            month
            
            day
            
            hour
            
            min
            
            sec

            printHeaderFlag - if 0, don't print header.  Default will print header.

            oneDParmList - a python list of one-D parameters as mnemonics.  Defaults to [].
            
            oneDParmValues - a python list of doubles representing values of the one-D
                             parameters set in oneDParmList.  Length must = len(oneDParmList).
                             Defaults to [].
        
        Returns: None.

        Affects: Prints a single Madrecord with lat, lon and alt as 2D parameters, along with all
        requested parameters. If stop_lon < start_lon, goes through 0 long.

        Exceptions: If problems with arguments
        """

        parmObj = madrigal.data.MadrigalParameters(self.__MadDB)

        # make sure longitudes between -180 and 180
        while start_lon < -180.0001:
            start_lon += 360.0
        while start_lon > 180.0001:
            start_lon -= 360.0

        while stop_lon < -180.0001:
            stop_lon += 360.0
        while stop_lon > 180.0001:
            stop_lon -= 360.0

        if len(oneDParmList) != len(oneDParmValues):
            raise ValueError, 'Len of oneDParmList=%i, must equal len of oneDParmValues=%i' % \
                  (len(oneDParmList), (oneDParmValues))

        # verify all parms are valid, and that doubles passed in
        for i in range(len(oneDParmList)):
            try:
                parmObj.getParmFormat(oneDParmList[i])
            except:
                raise ValueError, 'Illegal parm %s in oneDParmList' % (str(oneDParmList[i]))
            try:
                float(oneDParmValues[i])
            except:
                raise ValueError, 'Illegal float %s in oneDParmValues' % (str(oneDParmValues[i]))


        # get ut
        ut = madrigal._Madrec.getUtFromDate(year,month,day,hour,min,sec,0)

        # call looker_nonFile
        madrigal._Madrec.looker_nonFile(list(parmList),
                                        float(start_lat),
                                        float(stop_lat),
                                        float(step_lat),
                                        float(start_lon),
                                        float(stop_lon),
                                        float(step_lon),
                                        float(start_alt),
                                        float(stop_alt),
                                        float(step_alt),
                                        float(ut),
                                        int(printHeaderFlag),
                                        oneDParmList,
                                        oneDParmValues)
        


    def __getNumQueriesRunning(self):
        """__getNumQueriesRunning is a private helper function that returns the number of global queries running.

        Inputs: None.
        
        Returns: the number of global queries running

        Affects: Locks the global file metadata/userdata/queryPid.txt while being used; removes any dead process
        ids from that file.

        Exceptions: None
        """

        # lock access to queryPid.txt
        queryFileName = self.__MadDB.getMetadataDir() + '/userdata/' + self.__queryPidFile
        self.__getLock(queryFileName)

        # return 0 if no file
        if not os.access(queryFileName, os.F_OK):
            self.__dropLock(queryFileName)
            return 0
        
        # if file exists, but not writable, throw error
        if not os.access(queryFileName, os.W_OK):
            self.__dropLock(queryFileName)
            raise madrigal.admin.MadrigalError('Unable to write to file ' + str(queryFileName), None)

        # if file exists, but not readable, throw error
        if not os.access(queryFileName, os.R_OK):
            self.__dropLock(queryFileName)
            raise madrigal.admin.MadrigalError('Unable to read from file ' + str(queryFileName), None)

        # read in file
        queryFile = open(queryFileName, 'r')
        pidList = queryFile.read().split()
        queryFile.close()

        # loop through each process id, and check its validity
        newPidList = []
        for pid in pidList:
            try:
                # make sure its an integer, otherwise ignore it
                int(pid)
            except:
                continue
            # see if its running
            pidStr = os.popen2('ps -p ' + pid)[1].read()
            # if pid still running, pid will appear in result of ps command
            if pidStr.find(pid) != -1:
                newPidList.append(pid)

        # if no valid processes, remove file and return 0
        if len(newPidList) == 0:
            try:
                os.remove(queryFileName)
            except:
                pass
            self.__dropLock(queryFileName)
            return 0

        # otherwise rewrite queryPid.txt file, and return number of processes
        queryFile = open(queryFileName, 'w')
        delimiter = ' '
        queryFile.write(delimiter.join(newPidList))
        queryFile.close()
        self.__dropLock(queryFileName)
        return len(newPidList)

        
    def __addPid(self, pid):
        """__addPid is a private helper function that adds a new pid to queryPid.txt.

        Inputs: the pid to add.
        
        Returns: None

        Affects: Locks the global file metadata/userdata/queryPid.txt while being used; adds new pid to that file.

        Exceptions: None
        """
        
        # lock access to queryPid.txt
        queryFileName = self.__MadDB.getMetadataDir() + '/userdata/' + self.__queryPidFile
        self.__getLock(queryFileName)
        
        # if file doesn't exist, open in write mode
        if not os.access(queryFileName, os.F_OK):
            queryFile = open(queryFileName, 'w')
        else:
            queryFile = open(queryFileName, 'a')

        queryFile.write(' ' + str(pid))
        queryFile.close()
        self.__dropLock(queryFileName)
        
        
    def __getLock(self, filename):
        """__getLock is a private helper function that provides exclusive access to filename via a locking file.

        Inputs: filename = the file that exclusive access is required to.
        
        Returns: None

        Affects: Writes file filename + .LCK as a lock mechanism

        Exceptions: MadrigalError thrown if unable to write lock file

        Notes: Will sleep for 1 second at a time, for a maximum of _MaxSleep seconds (presently 10)
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

            if numTries > self.__MaxSleep:
                return

       
    def __dropLock(self, filename):
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


if __name__ == '__main__':

    test = MadrigalReport()
    """
    pid = test.genBackgroundReport('wrideout@haystack.mit.edu',
                             ['/home/grail/brideout/madR/madroot/experiments/1998/mlh/20jan98/mil980120g.003',
                              '/home/grail/brideout/madR/madroot/experiments/1998/mlh/23mar98/mlh980323g.001',
                              '/home/grail/brideout/madR/madroot/experiments/1997/mlh/06jan97/mil970106g.009',
                              '/home/grail/brideout/madR/madroot/experiments/1997/mlh/06jan97/mlh970106g.009',
                              '/home/grail/brideout/madR/madroot/experiments/1997/mlh/12mar97/mlh970312g.001'],
                             ['RANGE', 'gdalt'],
                             ['Kp,1,3', 'TI,/,DTI,50,'],
                             'Files: describe file filters here - 6 files\n' + \
                             'Data filters: 1<Kp<3, ti/dti > 50\n\n',
                             0)
    """
    pid = test.genBackgroundReport('wrideout@haystack.mit.edu',
                             ['/home/grail/brideout/madR/madroot/experiments/1998/mlh/20jan98/mil980120g.003',
                              '/home/grail/brideout/madR/madroot/experiments/1998/mlh/21sep98/mlh980921g.001'],
                             ['TI', 'DTI', 'GDALT'],
                             #['Kp,1,3', 'TI,/,DTI,50,,100,200'],
                             ['Range,+,GDALT,,','ti,1500,2000,,1000'],
                             'Files: describe file filters here\n' + \
                             'Data filters: 1<Kp<3, ti/dti > 50\n\n',
                             0)
    print 'Background process is running under pid %i.' % pid


    test.looker(("MAGCONJLAT","MAGCONJLON","SZEN","SZENC","SDWHT","MAGCONJSDWHT"),
                -60, -40, 10,
                -60, -40, 10,
                200, 400, 50,
                1998, 1, 1, 0, 0, 0)
