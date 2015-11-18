"""isprintExe is a private module designed to get output strings from the maddata engine in isprint format.

This module is not meant to be used directly by the user, and is only meant to
be called from other classes in the madrigal python library.

$Id: isprintExe.py,v 1.20 2009/03/09 19:42:48 brideout Exp $
"""

import os, sys
import types
import StringIO
import madrigal.metadata
import madrigal.data
import madrigal._Madrec

from math import *


class MadrigalIsprintExe:
    """MadrigalIsprintExe is a private class designed to get output strings from the maddata engine.

    MadrigalIsprintExe is a private class designed to get output strings using the maddata engine and
    possibly additional python postprocessing.

    Non-standard Python modules used:
    None
    """
    

    def __init__(self, madDB = None):
        """__init__ initializes MadrigalIsprintExe by reading from MadridalDB..

        Inputs: Existing MadrigalDB object, by default = None.
        
        Returns: void

        Affects: Initializes self.__metaDir.

        Exceptions: None.
        """

        # get bin dir
        if madDB == None:
            self.__madDB = madrigal.metadata.MadrigalDB()
        else:
            self.__madDB = madDB

        self.__binDir = self.__madDB.getBinDir()

        self.__parmObj = madrigal.data.MadrigalParameters(self.__madDB)


    def getModifiedIsprintString(self,
                                 filename,
                                 parmList,
                                 filterList = None,
                                 recordListingStyle = 0):
        """getModifiedIsprintString extends the maddata engine by allowing any possible filter.

        getModifiedIsprintString is a function that expands the maddata engine by allowing filtering to include all
        possible logical expression involving any Madrigal parameters.

        Inputs:

            filename - full path to madrigal file.
        
            parmList - a list of mnemonic or integer parameters.
        
            filterList - a list of strings filtering the output.  They must only contain parameters in mnemonic
            form that are included in the parmList.  See example below for examples.  Defaults to None.

            recordListingStyle - If 0 (the default), show record summary and all data that meet all filters.
            If 1, show only the record summary of any record that contains any 2d data that meets all the
            filters.  If 2, show only any 2d data that meets all the filters without record headers.
        
        
        Returns: String containing report formatted in isprint fashion, or empty string if no data.

        Affects: None

        Exceptions: If any item in filterList is not a valid logical expression.  

        Usage example::

            import madrigal.ui.isprintExe

            test = madrigal.ui.isprintExe.MadrigalIsprintExe()
    
            filepath = os.environ.get('MADROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

            parmList = ['range',  'ti', 'TN',  'kinst']

            filterList = ['range > 900 and ti > 2000', 'ti < 2500']

            result = test.getModifiedIsprintString(filename, parmList, filterList)

            print result

        """

        returnStr = ''

        # convert parmList to all mnemonics
        parmList = self.__parmObj.getParmMnemonicList(parmList)

        # get subset of parameters actually used in filterList
        neededParms = self.__getNeededParms(parmList, filterList)

        # create maddata struct with no filters
        delimiter = ','
        maddata = madrigal._Madrec.createMaddata(filename,
                                                 '',
                                                 parmList,
                                                 [],
                                                 [],
                                                 [],
                                                 [],
                                                 [],
                                                 [])

        # loop through each cycle
        isFirstRec = 1
        cycList = range(0, maddata[1])
        recNumList = maddata[2]
        
        for cycNum in cycList:

            # loop through each record
            recList = range(0, recNumList[cycNum])
            for recNum in recList:

                recStrings = madrigal._Madrec.getMadrecord(maddata[0], cycNum, recNum)
                headerStr = recStrings[0]
                mnemStr   = recStrings[1]
                labelStr  = recStrings[2]
                dataStr   = recStrings[3]

                # get lineList by spliting datalist (lines are comma separated)
                lineList = dataStr.split(',')
                # get dataList by splitting each line (data is whitespace separated)
                dataList = []
                for line in lineList:
                    dataList.append(line.strip().split())

                # finally, loop through each line, and add it to accepted line list if needed
                lineNumList = range(0, len(lineList))
                acceptList = []
                for lineNum in lineNumList:
                    self.__getLine(filterList, lineNum, lineList, dataList, neededParms, acceptList)

                if len(acceptList) == 0:  # all lines rejected
                    continue
            
                # append data according to recordListingStyle
                if recordListingStyle == 1:  # show only summary
                    returnStr += headerStr + '\n'
                elif recordListingStyle == 0: # show headers, label and data
                    returnStr += headerStr + '\n'
                    returnStr += labelStr + '\n'
                    for line in acceptList:
                        returnStr += line + '\n'
                    returnStr += '\n'
                elif recordListingStyle == 2: # show data and label only if first record
                    if isFirstRec:
                        returnStr += labelStr + '\n'
                        isFirstRec = 0
                    for line in acceptList:
                        returnStr += line + '\n'

        # free maddata
        madrigal._Madrec.destroyMaddata(maddata[0])
        
        return returnStr



    def __getLine(self, filterList, lineNum, lineList, dataList, neededParms, acceptList):
        """__getLine is a private function that adds a line of data to acceptList if filters accept the data.

        Inputs:
                    
            filterList - the list of filter strings to be applied

            lineNum - number of present row
        
            lineList - a list of line strings, one for each row

            dataList - a list of lists of data elements (may be floats, or special strings such as missing)
        
            neededParms - a list parameters used in filters.  Each item in list
            is a list containing a uppercase mnemonic, and an integer representing column number

            acceptList - a list containing accepted line so far
        
        
        Returns: None.

        Affects: Appends the appropriate line from lineList to acceptList if accepted.

        Exceptions: None.
        """

        # if no filter, automatically accept
        accept = 0
        if filterList == None:
            accept = 1
        elif len(filterList) == 0:
            accept = 1

        # set all variables in neededParms
        for parm in neededParms:
            # check if data is a double
            try:
                float(dataList[lineNum][parm[1]])
            except:
                return
            strSetParms = parm[0] + ' = ' + dataList[lineNum][parm[1]]
            exec(strSetParms)

        # create string that will accept or reject line if any filters given
        # convert filters to stdExpression form via getStdExpression
        if accept == 0:
            strFilter = 'accept = '
            filterCount = 0
            for filter in filterList:
                if filterCount != 0:
                    strFilter = strFilter + ' and (' + self.__parmObj.getStdExpression(filter) + ')'
                else:
                    strFilter = strFilter + '(' + self.__parmObj.getStdExpression(filter) + ')'
                filterCount = filterCount + 1

            # if any exceptions are thrown, reject the line
            try:
                exec(strFilter)
            except:
                accept = 0

                
        if accept == 1:
            acceptList.append(lineList[lineNum])

        return 


        
    def __getNeededParms(self, parmList, filterList):
        """__getNeededParms is a private function that returns a list of parameters found in filterList.

        Inputs:
        
            parmList - a list of mnemonic string parameters being displayed
        
            filterList - the list of filter strings to be applied
        
        Returns: a list which is a subset of parmList of all parameters found in filterList.  Each member of
        the list is a list with two members, the string mnemonic and the position
        in the parmList where that parameter appears.

        Affects: None.

        Exceptions: None.
        """

        returnList = []

        parmNum = -1

        for parmStr in parmList:
            parmNum = parmNum + 1
            found = 0
            # check if this parmStr is in any member of the filter list
            if filterList != None:
                for filter in filterList:
                    if filter.upper().find(parmStr) != -1:
                        returnList.append([parmStr, parmNum])
                        found = 1
                        break

        return returnList



    
   

if __name__ == '__main__':

    test = MadrigalIsprintExe()

    # edit this line if this file does not exist on your machine
    filename = os.environ.get('MADROOT') + '/experiments/1998/mlh/20jan98/mil980120g.003'

    parmList = ['UTH', 'RANGE', 'TI', 'TE', 'ELM', 'KP']

    print 'Now running example with recordListingStyle = 0:'

    print test.getModifiedIsprintString(filename, parmList, ['Range > 100', 'RANGE < 200', '0 < ti < 1000'])
    
    print 'Now running example with recordListingStyle = 1:'

    print test.getModifiedIsprintString(filename, parmList, ['Range > 100', 'RANGE < 200', '0 < ti < 1000'], 1)
    
    print 'Now running example with recordListingStyle = 2:'

    print test.getModifiedIsprintString(filename, parmList, ['Range > 100', 'RANGE < 200', '0 < ti < 1000'], 2)
    
    #print test.getModifiedIsprintString(filename, parmList)
    
    print 'All done'
