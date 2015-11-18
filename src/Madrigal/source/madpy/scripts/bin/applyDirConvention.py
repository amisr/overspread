#!PYTHONEXE


"""applyDirConvention.py is a script that will mark all experiments as
ignore if they violate the directory naming convention DDmmmYY[char].
This script is meant to be used when installing Madrigal2.5, which drops
that convention.  This means that if Madrigal administrators were previously
using this convention to hide experiment directories, they need to run this
script to use expTab.txt to hide them

$Id: applyDirConvention.py,v 1.3 2008/06/09 19:31:30 brideout Exp $
"""

import os, os.path, sys
import time

import madrigal.metadata



madDBObj = madrigal.metadata.MadrigalDB()

# create a list of all experiments, regardless of naming convention
allExpList = []
fileList = madDBObj.getFileList(enforcePathConvention=0, includeNonDefault = 1)
for thisFile in fileList:
    if os.path.dirname(thisFile) not in allExpList:
        allExpList.append(os.path.dirname(thisFile))


# create a list of all experiments that follow the DDmmmYY[char] convention
convExpList = []
fileListConv = madDBObj.getFileList(enforcePathConvention=1, includeNonDefault = 1)
for thisFile in fileListConv:
    if os.path.dirname(thisFile) not in convExpList:
        convExpList.append(os.path.dirname(thisFile))

# mark as ignore any from the first list not in the second
for exp in allExpList:
    if exp not in convExpList:
        print 'Marking experiment <%s> as ignored' % (exp)
        expTab = os.path.join(exp, 'expTab.txt')
        expObj = madrigal.metadata.MadrigalExperiment(madDBObj, expTab)
        expObj.setSecurityByPosition(0, -1)
        expObj.writeMetadata()
                                                      
