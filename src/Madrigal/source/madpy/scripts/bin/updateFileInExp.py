#!PYTHONEXE

#$Id: updateFileInExp.py,v 1.3 2008/05/08 18:34:54 brideout Exp $

usage = """
updateFileInExp.py is a script used to update an existing Madrigal file in an
existing experiment.  Information such as the duration of the
experiment is updated by analyzing the file.  This script is use to replace
an existing Madrigal file.  Use addFileToExp.py to add a new file, and
changeFileStatus.py to change any file attribute.

Required arguments:

    --madFilename - full path to the new version of the Madrigal file. Basename will
                    be maintained.

    --expDir - full path to experiment directory. Example:
               "/opt/madrigal/experiments/1998/mlh/20jan98"
"""

import sys
import os, os.path
import getopt
import traceback

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['madFilename=',
               'expDir=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
madFilename = None
expDir = None


for opt in optlist:
    if opt[0] == '--madFilename':
        madFilename = opt[1]
    elif opt[0] == '--expDir':
        expDir = opt[1]
        
    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

# check that all required arguments passed in
if madFilename == None:
    print '--madFilename argument required - must be full path to madrigal file'
    print usage
    sys.exit(0)

if expDir == None:
    print '--expDir argument required - must be full path to experiment directory'
    sys.exit(0)


adminObj = madrigal.admin.MadrigalDBAdmin()

adminObj.overwriteMadrigalFile(expDir,
                               madFilename)

print 'File %s successfully updated in experiment at %s' % (madFilename, expDir)
