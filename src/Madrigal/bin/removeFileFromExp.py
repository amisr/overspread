#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: removeFileFromExp.py,v 1.4 2008/05/08 18:34:53 brideout Exp $

usage = """
removeFileFromExp.py is a script used to remove an existing Madrigal file from an
existing experiment.  Information such as the duration of the
experiment is updated by analyzing the file.  

Required arguments:

    --filename - basename of the Madrigal file to be removed.

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
longarglist = ['filename=',
               'expDir=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
filename = None
expDir = None


for opt in optlist:
    if opt[0] == '--filename':
        filename = opt[1]
    elif opt[0] == '--expDir':
        expDir = opt[1]
        
    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

# check that all required arguments passed in
if filename == None:
    print '--filename argument required - must be basename of madrigal file to be removed'
    print usage
    sys.exit(0)

if expDir == None:
    print '--expDir argument required - must be full path to experiment directory'
    sys.exit(0)


adminObj = madrigal.admin.MadrigalDBAdmin()

adminObj.removeMadrigalFile(expDir,
                            filename)

# remove any associated individual records plots
cmd = 'rm -rf %s' % (os.path.join(expDir, 'plots', filename))
os.system(cmd)

print 'File %s successfully removed from experiment at %s - run updateMaster to register' % (filename, expDir)
