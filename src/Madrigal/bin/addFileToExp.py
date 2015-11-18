#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: addFileToExp.py,v 1.5 2008/05/12 13:25:39 brideout Exp $

usage = """
addFileToExp.py is a script used to add a new Madrigal file to an
existing experiment.  Information such as the duration of the
experiment is updated by analyzing the file.

Required arguments:

    --madFilename - full path to the complete Madrigal file. Basename will
                    be maintained.

    --expDir - full path to experiment directory. Example:
               "/opt/madrigal/experiments/1998/mlh/20jan98"
    
    --permission - 0 for public, 1 for private (restricted to certain IP range)

    --fileDesc - file decription

Optional arguments:

    --category - 1=default, 2=variant, or 3=history If this argument is missing,
                 1 (default) used.

    --kindat  -  Set file kindat independently from one (or more) in file
"""

import sys
import os, os.path
import getopt
import traceback

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['madFilename=',
               'expDir=',
               'permission=',
               'fileDesc=',
               'category=',
               'kindat=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
madFilename = None
expDir = None
permission = None
fileDesc = None
category = 1
kindat = None


for opt in optlist:
    if opt[0] == '--madFilename':
        madFilename = opt[1]
    elif opt[0] == '--expDir':
        expDir = opt[1]
    elif opt[0] == '--permission':
        permission = int(opt[1])
    elif opt[0] == '--fileDesc':
        fileDesc = opt[1]
    elif opt[0] == '--category':
        category = int(opt[1])
    elif opt[0] == '--kindat':
        kindat = int(opt[1])
        
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

if permission == None:
    print '--permission argument required - must be 0 for public, 1 for private'
    sys.exit(0)

if fileDesc == None:
    print '--fileDesc argument required - must be file description'
    sys.exit(0)

adminObj = madrigal.admin.MadrigalDBAdmin()

adminObj.addMadrigalFile(expDir,
                         madFilename,
                         permission,
                         fileDesc,
                         category,
                         kindat)

print 'File %s successfully added to experiment at %s - run updateMaster to register change' % (madFilename, expDir)
