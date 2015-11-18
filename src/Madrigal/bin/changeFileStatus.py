#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

#$Id: changeFileStatus.py,v 1.3 2008/05/08 18:34:53 brideout Exp $

usage = """
changeFileStatus.py is a script used to change the status of an
existing Madrigal file.  The file permsiion, the file description,
or the file category can be changed.  updateMaster is run at end
of script to push local metadata into summary metadata.

Required arguments:

    --filename - basename of existing Madrigal file. 

    --expDir - full path to experiment directory. Example:
               "/opt/madrigal/experiments/1998/mlh/20jan98"

Optional arguments - set these to change a file attribute:
    
    --permission - 0 for public, 1 for private (restricted to certain IP range)

    --fileDesc - file decription

    --category - 1=default, 2=variant, or 3=history 
"""

import sys
import os, os.path
import getopt
import traceback

import madrigal.admin



# parse command line
arglist = ''
longarglist = ['filename=',
               'expDir=',
               'permission=',
               'fileDesc=',
               'category=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
filename = None
expDir = None
permission = None
fileDesc = None
category = None


for opt in optlist:
    if opt[0] == '--filename':
        filename = opt[1]
    elif opt[0] == '--expDir':
        expDir = opt[1]
    elif opt[0] == '--permission':
        permission = int(opt[1])
    elif opt[0] == '--fileDesc':
        fileDesc = opt[1]
    elif opt[0] == '--category':
        category = opt[1]
        
    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

# check that all required arguments passed in
if filename == None:
    print '--filename argument required - must be basename of existing madrigal file'
    print usage
    sys.exit(0)

if expDir == None:
    print '--expDir argument required - must be full path to experiment directory'
    sys.exit(0)

if permission == None and fileDesc == None and category == None:
    print 'No file attributes are being changed'
    sys.exit(0)


adminObj = madrigal.admin.MadrigalDBAdmin()

adminObj.changeFileStatus(expDir,
                          filename,
                          category,
                          fileDesc,
                          permission)


print 'Status of file %s changed in local metadata - run updateMaster to register' % (filename)
