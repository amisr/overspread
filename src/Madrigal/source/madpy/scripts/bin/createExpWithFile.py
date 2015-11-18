#!PYTHONEXE

#$Id: createExpWithFile.py,v 1.6 2008/05/12 13:25:39 brideout Exp $

usage = """
createExpWithFile.py is a script used to create a new Madrigal experiment
based on an already existing file.  Information such as the duration of the
experiment is obtained by analyzing the file.

Required arguments:

    --madFilename - full path to the complete Madrigal file. Basename will
                    be maintained.

    --expTitle - experiment title. Use quotes if title contains spaces.

    
    --permission - 0 for public, 1 for private (restricted to certain IP range)
                   (both the experiment and the file will set)

    --fileDesc - file decription

Optional arguments:

    --instCode - instrument code. If this argument missing, instrument code is
                 taken from file, but error is thrown if more than one kinst found.

    --category - 1=default, 2=variant, or 3=history If this argument is missing,
                 1 (default) used.

    --dirName  - directory name to use for experiment.  If not given, the directory
                 name will be the default name DDmmmYY[optChar].  Cannot contain "/"

    --optChar  - optional character to be added to experiment directory if no dirName
                 given.  If dirName argument given, this argument ignored.  optChar
                 is used if the default directory name DDmmmYY is used for
                 more than one experiment created for a given instrument on a given day.
                 For example, if --optChar=h for a MLH experiment on September 12, 2005,
                 then the experiment directory created would be experiments/2005/mlh/12sep05h.

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
               'expTitle=',
               'permission=',
               'fileDesc=',
               'instCode=',
               'category=',
               'dirName=',
               'optChar=',
               'kindat=']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
madFilename = None
expTitle = None
permission = None
fileDesc = None
instCode = None
category = 1
dirName = None
kindat = None
optChar = ''


for opt in optlist:
    if opt[0] == '--madFilename':
        madFilename = opt[1]
    elif opt[0] == '--expTitle':
        expTitle = opt[1]
    elif opt[0] == '--permission':
        permission = int(opt[1])
    elif opt[0] == '--fileDesc':
        fileDesc = opt[1]
    elif opt[0] == '--instCode':
        instCode = int(opt[1])
    elif opt[0] == '--category':
        category = opt[1]
    elif opt[0] == '--dirName':
        dirName = opt[1]
    elif opt[0] == '--kindat':
        kindat = int(opt[1])   
    elif opt[0] == '--optChar':
        optChar = opt[1]
        if len(optChar) != 1:
            raise 'optChar argument must contain exactly one character, not %s' % (optChar)
        
    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

# check that all required arguments passed in
if madFilename == None:
    print '--madFilename argument required - must be full path to madrigal file'
    print usage
    sys.exit(0)

if expTitle == None:
    print '--expTitle argument required - must be experiment title'
    sys.exit(0)

if permission == None:
    print '--permission argument required - must be 0 for public, 1 for private'
    sys.exit(0)

if fileDesc == None:
    print '--fileDesc argument required - must be file description'
    sys.exit(0)

adminObj = madrigal.admin.MadrigalDBAdmin()

expDir = adminObj.createMadrigalExperiment(madFilename,
                                           expTitle,
                                           permission,
                                           fileDesc,
                                           instCode,
                                           category,
                                           optChar,
                                           dirName,
                                           kindat)

print 'New experiment successfully created at %s with file %s - run updateMaster to register' % (expDir,
                                                                  madFilename)
