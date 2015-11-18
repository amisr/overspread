""" modifyFortran.py - a python script to modify fortran code to solve two
    problems:
        1. removes ! as a comment
        2. modifies method names to effectively create a namespace

    usage:  python modifyFortran.py [--applyNS] <fortran file in> <fortran file out>
"""

import os,sys
import string
import getopt

usage = 'python modifyFortran.py [--applyNS] <fortran file in> <fortran file out>'


# the following lines tell which methods to rename
# for the moment these are for GEOPACK_2003 to avoid collision with geo-cgm

methodList = ['SUN',
              'SPHCAR',
              'BSPCAR',
              'RECALC',
              'GEOMAG',
              'MAGSM',
              'SMGSM']

nsExtension = 'TS_'

# parse command line
arglist = ''
longarglist = ['applyNS']

optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)


# set default values
applyNS = 0
inFilename = ''
outFilename = ''

for opt in optlist:
    if opt[0] == '--applyNS':
        applyNS = 1
    else:
        raise 'Illegal option %s\n%s' % (opt[0], usage)

try:
    inFilename = args[0]
    outFilename = args[1]
except:
    print 'Missing required file names'
    print usage
    sys.exit(-1)

inFile = open(inFilename)
inFileLines = inFile.readlines()
inFile.close()

outFile = open(outFilename, 'w')

print 'applyNS = %i' % (applyNS)

# replace everything after ! and modify methods if desired
for line in inFileLines:
    if string.find(line, '!') != -1:
        line = line[:string.find(line, '!')]
        line = line + '\n'
    if applyNS == 1:
        for method in methodList:
            if string.find(line, method) != -1:
                line = string.replace(line, method, nsExtension + method)
                print 'replaced %s with %s' % (method, nsExtension + method)
    outFile.write(line)

