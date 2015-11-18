"""madrigalPColor.py is a remote command-line application  that creates PColor plots from Madrigal data

Usage::

    python madrigalPColor.py  --url=madrigalUrl --file=filename --parm=parameter
    --output=outputFile --name=userName --email=userEmail --affiliation=userAffilitation
    [--filter=filterString --title=plotTitle --startHour=startHour --endHour=endHour
    --minAlt=minimum_altitude --maxAlt=maximum_altitude --minParm=minimum_parm_value 
    --maxParm=maximum_parameter_value]

See http://madrigal.haystack.mit.edu/madrigal/ug_commandLine.html#isprint
for details of how filters work. Optional startHour and endHour are UT hours, with 0
being midnight UT on the day the experiment started.  Default is entire experiment.

Example::

    python madrigalPColor.py  --url=http://madrigal.haystack.mit.edu/madrigal \
    --file=/opt/madrigal/experiments/1998/mlh/20jan98/mlh980120g.002 --parm=nel \
    --output=/tmp/mlh_20jan98.png --name="Bill Rideout" --email=brideout@haystack.mit.edu \
    --affiliation=MIT --filter="filter=elm,80,90 filter=gdalt,,500"

This script requires Matplotlib be installed.

"""

# $Id: madrigalPColor.py,v 1.4 2009/05/01 13:32:52 brideout Exp $

import os, os.path, sys
import getopt


import madrigalWebPlot.madrigalPlot
import madrigalWeb.madrigalWeb

usage = """python madrigalPColor.py  --url=<url> --file=<file> --parm=<parm>
--output=<output> --name=<name> --email=<email> --affiliation=<affiliation>
[--filter=<filter> --title=<title> --startHour=<startHour> --endHour=<endHour>
--minAlt=<minimum_altitude> --maxAlt=<maximum_altitude> --minParm=<minimum_parm_value> 
--maxParm=<maximum_parameter_value>]

See http://madrigal.haystack.mit.edu/madrigal/ug_commandLine.html#isprint
for details of how filters work. Optional startHour and endHour are UT hours, with 0
being midnight UT on the day the experiment started.  Default is entire experiment.

Example:

python madrigalPColor.py  --url=http://madrigal.haystack.mit.edu/madrigal \
--file=/opt/madrigal/experiments/1998/mlh/20jan98/mlh980120g.002 --parm=nel \
--output=/tmp/mlh_20jan98.png --name="Bill Rideout" --email=brideout@haystack.mit.edu \
--affiliation=MIT --filter="filter=elm,80,90 filter=gdalt,,500"

"""

# default arguments
madWebObj = None
filename = None
parm = None
savedFile = None
user_fullname = None
user_email = None
user_affiliation = None
filtStr = ''   # default
title = None   # default
startHour=None
endHour=None
minAlt=None
maxAlt=None
minParm=None
maxParm=None

args = ('url=',
        'file=',
        'parm=',
        'output=',
        'name=',
        'email=',
        'affiliation=',
        'filter=',
        'title=',
        'startHour=',
        'endHour=',
        'minAlt=',
        'maxAlt=',
        'minParm=',
        'maxParm=',
        'help')

try:
    opts, argList = getopt.getopt(sys.argv[1:], '-h', args)
    if len(opts) == 0:
        print usage
        sys.exit(0)
except getopt.GetoptError, err:
    print usage
    print str(err)  
    sys.exit(2)
        
for o, a in opts:
    if o == "--url":
        try:
            madWebObj = madrigalWeb.madrigalWeb.MadrigalData(a)
        except:
            print usage
            raise IOError, 'Unable to connect to Madrigal site %s' % (a)
    elif o in ("-h", "--help"):
        print usage
        sys.exit(0)
    elif o == '--file':
        filename = a
        if filename[0] != '/':
            print usage
            raise IOError, '--file argument must be absolute path starting with /, not %s' % (a)
    elif o == '--parm':
        parm = a
        # verify only one parameter given
        if parm.find(',') != -1 or parm.find(' ') != -1:
            print usage
            raise IOError, '--parm must specify only one parameter to plot, not <%s>' % (a)
    elif o == '--output':
        savedFile = a
        # verify this file can be written
        if not os.access(os.path.dirname(savedFile), os.W_OK):
            print usage
            raise IOError, '--output directory <%s> not writable' % (os.path.dirname(a))
    elif o == '--name':
        user_fullname = a
    elif o == '--email':
        user_email = a
        if user_email.find('@') == -1:
            print usage
            raise IOError, '--email <%s> does not contain character @' % (os.path.dirname(a))
    elif o == '--affiliation':
        user_affiliation = a
    elif o == '--filter':
        filtStr = a
    elif o == '--title':
        title = a
    elif o == '--startHour':
        try:
            startHour = float(a)
        except:
            print usage
            raise IOError, 'startHour must be number, not <%s>' % (a)
    elif o == '--endHour':
        try:
            endHour = float(a)
        except:
            print usage
            raise IOError, 'endHour must be number, not <%s>' % (a)
    elif o == '--minAlt':
        try:
            minAlt = float(a)
        except:
            print usage
            raise IOError, 'minAlt must be number, not <%s>' % (a)
    elif o == '--maxAlt':
        try:
            maxAlt = float(a)
        except:
            print usage
            raise IOError, 'maxAlt must be number, not <%s>' % (a)
    elif o == '--minParm':
        try:
            minParm = float(a)
        except:
            print usage
            raise IOError, 'minParm must be number, not <%s>' % (a)
    elif o == '--maxParm':
        try:
            maxParm = float(a)
        except:
            print usage
            raise IOError, 'maxParm must be number, not <%s>' % (a)

    else:
        print usage
        raise IOError, 'Unknown option <%s>' % (a)

if len(argList) != 0:
    print usage
    raise IOError, 'Unknown arguments <%s>' % (str(argList))

# verify all needed input given
if madWebObj == None:
    print usage
    raise IOError, '--url required argument (example: --url=http://madrigal.haystack.mit.edu/madrigal)'
if filename == None:
    print usage
    raise IOError, '--filename required argument (example: --file=/opt/madrigal/experiments/1998/mlh/20jan98/mlh980120g.002)'
if parm == None:
    print usage
    raise IOError, '--parm required argument (example: --parm=nel)'
if savedFile == None:
    print usage
    raise IOError, '--output required argument (example: --output=/tmp/mlh_20jan98.png)'
if user_fullname == None:
    print usage
    raise IOError, '--name required argument (example: --name="Bill Rideout")'
if user_email == None:
    print usage
    raise IOError, '--email required argument (example: --email=brideout@haystack.mit.edu)'
if user_affiliation == None:
    print usage
    raise IOError, '--affiliation required argument (example: --affiliation=MIT)'


### start graph creation ###


parmStr = 'uth,gdalt,%s' % (parm)

# we need to make another isprint call just to find the first date in the file
dateStr = madWebObj.isprint(filename, 'year,month,day', 'filter=recno,0,4',
                           user_fullname, user_email, user_affiliation)
lines = dateStr.split('\n')
year = None
for line in lines:
    try:
        items = line.split()
        if len(items) == 3:
            year = int(items[0])
            month = int(items[1])
            day = int(items[2])
            break
    except:
        continue

if year == None:
    raise IOError, 'Unable to open file %s' % (filename)

# make sure missing data is removed
filtStr += ' filter=%s,, ' % (parm)

isprintStr = madWebObj.isprint(filename, parmStr, filtStr,
                               user_fullname, user_email, user_affiliation)

if title == None:
    titleStr = 'Plot of %s for file %s' % (parm, os.path.basename(filename))
else:
    titleStr = title

xLabelStr = 'Hours since midnight UT %04i-%02i-%02i' % (year, month, day)
yLabelStr = 'Alt (km)'

madrigalWebPlot.madrigalPlot.madPcolorPlot(isprintStr, titleStr, xLabelStr,
                                           yLabelStr, savedFile,
                                           maxNumTimes = 500,
                                           maxNumAlt = 100,
                                           truncateIsprint = True,
                                           startTime=startHour,
                                           endTime=endHour,
                                           yMinimum=minAlt,
                                           yMaximum=maxAlt,
                                           minColormap=minParm,
                                           maxColormap=maxParm)
