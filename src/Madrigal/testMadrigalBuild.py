import os,os.path,sys
import traceback

"""testMadrigalBuild.py verifies whether Madrigal has been successfully built

Usage: python testMadrigalBuild.py [MADROOT]

If optional argument MADROOT given, search there for test Madrigal file to plot

$Id: testMadrigalBuild.py,v 1.3 2009/01/29 21:44:24 brideout Exp $
"""

print """This script will attempt to plot some Madrigal test data.
If you see a Pcolor plot that shows electron density versus
altitude, the script succeeded.  Close the display window
when you're done examining it.  This may take a few minutes.\n"""

import warnings
warnings.filterwarnings('ignore', '.*', UserWarning)

try:
    import numpy
    import matplotlib.pylab
    import madrigal
    import madrigal._Madrec
    import madrigal.ui.madrigalPlot
    
except:
    traceback.print_exc()
    print 'FAILURE: REQUIRED MADRIGAL MODULES NOT INSTALLED'
    sys.exit(-1)
    

if len(sys.argv) > 2:
    print 'FAILURE: Illegal usage %s' % (str(sys.argv))
    sys.exit(-1)

if len(sys.argv) == 2:
    madrootDir = sys.argv[1]
    if not os.access(os.path.join(madrootDir, 'experiments'), os.R_OK):
        print 'FAILURE: No experiments directory found in %s' % (madrootDir)
        sys.exit(-1)
else:
    madrootDir = os.environ['MADROOT']

# get filename to read
filename = os.path.join(madrootDir, 'experiments/1998/mlh/20jan98/mil980120g.003')
if not os.access(filename, os.R_OK):
    filename = os.path.join(madrootDir, 'experiments/1998/mlh/20jan98/mlh980120g.001')
if not os.access(filename, os.R_OK):
    print 'FAILURE: Unable to find basic Madrigal test files installed in experiments/1998/mlh/20jan98'
    sys.exit(-1)

# check that tmpfile can be created
if not os.access('/tmp/', os.W_OK):
    print 'FAILURE: test cannot write to /tmp directory'
    sys.exit(-1)
tmpfile = '/tmp/isprintTest.txt'
tmpplot = '/tmp/isprint.png'

try:
    madrigal._Madrec.getIsprintReport(filename,
                                      '',
                                      'uth,gdalt,nel',
                                      '1',
                                      'uth',
                                      '',
                                      '1',
                                      '0.0',
                                      '24.0',
                                      0,
                                      0,
                                      0,
                                      'missing',
                                      'assumed',
                                      'knownBad',
                                      tmpfile)
except:
    traceback.print_exc()
    print 'FAILURE: isprint failed'
    sys.exit(-1)

# read isprint string
f = open(tmpfile)
isprintText = f.read()
f.close()
os.remove(tmpfile)

# produce pcolor plot
try:
    madrigal.ui.madrigalPlot.madPcolorPlot(isprintText,
                                           'Nel (log(m^-3)) - Millstone Hill - Test day Jan 20, 1998\nClose this Window if okay',
                                           'Hours since midnight UT Jan 20, 1998',
                                           'Altitude (km)',
                                           tmpplot,
                                           size = 'large',
                                           minColormap = 9,
                                           maxColormap = 12)
except:
    traceback.print_exc()
    print 'FAILURE: plotting failed'
    sys.exit(-1)

os.system('display %s' % (tmpplot))

os.remove(tmpplot)

print 'SUCCESS - Madrigal build completed successfully'


