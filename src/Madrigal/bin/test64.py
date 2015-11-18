"""test64.py tests whether mktime can handle dates beyond 32 bit limit.

That is, is system 64 bits or higher.

$Id: test64.py,v 1.3 2009/01/22 19:38:53 brideout Exp $
"""

import time

try:
    time.mktime([2055,1,1,0,0,0,0,0,0])
    print 'Your system is 64 bits or greater, and so will correctly handle dates into the foreseeable future.'

except:
    print '\nWARNING: Because this Madrigal installation is 32 bit, it will not be able to handle dates beyond 2037.  Please upgrade Madrigal to a 64 bit machine before then.\n'
    
