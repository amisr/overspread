#!PYTHONEXE

import sys
import os
import time
import traceback

# catch any exception, and write an appropriate message admin
try:

    # check if pythonlibpath env variable exists
    # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
    temp = os.environ.get('PYTHON' + 'LIBPATH')
    if temp != None:
            sys.path.append(temp)
            
    # append path madroot/lib (needed only if python not installed by setup)
    sys.path.append('MADROOT/lib/python')

    # prepare to handle MadrigalError
    import madrigal.admin
    
except ImportError:
    
    # Fatal error - madpy library not found
    print "Unable to import the madrigal python library - please alert the sys admin!"
    sys.exit(0)

# try to run script, and report all errors to Madrigal sys admin

try:
    # see if user set -all flag

    allFlag = 0

    for arg in sys.argv:
        if arg == '-all':
            allFlag = 1

    import madrigal.metadata

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # if madroot not set, set it now
    if os.environ.get('MAD' + 'ROOT') == None:
        os.environ['MAD' + 'ROOT'] = madDBObj.getMadroot()

    import madrigal.data

    # create MadrigalInstrumentParameters object
    madInstParmObj = madrigal.metadata.MadrigalInstrumentParameters()

    madInstParmObj.rebuildInstParmTable(allFlag)

except madrigal.admin.MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in rebuildInstParmTab.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)

        
    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = madrigal.admin.MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running rebuildInstParmTab.py' )


except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running rebuildInstParmTab.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = madrigal.admin.MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running rebuildInstParmTab.py')


# end script


