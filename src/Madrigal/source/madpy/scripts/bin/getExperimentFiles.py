#!PYTHONEXE

"""getExperimentFiles.py is a script run to return a text output of selected experiment files data.

It is presently used by the madmatlab methods getExperimentFiles, and via the cgi script
"getExperimentFilesService.py":../services/getExperimentFilesService.py.html
the madmatlab method getExperimentFilesWeb.  It has the following input arguments:

     --id=<experiment id> (integer)

     --trusted=<0 if not, 1 if is> (default to not trusted)


Returns comma-delimited data, one line for each experiment file, with the following fields:

    1. file.name (string) Example '/opt/mdarigal/blah/mlh980120g.001'
    
    2. file.kindat (int) Kindat code.  Example: 3001
    
    3. file.kindat desc (string) Kindat description: Example 'Basic Derived Parameters'
    
    4. file.category (int) (1=default, 2=variant, 3=history, 4=real-time)
    
    5. file.status (string)('preliminary', 'final', or any other description)
    
    6. file.permission (int)  0 for public, 1 for private.  For now will not return private files.

Returns empty string if experiment id nor found, and returns status -1.

This script, and the corresponding cgi script
"getExperimentFilesService.py":../services/getExperimentFilesService.py.html are available to any scripting
language that wants to access Madrigal metadata.
"""

import sys
import os
import datetime
import traceback
import getopt


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
    from madrigal.admin import *

except ImportError:
    
    # Fatal error - madpy library not found
    print "Unable to import the madrigal python library - please alert the sys admin!"
    sys.exit(-1)

# try to run script, and report all errors to Madrigal sys admin
try:

    import madrigal.metadata

    # parse command line
    arglist = ''
    longarglist = ['id=',
                   'trusted=']
    
    optlist, args = getopt.getopt(sys.argv[1:], arglist, longarglist)

    
    # set default values
    id = None
    trusted = 0

    for opt in optlist:
        if opt[0] == '--id':
            id = int(opt[1])
        elif opt[0] == '--trusted':
            trusted = int(opt[1])
        else:
            raise 'Illegal option %s' % (opt[0])

    # raise exception if no id
    if id == None:
        raise 'getExperimentFiles requires --id argument'
        
    # messStr is emailed to madrigal admin if not empty
    messStr = ''

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # create MadrigalExperiments object to get full file name
    madExpObj = madrigal.metadata.MadrigalExperiment(madDBObj)

    # create Madrigal Kindat to get Kindat descriptions
    madKindatObj = madrigal.metadata.MadrigalKindat(madDBObj)

    # create Madrigal File object 
    madFileObj = madrigal.metadata.MadrigalMetaFile(madDBObj)

    # create expPath
    expPath = madDBObj.getMadroot()
    if expPath[-1] != '/':
        expPath += '/'
    expPath += 'experiments/'



    # loop through the experiments to get url
    position = 0
    while 1:
        thisId = madExpObj.getExpIdByPosition(position)
        # check for end
        if thisId == None:
            sys.exit(-1)
        thisId = int(thisId)
        # check for right id
        if thisId == id:
            thisUrl = madExpObj.getExpUrlByPosition(position)
            # add end of url to exp path
            index = thisUrl.find('/madtoc/')
            expPath += thisUrl[index+8:]
            break
        position += 1

    # loop though files to find all with right experiment id
    position = 0
    while 1:
        thisId = madFileObj.getExpIdByPosition(position)
        if thisId == None:
            break
        thisId = int(thisId)
        if thisId != id:
            position += 1
            continue
        # get data
        name = expPath + '/' + madFileObj.getFilenameByPosition(position)
        kindat = madFileObj.getKindatByPosition(position)
        kindatdesc = madKindatObj.getKindatDescription(kindat)
        category = madFileObj.getCategoryByPosition(position)
        status = madFileObj.getStatusByPosition(position)
        permission = madFileObj.getAccessByPosition(position)

        # skip private files if not trusted
        if trusted == 0 and int(permission) != 0:
            position += 1
            continue
            

        # print this file
        print('%s,%i,%s,%i,%s,%i') % \
               (name,
                kindat,
                kindatdesc,
                category,
                status,
                permission)

        position += 1
    


    # if messStr not empty, send message to admin
    if len(messStr) > 0:
        # create MadrigalNotify object
        notifyObj = MadrigalNotify(madDBObj)

        notifyObj.sendAlert(messStr, 'Problem detected by getExperimentFiles')


except MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in getExperimentFiles.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)
        
    errStr = errStr + '\nError occurred at ' + str(datetime.datetime.today().isoformat()) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getExperimentFiles.py' )

    sys.exit(-1)

except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running getExperimentFiles.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(datetime.datetime.today().isoformat()) + '<br>\n'

    admin = MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running getExperimentFiles.py')

    sys.exit(-1)


# end script

