#!PYTHONEXE

import sys
import os, os.path
import time
import traceback

"""updateFileTab.py updates every fileTab.txt file to correctly fill in fields 6 and 7
   (#6 - File contains at least one Catalog Record File - 0 for no, 1 for yes)
    #7 File contains at least one Header Record File - 0 for no, 1 for yes)
"""

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

    import madrigal.metadata
    import madrigal.data

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    fileList = madDBObj.getFileListFromMetadata()

    print 'Setting values for hasCatalog and hasHeader in fileTab.txt files:'

    # loop through each file
    dirName = None
    for dataFile in fileList:
        thisDir = os.path.dirname(dataFile)
        if thisDir != dirName:
            # new fileTab.txt found - fix it
            newFileTab = os.path.join(thisDir, 'fileTab.txt')
            metaFileObj = madrigal.metadata.MadrigalMetaFile(madDBObj, newFileTab)
            # now loop through each file in the metaFileObj
            for i in range(metaFileObj.getFileCount()):
                thisFile = metaFileObj.getFilenameByPosition(i)
                thisFileName = os.path.join(thisDir, thisFile)
                try:
                    catList, headList = madrigal._Madrec.cedarCatalogHeaderList(thisFileName)
                except:
                    # problem reading file
                    catList = []
                    headList = []
                if len(catList) > 0:
                    metaFileObj.setHasCatalogByPosition(i, True)
                else:
                    metaFileObj.setHasCatalogByPosition(i, False)
                if len(headList) > 0:
                    metaFileObj.setHasHeaderByPosition(i, True)
                else:
                    metaFileObj.setHasHeaderByPosition(i, False)

            # write new fileTab
            metaFileObj.writeMetadata()
            
            print '\t%s' % (newFileTab)

            dirName = thisDir


except madrigal.admin.MadrigalError, e:
    # handle a MadrigalError

        
    errStr = '<h1> Error occurred in updateFileTab.py</h1>'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)

        
    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = madrigal.admin.MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running updateFileTab.py' )


except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = '<h1> Error occurred in running updateFileTab.py.</h1>'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '<br>\n' + str(errItem)


    errStr = errStr + '\nError occurred at ' + str(time.asctime(time.localtime())) + '<br>\n'

    admin = madrigal.admin.MadrigalNotify()
    admin.sendAlert('<html>\n' + errStr + '</html>',
                         'Error running updateFileTab.py')


# end script


