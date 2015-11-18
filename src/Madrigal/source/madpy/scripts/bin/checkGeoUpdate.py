#!PYTHONEXE

import sys
import os, os.path
import traceback
import urllib2

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

    # path to OpenMadrigal download site
    openMadUrl = 'http://www.haystack.mit.edu/madrigal/distributionFiles/'

    # create MadrigalDB obj
    madDBObj = madrigal.metadata.MadrigalDB()

    # get ig_rz.dat
    f = urllib2.urlopen(openMadUrl + 'ig_rz.dat')
    f2 = open(os.path.join(madDBObj.getMadroot(), 'bin/ig_rz.dat'), 'w')
    f2.write(f.read())
    f.close()
    f2.close()

    # default is we need to update
    needUpdate = 1

    # try to open geoStatus.file and compare to open madrigal version
    try:
        localStatusFile = open(madDBObj.getMadroot() + '/geoStatus.dat')
        localLines = localStatusFile.readlines()
        localStatusFile.close()
    except:
        # no such file exists yet
        localLines = ('','','','','')
    
    openMadFile = urllib2.urlopen(openMadUrl + 'status.dat')
    openMadLines = openMadFile.readlines()
    openMadFile.close()
    for i in range(5):
        if localLines[i] != openMadLines[i]:
            break
        if i == 4:
            # no update needed - exit
            sys.exit(0)

    # write new geoStatus.dat
    print 'downloading latest geo files from OpenMadrigal'
    localStatusFile = open(madDBObj.getMadroot() + '/geoStatus.dat', 'w')
    for line in openMadLines:
        localStatusFile.write(line)
    localStatusFile.close()
    try:
        os.chmod(madDBObj.getMadroot() + '/geoStatus.dat', 0666)
    except:
        pass

    # get the updated files
    f = urllib2.urlopen('http://www.haystack.mit.edu/madrigal/distributionFiles/geofil.tar.Z')
    f2 = open(madDBObj.getMadroot() + '/geofil.tar.Z', 'w')
    f2.write(f.read())
    f.close()
    f2.close()
    os.chdir(madDBObj.getMadroot())
    os.system('gunzip -f ' + madDBObj.getMadroot() + '/geofil.tar.Z')
    os.system('tar -xf ' + madDBObj.getMadroot() + '/geofil.tar')
    os.system(madDBObj.getMadroot() + '/bin/configureGeoExperiments')
    os.remove(madDBObj.getMadroot() + '/geofil.tar')
    
except SystemExit:
    # nothing to be done
    pass

except madrigal.admin.MadrigalError, e:
    # handle a MadrigalError

        
    errStr = 'Error occurred in checkGeoUpdate.py\n'

    errStr = errStr + e.getExceptionHtml()
    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '\n' + str(errItem)

        
    print errStr


except:
    # handle a normal error

    # if a normal SystemExit, simply terminate
    if str(sys.exc_info()[0]) == 'exceptions.SystemExit':
        sys.exit(0)
    
        
    errStr = 'Error occurred in running checkGeoUpdate.py.\n'

    
    err = traceback.format_exception(sys.exc_info()[0],
                                     sys.exc_info()[1],
                                     sys.exc_info()[2])

    for errItem in err:
        errStr = errStr + '\n' + str(errItem)


    print errStr


# end script


