"""testMadrigalWebServices.py runs a test of the Madrigal Web Services interface
   for a given Madrigal server.

   usage:

   python testMadrigalWebServices.py [madrigal main url]

   If no madrigal main url given, tries to get it from madrigal.metadata if installed.
   Otherwise, prints usage with madrigal main url required.

   Prints "success" if no errors raised - no longer compares to std output file

   Assumes the basic Madrigal test files have been installed.

"""

# $Id: testMadrigalWebServices.py,v 1.8 2009/03/09 18:16:00 brideout Exp $

try:

    import sys
    import traceback
    
    import madrigalWeb.madrigalWeb

    if len(sys.argv) < 2:
        # see if we can get the url from madrigal.metadata
        try:
            import madrigal.metadata
            madDB = madrigal.metadata.MadrigalDB()
            madrigalUrl = madDB.getTopLevelUrl()
        except:
            print 'usage: python testMadrigalWebServices.py <madrigal main url>'
            sys.exit(-1)
    else:
        madrigalUrl = sys.argv[1]

    # constants
    user_fullname = 'Bill Rideout - automated test'
    user_email = 'brideout@haystack.mit.edu'
    user_affiliation = 'MIT Haystack'

    testData = madrigalWeb.madrigalWeb.MadrigalData(madrigalUrl)

    instList = testData.getAllInstruments()

    # print out Millstone
    for inst in instList:
        if inst.code == 30:
            print(str(inst) + '\n')
            

    expList = testData.getExperiments(30, 1998,1,19,0,0,0,1998,1,22,0,0,0)

    for exp in expList:
        # should be only one
        print(str(exp) + '\n')

    fileList = testData.getExperimentFiles(expList[0].id)

    thisFilename = None

    for file in fileList:
        if file.category == 1:
            print(str(file) + '\n')
            thisFilename = file.name
            break

    if thisFilename == None:
        thisFilename = fileList[0].name

    print(testData.isprint(thisFilename,
                                   'gdalt,ti',
                                   'filter=gdalt,500,600 filter=ti,1900,2000',
                                   user_fullname, user_email, user_affiliation))
    print

    result = testData.madCalculator(1999,2,15,12,30,0,45,55,5,-170,-150,10,200,200,0,'sdwht,kp')

    for line in result:
        print line

    print

    result = testData.madTimeCalculator(1999,2,15,12,30,0,1999,2,15,13,30,0,0.25,'kp,ap3')

    for line in result:
        print line

    print '\nMadrigalWebServices regression test result: SUCCESS'

except:
    traceback.print_exc()
    print '\nMadrigalWebServices regression test result: FAILURE'


