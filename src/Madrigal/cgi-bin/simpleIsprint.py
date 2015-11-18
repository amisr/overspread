#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, os.path
import traceback
import cgi, Cookie
import time, datetime
import types



class simpleIsprint:
    """simpleIsprint is the class that produces the simpleIsprint page.
    
    Like all my python cgi scripts, simpleIsprint has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for simpleIsprint are discussed below.

    The __init__ function then calls createObjects to create whatever python api objects are required
    to complete the script.  If the user has made a request that may succeed or may fail, that request is
    then processed, and self.success is set to either 'true' or 'false', and self.result is set to either
    1 (if success) or to the error message from the madpy library if failure.  The output html will then
    be determined by self.success.  The script then calls outputHead to output the header section and any required
    javascript.  Finally, __init__ calls a few functions for each of the main sections of the body.

    If any uncaught exception is thrown, its caught by the __init__ try block.  If its an MadrigalError,
    additional information is available.  The catch blocks attempt to display the error message on the screen
    by backing out of of large number of possible tags, which might prevent its display (in any case, the error
    message will always be available in the page source.  The formatted error message is also sent to the email
    address given in the siteTab.txt metadata file.

    Every attempt is made to generate easy to read source html, since it is often an easy starting point for analyzing
    the script.  Table structure is indicated by indentation, as is javascript code structure.

    The names of all form elements used by simpleIsprint are listed below:

    selectInstrument:	local instrument selected - id is instrument id

    selectExperiments:  list of experiment ids selected

    selectCoordinate: either "Geodetic", "Radar", or "Geomagnetic"

    format: default is html.  For STAP VO applications, can be TIME_SERIES-VOT or TIME_SERIES-ASCII

    

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 14, 2005

    $Id: simpleIsprint.py,v 1.12 2008/07/25 18:48:46 brideout Exp $
    """

    # constants
    __scriptName = 'simpleIsprint.py'

    __maxBytesInBrowser = 40000000 # 40 MB max output to browser


    def __init__(self):
        """__init__ run the entire simpleIsprint script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script simpleIsprint.

        Exceptions: None.
        """


        # catch any exception, and write an appropriate message to user and to admin
        try:

            # check if pythonlibpath env variable exists
            # written 'PYTHON' + 'LIBPATH' to stop automatic replacement during setup
            temp = os.environ.get('PYTHON' + 'LIBPATH')
            if temp != None:
                    sys.path.append(temp)
		    
            # append path madroot/lib (needed only if python not installed by setup)
            sys.path.append('/Users/mnicolls/Documents/Work/Madrigal/lib/python')

            # prepare to handle MadrigalError
            import madrigal.admin
	    
        except ImportError:
	    
	    # Fatal error - madpy library not found
            print "Content-Type: text/html"
            print
            print "Unable to import the madrigal python library - please alert the sys admin!"
            sys.exit(0)
	    
        try:

            # set flag as to whether script headers have been written
            self.scriptHeaders = 0

            # determine from form arguments and cookies which script state to use
            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()

            # output html

            #print header
            self.outputHead('Simple Print Data')

            #print body tag
            if self.format == 'html':
                print self.madDBObj.getHtmlStyle()

            self.printBody()

            if self.format == 'html':
                self.printEndPage()
            elif self.format == 'TIME_SERIES-VOT':
                self.printCloseVOT()

            

        except madrigal.admin.MadrigalError, e:
            # handle a MadrigalError

            # back out of any tag so error message appears
            if self.scriptHeaders != 0:
                print '</script></select></td></tr></table></td></tr></table>'
                
            errStr = '<h1> Error occurred in script ' + self.__scriptName + '.</h1>'

            errStr = errStr + e.getExceptionHtml()
            
            err = traceback.format_exception(sys.exc_info()[0],
                                             sys.exc_info()[1],
                                             sys.exc_info()[2])

            for errItem in err:
                errStr = errStr + '<br>\n' + str(errItem)

            # add info about called form:
            if self.madForm != None:
                errStr = errStr + '<h3>Form elements</h3>\n'
                for key in self.madForm.keys():
                    errStr = errStr + '<br>\n' + str(key)
                    errStr = errStr + ' = ' + str(self.madForm.getlist(key))

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/html"
                print
                
            print errStr + '<BR>'

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('<html>\n' + errStr + '</html>',
                                 'Error running ' + self.__scriptName)


            print '<br><b>Your system administrator has been notified.<b>'

        except SystemExit:
            sys.exit(0)

        except:
            # handle a normal error
            
            # back out of any tag so error message appears
            if self.scriptHeaders != 0:
                print '</script></select></td></tr></table></td></tr></table>'
                
            errStr = '<h1> Error occurred in script ' + self.__scriptName + '.</h1>'

            
            err = traceback.format_exception(sys.exc_info()[0],
                                             sys.exc_info()[1],
                                             sys.exc_info()[2])

            for errItem in err:
                errStr = errStr + '<br>\n' + str(errItem)

            # add info about called form:
            if self.madForm != None:
                errStr = errStr + '<h3>Form elements</h3>\n'
                for key in self.madForm.keys():
                    errStr = errStr + '<br>\n' + str(key)
                    errStr = errStr + ' = ' + str(self.madForm.getlist(key))

            if self.scriptHeaders == 0: # not yet printed
                print "Content-Type: text/html"
                print
                
            print errStr + '<BR>'

            self.admin = madrigal.admin.MadrigalNotify()
            self.admin.sendAlert('<html>\n' + errStr + '</html>',
                                 'Error running ' + self.__scriptName)


            print '<br><b>Your system administrator has been notified.<b>'

        # end __init__


    def setScriptState(self):
        
        #create a form object
        self.madForm = cgi.FieldStorage()

        if not self.madForm.has_key('selectInstrument'):
            if  self.scriptHeaders == 0:
                print "Content-Type: text/html\n"

            print '<h3> This cgi script was called without the proper arguments.</h3>' + \
                  'Since this script uses post, you cannot bookmark this page. ' + \
                  'Please contact your site administrator with any questions.'

            sys.exit(0)
            
        else:
            self.instrumentKinst = int(self.madForm.getvalue('selectInstrument'))
            self.experimentList = self.madForm.getlist('selectExperiments')
            self.coordinate = self.madForm.getvalue('selectCoordinate')
            self.format = self.madForm.getfirst('format')
            if self.format == None:
                self.format = 'html' # the default



    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

        # create a MadrigalInstrument object
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

	# get the name of the selected instrument
        self.instrumentName = self.madInstObj.getInstrumentName(self.instrumentKinst)


        # create a list of list of all default files, one list for each date
        import madrigalWeb.madrigalWeb
        self.madWebObj = madrigalWeb.madrigalWeb.MadrigalData(self.madDBObj.getTopLevelUrl())

        self.allExpFiles = []
        for exp in self.experimentList:
            expIdList = self.madWebObj.getExperiments(self.instrumentKinst,
                                                      int(exp[0:4]),
                                                      int(exp[5:7]),
                                                      int(exp[8:]),
                                                      0,
                                                      0,
                                                      0,
                                                      int(exp[0:4]),
                                                      int(exp[5:7]),
                                                      int(exp[8:]),
                                                      23,
                                                      59,
                                                      59)

            self.allExpFiles.append(self.madWebObj.getExperimentFiles(expIdList[0].id))


        # create a list of all parameters (ints) with error data in all those files, and also
        # simply all parameters
        import madrigal.data
        self.allParms = []
        self.allParmsWithErrors = []
        self.madParmObj = madrigal.data.MadrigalParameters(self.madDBObj)
        for theseFiles in self.allExpFiles:
            for thisFile in theseFiles:
                madFileObj = madrigal.data.MadrigalFile(thisFile.name, self.madDBObj)
                theseParms = madFileObj.getMeasuredParmList()
                for parm in theseParms:
                    if self.madParmObj.isAddIncrement(parm):
                        # skip additional increment parameters
                        continue
                    if parm < 1:
                        continue
                    if parm not in self.allParms:
                        self.allParms.append(parm)
                    if (parm * -1) in theseParms:
                        if parm not in self.allParmsWithErrors:
                            self.allParmsWithErrors.append(parm)
        self.allParms.sort()
        self.allParmsWithErrors.sort()

        # see which list we should use
        if len(self.allParmsWithErrors) > 0:
            self.allParms = self.allParmsWithErrors

        # create the list of all desired parameters
        self.parmStr = 'year,month,day,hour,min,sec,'
        if self.coordinate == 'Geodetic':
            self.parmStr += 'gdlat,glon,gdalt,'
        elif self.coordinate == 'Radar':
            self.parmStr += 'azm,elm,range,'
        elif self.coordinate == 'Geomagnetic':
            self.parmStr += 'paclat,paclon,gdalt,'
        else:
            raise 'Illegal selectCoordinate %s' % (str(self.coordinate))

        # make sure there are no repeats
        presentList = self.parmStr.split(',')

        for parm in self.allParms:
            if self.madParmObj.getParmMnemonic(parm).lower() in presentList:
                continue
            if parm == self.allParms[-1]:
                if len(self.allParmsWithErrors) > 0:
                    self.parmStr += '%s,%s' % (self.madParmObj.getParmMnemonic(parm), self.madParmObj.getParmMnemonic(parm * -1))
                else:
                    self.parmStr += '%s' % (self.madParmObj.getParmMnemonic(parm))
            else:
                if len(self.allParmsWithErrors) > 0:
                    self.parmStr += '%s,%s,' % (self.madParmObj.getParmMnemonic(parm), self.madParmObj.getParmMnemonic(parm * -1))
                else:
                    self.parmStr += '%s,' % (self.madParmObj.getParmMnemonic(parm))

        # get header string
        self.headerStr = self.getHeaderStr()
            
        # get user info for logging purposes
        # try to get name, email, affiliation from cookie
        cookie = Cookie.SimpleCookie()
        if os.environ.has_key('HTTP_COOKIE'):
            cookie.load(os.environ['HTTP_COOKIE'])
            try:
                self.user_fullname = cookie["user_fullname"].value
                self.user_email = cookie["user_email"].value
                self.user_affiliation = cookie["user_affiliation"].value
            except:
                self.user_fullname = 'Unknown'
                self.user_email = 'Unknown'
                self.user_affiliation ='Unknown'
        else:
            self.user_fullname = 'Unknown'
            self.user_email = 'Unknown'
            self.user_affiliation ='Unknown'


    def getHeaderStr(self):
        """getHeaderStr returns a string with parameter lables and and definitions.

        The format depends on self.format.  If html, format is <a href=JavaScript:popup("BN")>
        If TIME_SERIES-ASCII, format is just parm mnemonic
        """
        if self.format == 'html':
            spaceStr = '&nbsp;'
        else:
            spaceStr = ' '
        import madrigal._Madrec
        retStr = ''
        mnemList = self.parmStr.split(',')

        for mnem in mnemList:
            strLength = madrigal._Madrec.madGetParWidth(self.madParmObj.getParmMnemonic(mnem))
            
            if self.format == 'html':
                thisLabelStr = '<a href=JavaScript:popup("%s")>%s</a>' % (mnem.upper(), mnem.upper())
            else:
                thisLabelStr = mnem.upper()
            # pad according to string length
            if retStr == '':
                retStr = spaceStr *2 + thisLabelStr
            else:
                if len(mnem) < strLength:
                    retStr += spaceStr * (strLength-len(mnem)) + thisLabelStr
                elif self.format == 'html':
                    retStr += '<a href=JavaScript:popup("%s")>%s</a>' % (mnem.upper(), mnem.upper()[:strLength])
                else:
                    retStr += thisLabelStr[:strLength]

        return retStr
        
	


    def outputHead(self, title):

        if self.format == 'html':
            print "Content-Type: text/html"
            print                               # blank line, end of headers
            self.scriptHeaders = 1
            print '<html>'
            print '<head>'
            print '\t<title>' + title + '</title>'
            cssAddress = os.path.join(self.madDBObj.getRelativeTopLevel(), 'madrigal.css')
            print '\t<link href="/%s" rel="stylesheet" type="text/css" />' % (cssAddress)
            self.printJavaScript()
            print '</head>'
        elif self.format == 'TIME_SERIES-ASCII':
            print "Content-Type: text/plain"
            print                               # blank line, end of headers


    def printJavaScript(self):

        print '<script language = "JavaScript">'
        self.printDiffInst()
        self.printDiffExp()
        self.printPopup()
        self.printGetDesc()
        self.printMLookup()
        self.printCParamDiction()
        print '</script>'


    def printDiffInst(self):

        print '\tfunction diffInst(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simpleChooseInstrument.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'

    def printDiffExp(self):

        print '\tfunction diffExp(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simpleChooseExperiments.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'


    def printPopup(self):
        print '\tfunction popup(acronym)'
        print '\t{'
        print '\t\tinfo2 = window.open ("","","WIDTH=600,HEIGHT=400,SCROLLBARS=yes")'
        print '\t\tinfo2.document.write("<HTML><HEAD><TITLE>" + acronym + "<\/TITLE>")'
        print '\t\tinfo2.document.write("' + self.madDBObj.getHtmlStyle() + '")'
        print '\t\tinfo2.document.write("<CENTER><B>")'
        print '\t\tinfo2.document.write(acronym)'
        print '\t\tinfo2.document.write("<\/B><\/CENTER><P>")'
        print '\t\tinfo2.document.write(getDesc(acronym))'
        print '\t\tinfo2.document.write(\'<p><form><center><input type="button" value="Close Window" onClick="window.close()">\')'
        print '\t\tinfo2.document.write(\'<\/form><\/center>\')'
        print '\t\tinfo2.document.write("<\/BODY><\/HTML>")'
        print '\t\tinfo2.document.close()'
        print '\t}\n'


    def printGetDesc(self):
        print '\tfunction getDesc(name)'
        print '\t{'
        print '\t\tvar o = new cParamDiction()'
        print '\t\treturn(o.Lookup(name))'
        print '\t}\n'


    def printMLookup(self):
        print '\tfunction mLookup(strKeyName)'
        print '\t{'
        print '\t\treturn(this[strKeyName])'
        print '\t}\n'


    def printCParamDiction(self):
        print '\t// A dictionary object of Parameters and Descriptions:'
        print '\tfunction cParamDiction()'
        print '\t{'
        print '\t\tthis.Lookup = mLookup'
        i = 0
        mnemList = self.parmStr.split(',')
        descList = self.madParmObj.getParmDescriptionList(mnemList)
        for parm in mnemList:
            print '\t\tthis["' + parm.upper() + \
                    '"] = "' + descList[i].replace('</', '<\/') + '"'
            i = i + 1
        print '\t}\n'
    

    def printHiddenElements(self):

        for key in self.madForm.keys():
            if key in ('selectInstrument','selectExperiments'):
                if type(self.madForm.getvalue(key)) == types.ListType:
                    for value in self.madForm.getvalue(key):
                        print '<input type=hidden name=' + str(key) + \
                              ' value=' + value + '>'
                else:
                    print '<input type=hidden name=' + str(key) + \
                          ' value="' + str(cgi.escape(self.madForm.getvalue(key))) + '">'


    def printBody(self):
        if self.format == 'html':
            print'<table width="100%"  border="1" class="nav_cgi">'
            print'  <tr>'
            print'    <td><div align="center">Simple Madrigal data access - ascii data print...</div></td>'
            print'  </tr>'
            print'</table>'
            print'<form name="form1" method="post" action="simpleChooseExperiments.py">'
            self.printHiddenElements()
            print'<table width="100%"  border="1">'
            print'  <tr>'
            print'    <td valign="top"><p>Selected Instrument: </p>'
            print'        <ul>'
            print'          <li><em>%s</em></li>' % (self.instrumentName)
            print'        </ul>'
            print'        <p>'
            print'          <input type="button" name="diffInstButton" value="Choose different instrument" onClick="diffInst(this.form)">'
            print'      </p></td>'
            print'    <td><p>Selected dates: </p>'
            print'        <ul>'
            
            for index in range(len(self.experimentList)-1,-1,-1):
                print'          <li>%s</li>' % (self.experimentList[index])

            print'        </ul>'
            print'        <p>'
            print'          <input name="timeButton" type="button" id="timeButton3" value="Choose different dates" onClick="diffExp(this.form)">'
            print'      </p></td>'
            print'  </tr>'
            print'</table>'
            print'<h3 align="center">Ascii data print</h3>'
            print'<center><p><i>Click on any parameter name for a definition</i></p></center>'

            if len(self.allParmsWithErrors) == 0:
                print '<center><p><i>Caution: no parameters with error values found in these files.</i></p></center>'

            print '<pre>'

            print self.headerStr

        elif self.format == 'TIME_SERIES-ASCII':
            self.printAsciiHeader()

        elif self.format == 'TIME_SERIES-VOT':
            self.printVOTHeader()
        

        # to increase speed, check whether self.allExpFiles contains the same single file each time,
        # and all the dates are contiguous
        isContiguous = True
        if len(self.allExpFiles) > 1:
            previousDate = None
            for i in range(len(self.allExpFiles)):
                if len(self.allExpFiles[i]) > 1:
                    isContiguous = False
                    break
                if i > 0:
                    if self.allExpFiles[i][0].name != self.allExpFiles[i-1][0].name:
                        isContiguous = False
                        break
                thisDateStr = self.experimentList[i]
                thisDate = datetime.date(int(thisDateStr[0:4]), int(thisDateStr[5:7]), int(thisDateStr[8:]))
                if previousDate != None:
                    timeDelta = previousDate - thisDate
                    if timeDelta.days != 1:
                        isContiguous = False
                        break
                previousDate = thisDate
        else:
            isContiguous = False
            
        totalBytes = 0 # make sure we don't overwhelm the browser with data
        shouldBreak = False
        
        for index in range(len(self.allExpFiles)-1,-1,-1):
            theseFiles = self.allExpFiles[index]
            if not isContiguous:
                thisDate = self.experimentList[index]
                thisFilter = 'date1=%s/%s/%s time1=00:00:00 date2=%s/%s/%s time2=23:59:59' % (thisDate[5:7],
                                                                                              thisDate[8:],
                                                                                              thisDate[0:4],
                                                                                              thisDate[5:7],
                                                                                              thisDate[8:],
                                                                                              thisDate[0:4])
            else:
                # we only need to run isprint once to save time
                if index != len(self.allExpFiles)-1:
                    break
                firstDate = self.experimentList[-1]
                lastDate = self.experimentList[0]
                thisFilter = 'date1=%s/%s/%s time1=00:00:00 date2=%s/%s/%s time2=23:59:59' % (firstDate[5:7],
                                                                                              firstDate[8:],
                                                                                              firstDate[0:4],
                                                                                              lastDate[5:7],
                                                                                              lastDate[8:],
                                                                                              lastDate[0:4])

            for thisFile in theseFiles:
            
                thisIsprintStr = self.madWebObj.isprint(thisFile.name,
                                                        self.parmStr,
                                                        thisFilter,
                                                        self.user_fullname,
                                                        self.user_email,
                                                        self.user_affiliation)
                if self.format == 'TIME_SERIES-ASCII':
                    totalBytes += self.__prependISO8601ToIsprintStr(thisIsprintStr)

                elif self.format == 'TIME_SERIES-VOT':
                    totalBytes += self.__convertToVotable(thisIsprintStr)

                else:
                    sys.stdout.write(thisIsprintStr)
                    totalBytes += len(thisIsprintStr)
                
                if totalBytes > self.__maxBytesInBrowser:
                    print 'WARNING: DATA TRUNCATED DUE TO LARGE AMOUNT REQESTED - PLEASE SELECT FEWER TIME PERIODS!!!'
                    shouldBreak = True
                    break

            if shouldBreak:
                break
            
        if self.format == 'html':               
            print '</pre>'
            
            print'</form>'
            print'<table width="100%"  border="1" class="nav_cgi">'
            print'  <tr>'
            print'    <td><a href="/%s/wt_simple.html">Tutorial</a> on this page</td>' % (self.madDBObj.getRelativeTopLevel())
            print'    <td><a href="accessData.cgi">Return to Access Data page</a></td>'
            print'    <td><a href="/%s">Return to Madrigal home page </a></td>' % (self.madDBObj.getRelativeTopLevel())
            print'    <td><a href="/%s/simpleDifferences.html">How is the simple data access different?</a></td>' % (self.madDBObj.getRelativeTopLevel())
            print'  </tr>'
            print'</table>'
            # print feedback link
            print '<p><hr><i>Please send any comments or suggestions to the <a href="mailto:openmadrigal-users@openmadrigal.org">' + \
                  'Open Madrigal Users Mailing List.</a></i>'
            print'<p>&nbsp;</p>'
   

    def printEndPage(self):
        print '</body></html>'



    def printAsciiHeader(self):
        """printAsciiHeader prints parameter definitions and header line
        """
        mnemList = self.parmStr.split(',')

        # print hardcoded description of ISO8601_Timestamp
        print '# DESCRIPTION OF INCLUDED PARAMETERS\n#'
        print '# ISO8601_Timestamp - IS0-8601 format, eg. "2006-02-08T00:00:00". Times are expressed in UTC.'

        for mnem in mnemList:
            mnemStr = '# %s = %s  Units: %s' % (mnem,
                                                self.madParmObj.getSimpleParmDescription(mnem),
                                                self.madParmObj.getParmUnits(mnem))
            print mnemStr

            
        print '\n# ISO8601_Timestamp  ' + self.headerStr



    def printVOTHeader(self):
        """printVOTHeader prints the VOTable up to the DATA section
        """
        votableTemplate = """
<?xml version="1.0"?>
<VOTABLE version="1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <COOSYS ID="Madrigal_coosys" system="geo_app"/>
  <RESOURCE name="Madrigal output" type="meta">
    <TABLE name="results">
      <DESCRIPTION>Simple time access protocol data</DESCRIPTION>"""

        fieldTemplate ="""       <FIELD name="%s" ID="col%i" unit="%s" datatype="double">
          <DESCRIPTION>%s</DESCRIPTION></FIELD>"""
        
        print votableTemplate

        # loop through each parameter
        mnemList = self.parmStr.split(',')

        # print hardcoded description of ISO8601_Timestamp
        print """      <FIELD name="ISO8601_Timestamp" ID="col1" ucd="time.obs.start" unit="iso8601" datatype="char" arraysize="*"/>"""

        count = 2
        for mnem in mnemList:
            mnemStr = fieldTemplate % (mnem,
                                       count,
                                       self.madParmObj.getParmUnits(mnem),
                                       self.madParmObj.getSimpleParmDescription(mnem))
            print mnemStr
            count += 1

        print '      <DATA>\n         <TABLEDATA>'



   
    def __prependISO8601ToIsprintStr(self, thisIsprintStr):
        """__prependISO8601ToIsprintStr prepends an ISO8601 time (eg 1998-01-20T13:53:17) to each line,
        This string is printed.  Total count is returned.
        """
        count = 0
        lines = thisIsprintStr.split('\n')
        newStr = ''
        for line in lines:
            items = line.split()
            if len(items) > 6:
                year   = int(items[0])
                month  = int(items[1])
                day    = int(items[2])
                hour   = int(items[3])
                minute = int(items[4])
                second = int(items[5])
                isotime = '%04i-%02i-%02iT%02i:%02i:%02i  ' % (year,month,day,hour,minute,second)
                newStr += isotime + line
                print newStr
                count += len(newStr)
                newStr = ''
            else:
                newStr += '\n'

        return count


    def __convertToVotable(self, thisIsprintStr):
        """__convertToVotable takes the isprint string and formats it as a votable.
        This string is printed.  Total count is returned.
        """
        count = 0
        lines = thisIsprintStr.split('\n')
        newStr = ''
        for line in lines:
            items = line.split()
            if len(items) > 6:
                try:
                    year   = int(items[0])
                except:
                    # possibly no data found
                    return(count)
                month  = int(items[1])
                day    = int(items[2])
                hour   = int(items[3])
                minute = int(items[4])
                second = int(items[5])
                isotime = '%04i-%02i-%02iT%02i:%02i:%02i' % (year,month,day,hour,minute,second)
                newStr += '        <TR>\n          <TD>%s</TD>' % (isotime)
                # add the rest
                for item in items:
                    newStr += '<TD>%s</TD>' % (item)
                newStr += '\n        </TR>\n'
                print newStr
                count += len(newStr)
                newStr = ''
            else:
                continue

        return count


    def printCloseVOT(self):
        """printCloseVOT prints the closing tags of a votable
        """
        print """        </TABLEDATA>
      </DATA>
    </TABLE>
  </RESOURCE>
</VOTABLE>\n"""

if __name__ == '__main__':

    # Script simpleIsprint.py
    # This script only calls the init function of the class simpleIsprint
    # All work is done by the init function
    simpleIsprint()
