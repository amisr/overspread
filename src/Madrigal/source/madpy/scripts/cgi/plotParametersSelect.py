#!PYTHONEXE

import sys, os, traceback
import cgi, Cookie
import time, datetime
import types
import os.path




class plotParametersSelect:
    """plotParameters is the class that produces the plotParametersSelect page.
    
    plotParametersSelect has the following structure:  the entire cgi is contained in one class, with a
    main function at the end which serves simply to call the __init__  function of the class.  This
    __init__ function is responsible for calling all other class methods. It is made up of a single
    try block, with the purpose of reporting all exceptions in well-formatted html to both the user
    and the administrator. The __init__ function first makes sure the pythonlib can be found.  It
    then calls setScriptState to determine from any cgi arguments what the script is supposed to do.
    The script state is always set in self.state.  The particular values allowed for plotParametersSelect
    are discussed below.

    The __init__ function then calls createObjects to create whatever python api objects are required
    to complete the script.  If the user has made a request that may succeed or may fail, that request is
    then processed.  The script thereafter calls outputHead to output the header section and any required
    javascript.  Finally, __init__ calls a few functions for each of the main sections of the body.

    If any uncaught exception is thrown, its caught by the __init__ try block.  If it's a MadrigalError,
    additional information is available.  The catch blocks attempt to display the error message on the screen
    by backing out of of large number of possible tags, which might prevent its display (in any case, the error
    message will always be available in the page source.  The formatted error message is also sent to the email
    address given in the siteTab.txt metadata file.

    Every attempt is made to generate easy to read source html, since it is often an easy starting point for
    analyzing the script.  Table structure is indicated by indentation, as is javascript code structure.


    List of cgi arguments that this script uses:

    stationName - instrument id of instruments to be plotted.  More than one allowed.

    startYear, startMonth, startDay, startHour, startMin, startSec - starting time of plot to create

    endYear, endMonth, endDay, endHour, endMin, endSec - ending time of plot to create

    plotType - either "scatter" or "pcolor"

    parmSelect - mnemonic of parameter to plot

    parmLower - (optional) - lower value filter for parameter value

    parmUpper - (optional) - upper value filter for parameter value

    gdaltLower - (optional) - lower value filter for altitude - only if plotType == pcolor

    parmUpper - (optional) - upper value filter for titude - only if plotType == pcolor

    filterParm - (optional) - a parmeter other than the one selected that can be used to filter data

    filterLower - (optional) - lower value for filterParm parameter

    filterUpper - (optional) - upper value for filterParm parameter

    filterParm2 - (optional) - a 2nd parmeter other than the one selected that can be used to filter data

    filterLower2 - (optional) - lower value for filterParm2 parameter

    filterUpper2 - (optional) - upper value for filterParm2 parameter

    filterParm3 - (optional) - a 3rd parmeter other than the one selected that can be used to filter data

    filterLower3 - (optional) - lower value for filterParm3 parameter

    filterUpper3 - (optional) - upper value for filterParm3 parameter
  
  

    Change history:

    Written by "Bill Rideout":mailto:brideout@haystack.mit.edu  July 6, 2005

    Two additional parameters added to allow additional filtering. Sep. 20, 2005

    $Id: plotParametersSelect.py,v 1.9 2009/04/23 16:10:09 brideout Exp $
    """

    # constants
    __scriptName = 'plotParametersSelect.py'

    # number of parameters per column
    maxParmCols = 5

   
    def __init__(self):
        """__init__ run the entire plotParametersSelect script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script plotParametersSelect.

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
            sys.path.append('MADROOT/lib/python')

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
            
            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()

            self.verifyDataExists()
            
            # output html

            #print header
            self.outputHead('Choose parameter to plot')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printHeading()

            self.printDescription()

            self.printHiddenElements()

            self.printParameters()

            self.printEndPage()

            

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
                    errStr = errStr + ' = ' + str(self.madForm.getvalue(key))

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
                    errStr = errStr + ' = ' + str(self.madForm.getvalue(key))

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

                    

    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()


        # create object for MadrigalInstrument
        self.madInstrumentObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

        # create list of selected kinst values
        self.madKinstList = []
        stationName = self.madForm.getvalue("stationName")
        # checking for lists or strings and changing as appropriate
        if type(stationName) == types.ListType:
            stationNameList = stationName
        else:
            stationNameList = [stationName]
            
        for item in stationNameList:
            self.madKinstList.append(int(item))

        # create starting and ending datetimes
        self.startDatetime = datetime.datetime(int(self.madForm.getvalue("startYear")),
                                               int(self.madForm.getvalue("startMonth")),
                                               int(self.madForm.getvalue("startDay")),
                                               int(self.madForm.getvalue("startHour")),
                                               int(self.madForm.getvalue("startMin")),
                                               int(self.madForm.getvalue("startSec")))

        self.endDatetime  =  datetime.datetime(int(self.madForm.getvalue("endYear")),
                                               int(self.madForm.getvalue("endMonth")),
                                               int(self.madForm.getvalue("endDay")),
                                               int(self.madForm.getvalue("endHour")),
                                               int(self.madForm.getvalue("endMin")),
                                               int(self.madForm.getvalue("endSec")))

        self.plotType = self.madForm.getvalue("plotType")


        # create object for MadrigalParameters
        import madrigal.data
        self.madParametersObj = madrigal.data.MadrigalParameters(self.madDBObj)

        

        # create MadrigalExperiments object for local Madrigal site
        self.madExperimentsObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)

        # create a MadrigalWeb object
        import madrigal.ui.web
        self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)
        self.madWebFormatObj = madrigal.ui.web.MadrigalWebFormat()
                
        # create a list of measured and derivable parameters for these instruments
        self.parameterList = self.madParametersObj.getParametersForInstruments(self.madKinstList)
 
        # get category dict
        self.categoryDict = self.madParametersObj.getCategoryDict(self.parameterList)

        # get mnemonic list
        self.mnemonicList = self.madParametersObj.getParmMnemonicList(self.parameterList)

        # get description list
        self.descList = self.madParametersObj.getParmDescriptionList(self.parameterList)


    def verifyDataExists(self):
        """verifyDataExists checks whether experiments exits for the given instruments in the given time range
        """
        self.dataExists = False
        index = -1
        while 1:
            index += 1
            thisKinst = self.madExperimentsObj.getKinstByPosition(index)
            if thisKinst == None:
                break
            if thisKinst not in self.madKinstList:
                continue
            # see if this experiment overlaps at all
            thisStartTime = self.madExperimentsObj.getExpStartDateTimeByPosition(index)
            thisStartDateTime = datetime.datetime(thisStartTime[0],
                                                  thisStartTime[1],
                                                  thisStartTime[2],
                                                  thisStartTime[3],
                                                  thisStartTime[4],
                                                  thisStartTime[5])
            if thisStartDateTime > self.endDatetime:
                # experiment started after time period
                continue
            
            thisEndTime = self.madExperimentsObj.getExpEndDateTimeByPosition(index)
            thisEndDateTime = datetime.datetime(thisEndTime[0],
                                                thisEndTime[1],
                                                thisEndTime[2],
                                                thisEndTime[3],
                                                thisEndTime[4],
                                                thisEndTime[5])
            if thisEndDateTime < self.startDatetime:
                # experiment ended before time period
                continue

            self.dataExists = True
            break

        
    def outputHead(self, title):

        print "Content-Type: text/html"
        print                               # blank line
        self.scriptHeaders = 1
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        print '\t<style type="text/css">.lb {background: #eeeeff}</style>'
        self.printJavaScript()
        print '</head>'


    def printJavaScript(self):

        print '<script language = "JavaScript">'
        if self.dataExists:
            self.printTrim()
            self.printIsNotChecked()
            self.printIsFloat()
            self.printGetNumTokens()
            self.printValidateFilter()
            self.printPopup()
            self.printGetDesc()
            self.printMLookup()
            self.printCParamDiction()
            self.printClosePage()
        else:
            self.printRedirect()
        print '</script>'

    def printRedirect(self):

        # queryStr gets all names and values of hidden elements, so that a redirect to listExperiments.py
        # with all previously entered info is possible
        queryStr = ''
        for key in self.madForm.keys():
            value = self.madForm.getvalue(key)
            if type(value) == types.ListType:
                for item in value:
                    item = cgi.escape(str(item))
                    item = item.replace('+', '%2B')
                    queryStr += '" + escape("' + cgi.escape(str(key)) + '") + "=' + \
                                str(item) + '" + "&'               
            elif type(value) == types.StringType:
                value = cgi.escape(str(value))
                value = value.replace('+', '%2B')
                queryStr += '" + escape("' + cgi.escape(str(key)) + '") + "=' + \
                            value + '" + "&' 

        print '\tfunction redirect()'
        print '\t{'
        print '\t\twindow.location.href="listExperiments.py?' + queryStr + '"'
        print '\t}'
        print '\ttimerID = setTimeout("redirect()", 5000)'


    def printTrim(self):
        
        print '\tfunction trim(strText)'
        print '\t{'
        print '\t\t// this will get rid of leading spaces'
        print '\t\twhile (strText.substring(0,1) == \' \')'
        print '\t\t\tstrText = strText.substring(1, strText.length);'
        print '\t\t// this will get rid of trailing spaces'
        print '\t\twhile (strText.substring(strText.length-1,strText.length) == \' \')'
        print '\t\t\tstrText = strText.substring(0, strText.length-1);'
        print '\t\treturn strText;'
        print '\t}\n'

    
    def printIsNotChecked(self):
        
        print '\tfunction isNotChecked(radioObj)'
        print '\t{'
        print '\t\tvar newLength = radioObj.length;'
        print '\t\tfor( var i=0; i<newLength; i++ ) {'
        print '\t\t\tif( radioObj[i].checked ) {'
        print '\t\t\t\treturn false;'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\treturn true;'
        print '\t}\n'

    def printIsFloat(self):
        
        print '\tfunction isFloat(textObj)'
        print '\t{'
        print '\t\tvar newValue = trim(textObj.value);'
        print '\t\tvar newLength = newValue.length;'
        print '\t\tfor (var i = 0; i != newLength; i++)'
        print '\t\t{'
        print '\t\t\taChar = newValue.substring(i,i+1);'
        print '\t\t\tif((aChar < "0" || aChar > "9") && (aChar != ".") && (aChar != "-") && (aChar != "e") && (aChar != "E"))'
        print '\t\t\t{'
        print '\t\t\t\treturn false;'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\treturn true;'
        print '\t}\n'

    def printGetNumTokens(self):
        
        print '\tfunction getNumTokens(textObj)'
        print '\t{'
        print '\t\tvar newValue = trim(textObj.value);'
        print '\t\tvar arrayOfStrings = newValue.split(" ");'
        print '\t\treturn arrayOfStrings.length;'
        print '\t}\n'

    def printValidateFilter(self):
        
        # check that all filter values are valid via javascript before form is submitted
        print '\tfunction validateFilter(madForm)'
        print '\t{'

        # parmLower
        print '\t\tif (!isFloat(madForm.parmLower))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for selected parameter filter lower limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        # parmUpper
        print '\t\tif (!isFloat(madForm.parmUpper))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for selected parameter upper limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        
        if self.plotType == 'pcolor':
            # parmLower
            print '\t\tif (!isFloat(madForm.gdaltLower))'
            print '\t\t{'
            print '\t\t\talert("Invalid entry for altitude lower limit.  Please correct and try again.");'
            print '\t\t\treturn false;'
            print '\t\t}'
            # parmUpper
            print '\t\tif (!isFloat(madForm.gdaltUpper))'
            print '\t\t{'
            print '\t\t\talert("Invalid entry for altitude upper limit.  Please correct and try again.");'
            print '\t\t\treturn false;'
            print '\t\t}'

        #filterParm
        print '\t\tif (getNumTokens(madForm.filterParm) > 0 && madForm.filterParm.value.length > 0)'
        print '\t\t{'
        print '\t\t\tvar mnemStr=trim(madForm.filterParm.value)'
        print '\t\t\tvar mnemList = mnemStr.split(" ")'
        print '\t\t\tfor(var i=0; i<mnemList.length; i++)'
        print '\t\t\t{'
        print '\t\t\t\tif(typeof(getDesc(mnemList[i].toUpperCase())) != "string")'
        print '\t\t\t\t{'
        print '\t\t\t\t\talert("Illegal parameter name found in optional filter: " + mnemList[i])'
        print '\t\t\t\t\treturn false;'
        print '\t\t\t\t}'
        print '\t\t\t}'
        print '\t\t}'
        # filterLower
        print '\t\tif (!isFloat(madForm.filterLower))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for 1st optional parameter lower limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        # filterUpper
        print '\t\tif (!isFloat(madForm.filterUpper))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for 1st optional parameter upper limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'

        #filterParm2
        print '\t\tif (getNumTokens(madForm.filterParm2) > 0 && madForm.filterParm2.value.length > 0)'
        print '\t\t{'
        print '\t\t\tvar mnemStr=trim(madForm.filterParm2.value)'
        print '\t\t\tvar mnemList = mnemStr.split(" ")'
        print '\t\t\tfor(var i=0; i<mnemList.length; i++)'
        print '\t\t\t{'
        print '\t\t\t\tif(typeof(getDesc(mnemList[i].toUpperCase())) != "string")'
        print '\t\t\t\t{'
        print '\t\t\t\t\talert("Illegal parameter name found in optional filter: " + mnemList[i])'
        print '\t\t\t\t\treturn false;'
        print '\t\t\t\t}'
        print '\t\t\t}'
        print '\t\t}'
        # filterLower2
        print '\t\tif (!isFloat(madForm.filterLower2))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for 2nd optional parameter lower limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        # filterUpper2
        print '\t\tif (!isFloat(madForm.filterUpper2))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for 2nd optional parameter upper limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'

        #filterParm3
        print '\t\tif (getNumTokens(madForm.filterParm3) > 0 && madForm.filterParm3.value.length > 0)'
        print '\t\t{'
        print '\t\t\tvar mnemStr=trim(madForm.filterParm3.value)'
        print '\t\t\tvar mnemList = mnemStr.split(" ")'
        print '\t\t\tfor(var i=0; i<mnemList.length; i++)'
        print '\t\t\t{'
        print '\t\t\t\tif(typeof(getDesc(mnemList[i].toUpperCase())) != "string")'
        print '\t\t\t\t{'
        print '\t\t\t\t\talert("Illegal parameter name found in optional filter: " + mnemList[i])'
        print '\t\t\t\t\treturn false;'
        print '\t\t\t\t}'
        print '\t\t\t}'
        print '\t\t}'
        # filterLower3
        print '\t\tif (!isFloat(madForm.filterLower3))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for 3rd optional parameter lower limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        # filterUpper3
        print '\t\tif (!isFloat(madForm.filterUpper3))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for 3rd optional parameter upper limit.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'

        print '\t\treturn true;'
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
        for parm in self.mnemonicList:
                print '\t\tthis["' + parm + \
                      '"] = "' + self.descList[i].replace('</', '<\/') + '"'
                i = i + 1
        print '\t}\n'
    

    def printClosePage(self):

        print '\tfunction ClosePage(madForm)'
        print '\t{'
        print '\t\tif (isNotChecked(madForm["parmSelect"]))'
        print '\t\t{'
        print '\t\t\talert("Please choose a parameter first.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!validateFilter(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tmadForm.action="globalPlot.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'

    # end javascript
    

    def printHeading(self):

        if not self.dataExists:
            print """<B>No experiments found for the instruments and time period you selected.  You will be redirected
                      to a page that lists when data exists for your selected instruments.</B>"""
            sys.exit(0)

        print '<h1>Choose a single parameter to plot for the selected instruments</h1>'
        if self.plotType == 'scatter':
            print '<p>The parameter you choose will be plotted as a scatter plot versus the time period you selected.</p>'
        elif self.plotType == 'pcolor':
            print '<p>The parameter you choose will be plotted as a pcolor plot versus altitude and the time period you selected.</p>'
        print '<p>Click on any parameter name to see a full description.</p>'
        
       
    def printDescription(self):

        print '<form method=get enctype="application/x-www-form-urlencoded">'          #begin form>'
        
        
    def printHiddenElements(self):

        # now print all received post elements from plotInstrumentsSelect.py as hidden elements
        # if form just loaded
        print '<input type=hidden name=callingpage value=plotParametersSelect.py>'
        for key in self.madForm.keys():
            if key in ('callingpage', 'parmSelect', 'parmLower', 'parmUpper',
                       'gdaltLower', 'gdaltUpper', 'filterParm', 'filterLower', 'filterUpper',
                       'filterParm2', 'filterLower2', 'filterUpper2',
                       'filterParm3', 'filterLower3', 'filterUpper3'):
                continue
            if type(self.madForm.getvalue(key)) == types.ListType:
                for value in self.madForm.getvalue(key):
                    print '<input type=hidden name=' + str(key) + \
                          ' value=' + value + '>'
            else:
                print '<input type=hidden name=' + str(key) + \
                          ' value="' + str(cgi.escape(self.madForm.getvalue(key))) + '">'



    def printParameters(self):

        # categories to skip
        skipCategories = ('Time Related Parameter', 'Prolog Parameters')
        
        # categories to move to the bottom
        bottomCategories = ['Geographic Coordinate', 'Magnetic Coordinate']
        if self.plotType == 'pcolor':
            bottomCategories.append('Geophysical Index')
            bottomCategories.append('Interplanetary Magnetic Field')
            bottomCategories.append('Data Quality Parameter')
                          
        
        # loop through each parameter category, and print all normal categories
        keyList = self.categoryDict.keys()
        keyList.sort()
        for key in keyList:
            item = self.categoryDict[key]
            catName = item[0]
            if catName in skipCategories:
                continue
            if catName in bottomCategories:
                continue
            parmList = item[1]
            self.printCategoryTableIntro(catName)
            # set up count of number printed this column
            numThisCol = 1
            # set up parameter index
            parmIndex = -1
            # now loop through each parameter
            for parm in parmList:
                parmIndex = parmIndex + 1
                if self.madParametersObj.isAddIncrement(parm):
                    continue
                # print the parameter
                self.printParameterCheckbox(parm, parmIndex)
                numThisCol = numThisCol + 1
                # check if category done
                if parmIndex + 1 == len(item[1]):
                    # fill up the rest of the cols with blanks
                    self.fillRowWithBlanks(self.maxParmCols + 1 - numThisCol)

                # check if new row needed
                if numThisCol > self.maxParmCols:
                    print '\t\t</tr><tr>'
                    numThisCol = 1

            print '\t\t</tr></table></ul></ul>'

        # loop through each parameter category, and print all bottom categories
        keyList = self.categoryDict.keys()
        keyList.sort()
        for key in keyList:
            item = self.categoryDict[key]
            catName = item[0]
            if catName not in bottomCategories:
                continue
            parmList = item[1]
            self.printCategoryTableIntro(catName)
            # set up count of number printed this column
            numThisCol = 1
            # set up parameter index
            parmIndex = -1
            # now loop through each parameter
            for parm in parmList:
                parmIndex = parmIndex + 1
                if self.madParametersObj.isAddIncrement(parm):
                    continue
                # print the parameter
                self.printParameterCheckbox(parm, parmIndex)
                numThisCol = numThisCol + 1
                # check if category done
                if parmIndex + 1 == len(item[1]):
                    # fill up the rest of the cols with blanks
                    self.fillRowWithBlanks(self.maxParmCols + 1 - numThisCol)

                # check if new row needed
                if numThisCol > self.maxParmCols:
                    print '\t\t</tr><tr>'
                    numThisCol = 1

            print '\t\t</tr></table></ul></ul>'

  
    def printCategoryTableIntro(self, categoryName):

        print '\t\t<ul><br><b>' + categoryName + '</b>'
        print '\t\t<ul><table COLS=' + str(self.maxParmCols) + ' WIDTH="80%"><tr>'


    def printParameterCheckbox(self, parm, parmIndex):

        print '\t\t\t<td width="' + str(100.0/self.maxParmCols) + \
              '%"><input class="lb" type=radio name=parmSelect value=' + \
              str(parm)
        
        print '\t\t\t\t>&nbsp;<a href=JavaScript:popup("' + \
                  parm + '")>' + parm + '</a></td>'


    def fillRowWithBlanks(self, num):

        if num < 0 or num > self.maxParmCols:
            return
        while (num):
            print '\t\t\t<td width="' + str(100.0/self.maxParmCols) + '%">&nbsp;</td>'
            num = num - 1
        
    
    def printEndPage(self):

        print """ <br>
                  <br>
                  <span style="font-weight: bold; font-style: italic;">Filter Data<br>
                  </span>
                  </ul>
                  </ul>
                  <table
                  style="width: 80%; text-align: left; margin-left: auto; margin-right: auto;"
                  border="1" cellpadding="2" cellspacing="2">
                  <tbody>
                  <tr>
                  <td style="vertical-align: top; text-align: left;">Set
                  limits for the parameter you selected (leave blank for all data)<br>
                  </td>
                  <td style="vertical-align: top;">Lower<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="parmLower"></font></td>
                  <td style="vertical-align: top;">Upper</td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="parmUpper"></font></td>
                  </tr>"""
        
        if self.plotType == 'pcolor':
            print """<tr>
                  <td style="vertical-align: top;">Set altitude range (leave
                  blank for all altitudes)<br>
                  </td>
                  <td style="vertical-align: top;">Lower</td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="gdaltLower"></font></td>
                  <td style="vertical-align: top;">Upper</td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="gdaltUpper"></font></td>
                  </tr>"""
            
        print """</tbody>
                  </table>
                  <ul>
                  <ul>
                  <span style="font-weight: bold; font-style: italic;"></span>
                  </ul>
                  </ul>
                  <table
                  style="width: 80%; text-align: left; margin-left: auto; margin-right: auto;"
                  border="1" cellpadding="2" cellspacing="2">
                  <tbody>
                  <tr>
                  <td style="vertical-align: top; text-align: left;">Optional -
                  filter data using other parameters<br>
                  </td>
                  <td style="vertical-align: top;">Parm (use&nbsp; a name from
                  list above)<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterParm"></font></td>
                  <td style="vertical-align: top;">Lower<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterLower"></font></td>
                  <td style="vertical-align: top;">Upper<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterUpper"></font></td>
                  </tr>
                  <tr>
                  <td style="vertical-align: top; text-align: left;">Optional -
                  filter data using other parameters<br>
                  </td>
                  <td style="vertical-align: top;">Parm (use&nbsp; a name from
                  list above)<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterParm2"></font></td>
                  <td style="vertical-align: top;">Lower<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterLower2"></font></td>
                  <td style="vertical-align: top;">Upper<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterUpper2"></font></td>
                  </tr>
                  <tr>
                  <td style="vertical-align: top; text-align: left;">Optional -
                  filter data using other parameters<br>
                  </td>
                  <td style="vertical-align: top;">Parm (use&nbsp; a name from
                  list above)<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterParm3"></font></td>
                  <td style="vertical-align: top;">Lower<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterLower3"></font></td>
                  <td style="vertical-align: top;">Upper<br>
                  </td>
                  <td style="vertical-align: top;"><font style="font-size: 13pt;"
                  size="5"><input size="8" name="filterUpper3"></font></td>
                  </tr>
                  </tbody>
                  </table>
                  <ul>
                  <ul>
                  <span style="font-weight: bold; font-style: italic;"><br>
                  </span>
        """

        self.printButtons()
        print '</form></body></html>'       # end form, body, and html 


    def printButtons(self):

        print '<center><p>'
        print '<input type="reset" value="Reset">'
        if self.plotType == 'scatter':
            buttonName = 'Create scatter plot versus time'
        elif self.plotType == 'pcolor':
            buttonName = 'Create pcolor plot of altitude versus time'
        else:
            buttonName = 'Done'
        print '<input class=lb type=button value="%s" onClick=ClosePage(this.form)>' % (buttonName)
        print '</p></center>'


    
          

if __name__ == '__main__':

    # Script madParmList
    # This script only calls the init function of the class plotParametersSelect
    # All work is done by the init function
    plotParametersSelect()
