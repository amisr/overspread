#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, os.path
import traceback
import cgi, Cookie
import time, datetime
import types



class madInvent:
    """madInvent is the class that produces the madInvent page.
    
    Like all my python cgi scripts, madInvent has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for madInvent are discussed below.

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

    The names of all form elements used by madInvent are listed below:

    state: gives state of page (new, reload, inventory)

    categoryList: list of categories of instruments to display (0=all categories)

    stationList: local instrument selected - id is instrument id

    sy, sm, sd, ey, em, ed - start and end year, month, day

    sites - 0 = all madrigal sites, 1 = this madrigal site

    displayLevel - 0 = display default only, 1 = display all

    

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jan. 16, 2008 to replace tcl version with greater
    functionality.

    $Id: madInvent.cgi,v 1.13 2009/03/17 13:18:31 brideout Exp $
    """

    # constants
    __scriptName = 'madInvent.cgi'



    def __init__(self):
        """__init__ run the entire madInvent script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script madInvent.

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
            self.outputHead()

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printBody()

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

        self.now = datetime.datetime.now()
        
        #create a form object
        self.madForm = cgi.FieldStorage()

        if self.madForm.has_key('state'):
            self.state = self.madForm.getvalue('state')
            self.sites = int(self.madForm.getvalue('sites'))
            self.displayLevel = int(self.madForm.getvalue('displayLevel'))
            
            # get instruments as list of kinsts
            self.stationList = []
            instList = self.madForm.getlist('stationList')
            for inst in instList:
                self.stationList.append(int(inst))

            # get categories as a list of categoryId's (ints)
            self.categoryList = []
            catList = self.madForm.getlist('categoryList')
            for cat in catList:
                self.categoryList.append(int(cat))

            self.sy = int(self.madForm.getvalue('sy'))
            self.sm = int(self.madForm.getvalue('sm'))
            self.sd = int(self.madForm.getvalue('sd'))
            self.ey = int(self.madForm.getvalue('ey'))
            self.em = int(self.madForm.getvalue('em'))
            self.ed = int(self.madForm.getvalue('ed'))
            
        else:
            self.state = 'new'
            self.sites = 0
            self.displayLevel = 0
            self.stationList = [0]
            self.sy = 1950
            self.sm = 1
            self.sd = 1
            self.ey = self.now.year
            self.em = 12
            self.ed = 31
            self.categoryList = [0]


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        self.siteId = self.madDBObj.getSiteID()

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

        # create a MadrigalInstrument object
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)

        # create MadrigalSite object
        self.madSiteObj = madrigal.metadata.MadrigalSite(self.madDBObj)
        self.siteName = self.madSiteObj.getSiteName(self.madDBObj.getSiteID())

        # create web obj to test whether the user is trusted
        import madrigal.ui.web
        self.webObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)

        # create MadrigalWeb object
        self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)
      
        self.globalExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj,
                                                           os.path.join(self.madDBObj.getMadroot(),
                                                                        'metadata/expTabAll.txt'))
        self.localExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)

        if self.sites == 0:
            self.expObj = self.globalExpObj
        else:
            self.expObj = self.localExpObj

        # create an ordered list of all instruments to display
        # each tuple is made up of (instName, localStartYear, localEndYear,
        #  globalStartYear, globalEndYear, kinst, categoryId)
        # list id ordered by categoryId, then kinst
        # also create categoryDict - key = categoryId, value = category Description
        self.orderInstList, self.categoryDict = \
            self.madInstObj.getOrderedInstrumentListWithData(self.webObj.isTrusted(),
                                                             False,
                                                             self.localExpObj,
                                                             self.globalExpObj,
                                                             allowArchive=False)

        # finally, create a list of local categories, so we can tell which categories are
        # global only
        self.localCategoryList = []
        for item in self.orderInstList:
            if item[1] == 0:
                continue
            if item[6] not in self.localCategoryList:
                self.localCategoryList.append(item[6])

        

    def outputHead(self):

        if self.state != 'inventory':
            title = 'Madrigal Inventory'
        else:
            title = 'Madrigal Experiment List'

        print "Content-Type: text/html"
        print                               # blank line, end of headers
        self.scriptHeaders = 1
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        self.printJavaScript()
        print '</head>'


    def printJavaScript(self):

        print '<script language = "JavaScript">'
        self.printTrim()
        self.printIsInt()
        self.printInRange()
        self.printIsEmpty()
        self.printCategoryObject()
        self.printInstrumentObject()
        self.printCategoryArray()
        self.printInstrumentArray()
        self.printValidateFilter()
        self.printValidateStationList()
        self.printTypeChange()
        self.printInventoryCall()
        self.printRedirectListExperiments()
        print '</script>'


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

        
    def printIsInt(self):
        print '\tfunction isInt(textObj)'
        print '\t{'
        print '\t\tvar newValue = trim(textObj.value);'
        print '\t\tvar newLength = newValue.length;'
        print '\t\tfor (var i = 0; i != newLength; i++)'
        print '\t\t{'
        print '\t\t\taChar = newValue.substring(i,i+1);'
        print '\t\t\tif(aChar < "0" || aChar > "9")'
        print '\t\t\t{'
        print '\t\t\t\treturn false;'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\treturn true;'
        print '\t}\n'


    def printInRange(self):
        print '\tfunction inRange(num, lowerLimit, upperLimit)'
        print '\t{'
        print '\t\tif(num >= lowerLimit && num <= upperLimit)'
        print '\t\t{'
        print '\t\t\treturn true;'
        print '\t\t}'
        print '\t\tif(typeof num != "number")'
        print '\t\t{'
        print '\t\t\treturn true;'
        print '\t\t}'
        print '\t\treturn false;'
        print '\t}\n'


    def printIsEmpty(self):
        print '\tfunction isEmpty(textObj)'
        print '\t{'
        print '\t\tvar newValue = trim(textObj.value);'
        print '\t\tvar newLength = newValue.length;'
        print '\t\tif(newLength == 0)'
        print '\t\t{'
        print '\t\t\treturn true;'
        print '\t\t}'
        print '\t\treturn false;'
        print '\t}\n'


    def printCategoryObject(self):
        """printCategoryObject prints the javascript to create a Javascript Category object.
        """
        print '\t//Category object to hold data for each individual instrument category'
        print '\tfunction Category(name,catId,globalOnly)'
        print '\t{'
        print '\t\tthis.name=name;'
        print '\t\tthis.catId=catId;'
        print '\t\tthis.globalOnly=globalOnly;'
        print '\t}\n'


    def printInstrumentObject(self):
        """printInstrumentObject prints the javascript to create a Javascript Instrument object.
        """
        print '\t//Instrument object to hold data for each individual instrument'
        print '\tfunction Instrument(name,catId,instId,local_sy,local_ey,global_sy,global_ey)'
        print '\t{'
        print '\t\tthis.name=name;'
        print '\t\tthis.catId=catId;'
        print '\t\tthis.instId=instId;'
        print '\t\tthis.local_sy=local_sy;'
        print '\t\tthis.local_ey=local_ey;'
        print '\t\tthis.global_sy=global_sy;'
        print '\t\tthis.global_ey=global_ey;'
        print '\t}\n'


    def printCategoryArray(self):
        """printCategoryArray print the javascript array of Catalog objects
        """
        print '\t//categoryArr holds an array of category objects'
        print '\tvar categoryArr = new Array();'
        print '\t\tcategoryArr[0]=new Category("All Instrument Types", 0);'
        catIdList = self.categoryDict.keys()
        catIdList.sort()
        for i in range(len(catIdList)):
            catId = catIdList[i]
            catName = self.categoryDict[catId]
            if catId in self.localCategoryList:
                globalOnly = 'false'
            else:
                globalOnly = 'true'
            print '\t\tcategoryArr[%i]=new Category("%s", %i, %s);' % (i+1, catName, catId, globalOnly)
        print


    def printInstrumentArray(self):
        """printInstrumentArray print the javascript array of Instrument objects
        """
        print '\t//instArr holds an array of instrument objects'
        print '\tvar instArr = new Array();'
        print '\t\tinstArr[0]=new Instrument("All Instruments", 0, 0, 0, 0, 0, 0);'

        for i in range(len(self.orderInstList)):
            inst = self.orderInstList[i]
            name = inst[0]
            kinst = inst[5]
            catId = inst[6]
            print '\t\tinstArr[%i]=new Instrument("%s", %i,%i,%i,%i,%i,%i);' % (i+1, name,
                                                                                catId, kinst,
                                                                                inst[1],inst[2],
                                                                                inst[3],inst[4])
        print
		

    def printValidateFilter(self):
        # check that all filter values are valid via javascript before form submitted
        print '\tfunction validateFilter(madForm)'
        print '\t{'
        print '\t\tif (!isInt(madForm.sy) || isEmpty(madForm.sy))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start year.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!isInt(madForm.ey) || isEmpty(madForm.ey))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end year.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'

        print '\t\tif (!isInt(madForm.sm) || isEmpty(madForm.sm))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start month.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.sm.value), 1, 12) && madForm.sm.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start month.  Must be between 1 and 12.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        
        print '\t\tif (!isInt(madForm.em) || isEmpty(madForm.em))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end month.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.em.value), 1, 12) && madForm.em.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end month.  Must be between 1 and 12.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        
        print '\t\tif (!isInt(madForm.sd) || isEmpty(madForm.sd))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start day.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.sd.value), 1, 31) && madForm.sd.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start day.  Must be between 1 and 31.");'
        print '\t\t\treturn false;'
        print '\t\t}'

        
        print '\t\tif (!isInt(madForm.ed) || isEmpty(madForm.ed))'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end day.  Please correct and try again.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!inRange(parseInt(madForm.ed.value), 1, 31) && madForm.ed.value.length != 0)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end day.  Must be between 1 and 12.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        
        print '\t\tvar year = parseInt(madForm.sy.value);'
        print '\t\tvar month = parseInt(madForm.sm.value);'
        print '\t\tvar day = parseInt(madForm.sd.value);'
        print '\t\ttry'
        print '\t\t{'
        print '\t\t\tvar thisDate1 = new Date(year, month-1, day);'
        print '\t\t\tvar thisDate = new Date(thisDate1.getTime());'
        print '\t\t\tif (thisDate.getDate() != day)'
        print '\t\t\t{'
        print '\t\t\t\tthrow "Date error";'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\tcatch (e)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for start date.  Check number of days in month.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print
        print '\t\tvar year = parseInt(madForm.ey.value);'
        print '\t\tvar month = parseInt(madForm.em.value);'
        print '\t\tvar day = parseInt(madForm.ed.value);'
        print '\t\ttry'
        print '\t\t{'
        print '\t\t\tvar thisDate2 = new Date(year, month-1, day);'
        print '\t\t\tvar thisDate = new Date(thisDate2.getTime());'
        print '\t\t\tif (thisDate.getDate() != day)'
        print '\t\t\t{'
        print '\t\t\t\tthrow "Date error";'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\tcatch (e)'
        print '\t\t{'
        print '\t\t\talert("Invalid entry for end date.  Check number of days in month.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print
        print '\t\tif (thisDate1 > thisDate2)'
        print '\t\t{'
        print '\t\t\talert("Starting date must be before ending date.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        print
        print '\t\treturn true;'
        print '\t}\n'


    def printValidateStationList(self):
        print '\tfunction validateStationList(madForm)'
        print '\t{'
        print '\t\tvar isSelected = false;'
        print '\t\tfor( var i=0; i<madForm.stationList.options.length; i++)'
        print '\t\t{'
        print '\t\t\tif (madForm.stationList.options[i].selected)'
        print '\t\t\t{'
        print '\t\t\t\tisSelected = true;'
        print '\t\t\t}'
        print '\t\t}'
        print '\t\tif (!isSelected)'
        print '\t\t{'
        print '\t\t\talert("You must select at least one instrument.");'
        print '\t\t\treturn false;'
        print '\t\t}'
        
        print '\t\treturn true;'
        print '\t}\n'

        
    def printTypeChange(self):
        print '\tfunction typeChange(madForm)'
        print '\t{'
        print '\t\tif (!validateFilter(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tvar sites = 0;'
        print '\t\tif (madForm.sites[1].checked)'
        print '\t\t\tsites = 1;'
        print
        print '\t\tvar title_text = document.getElementById("main_title");'
        print '\t\tif (sites == 1)'
        print '\t\t\ttitle_text.innerHTML = "Madrigal local experiment selector";'
        print '\t\telse'
        print '\t\t\ttitle_text.innerHTML = "Madrigal global experiment selector";'
        print
        print '\t\tvar instSelectArr = new Array();'
        print '\t\tfor(var count = 0; count < madForm.stationList.options.length; count++)'
        print '\t\t{'
        print '\t\t\tif (madForm.stationList[count].selected)'
        print '\t\t\tinstSelectArr[instSelectArr.length] = parseInt(madForm.stationList.options[count].value);'
        print '\t\t}'
        print
        print '\t\tvar catSelectArr = new Array();'
        print '\t\tfor(var count = 0; count < madForm.categoryList.options.length; count++)'
        print '\t\t{'
        print '\t\t\tif (madForm.categoryList[count].selected)'
        print '\t\t\tcatSelectArr[catSelectArr.length] = parseInt(madForm.categoryList.options[count].value);'
        print '\t\t}'
        print
        print '\t\tmadForm.categoryList.options.length = 0;'
        print
        print '\t\tfor(var count = 0; count < categoryArr.length; count++)'
        print '\t\t{'
        print '\t\t\tif (sites == 1 && categoryArr[count].globalOnly)'
        print '\t\t\t\tcontinue;'
        print '\t\t\t//check if this category was selected'
        print '\t\t\tvar isSelected = false;'
        print '\t\t\tfor (var catCount=0; catCount<catSelectArr.length; catCount++)'
        print '\t\t\t{'
        print '\t\t\t\tif (categoryArr[count].catId == catSelectArr[catCount])'
        print '\t\t\t\t\tisSelected = true;'
        print '\t\t\t}'
        print '\t\t\tif (isSelected)'
        print '\t\t\t\tnewOpt = new Option(categoryArr[count].name, categoryArr[count].catId, false, true);'
        print '\t\t\telse'
        print '\t\t\t\tnewOpt = new Option(categoryArr[count].name, categoryArr[count].catId, false, false);'
        print '\t\t\tmadForm.categoryList.options[madForm.categoryList.options.length] = newOpt;'
        print '\t\t}'
        print
        print '\t\tmadForm.stationList.options.length = 0;'
        print '\t\tfor(var count = 0; count < instArr.length; count++)'
        print '\t\t{'
        print '\t\t\tif (sites == 1 && instArr[count].local_sy == 0 && instArr[count].instId > 0)'
        print '\t\t\t\tcontinue;'
        print '\t\t\t//check if this instrument has right category'
        print '\t\t\tvar isOkay = false;'
        print '\t\t\tfor (var catCount=0; catCount<catSelectArr.length; catCount++)'
        print '\t\t\t{'
        print '\t\t\t\tif (instArr[count].catId == catSelectArr[catCount] || catSelectArr[catCount] == 0)'
        print '\t\t\t\t\tisOkay = true;'
        print '\t\t\t}'
        print '\t\t\tif (!isOkay)'
        print '\t\t\t\tcontinue;'
        print '\t\t\t//check if this instrument was selected'
        print '\t\t\tvar isSelected = false;'
        print '\t\t\tfor (var instCount=0; instCount<instSelectArr.length; instCount++)'
        print '\t\t\t{'
        print '\t\t\t\tif (instArr[count].instId == instSelectArr[instCount])'
        print '\t\t\t\t\tisSelected = true;'
        print '\t\t\t}'
        print '\t\t\tif (sites == 1)'
        print '\t\t\t{'
        print '\t\t\t\tvar startYear = instArr[count].local_sy;'
        print '\t\t\t\tvar endYear = instArr[count].local_ey;'
        print '\t\t\t}'
        print '\t\t\telse'
        print '\t\t\t{'
        print '\t\t\t\tvar startYear = instArr[count].global_sy;'
        print '\t\t\t\tvar endYear = instArr[count].global_ey;'
        print '\t\t\t}'
        print '\t\t\tif(instArr[count].instId != 0)'
        print '\t\t\t\tvar name = instArr[count].name + " [" + startYear.toString() + "-" + endYear.toString() + "]";'
        print '\t\t\telse'
        print '\t\t\t\tvar name = instArr[count].name;'
        print '\t\t\tif (isSelected)'
        print '\t\t\t\tnewOpt = new Option(name, instArr[count].instId, false, true);'
        print '\t\t\telse'
        print '\t\t\t\tnewOpt = new Option(name, instArr[count].instId, false, false);'
        print '\t\t\tmadForm.stationList.options[madForm.stationList.options.length] = newOpt;'
        print '\t\t}'
        print '\t}\n'


    def printInventoryCall(self):
        print '\tfunction inventory(madForm)'
        print '\t{'
        print '\t\tif (!validateFilter(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tif (!validateStationList(madForm))'
        print '\t\t{'
        print '\t\t\treturn false;'
        print '\t\t}'
        print '\t\tmadForm.state.value = "inventory"'
        print '\t\tmadForm.action="madInvent.cgi"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}\n'
        

    def printRedirectListExperiments(self):
        expStr = ''
        for inst in self.stationList:
            expStr += '&stationName=%i' % (inst)
                
        print '\tfunction redirectListExperiments()'
        print '\t{'
        print '\t\twindow.location.href="listExperiments.py?callingpage=madInvent.cgi%s"' % (expStr)
        print '\t}'


    def printBody(self):
        print '<div align=center>'
        if self.sites == 0:
            print '<h1 id="main_title">Madrigal global experiment selector</h1>'
        else:
            print '<h1 id="main_title">Madrigal local experiment selector</h1>'
        print '<table width="80%%" border="1"><tr><td>Return to <a href="/%s">Madrigal homepage</a></td>' % \
              self.madDBObj.getRelativeTopLevel()
        print '<td><a href="/%s/wt_browseExp.html">Tutorial</a> on this page</td>' % \
              self.madDBObj.getRelativeTopLevel()
        print '<td>Return to <a href="accessData.cgi">access data</a> page</td></tr></table></div>'
        if self.state != 'inventory':
            self.printSelect()
        else:
            self.printInventoryPage()


    def printSelect(self):
        # print the page to select experiments
        print '<form name="invForm" action="madInvent.cgi" method=post>'
        # set up hidden form elements
        print '<input type=hidden name="state" value="%s">' % (self.state)
        print '<table cellpadding=20>'
        print '<tr>'
        print '<td valign=top width=60%>'
        print '<h3>Select instrument(s)</h3>'
        # select catagories
        print '<p>Choose what instrument type(s) to select from:</p>'
        print '<select name="categoryList" multiple size=6 onChange="typeChange(this.form)">'
        self.printCategories()
        print '</select>'
        # select instruments
        print '<p>Choose instrument(s): <i>(Year range shows data available)</i><p>'
        print '<select name="stationList" multiple size=10>'
        self.printInstruments()
        print '</select>'
        print '</td>'
        print '<td>'
        print '<table cellpadding=5>'
        print '<tr>'
        print '<th>Show Experiments at</th>'
        print '</tr>'
        print '<tr>'
        print '<td valign=top>'
        if self.sites == 0:
            print '<input type=radio name="sites" value="0" onClick="typeChange(this.form)" checked>'
        else:
            print '<input type=radio name="sites" value="0" onClick="typeChange(this.form)">'
        print 'All Madrigal Sites'
        print '</td>'
        print '</tr>'
        print '<tr>'
        print '<td valign=top>'
        if self.sites == 1:
            print '<input type=radio name="sites" value="1" onClick="typeChange(this.form)" checked>'
        else:
            print '<input type=radio name="sites" value="1" onClick="typeChange(this.form)">'
        print 'This Madrigal Site (%s)' % (str(self.siteName))
        print '</td>'
        print '</tr>'
        print '<tr>'
        print '<th>File Selection</th>'
        print '</tr>'
        print '<tr>'
        print '<td valign=top>'
        if self.displayLevel == 0:
            print '<input type=radio name="displayLevel" value="0" checked>'
        else:
            print '<input type=radio name="displayLevel" value="0">'
        print 'Show Default Files'
        print '</td>'
        print '</tr>'
        print '<tr>'
        print '<td valign=top>'
        if self.displayLevel == 1:
            print '<input type=radio name="displayLevel" value="1" checked>'
        else:
            print '<input type=radio name="displayLevel" value="1">'
        print 'Show All Files'
        print '</td>'
        print '</tr>'
        print '<tr>'
        print '<td valign=top>'
        print '<h3>Select date range</h3>'
        print '<input name="sd" value="%i" size=2 maxlength=2>' % (self.sd)
        print '<input name="sm" value="%i" size=2 maxlength=2>' % (self.sm)
        print '<input name="sy" value="%i" size=4 maxlength=4>' % (self.sy)
        print 'Start Day, Month, Year<br>'
        print '<input name="ed" value="%i" size=2 maxlength=2>' % (self.ed)
        print '<input name="em" value="%i" size=2 maxlength=2>' % (self.em)
        print '<input name="ey" value="%i" size=4 maxlength=4>' % (self.ey)
        print 'End Day, Month, Year<br>'
        print '</td>'
        print '</tr>'
        print '<tr>'
        print '<td valign=top>'
        print '<input type=button value="List selected experiments" onClick =inventory(this.form)>'
        
        print '</td>'
        print '</tr>'
        print '</table>'

        print '</table>'
        

        print '</form>'

    def printInventoryPage(self):
        """print out experiment inventory if any found, otherwise
        redirect to listExperiments.py
        """
        atLeastOneFound = False

        startTime = datetime.datetime(self.sy, self.sm, self.sd, 0,0,0)
        endTime = datetime.datetime(self.ey, self.em, self.ed, 23,59,59)
        
        print '<pre>'
        print '    Site         Exp start            Exp end          Instrument name    Inst Id   Exp name'
        print '    ----         ---------            -------          ---------------    -------   --------'
        # sort experiments by date, then by site
        self.expObj.sortByDateSite()
        for i in range(self.expObj.getExpCount()):
            kinst = self.expObj.getKinstByPosition(i)
            # instrument filter
            if 0 not in self.stationList:
                if kinst not in self.stationList:
                    continue
            # date select
            st = self.expObj.getExpStartDateTimeByPosition(i)
            et = self.expObj.getExpEndDateTimeByPosition(i)
            thisStartTime = datetime.datetime(st[0],st[1],st[2],st[3],st[4],st[5])
            if thisStartTime > endTime:
                continue
            thisEndTime = datetime.datetime(et[0],et[1],et[2],et[3],et[4],et[5])
            if thisEndTime < startTime:
                continue

            # check access
            thisSite = self.expObj.getExpSiteIdByPosition(i)
            thisSecurity = self.expObj.getSecurityByPosition(i)
            if thisSite == self.siteId:
                # local experiment
                if thisSecurity != 0 and not self.webObj.isTrusted():
                    continue
                if thisSecurity in (-1,2,3):
                    continue
            else:
                # remote experiment
                if thisSecurity != 0:
                    continue

            # exp okay
            atLeastOneFound = True
            startTimeStr = thisStartTime.strftime('%Y-%m-%d %H:%M:%S')
            endTimeStr = thisEndTime.strftime('%Y-%m-%d %H:%M:%S')
            siteId = self.expObj.getExpSiteIdByPosition(i)
            siteStr = self.madSiteObj.getSiteName(siteId)
            if len(siteStr) < 5:
                siteStr += (7-len(siteStr)) * ' '
            else:
                siteStr = siteStr[:5] + 2*' '
            instStr = str(self.madInstObj.getInstrumentName(kinst))
            if len(instStr) < 20:
                instStr += (22-len(instStr)) * ' '
            else:
                instStr = instStr[:20] + 2*' '

            expNameStr = str(self.expObj.getExpNameByPosition(i))

            # build link
            url = self.expObj.getExpUrlByPosition(i)
            url = url.replace('madtoc/', 'madExperiment.cgi?exp=')
            url += '&displayLevel=%i&expTitle=%s' % (self.displayLevel,
                                                     self.madWebObj.getCgiString(expNameStr))
            linkStr = '<a href="%s">GO</a>' % (url)
            print '%s  %s%s  %s  %s%5i   %s' % (linkStr, siteStr, startTimeStr, endTimeStr,
                                              instStr, kinst, expNameStr)
            
        print '</pre>'

        # if no experiments found, give user option to print all experiments.
        if not atLeastOneFound:
            # provide user feedback about their query
            instStr = ''
            for thisKinst in self.stationList:
                if thisKinst == 0:
                    text = 'All instruments'
                else:
                    text = self.madInstObj.getInstrumentName(thisKinst)
                if thisKinst == self.stationList[-1]:
                    instStr += '%s' % (text)
                else:
                    instStr += '%s, ' % (text)
            startTimeStr = startTime.strftime('%Y-%m-%d')
            endTimeStr = endTime.strftime('%Y-%m-%d')
            if self.sites == 1:
                extraText = 'on %s' % (self.siteName)
            else:
                extraText = 'on all Madrigal servers'
            print '<p>No experiments found %s for %s and date range %s to %s. ' % (extraText,
                                                                                   instStr,
                                                                                   startTimeStr,
                                                                                   endTimeStr)
            print 'Hit back button, or click on <b>Show All Experiments</b> to see all the experiments '
            print 'on all Madrigal servers for the instrument(s) you selected.</p>'
            print '<center><input type=button value="Show All Experiments" onClick ="redirectListExperiments()"></center>'

        return


    def printInstruments(self):
        """printInstruments prints a select list with all instruments that have data,
        along with year range of data"""

        # print all instruments first
        if 0 in self.stationList:
            print '<option value="0" selected>All Instuments'
        else:
            print '<option value="0">All Instuments'

        for inst in self.orderInstList:
            if inst[5] in self.stationList:
                selectedStr = ' selected'
            else:
                selectedStr = ''
            labelStr = '%s&nbsp;&nbsp;&nbsp;[%04i-%04i]' % (inst[0], inst[3], inst[4])
            print '<option value="%i"%s>%s' % (inst[5], selectedStr, labelStr)


    def printCategories(self):
        """printCategories prints a select list with all instrument categories, values - categoryId"""

        # print all categories first
        if 0 in self.categoryList:
            print '<option value="0" selected>All Instument Types'
        else:
            print '<option value="0">All Instument Types'
        
        catIdList = self.categoryDict.keys()
        catIdList.sort()

        for catId in catIdList:
            if catId in self.categoryList:
                print '<option value="%i" selected>%s' % (catId, self.categoryDict[catId])
            else:
                print '<option value="%i">%s' % (catId, self.categoryDict[catId])
   

    def printEndPage(self):
        print '</body></html>'



    

if __name__ == '__main__':

    # Script madInvent.cgi
    # This script only calls the init function of the class madInvent
    # All work is done by the init function
    madInvent()
