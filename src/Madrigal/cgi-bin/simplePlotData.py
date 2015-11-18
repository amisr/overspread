#!/Users/mnicolls/Documents/Work/Madrigal/bin/python

import sys, os, os.path
import traceback
import cgi, Cookie
import time
import types



class simplePlotData:
    """simplePlotData is the class that produces the simplePlotData page.
    
    Like all my python cgi scripts, simplePlotData has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for simplePlotData are discussed below.

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

    The names of all form elements used by simplePlotData are listed below:

    selectInstrument:	local instrument selected - id is instrument id

    selectExperiments:  list of experiment ids selected

    

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Nov. 14, 2005

    $Id: simplePlotData.py,v 1.12 2009/04/22 19:05:04 brideout Exp $
    """

    # constants
    __scriptName = 'simplePlotData.py'


    def __init__(self):
        """__init__ run the entire simplePlotData script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script simplePlotData.

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
            self.outputHead('Simple Plot Data')

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
            self.selectYAxis = 'Altitude'
            try:
                self.selectYAxis = self.madForm.getvalue('selectYAxis')
            except:
                self.selectYAxis = 'Altitude'


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
	
        # create a MadrigalExperiment object
        self.madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)

        # create a list of list of all default files, one list for each date
        import madrigalWeb.madrigalWeb
        self.madWebObj = madrigalWeb.madrigalWeb.MadrigalData(self.madDBObj.getTopLevelUrl())
        
        # object to examine parameters
        self.madParmObj = madrigal.data.MadrigalParameters(self.madDBObj)

        # create self.allExpFiles and self.linkDict
        # self.linkDict has key = date as YYYY-MM-DD, value = list of (tile, url) tuples.
        self.allExpFiles = []
        self.linkDict = {}
        for exp in self.experimentList:
            expIdList = self.madWebObj.getExperiments(self.instrumentKinst,
                                                      int(exp[0:4]),
                                                      int(exp[5:7]),
                                                      int(exp[8:]),
                                                      0,
                                                      0,
                                                      1,
                                                      int(exp[0:4]),
                                                      int(exp[5:7]),
                                                      int(exp[8:]),
                                                      23,
                                                      59,
                                                      59)

            self.allExpFiles.append(self.madWebObj.getExperimentFiles(expIdList[0].id))

            self.linkDict[exp] = self.madExpObj.getExpLinksByExpId(expIdList[0].id)

        # find out if there are links to show here
        self.hasLinks = False
        for item in self.linkDict.keys():
            if len(self.linkDict[item]) > 0:
                self.hasLinks = True
                break


        # create a list of all parameters (ints) with error data in all those files, and also
        # simply all parameters
        import madrigal.data
        self.allParms = []
        self.allParmsWithErrors = []
        requiredParms = ('GDALT', 'RANGE', 'GDLAT', 'GLON', 'PACLAT', 'PACLON')
        self.derivableParms = []
        measParmList = []
        derivedParmList = []
        allParmList = []
        sureParmList = []
        for theseFiles in self.allExpFiles:
            for thisFile in theseFiles:
                madFileObj = madrigal.data.MadrigalFile(thisFile.name, self.madDBObj)
                theseParms = madFileObj.getMeasuredParmList()
                madFileObj.getMeasDervBothParmLists(requiredParms, measParmList, derivedParmList,
                                                    allParmList, sureParmList)
                for parm in theseParms:
                    if parm < 1:
                        continue
                    if self.madParmObj.isAddIncrement(parm):
                        continue
                    if parm not in self.allParms:
                        self.allParms.append(parm)
                    if (parm * -1) in theseParms:
                        if parm not in self.allParmsWithErrors:
                            self.allParmsWithErrors.append(parm)

                # see which of the yaxis parameters can be derived
                for parm in allParmList:
                    if parm in requiredParms:
                        if parm not in self.derivableParms:
                            self.derivableParms.append(parm)
                            
        self.allParms.sort()
        self.allParmsWithErrors.sort()

        # see which list we should use
        if len(self.allParmsWithErrors) > 0:
            self.allParms = self.allParmsWithErrors

        # create the list of (parameter description, mnemonic)
        self.parameterList = []

        for parm in self.allParms:
            self.parameterList.append((self.madParmObj.getSimpleParmDescription(parm),
                                       self.madParmObj.getParmMnemonic(parm)))


    def outputHead(self, title):

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


    def printJavaScript(self):

        print '<script language = "JavaScript">'
        self.printDiffInst()
        self.printDiffExp()
        self.printPlotData()
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


    def printPlotData(self):

        print '\tfunction plotData(madForm)'
        print '\t{'
        print '\t\tmadForm.action="simplePcolor.py"'
        print '\t\tmadForm.target=""'
        print '\t\tmadForm.submit()'
        print '\t}'


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
        print'<table width="100%"  border="1" class="nav_cgi">'
        print'  <tr>'
        print'    <td><div align="center">Simple Madrigal data access - select parameter and y axis for plotting...</div></td>'
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
        print'          <input name="timeButton" type="button" id="timeButton3" value="Choose different date" onClick="diffExp(this.form)">'
        print'      </p></td>'
        print'  </tr>'
        print'</table>'
        
        if not self.hasLinks:
            print'<h3 align="center">Create a plot</h3>'
        else:
            print'<p></p>'
            
        print'<div align="center">'
        
        if self.hasLinks:
            print'    <table width="95%"  border="3" bordercolor="black" cellpadding="1">'
            print'        <tr><td>'
            
        print'    <table  border="0" cellpadding="1">'

        if self.hasLinks:
            print'      <tr><td colspan="2" align="center"><h4>Create a new plot...</h4></td></tr>'
            print'       <tr><td colspan="2"><p>&nbsp;</p></td></tr>'

        print'      <tr>'
        print'        <td align="right"><strong>Choose parameter to plot:</strong>&nbsp; </td>'
        print'        <td align="left"><select name="selectParm" id="select">'
        print'          <option value="%s" selected>%s</option>' % (self.parameterList[0][1], self.parameterList[0][0])
        
        for parm in self.parameterList[1:]:
            print'          <option value="%s">%s</option>' % (parm[1], parm[0])
            
        print'        </select></td>'
        print'      </tr>'
        print'      <tr>'
        print'        <td align="right" valign="middle"><p>&nbsp;'
        print'          </p>'
        print'        <p><strong>Select y axis:</strong> '         
        print'          <select name="selectYAxis" id="select3">'
        
        if 'GDALT' in self.derivableParms:
            print'            <option value="Altitude" selected>Altitude</option>'
            print'            <option value="Scatter">Simple scatter plot</option>'
        else:
            print'            <option value="Scatter" selected>Simple scatter plot</option>'
            
        if 'RANGE' in self.derivableParms:
            print'            <option value="Range">Range</option>'

        """if 'GDLAT' in self.derivableParms:
            print'            <option value="Latitude">Latitude</option>'

        if 'GLON' in self.derivableParms:
            print'            <option value="Longitude">Longitude</option>'

        if 'PACLAT' in self.derivableParms:
            print'            <option value="PACE_Magnetic_Latitude">PACE Magnetic latitude</option>'
        
        if 'PACLON' in self.derivableParms:
            print'            <option value="PACE_Magnetic_Longitude">PACE Magnetic longitude</option>'"""
                
        print'          </select>'
        print'</p></td>'
        
        if (self.selectYAxis == 'Altitude' or self.selectYAxis == None or self.selectYAxis == 'Range') and ('GDALT' in self.derivableParms or 'RANGE' in self.derivableParms):
            print'        <td><img id="plotImage" src="/%s/icons/pcolor.jpeg" width="369" height="260"></td>' % (self.madDBObj.getRelativeTopLevel())
        else:
            print'        <td><img id="plotImage" src="/%s/icons/scatter.jpeg" width="369" height="260"></td>' % (self.madDBObj.getRelativeTopLevel())

        print'      </tr>'
        print'      <tr>'
        print'        <td>&nbsp;</td>'
        print'        <td align="center">Time</td>'
        print'      </tr>'
        print'      <tr><td colspan="2" align="center">'
        print'         <input type="button" name="plotDataButton" value="Plot data" onClick="plotData(this.form)">'
        print'      </td></tr>'
        print'    </table>' # end of plot new graph table

        if self.hasLinks:
            print'      </td>'
            print'      <td valign="top">'
            # existing links table
            print'    <table  border="0" cellpadding="1">'
            print'        <tr><td valign="top" align="left"><h4>or, view existing plots and descriptions:</h4></td></tr>'
            print '       <tr><td><p>&nbsp;</p></td></tr>'
            print '       <tr><td valign="top" valign="center"><ul>'
            for index in range(len(self.experimentList)-1,-1,-1):
                print'        <li>%s' % (self.experimentList[index])
                linkList = self.linkDict[self.experimentList[index]]
                if len(linkList) > 0:
                    print '        <ul>'
                    for item in linkList:
                        print'           <li><a href="%s">%s</a></li>' % (item[1], item[0])
                    print '        </ul>'
                print'        </li>'
            print'         </ul></td></tr></table>' # end existing data table
            print'     </table>' # end outer table
        
        print'  </div>'
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


   
   
        
            

if __name__ == '__main__':

    # Script simplePlotData.py
    # This script only calls the init function of the class simplePlotData
    # All work is done by the init function
    simplePlotData()
