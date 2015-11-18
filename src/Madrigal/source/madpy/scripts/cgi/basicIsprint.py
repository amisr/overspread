#!PYTHONEXE

import sys, os, os.path
import traceback
import cgi, Cookie
import time, datetime



class basicIsprint:
    """basicIsprint is the class that produces the basicIsprint page.  The basicIsprint page
    simply prints the entire file with just the measured parameters
    
    Like all my python cgi scripts, basicIsprint has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.  The script state is always set in self.state.  The particular values
    allowed for basicIsprint are discussed below.

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

    The names of all form elements used by basicIsprint are listed below:

    madFilename: full path to Madrigal file to print

    instrumentName

    experimentName
    

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Dec. 4, 2007

    $Id: basicIsprint.py,v 1.5 2008/08/28 14:40:45 brideout Exp $
    """

    # constants
    __scriptName = 'basicIsprint.py'


    def __init__(self):
        """__init__ run the entire basicIsprint script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script basicIsprint.

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

            # determine from form arguments and cookies which script state to use
            self.setScriptState()
            
            # create needed Madrigal objects
            self.createObjects()

            # output html

            #print header
            self.outputHead('Basic Ascii file print')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printTopLinks()

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

        if not self.madForm.has_key('madFilename'):
            if  self.scriptHeaders == 0:
                print "Content-Type: text/html\n"

            print '<h3> This cgi script was called without the proper arguments.</h3>' + \
                  'Since this script uses post, you cannot bookmark this page. ' + \
                  'Please contact your site administrator with any questions.'

            sys.exit(0)
            
        else:
            self.madFilename = self.madForm.getvalue('madFilename')
            self.instrumentName = self.madForm.getvalue('instrumentName')
            self.experimentName = self.madForm.getvalue('experimentName')



    def createObjects(self):

        # create a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # if madroot not set, set it now
        if os.environ.get('MAD' + 'ROOT') == None:
            os.environ['MAD' + 'ROOT'] = self.madDBObj.getMadroot()

        self.madParmObj = madrigal.data.MadrigalParameters(self.madDBObj)

        import madrigalWeb.madrigalWeb
        self.madWebObj = madrigalWeb.madrigalWeb.MadrigalData(self.madDBObj.getTopLevelUrl())

        import madrigal.ui.web
        self.madWebUIObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)


        # create a list of all parameters (ints) with error data in all those files, and also
        # simply all parameters
        import madrigal.data
        madFileObj = madrigal.data.MadrigalFile(self.madFilename, self.madDBObj)
        allParms = madFileObj.getMeasuredParmList()

        # add six time parameters first
        self.allParms = ['year', 'month', 'day', 'hour', 'min', 'sec']
        for item in allParms:
            if self.madParmObj.isAddIncrement(item):
                # skip additional increment parameters
                continue
            mnem = self.madParmObj.getParmMnemonic(item)
            self.allParms.append(mnem)

        # create a parameter string
        self.parmStr = ''
        for parm in self.allParms:
            if parm == self.allParms[-1]:
                self.parmStr += '%s' % (self.madParmObj.getParmMnemonic(parm))
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

        # get kindat description
        kindatList = madFileObj.getKindatList()
        if len(kindatList) == 0:
            self.kindatDescription = 'Unknown'
        else:
            madKindatObj = madrigal.metadata.MadrigalKindat(self.madDBObj)
            self.kindatDescription = madKindatObj.getKindatDescription(kindatList[0])


    def getHeaderStr(self):
        """getHeaderStr returns a string with parameter lables and and definitions.

        """
        spaceStr = '&nbsp;'

        import madrigal._Madrec
        retStr = ''
        mnemList = self.parmStr.split(',')

        for mnem in mnemList:
            strLength = madrigal._Madrec.madGetParWidth(self.madParmObj.getParmMnemonic(mnem))
            
            thisLabelStr = '<a href=JavaScript:popup("%s")>%s</a>' % (mnem.upper(), mnem.upper())

            # pad according to string length
            if retStr == '':
                retStr = spaceStr *2 + thisLabelStr
            else:
                if len(mnem) < strLength:
                    retStr += spaceStr * (strLength-len(mnem)) + thisLabelStr
                
        return retStr
        
	


    def outputHead(self, title):

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
        self.printPopup()
        self.printGetDesc()
        self.printMLookup()
        self.printCParamDiction()
        print '</script>'


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


    def printTopLinks(self):
        print '<h2><center>One-click file print</center></h2>'
        print '<h3>Experiment: ' + self.madWebUIObj.getSpaceString(str(self.experimentName)) + \
                  ' &nbsp;&nbsp;&nbsp;File: ' + os.path.basename(self.madFilename) + \
                  ' &nbsp;&nbsp;&nbsp;Type of data: ' + str(self.kindatDescription) + '</h3>'
        print '<center><table width="90%" border="1"><tr>'
        print '\t<td>'
        # get expName for madExperiments.cgi from fileName
        dirPath = self.madFilename.split(self.madDBObj.getMadroot() + '/experiments/')
        try:
            expName = dirPath[1]
            # remove file name at end
            expName = expName[0:expName.rfind('/')]
            # replace / in argument for tcl scripts
            expName = expName.replace('/', '%2f')
        except:
            expName = 'Unknown'
        print '\t<a href=madExperiment.cgi?exp=' + \
              expName + '&expTitle=' + \
              self.madWebUIObj.getCgiString(str(self.experimentName)).replace('%20', '+') + \
              '&displayLevel=0>Return to experiment list</a>'
        print '\t</td>'
        print '\t<td>'
        print '\tReturn to <a href=/' + self.madDBObj.getRelativeTopLevel() + '/index.html>Madrigal homepage</a>'
        print '\t</td>'
        print '\t<td>'
        print '\tReturn to <a href=accessData.cgi>access data</a> page'
        print '\t</td>'
        print '</tr>'
        print '</table></center>'


    def printBody(self):

        print'<center><p><i>Click on any parameter name for a definition</i></p></center>'
        print '<pre>'
        print self.headerStr
        thisIsprintStr = self.madWebObj.isprint(self.madFilename,
                                                self.parmStr,
                                                '',
                                                self.user_fullname,
                                                self.user_email,
                                                self.user_affiliation)

        sys.stdout.write(thisIsprintStr)
                    
                          
        print '</pre>'
        
        # print feedback link
        print '<p><hr><i>Please send any comments or suggestions to the <a href="mailto:openmadrigal-users@openmadrigal.org">' + \
              'Open Madrigal Users Mailing List.</a></i>'
        print'<p>&nbsp;</p>'
   

    def printEndPage(self):
        print '</body></html>'




if __name__ == '__main__':

    # Script basicIsprint.py
    # This script only calls the init function of the class basicIsprint
    # All work is done by the init function
    basicIsprint()
