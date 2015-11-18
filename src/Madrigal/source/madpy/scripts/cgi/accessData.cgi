#!PYTHONEXE

#$Id: accessData.cgi,v 1.9 2008/07/25 18:57:20 brideout Exp $


import sys, os, traceback
import cgi, Cookie
import time, datetime
import os.path



class accessData:
    """accessData is the class that produces the accessData page.

    This python script replaces the tcl script accessData.cgi.
    
    accessData has the following structure:  the entire cgi is contained in one class, with a
    main function at the end which serves simply to call the __init__  function of the class.  This
    __init__ function is responsible for calling all other class methods. It is made up of a single
    try block, with the purpose of reporting all exceptions in well-formatted html to both the user
    and the administrator. The __init__ function first makes sure the pythonlib can be found.  It
    then calls setScriptState to determine from any cgi arguments what the script is supposed to do.
    The script state is always set in self.state.  The particular values allowed for accessData
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

    List of cgi arguments that this script uses: None
  

    Change history:

    Written by "Bill Rideout":mailto:brideout@haystack.mit.edu  Sep 9, 2005
    """

    # constants
    __scriptName = 'accessData.cgi'


   
    def __init__(self):
        """__init__ run the entire accessData script.  All other functions are private and called by __init__.

        Inputs: None
        
        Returns: void

        Affects: Ouputs cgi script accessData.

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

            # create needed Madrigal objects
            self.createObjects()
            
            self.setScriptState()
            
            # output html

            #print header
            self.outputHead('Madrigal Database Access')

            #print body tag
            print self.madDBObj.getHtmlStyle()

            self.printHeading()

            if self.user_fullname == None or self.madForm.has_key('modify'):
                self.printGetUserInfo()
            else:
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

        # get name, email, affiliation from cookie
        self.cookie = Cookie.SimpleCookie()
        if os.environ.has_key('HTTP_COOKIE'):
            self.hasCookie = True
            self.cookie.load(os.environ['HTTP_COOKIE'])
            try:
                self.user_fullname = self.cookie["user_fullname"].value
                self.user_email = self.cookie["user_email"].value
                self.user_affiliation = self.cookie["user_affiliation"].value
            except:
                self.user_fullname = None
                self.user_email = None
                self.user_affiliation = None
                    

        else:
            # no cookie exists
            self.user_fullname = None
            self.hasCookie = False

        # now see if cookie needs to be written
        if self.madForm.has_key('user_fullname') and \
           self.madForm.has_key('user_email') and \
           self.madForm.has_key('user_affiliation'):
            self.writeCookie = True
            self.user_fullname = self.madForm.getvalue('user_fullname')
            self.cookie['user_fullname'] = self.user_fullname
            self.cookie['user_fullname']['expires'] = self.madWebObj.getCookieDateOneYearAhead()
            self.user_email =  self.madForm.getvalue('user_email')
            self.cookie['user_email'] = self.user_email
            self.cookie['user_email']['expires'] = self.madWebObj.getCookieDateOneYearAhead()
            self.user_affiliation =  self.madForm.getvalue('user_affiliation')
            self.cookie['user_affiliation'] = self.user_affiliation
            self.cookie['user_affiliation']['expires'] = self.madWebObj.getCookieDateOneYearAhead()
        else:
            self.writeCookie = False
  

    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # create a MadrigalWeb object
        import madrigal.ui.web
        self.madWebObj = madrigal.ui.web.MadrigalWeb(self.madDBObj)

        # create object for MadrigalSite
        self.madSiteObj = madrigal.metadata.MadrigalSite(self.madDBObj)
        self.siteName = self.madSiteObj.getSiteName(self.madDBObj.getSiteID())
        self.siteId = self.madDBObj.getSiteID()
        
    def outputHead(self, title):

        print "Content-Type: text/html"
        if self.writeCookie:
            print self.cookie
        print                               # blank line
        self.scriptHeaders = 1
        print '<html>'
        print '<head>'
        print '\t<title>' + title + '</title>'
        self.printJavaScript()
        print '</head>'
    

    def printJavaScript(self):
        
        print '<script language = "JavaScript">'
        self.printNewSite()
        print '</script>'


    def printNewSite(self):
        print '\tfunction gotoNewSite(madForm)'
        print '\t{'
        for site in self.madSiteObj.getSiteList():
            url = 'http://' + os.path.join(self.madSiteObj.getSiteServer(site[0]),
                                           self.madSiteObj.getSiteRelativeCGI(site[0]),
                                           'accessData.cgi')
            print '\t\tif (parseInt(madForm.newSite.value) == %i)' % (site[0])
            print '\t\t\twindow.location.href="%s";' % (url)
        print '\t}'


        
    def printHeading(self):

        print '<table cellpadding="15"><tr>'
        print '<form>'
        print '<td width="25%%">Back to <a href="/%s">%s homepage</a></td>' % \
              (self.madDBObj.getRelativeTopLevel(), self.siteName)
        print '<td align="center" width="50%"><h1>Access Madrigal Data</h1></td>'
        print '<td align="right" width="25%">Go to a different Madrigal site: '
        print '<select name="newSite" onChange="gotoNewSite(this.form)">'
        for site in self.madSiteObj.getSiteList():
            if site[0] == self.siteId:
                print '<option value="%i" selected>%s</option>' % (site[0], site[1])
            else:
                print '<option value="%i">%s</option>' % (site[0], site[1])
        print '</select></td>'
        print '</tr></table>'
        
       
    def printGetUserInfo(self):

        print '<form method=get action="accessData.cgi" enctype="application/x-www-form-urlencoded">'
        print '<span style="font-weight: bold;">Please enter your name, '
        print 'email, and affliation below.&nbsp; This information will be saved as a '
        print 'cookie on your browser, so these fields need only be filled in '
        print 'once.&nbsp; No password is required.<br><br></span>'

        print '<table style="text-align: left; width: 620px; height: 89px;" border="1"'
        print 'cellpadding="2" cellspacing="2"> '
        print '<tbody>'
        print '\t<tr>'
        print '\t\t<td><span style="font-weight: bold;">Full name:</span></td>'
        print '\t\t<td style="vertical-align: top;"><input name="user_fullname" size="50"></td>'
        print '\t</tr><tr>'
        print '\t\t<td style="vertical-align: top;"><span style="font-weight: bold;">Email:</span></td>'
        print '\t\t<td style="vertical-align: top;"><input name="user_email" size="50"></td>'
        print '\t</tr><tr>'
        print '\t\t<td style="vertical-align: top;"><span style="font-weight: bold;">Affiliation '
        print '\t\t(use None if individual):</span></td>'
        print '\t\t<td style="vertical-align: top;"><input name="user_affiliation" size="50"></td>'
        print '\t</tr>'
        print '</tbody></table><br>'

        print '<div align="center"><input value="Submit" type="submit"></div><br>'
        print '</form>'
        
        



    def printBody(self):


        # create query
        query = '?user_fullname=%s&user_email=%s&user_affiliation=%s' % (self.user_fullname,
                                                                         self.user_email,
                                                                         self.user_affiliation)

        # create table of access methods
        print '<table cellpadding="15"><tr>'
        print '<td valign="top">'
        print """<h2><a href="/%s/simpleChooseInstrument.py">
                 Simple Madrigal Data Access</a></h2>
                 <p>This link allows you
                 to print and plot local %s Madrigal data easily.
                 Use the other three Madrigal interfaces to access
                 more powerful capabilities, such as displaying derived parameters
                 or searching over
                 all Madrigal servers. Click <a href="/%s/wt_simple.html">here</a> for a
                 tutorial.</p>
        """ % (self.madDBObj.getRelativeCGI(), self.siteName, self.madDBObj.getRelativeTopLevel())

        
        print '</td><td valign="top">'

        print """<h2><a href="/%s/madInvent.cgi">Browse for Individual Madrigal Experiments</a></h2>
                 <p>Use this link to search available
                    experiments.  You can search
                    either <i>all</i> Madrigal databases, or just the local %s database. You can choose
                    which parameters to print, including derived parameters, and 
                    can filter the data using any parameter. Click
                    <a href="/%s/wt_browseExp.html">here</a> for a
                    tutorial.</p>
        """ % (self.madDBObj.getRelativeCGI(), self.siteName, self.madDBObj.getRelativeTopLevel())

        print '</td></tr><tr><td valign="top">'

        print """<h2><a href="/%s/madSearch">
                 Global Madrigal Database Report</a></h2>
                 <p>This link allows you to generate a report on
                    multiple local %s experiments at once.  Experiments can be
                    filtered in a number of ways.  Data from the local Madrigal database
		    matching your criteria will be returned in
                    a single report. Click <a href="/%s/wt_global.html">here</a> for a
                    tutorial.</p>
        """ % (self.madDBObj.getRelativeCGI(), self.siteName, self.madDBObj.getRelativeTopLevel())

        print '</td><td valign="top">'

        print """<h2><a href="/%s/plotInstrumentsSelect.py">
                 Plot Data from Instruments</a></h2>
                 <p>This link allows you to create new plots from one or more instruments
                    and/or Madrigal experiments 
                    versus time on a single web page. The data comes from
                    the local %s database. Click
                    <a href="/%s/wt_plotData.html">here</a> for a
                    tutorial.</p>
        """ % (self.madDBObj.getRelativeCGI(), self.siteName, self.madDBObj.getRelativeTopLevel())

        print '</td></tr></table></form>'

        # print modify form
        print """<table style="width: 100%%; text-align: left;" border="1" cellpadding="2" cellspacing="2">
                 <tbody><tr>
                 <td style="vertical-align: top;">User:&nbsp; %s<br></td>
                 <td style="vertical-align: top;">Email:&nbsp; %s<br></td>
                 <td style="vertical-align: top;">Affliliation:&nbsp; %s<br></td>
                 <td style="vertical-align: top;"><a href="accessData.cgi?modify=true">
                   Click here to modify</a>
                 </td></tr></tbody></table>
        """ % (self.user_fullname, self.user_email, self.user_affiliation)

        print """<blockquote>
  <p><em>Use of the Madrigal Database is generally subject to the
    <a href="http://cedarweb.hao.ucar.edu/catalog/Rules.html">
    CEDAR Database Rules-of-the-Road</a>.
    Prior permission to access the data is not required.
    However, the user is required to establish early
    contact with any organization whose data are
    involved in the project to discuss the intended
    usage. Data are often subject to limitations which
    are not immediately evident to new users.  Before
    they are formally submitted, draft copies of all
    reports and publications must be sent to the contact
    scientist at all data-supplying organizations along
    with an offer of co-authorship to scientists who
    have provided data. This offer may be declined.  The
    Database and the organizations that contributed data
    must be acknowledged in all reports and
    publications, and whenever this data is
    made available through another database. If you have any questions about
    appropriate use of these data, contact %s</em></p>
        """ % (self.madDBObj.getContactLink())



            

        
    
    def printEndPage(self):

        print '<p><em>Please send any comments or suggestions about any part of the '
        print 'Madrigal web site to the <a href="mailto:openmadrigal-users@openmadrigal.org">'
        print 'Open Madrigal Users Mailing List</a>.</p></em></blockquote>'
        print '</body></html>'       # end body, and html 





        return

if __name__ == '__main__':

    # Script accessData.cgi
    # This script only calls the init function of the class accessData
    # All work is done by the init function
    accessData()
