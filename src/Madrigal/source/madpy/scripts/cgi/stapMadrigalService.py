#!PYTHONEXE

import sys, os, os.path
import traceback
import cgi, Cookie
import time, datetime
import types



class stapMadrigalService:
    """stapMadrigalService is the class that provides the Simple time access protocol service.
    
    Like all my python cgi scripts, stapMadrigalService has the following structure:  the entire cgi is
    contained in one class, with a main function at the end which serves simply to call the __init__
    function of the class.  This __init__ function is responsible for calling all other class methods.
    It is made up of a single try block, with the purpose of reporting all exceptions in well-formatted
    html to both the user and the administrator. The __init__ function first makes sure the pythonlib
    can be found.  It then calls setScriptState to determine from any cgi arguments and cookies what the
    script is supposed to do.

    This script serves to expose Madrigal to Virtual Observatories via the Simple Time Access
    Protocol, as described in http://www.mssl.ucl.ac.uk/~eca/stap.html.


    The names of all form elements used by stapMadrigalService are listed below:

    START:  start time of the interval. This should be specified in IS0-8601 format,
            eg. '2006-02-08T00:00:00'. Times are expressed in UTC.

    END:    end time of the interval, in the same format as START

    FORMAT: See http://www.mssl.ucl.ac.uk/~eca/stap.html (optional)

    INSTRUMENT_ID = ID of instrument to be retrieved (optional)

    

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 31, 2007

    $Id: stapMadrigalService.py,v 1.6 2008/07/25 18:48:45 brideout Exp $
    """

    # constants
    __scriptName = 'stapMadrigalService.py'


    def __init__(self):
        """__init__ run the entire stapMadrigalService script.  All other functions are private and called by __init__.

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

            # output xml
            print self.outputXml()

            

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

        if not self.madForm.has_key('START'):
            if  self.scriptHeaders == 0:
                print "Content-Type: text/html\n"

            print '<h3> This cgi script was called without the proper arguments.</h3>' + \
                  'Since this script uses post, you cannot bookmark this page. ' + \
                  'Please contact your site administrator with any questions.'

            sys.exit(0)
            
        else:
            self.START = self.madForm.getvalue('START')
            self.END = self.madForm.getvalue('END')
            self.FORMAT = self.madForm.getvalue('FORMAT')
            self.INSTRUMENT_ID = self.madForm.getvalue('INSTRUMENT_ID')

        # convert times to datetime.datetime
        startTime = time.strptime(self.START, '%Y-%m-%dT%H:%M:%S')
        endTime = time.strptime(self.END, '%Y-%m-%dT%H:%M:%S')
        self.startTime = datetime.datetime(*(startTime[0:6]))
        self.endTime = datetime.datetime(*(endTime[0:6]))

        if self.endTime < self.startTime:
            raise ValueError, 'END %s cannot be before START %s' % (self.END, self.START)
        


    def createObjects(self):

        # all states require a MadrigalDB object
        import madrigal.metadata
        self.madDBObj = madrigal.metadata.MadrigalDB()

        # find out the format the user wants
        # possibilities are TIME_SERIES-ASCII, TIME_SERIES-VOT (default), or NONE
        if self.FORMAT != None:
            if self.FORMAT.upper().find('ALL') != -1:
                self.outputFormat = 'TIME_SERIES-VOT'
            elif self.FORMAT.upper().find('TIME_SERIES-VOT') != -1:
                self.outputFormat = 'TIME_SERIES-VOT'
            elif self.FORMAT.upper().find('TIME_SERIES-ASCII') != -1:
                self.outputFormat = 'TIME_SERIES-ASCII'
            else:
                self.outputFormat = 'NONE'
        else:
            self.outputFormat = 'TIME_SERIES-VOT'

        # if a specific instrument was reqested, get kinst
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)
        kinst = None
        if self.INSTRUMENT_ID != None:
            try:
                kinst = int(self.INSTRUMENT_ID)
            except:
                # instrument was input as a name
                instrumentList = self.madInstObj.getInstrumentList()
                # see if we can find a matching name or description
                for inst in instrumentList:
                    if inst[0].lower() == self.INSTRUMENT_ID.lower() or \
                       inst[1].lower() == self.INSTRUMENT_ID.lower():
                        kinst = inst[2]
                        break
                if kinst == None:
                    # no matches found
                    raise ValueError, 'Instrument %s not in Madrigal' % (self.INSTRUMENT_ID)

        # get a list of all experiment days and kinsts set by Query
        self.madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)
        self.expDict = {} # a dict of key=kinst, value = list of days/exp name as
                          # strings in form '2006-02-27<ExpName>'
        position = -1
        while 1:
            position += 1
            st = self.madExpObj.getExpStartDateTimeByPosition(position)
            et = self.madExpObj.getExpEndDateTimeByPosition(position)
            if st == None:
                break
            thisStartTime = datetime.datetime(*(st[0:6]))
            thisEndTime = datetime.datetime(*(et[0:6]))

            if thisEndTime < self.startTime or thisStartTime > self.endTime:
                continue

            thisName = self.madExpObj.getExpNameByPosition(position)
            if thisName == None:
                thisName = ''

            # time matches, check kinst
            thisKinst = self.madExpObj.getKinstByPosition(position)
            if kinst != None:
                if kinst != thisKinst:
                    continue

            # match okay - get list of all days in experiment
            beginDay = datetime.datetime(thisStartTime.year,
                                         thisStartTime.month,
                                         thisStartTime.day)
            endDay   = datetime.datetime(thisStartTime.year,
                                         thisStartTime.month,
                                         thisStartTime.day,
                                         23,59,59)
            while 1:
                if endDay < self.startTime:
                    beginDay += datetime.timedelta(1)
                    endDay += datetime.timedelta(1)
                    continue

                if beginDay > thisEndTime or beginDay > self.endTime:
                    break

                dayStr = beginDay.strftime('%Y-%m-%d')+ thisName

                if self.expDict.has_key(thisKinst):
                    if dayStr not in self.expDict[thisKinst]:
                        self.expDict[thisKinst].append(dayStr)
                else:
                    self.expDict[thisKinst] = [dayStr]

                beginDay += datetime.timedelta(1)
                endDay += datetime.timedelta(1)


        # make sure they are sorted
        for key in self.expDict.keys():
            self.expDict[key].sort()


    def outputXml(self):
        """outputXml writes the results in the form of a Votable as specified
        in http://www.mssl.ucl.ac.uk/~eca/stap.html
        """
        headTemplate = """
<?xml version="1.0"?>
<VOTABLE version="1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <COOSYS ID="Madrigal_coosys" system="geo_app"/>
  <RESOURCE name="Madrigal output" type="meta">
    <INFO name="QUERY_STATUS" value="OK"/>
    <TABLE name="results">
      <DESCRIPTION>Simple time access protocol data</DESCRIPTION>
      <FIELD name="PROVIDER" ID="col1" ucd="meta.curation" datatype="char" arraysize="*"/>
      <FIELD name="DATA_ID" ID="col2" ucd="meta.title" datatype="char" arraysize="*"/>
      <FIELD name="INSTRUMENT_ID" ID="col3" ucd="INST_ID" datatype="char" arraysize="*"/>
      <FIELD name="TIME_START" ID="col4" ucd="time.obs.start" unit="iso8601" datatype="char" arraysize="*"/>
      <FIELD name="TIME_END" ID="col5" ucd="time.obs.end" unit="iso8601" datatype="char" arraysize="*"/>
      <FIELD name="ACCESS_URL" ID="col6" ucd="VOX:AccessReference" datatype="char" arraysize="*"/>
      <FIELD name="FORMAT" ID="col7" ucd="VOX:Format" datatype="char" arraysize="*"/>
      <FIELD name="DESCRIPTION" ID="col8" ucd="meta" datatype="char" arraysize="*"/>
      <FIELD name="DESCRIPTION_URL" ID="col9" ucd="meta.ref.url" datatype="char" arraysize="*"/>
      <DATA>
        <TABLEDATA>
"""

        footTemplate = """
        </TABLEDATA>
      </DATA>
    </TABLE>
  </RESOURCE>
</VOTABLE>
"""
        retStr = headTemplate
        # add one row for each result in self.expDict
        for key in self.expDict.keys():
            for expStr in self.expDict[key]:
                retStr += self.getRow(key, expStr)

        retStr += footTemplate
        return(retStr)


    def getRow(self, kinst, expStr):
        """getRow returns one <TR>...</TR> row of data.

        kinst - instrument id (int) of this experiment

        expStr - string giving experiment day and name in form '2006-02-27<ExpName>'
        """
        if self.outputFormat == 'NONE':
            return ''
   
        url = self.madDBObj.getTopLevelUrl()
        expTitle = expStr[10:]
        instName = self.madInstObj.getInstrumentName(kinst)
        dayStr = expStr[:10]
        cgiUrl = self.madDBObj.getCGIHomeBase()
        rulesOfRoad = self.madDBObj.getLocalRulesOfRoad()
        # remove linefeeds from rules of road
        rulesOfRoad = rulesOfRoad.replace('\n', ' ')
        
        retStr = """
        <TR>
          <TD><![CDATA[%s]]></TD>
	  <TD>Instrument: %s, Title: %s, Date: %s</TD>
	  <TD>%s</TD>
	  <TD>%sT00:00:00</TD>
	  <TD>%sT23:59:59</TD>
	  <TD><![CDATA[%s/simpleIsprint.py?selectInstrument=%i&selectExperiments=%s&selectCoordinate=Geodetic&format=%s]]></TD>
	  <TD>%s</TD>
	  <TD><![CDATA[%s]]></TD>
	  <TD><![CDATA[%s]]></TD>	  
        </TR>
        """ % (url,
               instName,
               expTitle,
               dayStr,
               instName,
               dayStr,
               dayStr,
               cgiUrl,
               kinst,
               dayStr,
               self.outputFormat,
               self.outputFormat,
               rulesOfRoad,
               url)
        
        return(retStr)
               
        
            

if __name__ == '__main__':

    # Script stapMadrigalService.py
    # This script only calls the init function of the class stapMadrigalService
    # All work is done by the init function
    stapMadrigalService()
