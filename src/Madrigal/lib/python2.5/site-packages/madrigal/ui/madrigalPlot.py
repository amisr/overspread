"""madrigalPlot is the module that produces plots of Madrigal data.

Presently based on madplotlib: http://matplotlib.sourceforge.net/

$Id: madrigalPlot.py,v 1.49 2009/04/29 18:14:18 brideout Exp $

"""
import sys,os
import os.path
import traceback
import types
import datetime
import bisect
import math

import madrigal.metadata
import madrigal._Madrec

import madrigalWeb.madrigalWeb


def defineHomeEnvVariable():
    """defineHomeEnvVariable makes sure HOME env variable is defined, as required by matplotlib.

    If not defined, sets HOME to MADROOT/metadata/userdata
    """
    try:
        os.environ['HOME']
        return
    except KeyError:
        import madrigal.metadata
        madDB = madrigal.metadata.MadrigalDB()
        os.environ['HOME'] = os.path.join(madDB.getMadroot(), 'metadata/userdata')


defineHomeEnvVariable()

import matplotlib
# set rendering
matplotlib.use('Agg')
import matplotlib.pylab
import matplotlib.numerix
import matplotlib.colors
import matplotlib.cm
import matplotlib.numerix.ma



def convertToAbsoluteTimeStr(xticks, noTime=False):
    """convertToAbsoluteTimeStr converts a list of strings containing seconds since 1/1/1950 to datetime string.
    
    Input: xticks - a list of strings containing seconds since 1/1/1950
    
    Returns: a list of strings formated as YYYY-MM-DD HH-MM-SS.  If noTime, format as YYYY-MM-DD
    """
    datetime1950 = datetime.datetime(1950,1,1,0,0,0)
    
    newList = []
    
    for item in xticks:
        seconds = long(item)
        newDatetime = datetime1950 + datetime.timedelta(0, seconds)
        if noTime:
            newList.append(newDatetime.strftime('%Y-%m-%d'))
        else:
            newList.append(newDatetime.strftime('%Y-%m-%d %H:%M:%S'))
    
    return newList


def get_vo_cmap():
    """get_vo_cmap is a function to return a colormap optimized to show sign changes in the middle of the range.
    """
    LUTSIZE = matplotlib.rcParams['image.lut']

    _vo_cm_data =     {'red':   ((0, 0.75, 0.75), (0.4, 0.0, 0.0), (0.5, 0.0, 0.0),   (0.6, 1.0, 1.0),   (1.0, 1.0, 1.0)),
                       'green': ((0, 1.0, 1.0), (0.4, 0.5, 0.5), (0.5, 0.25, 0.25), (0.6, 0.25, 0.25), (1.0, 1.0, 1.0)),
                       'blue':  ((0, 1.0, 1.0), (0.4, 1.0, 1.0), (0.5, 0.5, 0.5),   (0.6, 0.0, 0.0),   (1.0, 0.5, 0.5))}


    vo_cm = matplotlib.colors.LinearSegmentedColormap('vo_cm',_vo_cm_data, LUTSIZE)

    return vo_cm



class madScatterPlot:
    """madScatterPlot is the class the produces two dimensional scatter plots of x versus y.


    """
    def __init__(self, isprintText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
		 fullFilename,
                 size = 'small',
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None):
        """__init__ writes a madScatter plot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
			  to the experiment beginning (UTH) or absolute (UT1).
			  The second must be the parameter to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float. 
            
            titleStr - plot title (string) - should describe parameter being plotted
            
            xLabelStr - x label string
	    
	    yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
			    
	    useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
	                      since beginning of experiment (UTH).  If useAbsoluteTime is true, first
			      parameter in isprintText must be UT1, if false, it must be UTH.

	    startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
	                in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	                startTime must be in units of UTH (hours since midnight UT of first day of
	                experiment). Default is None, which means start at lowest time found.

	    endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
	              in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	              endTime must be in units of UTH (hours since midnight UT of first day of
	              experiment). Default is None, which means end at largest time found.

	    maxNumPoints - maximum number of points to plot.  If not None, truncate isprintText if
	                   needed to have at most maxNumPoints lines.
        
        
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, since numpy doesn't handle NaN
        self.__parameter_count = 2

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError, 'size must be "small", "wide", or "large", not %s' % (str(size))
        
        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumPoints != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumPoints)
               
        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = map(self.__filter_missing, split_data)
            array_data = matplotlib.numerix.asarray(float_data)
            array_data = matplotlib.numerix.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError, 'input text is not parseable'
        
        # find min, max of x (time)
        if startTime == None:
            xMin = array_data[0,0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = array_data[-1,0]
        else:
            xMax = endTime
        
	    
        # find min, max of y, not including missing
        yMin = None
        yMax = None
        for y in array_data[:,1]:
            if y == self.__missing:
                continue
            if yMin == None:
                yMin = y
            elif yMin > y:
                yMin = y
            if yMax == None:
                yMax = y
            elif yMax < y:
                yMax = y
		
        if yMin == None:
            raise ValueError, 'No valid y data found'

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(6,4), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
	    
        if useAbsoluteTime:
             # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.8, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.66, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.15, 0.2, 0.7, 0.7]) 
      
        matplotlib.pylab.scatter(array_data[:,0],array_data[:,1])

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)

        if useAbsoluteTime:
            locs, labels = matplotlib.pylab.xticks()
            if len(locs) > 5:
                # truncate by len(locs) / 5
                scale = 1 + int(len(locs) / 5)
                new_locs = []
                for i in range(len(locs)):
                    if i % scale == 0:
                        new_locs.append(locs[i])
                locs = new_locs
            newXTicks = convertToAbsoluteTimeStr(locs)
            matplotlib.pylab.xticks(locs, newXTicks, rotation=15)
	    
        matplotlib.pylab.xticks(fontsize=fontSize)

        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()
          
        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()


    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText
        

    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

        

class madLineTimePlot:
    """madLineTimePlot is the class the produces line plots of one or more parameters versus time.


    """
    def __init__(self, isprintText,
                 yParmList,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
		 fullFilename,
                 size = 'small',
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None,
                 dateOnly = False,
                 yMinimum = None,
                 yMaximum = None):
        """__init__ writes a madLineTimePlot plot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
			  to the experiment beginning (UTH) or absolute (UT1).
			  The the following must be the parameters to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float.

            yParmList - a list of y parameters (strings).  Length must == num columns in isprintText - 1
            
            titleStr - plot title (string) - should describe parameter being plotted
            
            xLabelStr - x label string
	    
	    yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
			    
	    useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
	                      since beginning of experiment (UTH).  If useAbsoluteTime is true, first
			      parameter in isprintText must be UT1, if false, it must be UTH.

	    startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
	                in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	                startTime must be in units of UTH (hours since midnight UT of first day of
	                experiment). Default is None, which means start at lowest time found.

	    endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
	              in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	              endTime must be in units of UTH (hours since midnight UT of first day of
	              experiment). Default is None, which means end at largest time found.

	    maxNumPoints - maximum number of points to plot.  If not None, truncate isprintText if
	                   needed to have at most maxNumPoints lines.

	    dateOnly - if True and useAbsoluteTime True, then dates without times printed on x axis.

	    yMinimum - set y minimum.  If default=None, use data minimum

	    yMaximum - set y maximum.  If default=None, use data maximum
        
        
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, since numpy doesn't handle NaN
        self.__parameter_count = 1 + len(yParmList)
        self.__colorList = 'bgrcmykw' # a list of colors for the lines

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError, 'size must be "small", "wide", or "large", not %s' % (str(size))
        
        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumPoints != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumPoints)
               
        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = map(self.__filter_missing, split_data)
            array_data = matplotlib.numerix.asarray(float_data)
            array_data = matplotlib.numerix.reshape(array_data,(-1,self.__parameter_count))
            threshold = self.__missing*0.9999
            array_data = matplotlib.numerix.ma.masked_where(array_data > threshold, array_data)
        except:
            traceback.print_exc()
            raise ValueError, 'input text is not parseable'
        
        # find min, max of x (time)
        if startTime == None:
            xMin = array_data[0,0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = array_data[-1,0]
        else:
            xMax = endTime
        
	    
        # find yMin just to ensure there is data
        yMin = None
        for column in range(len(yParmList)):
            for y in array_data[:,1+column]:
                if y == self.__missing:
                    continue
                if yMin == None:
                    yMin = y
                elif yMin > y:
                    yMin = y
		
        if yMin == None:
            raise ValueError, 'No valid y data found'

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(6,4), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
	    
        if useAbsoluteTime:
             # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.8, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.9, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.15, 0.2, 0.7, 0.7]) 
      
        for column in range(len(yParmList)):
            color = self.__colorList[column % len(self.__colorList)]
            matplotlib.pylab.plot(array_data[:,0],array_data[:,1 + column], '%so-' % (color))

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)

        if useAbsoluteTime:
            locs, labels = matplotlib.pylab.xticks()
            if len(locs) > 5:
                # truncate by len(locs) / 5
                scale = 1 + int(len(locs) / 5)
                new_locs = []
                for i in range(len(locs)):
                    if i % scale == 0:
                        new_locs.append(locs[i])
                locs = new_locs
            newXTicks = convertToAbsoluteTimeStr(locs, dateOnly)
            matplotlib.pylab.xticks(locs, newXTicks, rotation=15)
	    
        matplotlib.pylab.xticks(fontsize=fontSize)

        if yMinimum != None and yMaximum != None:
            matplotlib.pylab.ylim(yMinimum, yMaximum)
        elif yMinimum != None and yMaximum == None:
            matplotlib.pylab.ylim(yMin=yMinimum)
        elif yMinimum == None and yMaximum != None:
            matplotlib.pylab.ylim(yMax=yMaximum)

        matplotlib.pylab.title(titleStr, fontsize=fontSize)
        matplotlib.pylab.legend(yParmList)
          
        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()

    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText
        
	
    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

    def writeToFile(self, fullFilename):
        pass

    def displayToScreen(self):
        pass

    def getFigureHandle(self):
        pass


                 
class madXYScatterPlot:
    """madXYScatterPlot is the class the produces XY scatter plots.


        Change History:

            Written by "Brandon Scott Fines":mailto:bfines@haystack.mit.edu Aug 2, 2006
            Written by "Bill Rideout":mailto:brideout@haystack.mit.edu Aug 2, 2006

    """

    def __init__(self,inputText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size='small',
                 lowBound=None,
                 highBound=None,
                 maxNumPoints=None):

        """
        Inputs:

            inputText - string of values to be plotted. The formatting is as
                        follows:

                        xval yval
                        xval yval
                        xval yval

            titleStr - title of the Plot

            xLabelStr - label for the x axis

            yLabelStr - label for the y axis

            fullFilename - full file path to save the resulting picture to

            size - size of plot to be saved. Must be 'small','wide', or 'large'.
                defaults to 'small'.

            lowBound - lower bound on the x-axis value. If no bound is specified,
                    the lowest value found in inputText will be used.

            highBound - upper bound on the x-axis value. If no bound is specified,
                    the highest value found in inputText will be used.

            maxNumPoints - maximum number of points to be plotted.

        Outputs: None

        Affects: Creates a scatter plot using matplotlib and writes that to the
                    file designated by the variable 'fullFilename'

        Exceptions: ValueError if lowBound or highBound cannot be converted to
                    a float value

        Non-standard python modules used:

            matplotlib
        """
        self.__missing = 1.0E30 #special value since numpy can't handle NaN



        #verify input
        if size not in ('small','wide','large'):
            raise ValueError, 'size must be "small","wide", or "large", not %s'%(str(size))

        if size in ('small','wide'):
            fontSize = 12
        elif size in 'large':
            fontSize = 18

        if maxNumPoints !=None:
            inputText = self.__truncateInput(inputText, maxNumPoints)


        if lowBound !=None:
            #check that it is a number
            try:
                lowBound = float(lowBound)
            except ValueError:
                raise ValueError, 'lowBound not a number'
            try:
                highBound = float(highBound)
            except:
                raise ValueError, 'lowBound not a number'     
                

        #convert the input data into numeric arrays
        x=[]
        y=[]

        split_data = inputText.split()
        #send x-values to x, y-values to y
        i=0
        while i <len(split_data)-1:
            try:
                x.append(float(split_data[i]))
                y.append(float(split_data[i+1]))
            except:
                pass

            i=i+2
        
        xlow =None
        xhigh = None
        # set min and max in x if specified
        if lowBound !=None and highBound !=None:
            xlow = lowBound
            xhigh = highBound
        elif lowBound !=None and highBound==None:
            #find the upper bound in x
            for i in x:
                if xhigh==None:
                    xhigh = i
                if i>xhigh:
                    xhigh = i
            xlow = lowBound
        elif lowBound==None and highBound !=None:
            #find the lower bound in x
            for i in x:
                if xlow==None:
                    xlow = i
                if i<xlow:
                    xlow = i
            xhigh=highBound
        else:
            #find lower and upper bounds
            for i in x:
                if xlow==None:
                    xlow = i
                elif i<xlow:
                    xlow = i
                if xhigh==None:
                    xhigh = i
                elif i>xhigh:
                    xhigh = i

        #find upper and lower bounds in y
        ylow = None
        yhigh = None
        for i in y:
            if ylow==None:
                ylow = i
            elif i<ylow:
                ylow = i
            if yhigh==None:
                yhigh = i
            elif i>yhigh:
                yhigh = i



        #check for good numbers
        if ylow == None:
            raise ValueError, 'no valid y data found'

        #select plot sizes
        if size=='small':
            matplotlib.pylab.figure(1,figsize=(6,4),facecolor='w')
        elif size=='wide':
            matplotlib.pylab.figure(1,figsize=(10,4),facecolor='w')
        elif size=='large':
            matplotlib.pylab.figure(1,figsize=(12,6),facecolor='w')


        #draw scatter plot
        matplotlib.pylab.scatter(x,y)

        thisFont = {'size' : fontSize}
        matplotlib.pylab.rc('font', **thisFont) #pass in font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr,fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr,fontsize=fontSize)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.xticks(fontsize=fontSize)
        matplotlib.pylab.xlim(xlow,xhigh)
        matplotlib.pylab.title(titleStr,fontsize=fontSize)

        #save the figure
        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()

    def __truncateInput(self,inputStr,maxLines):
        """__truncateInput truncates inputStr to have maxLines at most
        """
        inputList = inputStr.split('\n')
        if len(inputList)<maxLines:
            return inputStr
        else:
            dropNumber = int(len(inputList)/maxLines)
            newline = '\n'
            newInputText = newline.join(inputList[::dropNumber])

            return newInputText
        


class madPcolorPlot:
    """madPcolorPlot is the class that produces pcolor plots of x versus y with z intensity.

    Assumes the x axis is time.

    Usage example::

    	obj = madPcolorPlot(isprintText,
                        'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2003 - Alt code',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Altitude (km)',
                        './isprint.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12,
			smoothAltitude = False)    

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Mar. 31, 2005
    """


    def __init__(self, isprintText,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 minColormap = None,
                 maxColormap = None,
		 smoothAltitude = True,
		 insertDataGap = 5,
		 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 sortTimeFlag = False,
                 maxNumTimes = None,
                 maxNumAlt = None,
                 truncateIsprint = False,
                 colorMap = matplotlib.cm.jet,
                 yMinimum = None,
                 yMaximum = None,
                 altYTitle = None,
                 altYLabels = None):
        """__init__ writes a madPColorPlot to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be UTH or UT1, depending on whether time scale should be relative
			  to the experiment beginning (UTH) or absolute (UT1).
			  The second must be gdalt, and third parameter to be plotted. Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float. 
            
            titleStr - plot title (string) - should describe parameter being plotted
	    
	    xLabelStr - x label string
	    
	    yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.
	    
	    smoothAltitude - if True, extrapolate between existing data between altitudes to fill
	                     in missing data; if False, leave missing
			     
	    insertDataGap - this parameter sets the threshold for inserting a data gap.  The time intervals
	                    being plotted are ordered, and the time gap larger than 90% of the rest is determined.
			    Any time interval more than insertDataGap times bigger is then considered missing
			    data.  Defaults to five.  If None, no gaps are ever inserted.  For data with close
			    to uniform time intervals, no gaps will be inserted.
			    
	    useAbsoluteTime - if true, print time as YYYY-MM-DD HH:MM:SS.  If false (default), print time as hour
	                      since beginning of experiment (UTH).  If useAbsoluteTime is true, first
			      parameter in isprintText must be UT1, if false, it must be UTH.
        
            startTime - start plot at given time.  If useAbsoluteTime == True, then startTime must be
	                in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	                startTime must be in units of UTH (hours since midnight UT of first day of
	                experiment). Default is None, which means start at lowest time found.

	    endTime - end plot at given time.  If useAbsoluteTime == True, then endTime must be
	              in units of seconds since 1/1/1950.  If useAbsoluteTime == False, then
	              endTime must be in units of UTH (hours since midnight UT of first day of
	              experiment). Default is None, which means end at largest time found.

	    sortTimeFlag - if true, check that time is correctly sorted.  If false (the default),
	                   assume time already sorted

	    maxNumTimes - if not None, decimate the number of times in the isprint string to
	                  maxNumTimes.  If None (the default), plot all times.

	    maxNumAlt - if not None, reduce the number of altitudes to maxNumAlt.  If None (the default),
	                plot all altitudes.

	    truncateIsprint - if True, and both maxNumTimes and maxNumAlt not = None, then truncate
	                       the number of isprint lines to be maxNumTimes * maxNumAlt

	    colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

	    yMinimum - minumum y value.  If None (default), set by data y minimum.

	    yMaximum - maximum y value.  If None (default), set by data y maximum.

	    altYTitle - title of right (second) y axis.  If None (the default),
	                 no Y axis on the right side.

	    altYLabels - a list of Y labels (strings) for the right axis.  If None (the default),
	                 no Y labels on the right axis.

	    
	              
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, used to create a masked array
        self.__parameter_count = 3

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError, 'size must be "small", "wide", or "large", not %s' % (str(size))

        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if truncateIsprint and maxNumTimes != None and maxNumAlt != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumTimes * maxNumAlt)

        # since matplotlib pcolor wants a regular grid, we create a grid with all altitudes

        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = map(self.__filter_missing, split_data)
            array_data = matplotlib.numerix.asarray(float_data)
            array_data = matplotlib.numerix.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError, 'input text is not parseable'

        if sortTimeFlag:
            array_data = self.sortArrayInTime(array_data)

        if maxNumTimes != None and maxNumTimes != 0 and not truncateIsprint:
            array_data = self.decimateTimes(array_data, int(maxNumTimes), insertDataGap)
        
        # The first pass through is to obtain the number and range of the  x and y variables
        
        xList = []
        yList = []
        zList = []
        zMin = None
        zMax = None

        # loop over all the lines of data in the array
        for j in range(len(array_data)):
            try:
                
                x = array_data[j][0]
                y = array_data[j][1]
                z = array_data[j][2]

                if x not in xList:
                    xList.append(x)

                if y not in yList:
                    yList.append(y)

                if z != self.__missing:
                    zList.append(z)

                if zMin != None:
                    if z < zMin and z != self.__missing:
                        zMin = z
                elif z != self.__missing:
                    zMin = z

                if zMax != None:
                    if z > zMax and z != self.__missing:
                        zMax = z
                elif z != self.__missing:
                    zMax = z

            except:
                continue
		
        if zMin == None:
            raise ValueError, 'No valid z data found'

        # if both minColormap and maxColormap == None, use autoscaling
        if minColormap == None and maxColormap == None:
            zList.sort()
            d10 = zList[int(len(zList)*0.10)]
            d90 = zList[int(len(zList)*0.90)]

            zMin = d10 - (d90-d10) * 0.75
            zMax = d90 + (d90-d10) * 0.75

        # now sort the X and Y axis lists and pull their length
        
        xList.sort()
        if startTime == None:
            xMin = xList[0]
        else:
            xMin = startTime
        if endTime == None:
            xMax = xList[-1]
        else:
            xMax = endTime
        yList.sort()
        max_x_dimension = len(xList)
        max_y_dimension = len(yList)

        if yMinimum == None:
            yMinimum = yList[0]
        if yMaximum == None:
            yMaximum = yList[-1]

        self.truncateAlt = False
        if maxNumAlt != None:
            if max_y_dimension > maxNumAlt:
                self.truncateAlt = True

        # build dictonary of indexes into xList
        self.xListDict = {}
        for i in range(len(xList)):
            self.xListDict[xList[i]] = i

        # if self.truncateAlt == False, build dictonary of indexes into yList,
        # else truncate y values by builing a list of maxNumAlt ranges
        if self.truncateAlt == False:
            self.yListDict = {}
            for i in range(len(yList)):
                self.yListDict[yList[i]] = i
        else:
            self.yListRanges = []
            for i in range(maxNumAlt):
                self.yListRanges.append(yList[int(i*(len(yList)/maxNumAlt))])
            max_y_dimension = maxNumAlt

        # now build arrays to handle the X axis label, Y axis label, and the Z data

        X = matplotlib.numerix.zeros((max_x_dimension, max_y_dimension), matplotlib.numerix.Float32)
        Y = matplotlib.numerix.zeros((max_x_dimension, max_y_dimension), matplotlib.numerix.Float32)
        Z = matplotlib.numerix.ones((max_x_dimension, max_y_dimension), matplotlib.numerix.Float32)

        # all parameter values default to missing
        Z = Z * self.__missing

        # fill the X and Y arrays

        for i in range(max_x_dimension):
            for j in range(max_y_dimension):
                X[i][j] = float(xList[i])
                if self.truncateAlt:
                    Y[i][j] = float(yList[int(j*(len(yList)/maxNumAlt))])
                else:
                    Y[i][j] = float(yList[j])

    
        # Now load up the data array Z with the array_data measurements as a function of x and y
        previousIndex = None
        previousValue = None
        presentTime = None
        newTimeFound = True

        for k in range(len(array_data)):
            try:

                xdata = array_data[k][0]
                ydata = array_data[k][1]
                zdata = array_data[k][2]

                if zdata == self.__missing:
                    continue

                if xdata != presentTime:
                    newTimeFound = True
                else:
                    newTimeFound = False
                presentTime = xdata

                # now find the right place in the array for this data point
                i = self.xListDict[xdata]
                j = self.__getYIndex(ydata)

                Z[i][j] = zdata
		
		# now see if we need to fill in any gaps
                if (not newTimeFound) and smoothAltitude:
                    if previousIndex < j - 1:
                        # fill in all missing points
                        for k in range(previousIndex + 1, j):
                            # simply average between the points based on index
                            thisValue = previousValue + (zdata - previousValue)*(float(k-previousIndex)/float(j-previousIndex))
                            Z[i][k] = thisValue
			   
                previousIndex = j
                previousValue = zdata
		
                
            except:
                continue
		
        # insert missing data to represent gaps if needed
        if insertDataGap != None:
            # first find the time interval greater than 90% of others
            timeIntervalList = []
            for i in range(len(xList) - 1):
                timeIntervalList.append(xList[i+1] - xList[i])
            timeIntervalList.sort()
            index = int(len(timeIntervalList)*0.9)
            maxInterval = timeIntervalList[index]
	    
            for i in range(len(xList) - 1):
                if xList[i+1] - xList[i] > maxInterval * insertDataGap:
                    Z[i,:] = self.__missing

        # make Z a masked array
        Znew = matplotlib.numerix.ma.masked_inside(Z, 0.99*self.__missing, 1.01*self.__missing)

        # set up plotting parameters
        if minColormap == None:
            minColormap = zMin
        if maxColormap == None:
            maxColormap = zMax

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(8,4), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
	    
        if useAbsoluteTime:
            # leave room for rotated datetime string
            if size == 'large':
                matplotlib.pylab.axes([0.1, 0.2, 0.85, 0.7])
            elif size == 'wide':
                matplotlib.pylab.axes([0.07, 0.2, 0.83, 0.7])
            elif size == 'small':
                matplotlib.pylab.axes([0.15, 0.2, 0.75, 0.7])
        else:
            if size == 'small':
                matplotlib.pylab.axes([0.1, 0.1, 0.82, 0.8])

        matplotlib.pylab.pcolor(X,Y,Znew, shading='flat', vmin=minColormap, vmax=maxColormap, cmap = colorMap, norm = matplotlib.pylab.normalize())
        matplotlib.pylab.colorbar()

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.ylim(yMinimum, yMaximum)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)

        if useAbsoluteTime:
            locs, labels = matplotlib.pylab.xticks()
            if len(locs) > 5:
                # truncate by len(locs) / 5
                scale = 1 + int(len(locs) / 5)
                new_locs = []
                for i in range(len(locs)):
                    if i % scale == 0:
                        new_locs.append(locs[i])
                locs = new_locs
            newXTicks = convertToAbsoluteTimeStr(locs)
            matplotlib.pylab.xticks(locs, newXTicks, rotation=15)

        else:
            # if plot is less than 48 hours, force ticks to be every 4 hours
            xmin, xmax = matplotlib.pylab.xlim()
            if xmax < 49:
                if xmax % 4 != 0:
                    newXmax = 4 * (int(xmax/4.0) + 1)
                else:
                    newXmax = xmax
                newXLocs = []
                newXLabels = []
                for i in range(0,int(4+newXmax),4):
                    newXLocs.append(i)
                    newXLabels.append(str(i))
                matplotlib.pylab.xticks(newXLocs, newXLabels)


        matplotlib.pylab.xticks(fontsize=fontSize)

        # add second y-axis if desired
        if altYTitle != None and altYLabels != None:
            ax2 = matplotlib.pylab.twinx()
            matplotlib.pylab.ylabel(altYTitle)
            ax2.yaxis.tick_right()
            matplotlib.pylab.yticks(range(len(altYLabels)), altYLabels)
            matplotlib.pylab.yticks(fontsize=fontSize)

        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()
          
        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()



    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

    def __getYIndex(self, yvalue):
        """__getYIndex returns the correct index into the y dimension for a given y value.

        Input: yvalue - value of y parameter

        Returns: the correct index into the y dimension

        Algorithm: if self.truncateAlt == False, simple use the dictionary self.yListDict.  Else
        loop through self.yListRanges and return the first greater than the requested value
        """
        if self.truncateAlt == False:
            return self.yListDict[yvalue]
        else:
            i = bisect.bisect_left(self.yListRanges, yvalue)
            if i >= len(self.yListRanges):
                i = len(self.yListRanges) - 1
            return i
        
            
    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newIsprintText = ''
            for i in range(0,len(isprintList),dropNumber):
                newIsprintText += isprintList[i] + '\n'

            return newIsprintText


        
    def displayToScreen(self):
        " to implement this takes a reworking away from pylab to use the underlying matplotlib code "
        pass 

    def getFigureHandle(self):
        return self.__figure


    def getAverage(self, X):
        """returns the average of items in a float array.  Does not including missing data.
        If all data missing, returns self.__missing
        """
        count = 0
        total = 0.0
        for i in range(X.shape[0]):
            if X[i] != self.__missing:
                count += 1
                total += X[i]

        if count == 0:
            return self.__missing
        else:
            return total / float(count)


    def sortArrayInTime(self, array_data):
        """sortArrayInTime sorts a two-dimensional array so that the first element in each row (time) is in ascending order.

        Input: array_data - two-dimensional array to be sorted by rearranging rows so
               that the first element in each row (time) is in ascending order

        Returns: new_array
        """

        sortIndex = matplotlib.numerix.argsort(array_data[:,0])[0]

        # if already sorted, just return original array
        if sortIndex == matplotlib.numerix.sort(sortIndex):
            return array_data
        
        # try different lines for different numerix libraries
        try:
            new_array = matplotlib.numerix.zeros(array_data.shape, array_data.dtype)
        except:
            new_array = matplotlib.numerix.zeros(array_data.shape, array_data.typecode())


        for i in range(len(sortIndex)):
            new_array[sortIndex[i],:] = array_data[i,:]


        return new_array


    def decimateTimes(self, array_data, maxNumTimes, insertDataGap):
        """decimateTimes decimates array_data to have at most maxNumTimes times.

        Input: array_data - two-dimensional array to be decimated by deleting times and missing data.

               maxNumTimes: int representing the maximum number of times to keep in array_data

               insertDataGap - this parameter sets the threshold for inserting a data gap.  The time intervals
	                    being plotted are ordered, and the time gap larger than 90% of the rest is determined.
			    Note that this parameter is used here to stop the truncation of isprint lines that
			    will eventually be considered edge lines.

        Returns: new array built from decimated array_data

        """

        # get the number of times in array_data, and make a list of all unique times
        numTimes = 0
        uniqueTimes = []
        time_array = array_data[:, 0]
        for i in range(len(time_array)):
            if i == 0:
               numTimes =  1
               uniqueTimes.append(time_array[i])
            elif time_array[i-1] != time_array[i]:
                uniqueTimes.append(time_array[i])
                numTimes +=  1

        if numTimes <= maxNumTimes:
            return array_data
        

        # insert missing data to represent gaps if needed
        gapTimes = []
        if insertDataGap != None:
            # first find the time interval greater than 90% of others
            timeIntervalList = []
            for i in range(len(time_array) - 1):
                if time_array[i+1] == time_array[i]:
                    continue
                timeIntervalList.append(time_array[i+1] - time_array[i])
            timeIntervalList.sort()
            index = int(len(timeIntervalList)*0.9)
            maxInterval = timeIntervalList[index]
	    
            for i in range(len(time_array) - 1):
                if time_array[i+1] - time_array[i] > maxInterval * insertDataGap:
                    gapTimes.append(time_array[i+1])
                    gapTimes.append(time_array[i])

        # get the number of times to skip each time
        numSkip = numTimes/maxNumTimes

        # get the number of rows in the new_array
        numRows = 0
        numTimes = 0
        useThisTime = False
        for i in range(len(time_array)):
            if i == 0:
                numTimes =  1
            elif time_array[i-1] != time_array[i]:
                numTimes +=  1
                if numTimes % (numSkip + 1) == 0 or time_array[i] in gapTimes:
                    useThisTime = True
                    if array_data[i, -1] != self.__missing:
                        numRows += 1
                else:
                    useThisTime = False
            else:
                if useThisTime:
                    if array_data[i, -1] != self.__missing:
                        numRows += 1

        # create new_array
        # try different versions for different versions of numerix
        try:
            new_array = matplotlib.numerix.zeros((numRows, array_data.shape[1]), array_data.dtype)
        except:
            new_array = matplotlib.numerix.zeros((numRows, array_data.shape[1]), array_data.typecode())

        # copy selected rows to new_array
        numRows = 0
        numTimes = 0
        useThisTime = False
        for i in range(len(time_array)):
            if i == 0:
                numTimes =  1
            elif time_array[i-1] != time_array[i]:
                numTimes +=  1
                if numTimes % (numSkip + 1) == 0 or time_array[i] in gapTimes:
                    useThisTime = True
                    if array_data[i, -1] != self.__missing:
                        new_array[numRows,:] = array_data[i,:]
                        numRows += 1
                else:
                    useThisTime = False
            else:
                if useThisTime:
                    if array_data[i, -1] != self.__missing:
                        new_array[numRows,:] = array_data[i,:]
                        numRows += 1

        return new_array



class madPcolorScan:
    """madPcolorScan is the class that produces pcolor scans.

    Usage example::

    	obj = madPcolorScan(isprintText,
                        'Nel (log(m^-3)) - 26 June 2006 13:49:43-14:07:36',
                        'Longitude',
                        'Latitude',
                        './isprint.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12)    

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 20, 2006
    """


    def __init__(self, isprintText,
                 xGridSize,
                 yGridSize,
                 titleStr,
                 xLabelStr,
                 yLabelStr,
                 fullFilename,
                 size = 'small',
                 xMinimum = None,
                 xMaximum = None,
                 yMinimum = None,
                 yMaximum = None,
                 minColormap = None,
                 maxColormap = None,
                 colorMap = matplotlib.cm.jet,
                 maxNumLines = None):
        """__init__ writes a madPcolorScan to a file.

        Inputs:

            isprintText - a string giving isprint output without headers. First parameter
                          must be the X axis value, and the second must be the Y axis value.
			  The third column is the value (intensity). Any
                          missing data should be written as "missing" or other string that
                          cannot be converted to a float.

            xGridSize - grid size for x data (for example 0.1 for 0.1 degree longitude grid)

            yGridSize - grid size for x data (for example 0.1 for 0.1 degree latitude grid)
            
            titleStr - plot title (string) - should describe parameter being plotted
	    
	    xLabelStr - x label string
	    
	    yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.

            xMinimum = minumum x value.  If None (default), uses lowest x value found.

            xMaximum = maximum x value.  If None (default), uses highest x value found.

            yMinimum = minumum y value.  If None (default), uses lowest y value found.

            yMaximum = maximum y value.  If None (default), uses highest y value found.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.

            colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

	    maxNumLine - max number of lines in isprintText before truncating.  If None, no truncation

	              
        Returns: void

        Affects: None
        """
        self.__missing = 1.0E30 # special value, used to create a masked array
        self.__parameter_count = 3
        self.__xGridSize = int(xGridSize)
        self.__yGridSize = int(yGridSize)
        self.__parseCount = 0 # used to tell which column self.__filter_input is working on

        # verify input
        if size not in ('small', 'wide', 'large'):
            raise ValueError, 'size must be "small", "wide", or "large", not %s' % (str(size))

        if size in ('small', 'wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        if maxNumLines != None:
            isprintText = self.__truncateIsprint(isprintText, maxNumLines)

        # since matplotlib pcolor wants a regular grid, we create a grid with all altitudes

        # convert the input data into numeric arrays of float assuming no headers and filter the missing values.

        try:
            split_data = isprintText.split()
            float_data = map(self.__filter_input, split_data)
            array_data = matplotlib.numerix.asarray(float_data)
            array_data = matplotlib.numerix.reshape(array_data,(-1,self.__parameter_count))
        except:
            traceback.print_exc()
            raise ValueError, 'input text is not parseable'

        array_data = self.sortArrayInX(array_data)

        
        # The first pass through is to obtain the number and range of the  x and y variables
        
        xList = []
        yList = []
        zList = []
        zMin = None
        zMax = None

        # loop over all the lines of data in the array
        for j in range(len(array_data)):
            try:
                
                x = array_data[j][0]
                y = array_data[j][1]
                z = array_data[j][2]

                if x not in xList:
                    xList.append(x)

                if y not in yList:
                    yList.append(y)

                if z != self.__missing:
                    zList.append(z)

                if zMin != None:
                    if z < zMin and z != self.__missing:
                        zMin = z
                elif z != self.__missing:
                    zMin = z

                if zMax != None:
                    if z > zMax and z != self.__missing:
                        zMax = z
                elif z != self.__missing:
                    zMax = z

            except:
                continue

		
        if zMin == None:
            raise ValueError, 'No valid z data found'

        # if both minColormap and maxColormap == None, use autoscaling
        if minColormap == None and maxColormap == None:
            zList.sort()
            d10 = zList[int(len(zList)*0.10)]
            d90 = zList[int(len(zList)*0.90)]

            zMin = d10 - (d90-d10) * 0.75
            zMax = d90 + (d90-d10) * 0.75

        # now sort the X and Y axis lists and pull their length
        
        xList.sort()
        xMin = xList[0]
        xMax = xList[-1]
        yList.sort()
        yMin = yList[0]
        yMax = yList[-1]
        max_x_dimension = len(xList)
        max_y_dimension = len(yList)


        # build dictonary of indexes into xList
        self.xListDict = {}
        for i in range(len(xList)):
            self.xListDict[xList[i]] = i

        # build dictonary of indexes into yList,
        self.yListDict = {}
        for i in range(len(yList)):
            self.yListDict[yList[i]] = i


        # now build arrays to handle the X axis label, Y axis label, and the Z data

        X = matplotlib.numerix.zeros((max_x_dimension, max_y_dimension), matplotlib.numerix.Float32)
        Y = matplotlib.numerix.zeros((max_x_dimension, max_y_dimension), matplotlib.numerix.Float32)
        Z = matplotlib.numerix.ones((max_x_dimension, max_y_dimension), matplotlib.numerix.Float32)

        # all parameter values default to missing
        Z = Z * self.__missing

        # fill the X and Y arrays

        for i in range(max_x_dimension):
            for j in range(max_y_dimension):
                X[i][j] = float(xList[i])
                Y[i][j] = float(yList[j])

    
        # Now load up the data array Z with the array_data measurements as a function of x and y
        previousIndex = None
        previousValue = None
        presentTime = None
        newTimeFound = True

        for k in range(len(array_data)):
            try:

                xdata = self.__round(array_data[k][0], self.__xGridSize)
                ydata = self.__round(array_data[k][1], self.__yGridSize)
                zdata = array_data[k][2]

                if zdata == self.__missing:
                    continue

                if xdata != presentTime:
                    newTimeFound = True
                else:
                    newTimeFound = False
                presentTime = xdata

                # now find the right place in the array for this data point
                i = self.xListDict[xdata]
                j = self.yListDict[ydata]

                Z[i][j] = zdata
			   
                previousIndex = j
                previousValue = zdata
		
                
            except:
                continue
		

        # make Z a masked array
        Znew = matplotlib.numerix.ma.masked_inside(Z, 0.99*self.__missing, 1.01*self.__missing)

        # set up plotting parameters
        if xMinimum == None:
            xMinimum = xMin
        if xMaximum == None:
            xMaximum = xMax
        if yMinimum == None:
            yMinimum = yMin
        if yMaximum == None:
            yMaximum = yMax
        if minColormap == None:
            minColormap = zMin
        if maxColormap == None:
            maxColormap = zMax

        # select the plot size
        if size == 'small':
            matplotlib.pylab.figure(1, figsize=(6,4), facecolor = 'w')
        elif size == 'wide':
            matplotlib.pylab.figure(1, figsize=(10,4), facecolor = 'w')
        elif size == 'large':
            matplotlib.pylab.figure(1, figsize=(12,6), facecolor = 'w')
	    

        matplotlib.pylab.pcolor(X,Y,Znew, shading='flat', vmin=minColormap, vmax=maxColormap, cmap = colorMap, norm = matplotlib.pylab.normalize())
        matplotlib.pylab.colorbar()

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont)  # pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.ylabel(yLabelStr, fontsize=fontSize)
        matplotlib.pylab.xlim(xMinimum, xMaximum)
        matplotlib.pylab.ylim(yMinimum, yMaximum)
        matplotlib.pylab.yticks(fontsize=fontSize)
        matplotlib.pylab.title(titleStr, fontsize=fontSize)


        # get the handle to the figure now that it has been created so we can manipulate on subsequent calls.
        
        #self.__figure = matplotlib.pylab.gcf()

        try:
            matplotlib.pylab.savefig(fullFilename)
        except:
            matplotlib.pylab.clf()
            raise
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()


    def __round(self, value, increment):
        """__round returns a value to the nearest increment 
        """
        return value - math.fmod(value, increment)



    def __filter_input(self,x):
        """__filter_input is called in map to convert missing strings to self.__missing, and to
        round x and y vales to the nearest xGridSize or yGridSize
        """
        try:
            value = float(x)
            if self.__parseCount % 3 == 0:
                # x value
                value = self.__round(value, self.__xGridSize)
            elif self.__parseCount % 3 == 1:
                # y value
                value = self.__round(value, self.__yGridSize)
                
        except:
            value = self.__missing

        self.__parseCount += 1
        return value
        
            
    def __truncateIsprint(self, isprintText, maxLines):
        """__truncateIsprint truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList) < maxLines:
            return isprintText
        else:
            dropNumber = int(1 + len(isprintList)/maxLines)
            newIsprintText = ''
            for i in range(0,len(isprintList),dropNumber):
                newIsprintText += isprintList[i] + '\n'

            return newIsprintText


        
    def displayToScreen(self):
        " to implement this takes a reworking away from pylab to use the underlying matplotlib code "
        pass 

    def getFigureHandle(self):
        return self.__figure


    def getAverage(self, X):
        """returns the average of items in a float array.  Does not including missing data.
        If all data missing, returns self.__missing
        """
        count = 0
        total = 0.0
        for i in range(X.shape[0]):
            if X[i] != self.__missing:
                count += 1
                total += X[i]

        if count == 0:
            return self.__missing
        else:
            return total / float(count)


    def sortArrayInX(self, array_data):
        """sortArrayInX sorts a two-dimensional array so that the first element in each row (x) is in ascending order.

        Input: array_data - two-dimensional array to be sorted by rearranging rows so
               that the first element in each row (x) is in ascending order

        Returns: new_array
        """

        sortIndex = matplotlib.numerix.argsort(array_data[:,0])[0]

        # if already sorted, just return original array
        if sortIndex == matplotlib.numerix.sort(sortIndex):
            return array_data
        
        # try different versions for different versions of numerix
        try:
            new_array = matplotlib.numerix.zeros(array_data.shape, array_data.dtype)
        except:
            new_array = matplotlib.numerix.zeros(array_data.shape, array_data.typecode())


        for i in range(len(sortIndex)):
            new_array[sortIndex[i],:] = array_data[i,:]


        return new_array


class scanPlotter:
    """scanPlotter is the class that produces a series of scan plots for a single Madrigal file

    Non-standard Python modules used:
    matplotlib

    Change history:

    Written by "Bill Rideout":mailto:wrideout@haystack.mit.edu  Jul. 26, 2006
    """
    def __init__(self, madFile,
                 madDBObj=None):
        """__init__ initializes a scanPlotter object.

        Inputs:

            madFile - the Madrigal file to be analyzed.  Must contain SCNTYP and CYCN parameters.

            madDBObj - a madrigal.metadata.MadrigalDB object.  If None (default),
            one created

	              
        Returns: void

        Affects: sets self.madFile, self.madDBObj
        """
        if os.access(madFile, os.R_OK):
            self.madFile = madFile
        else:
            raise IOError, 'unable to read %s' % (str(madFile))

        if madDBObj == None:
            self.madDBObj = madrigal.metadata.MadrigalDB()
        else:
            self.madDBObj = madDBObj


    def plotAllScans(self,
                     scanType,
                     fullFilenameTemplate,
                     plotParm,
                     size = 'small',
                     xMinimum = None,
                     xMaximum = None,
                     yMinimum = None,
                     yMaximum = None,
                     xGridSize = None,
                     yGridSize = None,
                     minColormap = None,
                     maxColormap = None,
                     colorMap = matplotlib.cm.jet,
                     maxNumLines = None):
        """plotAllScans creates a series of az or el scans.

        Inputs:

            scanType - must be 'az' or 'el'

            fullFilename - full path of file containing pcolor plot to be saved. Each image
            created will have _#.png appended, with # starting at 0

            plotParm - mnemonic of parameter to be plotted.

            size - size of plot to save. Must be "small", "wide", or "large". Defaults to small.

            xMinimum = minumum x value.  If None (default), uses lowest x value found for each scan.

            xMaximum = maximum x value.  If None (default), uses highest x value found for each scan.

            yMinimum = minumum y value.  If None (default), uses lowest y value found for each scan.

            yMaximum = maximum y value.  If None (default), uses highest y value found for each scan.
            
            minColormap - minimum parameter value (defaults to lowest parameter value)

            maxColormap - maximum parameter value (defaults to highest parameter value).  However, if
                          both minColormap and maxColormap == None, autoscaling applied.

            colorMap - sets colormap.  It not given, defaults to matplotlib.cm.jet

	    maxNumLine - max number of lines in isprintText before truncating.  If None, no truncation

	Returns - a list of tuples, where each tuple has two items: 1. time string,
	          2. metadata string in form for scanTab.txt.  List length = number
	          of plots created
	"""
        
        if scanType not in ('az', 'el'):
            raise ValueError, 'scantype must be az or el, not %s' % (str(scanType))

        self.scanType = scanType

        retList = []

        # handle ion velocity
        if plotParm.lower() == 'vo':
            cmap = madrigal.ui.madrigalPlot.get_vo_cmap()
        else:
            cmap = matplotlib.cm.jet

        # create isprint string
        self.madWebObj = madrigalWeb.madrigalWeb.MadrigalData(self.madDBObj.getTopLevelUrl())

        parms = 'ut1,scntyp,cycn,%s,gdalt,gdlat,glon,azm,elm,gcdist' % (plotParm.strip())

        isprintStr = self.madWebObj.isprint(self.madFile,
                                            parms,
                                            '','internal','internal','internal')

        scanList = self.createScanInfoList(isprintStr)





        # loop through each scan
        scanFailureCount = 0 # keep track of how many scans fail
        for i in range(len(scanList)):
            scanCount = i - scanFailureCount
            scanInfo = scanList[i]
            try:
                startDateStr = self.getDateStrFromUT(scanInfo[1])
            except:
                # no scans may have been found
                continue
            endDateStr = self.getTimeStrFromUT(scanInfo[4])
            # create title
            
            if scanType == 'el':
                if scanInfo[3] > scanInfo[6]:
                    ind = 'down'
                    direction = 1
                else:
                    ind = 'up'
                    direction = 0
                titleStr = 'El %s scan (%s) %s-%s, az=%i' % (plotParm,
                                                             ind,
                                                             startDateStr,
                                                             endDateStr,
                                                             int(scanInfo[2]))
                timeStr = '%s-%s' % (startDateStr, endDateStr)
                metaStr = 'El scan: startTime %i el %f az %f stopTime %i el %f az %f dir %i' % (scanInfo[1],
                                                                                                scanInfo[3],
                                                                                                scanInfo[2],
                                                                                                scanInfo[4],
                                                                                                scanInfo[6],
                                                                                                scanInfo[5],
                                                                                                direction)
                retList.append((timeStr,metaStr))
                
                if scanInfo[7] == 'Gdlat':
                    xLabelStr = 'Geodetic latitude'
                    xGridSize = 1.0
                elif scanInfo[7] == 'Glon':
                    xLabelStr = 'Longitude'
                    xGridSize = 1.0
                elif scanInfo[7] == 'Gcdist':
                    xLabelStr = 'Ground distance in km'
                    xGridSize = 100.0

                yLabelStr = 'Altitude (km)'
                if xGridSize == None:
                    xGridSize = 1.0
                if yGridSize == None:
                    yGridSize = 20.0
                
            else:
                if scanInfo[2] > scanInfo[5]:
                    ind = 'ccw'
                    direction = 1
                else:
                    ind = 'cw'
                    direction = 0
                titleStr = 'Az %s scan (%s) %s-%s, el=%i' % (plotParm,
                                                             ind,
                                                             startDateStr,
                                                             endDateStr,
                                                             int(scanInfo[3]))
                timeStr = '%s-%s' % (startDateStr, endDateStr)
                metaStr = 'Az scan: startTime %i az %f el %f stopTime %i az %f el %f dir %i' % (scanInfo[1],
                                                                                                scanInfo[2],
                                                                                                scanInfo[3],
                                                                                                scanInfo[4],
                                                                                                scanInfo[5],
                                                                                                scanInfo[6],
                                                                                                direction)

                retList.append((timeStr,metaStr))

                xLabelStr = 'Longitude'
                yLabelStr = 'Geodetic latitude'
                xGridSize = 1.0
                yGridSize = 1.0

            name = '%s_%06i.png' % (fullFilenameTemplate, scanCount)

            try:
                madPcolorScan(scanInfo[0],
                              xGridSize,
                              yGridSize,
                              titleStr,
                              xLabelStr,
                              yLabelStr,
                              name,
                              minColormap = minColormap,
                              maxColormap = maxColormap,
                              colorMap = cmap)
            except:
                print 'Problem creating scan %s' % (str(retList[-1]))
                traceback.print_exc()
                scanFailureCount += 1
                retList.remove(retList[-1])

        return (retList)
    



    def getDateStrFromUT(self, ut):
        """getDateStrFromUT returns a date string formated as YYYY-MM-DD HH:MM:SS from a ut time
        (seconds since 1/1/1950)
        """
        timeList = madrigal._Madrec.getDateFromUt(float(ut))
        return '%04i-%02i-%02i %02i:%02i:%02i' % (timeList[0],
                                                  timeList[1],
                                                  timeList[2],
                                                  timeList[3],
                                                  timeList[4],
                                                  timeList[5])


    def getTimeStrFromUT(self, ut):
        """getTimeStrFromUT returns a time string formated as HH:MM:SS from a ut time
        (seconds since 1/1/1950)
        """
        timeList = madrigal._Madrec.getDateFromUt(float(ut))
        return '%02i:%02i:%02i' % (timeList[3],
                                   timeList[4],
                                   timeList[5])



    def createScanInfoList(self, isprintStr):
        """createScanInfoList creates a list of tuples, which each tuple representing a single scan, and
        values of:

        (isprintString, startUT, startAz, startEl, endUT, endAz, endEl, type,
        minGdlat, maxGdlat,minGlon, maxGdlon,minGdalt, maxGdalt, minGcdist, maxGcdist).  Isprint string
        has values (x,y,plotParm), where for az scan x=lon, y=lat, and for el scan y=alt, x=lat if starting
        az within 15 degrees of north or south, x=lon if starting az within 15 degrees of east or west,
        of x=gcdist if other az. Type is Gdlat or Glon or Gcdist for el scans, None for az scans.

        Input isprint string has parameters ut1,scntyp,cycn,<parm>,gdalt,gdlat,glon,azm,elm,gcdist

        All azimuth scans from a single cycle are combined.  For elevation scans, there will be zero or one 
        north-south scans, zero or one east-west scans, and zero or more off azimuth scans.  An off azimuth
        scan is not within 15 degrees of north, south, east or west.  Off azimuth scans are not combined with
        scans 180 degrees in the other direction, because the x axis is ground distance, which does not reverse
        sign.
        """
        isprintItems = isprintStr.split()

        retList = []

        # loop variables
        thisIsprintStr = []
        cycNum = None
        startUT = []
        startAz = []
        startEl = []
        endUT = []
        endAz = []
        endEl = []
        elScan = []
        minGdlat = []
        maxGdlat = []
        minGlon = []
        maxGlon = []
        minGdalt = []
        maxGdalt = []
        minGcdist = []
        maxGcdist = []
        

        for i in range(len(isprintItems)):
            item = isprintItems[i]
            mod = i % 10
            if mod == 0:
                thisUt = float(item)
            elif mod == 1:
                thisScntyp = int(item)
            elif mod == 2:
                thisCycn = int(float(item))
                if cycNum == None:
                    cycNum = thisCycn
            elif mod == 3:
                thisParm = item
            elif mod == 4:
                thisGdalt = float(item)
            elif mod == 5:
                thisGdlat = float(item)
            elif mod == 6:
                thisGlon = float(item)
            elif mod == 7:
                thisAz = float(item)
            elif mod == 8:
                thisEl = float(item)
            elif mod == 9:
                thisGcdist = float(item)

                # if new cycle, close out value if data exists
                if thisCycn != cycNum:
                    for i in range(len(thisIsprintStr)):
                        retList.append((thisIsprintStr[i],
                                        startUT[i], startAz[i], startEl[i], endUT[i], endAz[i], endEl[i], elScan[i],
                                        minGdlat[i], maxGdlat[i], minGlon[i], maxGlon[i], minGdalt[i], maxGdalt[i],
                                        minGcdist[i], maxGcdist[i]))
                    thisIsprintStr = []
                    cycNum = None
                    startUT = []
                    startAz = []
                    startEl = []
                    endUT = []
                    endAz = []
                    endEl = []
                    elScan = []
                    minGdlat = []
                    maxGdlat = []
                    minGlon = []
                    maxGlon = []
                    minGdalt = []
                    maxGdalt = []
                    minGcdist = []
                    maxGcdist = []

                # end of line - process it
                if self.scanType == 'el' and thisScntyp == 3:
                    # add this elevation scan point
                    # check type of el scan
                    if -15.0 <= thisAz <= 15.0 or -165.0 >= thisAz or thisAz >= 165.0:
                        thisElScan = 'Gdlat'
                    elif 75.0 <= thisAz <= 105.0 or -105.0 <= thisAz <= -75.0:
                        thisElScan = 'Glon'
                    else:
                        thisElScan = 'Gcdist'
                    # get index of this scan, None if a new scan
                    index = None
                    for i, item in enumerate(elScan):
                        if item == 'Gdlat' and thisElScan == 'Gdlat':
                            index = i
                            break
                        elif item == 'Glon' and thisElScan == 'Glon':
                            index = i
                            break
                        elif item == 'Gcdist' and thisElScan == 'Gcdist' and int(thisAz) == int(startAz[i]):
                            index = i
                            break
                    if index == None:
                        # a new scan has been found
                        thisIsprintStr.append('')
                        startUT.append(thisUt)
                        startAz.append(thisAz)
                        startEl.append(thisEl)
                        endUT.append(None)
                        endAz.append(None)
                        endEl.append(None)
                        elScan.append(thisElScan)
                        minGdlat.append(None)
                        maxGdlat.append(None)
                        minGlon.append(None)
                        maxGlon.append(None)
                        minGdalt.append(None)
                        maxGdalt.append(None)
                        minGcdist.append(None)
                        maxGcdist.append(None)
                        index = len(thisIsprintStr) - 1
                            
                    endUT[index] = thisUt
                    endAz[index] = thisAz
                    endEl[index] = thisEl
                    if thisElScan == 'Gdlat':
                        thisIsprintStr[index] += '%f %f %s\n' % (thisGdlat, thisGdalt, thisParm)
                    elif thisElScan == 'Glon':
                        thisIsprintStr[index] += '%f %f %s\n' % (thisGlon, thisGdalt, thisParm)
                    else:
                        thisIsprintStr[index] += '%f %f %s\n' % (thisGcdist, thisGdalt, thisParm)

                elif thisScntyp == 2:
                    # az scan

                    # add this azimuth scan point
                    # get index of this scan, None if a new scan
                    index = None
                    for i, item in enumerate(elScan):
                        if item == None:
                            index = i
                            break
                    if index == None:
                        # a new azimuth scan has been found
                        thisIsprintStr.append('')
                        startUT.append(thisUt)
                        startAz.append(thisAz)
                        startEl.append(thisEl)
                        endUT.append(None)
                        endAz.append(None)
                        endEl.append(None)
                        elScan.append(None)
                        minGdlat.append(None)
                        maxGdlat.append(None)
                        minGlon.append(None)
                        maxGlon.append(None)
                        minGdalt.append(None)
                        maxGdalt.append(None)
                        minGcdist.append(None)
                        maxGcdist.append(None)
                        index = len(thisIsprintStr) - 1
                            
                    endUT[index] = thisUt
                    endAz[index] = thisAz
                    endEl[index] = thisEl
                    thisIsprintStr[index] += '%f %f %s\n' % (thisGlon, thisGdlat, thisParm)
                    
                else:
                    # not a scan line
                    continue

                if minGdlat[index] == None:
                    minGdlat[index] = thisGdlat
                    maxGdlat[index] = thisGdlat
                    minGlon[index] = thisGlon
                    maxGlon[index] = thisGlon
                    minGdalt[index] = thisGdalt
                    maxGdalt[index] = thisGdalt
                    minGcdist[index] = thisGcdist
                    maxGcdist[index] = thisGcdist
                else:
                    if minGdlat[index] > thisGdlat:
                        minGdlat[index] = thisGdlat
                    if maxGdlat[index] < thisGdlat:
                        maxGdlat[index] = thisGdlat
                    if minGlon[index] > thisGlon:
                        minGlon[index] = thisGlon
                    if maxGlon[index] < thisGlon:
                        maxGlon[index] = thisGlon
                    if minGdalt[index] > thisGdalt:
                        minGdalt[index] = thisGdalt
                    if maxGdalt[index] < thisGdalt:
                        maxGdalt[index] = thisGdalt
                    if minGcdist[index] > thisGcdist:
                        minGcdist[index] = thisGcdist
                    if maxGcdist[index] < thisGcdist:
                        maxGcdist[index] = thisGcdist

        # end, close out last cycle
        for i in range(len(thisIsprintStr)):
            retList.append((thisIsprintStr[i],
                            startUT[i], startAz[i], startEl[i], endUT[i], endAz[i], endEl[i], elScan[i],
                            minGdlat[i], maxGdlat[i], minGlon[i], maxGlon[i], minGdalt[i], maxGdalt[i],
                            minGcdist[i], maxGcdist[i]))

        return(retList)

    
class madHistogram:
    """madHistogram is the class that produces Histogram plots of Madrigal
        Data.

        Usage example::

            obj = madHistogram(isprintText,
                                'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2005 - Alt. code',
                                'Hours since midnight UT Oct. 30,2003'
                                'Altitude',
                                './isprint.png'
                                size = 'large')

        Inputs:

            isprintText - a string giving isprint output without headers. Any missing data should
                            be written as 'missing' or other string that cannot be converted into
                            a float.  Only one column.

            TitleStr - plot title(string) - should describe the plot being made

            xLabelStr - Label for x axis

            fullFilename - full path of file containing histogram plot to be saved. Extension must be .jpeg
                            or .png, or exception thrown

            size - size of plot to be saved. Must be 'small','wide', or 'large'. Defaults to small

            maxNumPoints - maximum number of points to be considered. Defaults to None, so all points in string shown


            numBins - number of bins to be used to store the data. More bins means closer resolution in Histogram.
                        Default is 30

            orientation - whether histogram shows horizontal or vertical. Must be written as 'horizontal' or
                            'vertical', or exception thrown--defaults to 'vertical'

            isNorm - whether histogram should be normalized or not. Default is 0, or False

            isBottom - If true, sets the lowest passed value as zero

            sdevs - number of standard deviations to take into account. When 0 is passed to it, this option is ignored and all data
                    is included in the distribution; otherwise, the range of values accepted is sdevs*(standard deviation of sample)

        Outputs:

            A .png file is written to the fullFilename path given, resulting in a Histogram drawn by matplotlib

            
        Change History:

            Written by "Brandon Scott Fines":mailto:bfines@haystack.mit.edu Aug. 1,2006
            Written by "Bill Rideout":mailto:brideout@haystack.mit.edu Aug. 1,2006
 
    """
    def __init__(self,isprintText,
                 titleStr,
                 xLabelStr,
                 fullFilename,
                 size = 'small',
                 maxNumPoints = None,
                 numBins = 30,
                 Orientation='vertical',
                 isNorm =0,
                 isBottom=0,
                 sdevs=0):
        """__init__ writes a madHistogram string to file

        """
        self.__missing = 1.0E30 #special value, since numpy doesn't Handle NaN
        self.__parameter_count = 2

        #verify input
        if not size in ('small','wide','large'):
            raise ValueError, 'size must be "small","wide" or "large", not %s'%(str(size))

        if size in ('small','wide'):
            fontSize = 12
        elif size in ('large'):
            fontSize = 18

        # remove all non-float values
        items = isprintText.split()
        keptItems = []
        for item in items:
            try:
                float(item)
                keptItems.append(item)
            except:
                continue
        delimiter = ' '
        isprintText = delimiter.join(keptItems)

        if maxNumPoints !=None:
            isprintText = self._truncateIsprint(isprintText, maxNumPoints)

        #convert the input data into numeric arrays of floats assuming no headers and filter the missing values

        try:
            split_data = isprintText.split()
            float_data = map(self.__filter_missing,split_data)
            array_data = matplotlib.numerix.asarray(float_data)
        except:
            traceback.print_exc()
            raise ValueError, 'input text is not parseable'

        #if needed, throw away outliers
        if sdevs !=0:
            array_data = self.removeOutliers(sdevs,array_data)

        #select the plot size
        if size=='small':
            matplotlib.pylab.figure(1,figsize=(6,4), facecolor = 'w')
        elif size=='wide':
            matplotlib.pylab.figure(1,figsize=(10,4),facecolor='w')
        elif size=='large':
            matplotlib.pylab.figure(1,figsize=(12,6),facecolor='w')


        matplotlib.pylab.hist(array_data,bins = numBins,normed=isNorm,bottom=isBottom,orientation = Orientation)

        thisFont = {'size': fontSize}
        matplotlib.pylab.rc('font', **thisFont) #pass in the font dict as kwargs

        matplotlib.pylab.xlabel(xLabelStr, fontsize=fontSize)
        matplotlib.pylab.title(titleStr,fontsize=fontSize)

        #self.__figure = matplotlib.pylab.gcf()

        matplotlib.pylab.savefig(fullFilename)

        matplotlib.pylab.clf()

    def __truncateIsprintStr(self,isprintText, maxLines):
        """__truncateIsprintStr truncates isprintText to have maxLines at most.
        """
        isprintList = isprintText.split('\n')
        if len(isprintList)<maxLines:
            return isprintText
        else:
            dropNumber = int(1+len(isprintList)/maxlines)
            newline = '\n'
            newIsprintText = newline.join(isprintList[::dropNumber])

            return newIsprintText

    def __filter_missing(self,x):
        try:
            return float(x)
        except:
            return self.__missing

    def removeOutliers(self,ndevs,values=[]):

        
        #find the mean value in values
        sums = 0.0
        count = 0
        for i in values:
            sums = sums+i
            count = count+1


        mean = sums/count
        

        #calculate a standard deviation
        innersum = 0
        for i in values:
            innersum = innersum + math.pow((i-mean),2)

        sdev = math.sqrt((1.0/count)*innersum)

        #throw away all data points falling outside three standard deviations of the mean, then return the resulting list
        retArray = []
        ndist = mean-ndevs*sdev
        pdist = mean+ndevs*sdev
        for i in values:
            if (i>=ndist) and (i<=pdist):
                retArray.append(i)

        return retArray

    #end eliminateOutliers()


class madIsrRecordSummary:
    """madIsrRecordSummary is the class that produces a summary plot of a single ISR record.


    """
    def __init__(self, TiData,
                 TiError,
                 TeData,
                 TeError,
                 VoData,
                 VoError,
                 NelData,
                 NelError,
		 isPopl,
                 description,
                 fullFilename,
                 useRange = False,
                 altResl = None):
        """__init__ writes a madIsrRecordSummary  to a file.

        Inputs:

            TiData - an Nx2 numeric array of Ti (col 0) and altitude in km (col 1)

            TiError - an N length array of error in Ti
            
            TeData - an Nx2 numeric array of Te (col 0) and altitude in km (col 1)

            TeError - an N length array of error in Te

            VoData - an Nx2 numeric array of Vo (col 0) and altitude in km (col 1)

            VoError - an N length array of error in Vo

            NelData - an Nx2 numeric array of (Popl or Nel in m^-3) (col 0) and altitude in km (col 1)

            NelError - an 2xN length array of lower, upper error in Popl or Nel
            
            isPopl - true if Nel contains Popl data, false if Nel data

            description - text to put in fourth panel of plot - use carriage returns to separate lines

            fullFilename - full filename to save output png file.

            useRange - if False (the default), y axis = Altitude.  If True, y axis = Range.

            altResl - altitude resolution in km.  If not None, show altitude resolution on each graph.
        
        
        Returns: void

        Affects: None
        """
        if useRange:
            yAxisStr = 'Range (km)'
        else:
            yAxisStr = 'Altitude (km)'
            
        # upper left - Ti and Te
        fig = matplotlib.pylab.figure(figsize=(10,10))
        axis = matplotlib.pylab.subplot(2,2,1)
        if TiData.shape[0] > 0 and TeData.shape[0] > 0:
            axis.errorbar(TiData[:,0], TiData[:,1], xerr=TiError, fmt='bo-')   
            axis.errorbar(TeData[:,0], TeData[:,1], xerr=TeError, fmt='rD-')
            matplotlib.pylab.text(0.65, 0.1, 'Ti: blue circle\nTe: red diamond',
                                  horizontalalignment='left',
                                  verticalalignment='top',
                                  transform = axis.transAxes,
                                  fontsize=10)
            matplotlib.pylab.text(0.5,0.5, 'Preliminary data\n\n\nfrom the\n\n\nMadrigal database',
                                  horizontalalignment='center',
                                  verticalalignment='center',
                                  fontsize=18,
                                  transform = axis.transAxes, alpha=0.2)
            matplotlib.pylab.xlabel('Temperature (K)')
            matplotlib.pylab.ylabel(yAxisStr)
            matplotlib.pylab.grid(True)
            # set limits based on data only
            tiMax = max(TiData[:,0])
            tiMin = min(TiData[:,0])
            teMax = max(TeData[:,0])
            teMin = min(TeData[:,0])
            lowerLimit = (int(min(tiMin, teMin)) / 1000) * 1000
            upperLimit = (1+int(max(tiMax, teMax)) / 1000) * 1000            
            
            # alt resolution
            if altResl != None:
                altLabel = 'Alt res: \n%i km' % (int(altResl))
                y_min, y_max = matplotlib.pylab.ylim()
                startX = lowerLimit + 0.8*(upperLimit - lowerLimit)
                startY = 0.15
                reslPercentage = altResl/(y_max-y_min)
                matplotlib.pylab.axvline(x=startX,
                                         ymin=startY,
                                         ymax=reslPercentage + startY,
                                         linewidth=2, color='b')
                matplotlib.pylab.text(0.83, 0.2, altLabel,
                                      horizontalalignment='left',
                                      verticalalignment='top',
                                      transform = axis.transAxes,
                                      fontsize=10)
                
            matplotlib.pylab.xlim(lowerLimit, upperLimit)
            
                
                                         

        # upper right - Vo and 10*Vo
        axis = matplotlib.pylab.subplot(2,2,2)
        if VoData.shape[0] > 0:
            axis.errorbar(VoData[:,0], VoData[:,1], xerr=VoError, fmt='bo-')
            axis.errorbar(10.0*VoData[:,0], VoData[:,1], xerr=10.0*VoError, fmt='rD-')
            # set x limits to be even around 0
            voMax = max(10.0*VoData[:,0])
            voMin = min(10.0*VoData[:,0])
            limit = max(abs(voMax), abs(voMin))
            matplotlib.pylab.xlim(-1.0*limit, limit)
            matplotlib.pylab.text(0.58, 0.1, 'Vo: blue circle\n10*Vo: red diamond',
                                  horizontalalignment='left',
                                  verticalalignment='top',
                                  transform = axis.transAxes,
                                  fontsize=10)
            matplotlib.pylab.text(0.5,0.5, 'Preliminary data\n\n\nfrom the\n\n\nMadrigal database',
                                  horizontalalignment='center',
                                  verticalalignment='center',
                                  fontsize=18,
                                  transform = axis.transAxes, alpha=0.2)
            matplotlib.pylab.xlabel('Line of sight drift (m/s)')
            matplotlib.pylab.ylabel(yAxisStr)
            matplotlib.pylab.grid(True)

            # alt resolution
            if altResl != None:
                altLabel = 'Alt res: \n%i km' % (int(altResl))
                y_min, y_max = matplotlib.pylab.ylim()
                startX = limit*0.6
                startY = 0.15
                reslPercentage = altResl/(y_max-y_min)
                matplotlib.pylab.axvline(x=startX,
                                         ymin=startY,
                                         ymax=reslPercentage + startY,
                                         linewidth=2, color='b')
                matplotlib.pylab.text(0.83, 0.2, altLabel,
                                      horizontalalignment='left',
                                      verticalalignment='top',
                                      transform = axis.transAxes,
                                      fontsize=10)
                
            matplotlib.pylab.xlim(-1.0*limit, limit)

            

        # lower left - Nel or Popl
        axis = matplotlib.pylab.subplot(2,2,3)
        if isPopl:
            labelStr = 'Log Power (m^-3)'
        else:
            labelStr = 'Log Electron density (m^-3)'
        if NelData.shape[0] > 0:
            # check that not every point equal
            if max(NelData[:,0]) == min(NelData[:,0]):
                pass
            else:
                axis.errorbar(NelData[:,0], NelData[:,1], xerr=NelError, fmt='bo-')
                matplotlib.pylab.xlabel(labelStr)
                matplotlib.pylab.ylabel(yAxisStr)
                matplotlib.pylab.grid(True)
                nelMax = max(NelData[:,0])
                nelMin = min(NelData[:,0])
                matplotlib.pylab.xlim(math.floor(nelMin), math.ceil(nelMax))
                matplotlib.pylab.text(0.5,0.5, 'Preliminary data\n\n\nfrom the\n\n\nMadrigal database',
                                  horizontalalignment='center',
                                  verticalalignment='center',
                                  fontsize=18,
                                  transform = axis.transAxes, alpha=0.2)
                
                # alt resolution
                if altResl != None:
                    altLabel = 'Alt res: \n%i km' % (int(altResl))
                    y_min, y_max = matplotlib.pylab.ylim()
                    x_min, x_max = matplotlib.pylab.xlim()
                    startX = x_min + 0.8*(x_max-x_min)
                    startY = 0.15
                    reslPercentage = altResl/(y_max-y_min)
                    matplotlib.pylab.axvline(x=startX,
                                             ymin=startY,
                                             ymax=reslPercentage + startY,
                                             linewidth=2, color='b')
                    matplotlib.pylab.text(0.83, 0.2, altLabel,
                                          horizontalalignment='left',
                                          verticalalignment='top',
                                          transform = axis.transAxes,
                                          fontsize=10)

                    matplotlib.pylab.xlim(math.floor(nelMin), math.ceil(nelMax))
        

        # lower right - text
        axis = matplotlib.pylab.subplot(2,2,4)
        matplotlib.pylab.text(0.05, 0.9, description,
                              horizontalalignment='left',
                              verticalalignment='top',
                              transform = axis.transAxes,
                              fontsize=9)
        matplotlib.pylab.xticks([])
        matplotlib.pylab.yticks([])
        
        

        matplotlib.pylab.subplots_adjust(wspace = 0.25)
        

        matplotlib.pylab.savefig(fullFilename)
        #matplotlib.pylab.show()

        matplotlib.pylab.clf()
        matplotlib.pylab.close()

        
if __name__ == '__main__':

    import time

    # test madPcolorScan

    # az scan
    f = open('testFiles/isprint_az_scan.txt')
    isprintText = f.read()
    f.close()

    print 'creating az scan at /tmp/isprint_az_scan.png'
    obj = madPcolorScan(isprintText,
                        1.0, 1.0,
                        'Az scan - NEL 2006-06-26 13:49:43-14:07:36',
                        'Longitude',
                        'Latitude',
                        '/tmp/isprint_az_scan.png',
                        'small',
                        -100,-50,40,60,
                        9,12)
    
    # el scan
    f = open('testFiles/isprint_el_scan.txt')
    isprintText = f.read()
    f.close()
    
    print 'creating el scan at /tmp/isprint_el_scan.png'
    obj = madPcolorScan(isprintText,
                        1.0, 40.0,
                        'El scan - NEL 2006-06-26 13:49:43-14:07:36',
                        'Latitude',
                        'Altitude (km)',
                        '/tmp/isprint_el_scan.png',
                        'small')

    # test scanPlotter
    print 'creating az scan series using mlh060622a.000'
    scanObj = scanPlotter('/home/grail/brideout/madroot/experiments/2006/mlh/22jun06/mlh060622a.000')
    scanObj.plotAllScans('az','/tmp/junkAz', 'Nel')
    
    t = time.time()

    # test madIsrRecordSummary
    TiData = matplotlib.numerix.array([[800, 150],[900, 200],[910, 300], [945, 500], [980, 600]])
    TiError = matplotlib.numerix.array([50,100,50,100,50])
    TeData = matplotlib.numerix.array([[850, 150],[1000, 200],[1800, 300], [2000, 500], [2000, 600]])
    TeError = matplotlib.numerix.array([50,100,50,100,50])
    VoData = matplotlib.numerix.array([[-30, 150],[0, 200],[10, 300], [35, 500], [10, 600]])
    VoError = matplotlib.numerix.array([5,10,5,10,5])
    NelData = matplotlib.numerix.array([[11, 150],[11.5, 200],[11.3, 300], [11, 500], [10.5, 600]])
    NelError = matplotlib.numerix.array(((2.0,0.5),(2.0,1.0), (2.0,0.5), (2.0,1.0), (2.0,0.5)))
    isPopl = True
    text = 'Kinst = Millstone Zenith Antenna\nMore stuff here\nand more\nand more'
    fullFilename = '/tmp/junk.png'
    madIsrRecordSummary(TiData,TiError,TeData,TeError,VoData,VoError,NelData,matplotlib.numerix.transpose(NelError),isPopl,text,fullFilename)


    
    
    f = open('testFiles/isprint_uth_fof2.txt')
    isprintText = f.read()
    f.close()
    
    print 'creating scatterplot at /tmp/isprint_fof2.png'
    obj = madScatterPlot(isprintText,
                        'Fof2 - Millstone Hill - Oct. 30, 2003',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Fof2',
                        '/tmp/isprint_fof2.png',
                        size = 'large',
			useAbsoluteTime = False)
			

    f = open('testFiles/isprint_nel.txt')
    isprintText = f.read()
    f.close()

    print 'creating pcolor plot at /tmp/isprint_nel.png'
    obj = madPcolorPlot(isprintText,
                        'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2003 - Alt code',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Altitude (km)',
                        '/tmp/isprint_nel.png',
                        size = 'large',
                        minColormap = 9,
                        maxColormap = 12,
			smoothAltitude = True,
			insertDataGap = 5,
			useAbsoluteTime = False,
			startTime = None,
			endTime = None,
			sortTimeFlag = False,
			maxNumTimes = None,
			maxNumAlt = 50,
                        truncateIsprint = True,
                        colorMap = matplotlib.cm.spring,
                        altYTitle = 'Latitude',
                        altYLabels = ('40', '41', '42', '43', '44', '45'))
    
			
    print 'took %i secs' % (time.time() - t)
