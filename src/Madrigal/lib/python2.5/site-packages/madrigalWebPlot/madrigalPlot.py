"""madrigalPlot is the module that produces plots of Madrigal data.

It is meant to be included as part of the Remote Python Madrigal API.

Presently based on madplotlib: http://matplotlib.sourceforge.net/


$Id: madrigalPlot.py,v 1.3 2009/04/30 20:32:01 brideout Exp $

"""
import sys,os
import os.path
import traceback
import types
import datetime
import bisect
import math

import madrigalWeb.madrigalWeb


def defineHomeEnvVariable():
    """defineHomeEnvVariable makes sure HOME env variable is defined, as required by matplotlib.

    If not defined, sets HOME to PWD
    """
    try:
        os.environ['HOME']
        return
    except KeyError:
        os.environ['HOME'] = os.getcwd()


defineHomeEnvVariable()
try:
    import matplotlib
except:
    print 'You need to install the numpy and matplotlib python module first.'
    print '    For numpy, go to http://numpy.scipy.org/.'
    print '    For matplotlib, go to http://matplotlib.sourceforge.net/.'
    sys.exit(-1)
# set rendering
matplotlib.use('Agg')
import matplotlib.pylab
import matplotlib.numerix
import matplotlib.colors
import matplotlib.cm



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
                 useAbsoluteTime = False,
                 startTime = None,
                 endTime = None,
                 maxNumPoints = None,
                 yMin=None,
                 yMax=None):
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
        self.__missing = 1.0E30 # special value, since Numeric doesn't handle NaN
        self.__parameter_count = 2

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
        
	    
        # make sure there's valid data
        isValidData = False
        for y in array_data[:,1]:
            if y == self.__missing:
                continue
            isValidData = True
            break
		
        if not isValidData:
            raise ValueError, 'No valid y data found'

      
        matplotlib.pylab.scatter(array_data[:,0],array_data[:,1])

        matplotlib.pylab.xlabel(xLabelStr)
        matplotlib.pylab.ylabel(yLabelStr)
        matplotlib.pylab.yticks()
        matplotlib.pylab.xlim(xMin, xMax)
        if yMin != None:
            matplotlib.pylab.ylim(ymin=yMin)
        if yMax != None:
            matplotlib.pylab.ylim(ymax=yMax)
        matplotlib.pylab.title(titleStr)

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
	    
        matplotlib.pylab.xticks()

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

        


        


class madPcolorPlot:
    """madPcolorPlot is the class that produces pcolor plots of x versus y with z intensity.

    Assumes the x axis is time.

    Usage example::

    	obj = madPcolorPlot(isprintText,
                        'Nel (log(m^-3)) - Millstone Hill - Oct. 30, 2003 - Alt code',
                        'Hours since midnight UT Oct. 30, 2003',
                        'Altitude (km)',
                        './isprint.png',
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
            			  The second must be gdalt, and third parameter to be plotted.
            			  For now missing data is not allowed
            
            titleStr - plot title (string) - should describe parameter being plotted
	    
	        xLabelStr - x label string
	    
	        yLabelStr - ylabel string

            fullFilename - full path of file containing pcolor plot to be saved.  Extension must
                           be jpeg or png, or exception thrown.
            
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


        # set up plotting parameters
        if minColormap == None:
            minColormap = zMin
        if maxColormap == None:
            maxColormap = zMax

        matplotlib.pylab.pcolor(X,Y,Z, shading='flat', vmin=minColormap, vmax=maxColormap, cmap = colorMap, norm = matplotlib.pylab.normalize())
        matplotlib.pylab.colorbar()

        matplotlib.pylab.xlabel(xLabelStr)
        matplotlib.pylab.ylabel(yLabelStr)
        matplotlib.pylab.xlim(xMin, xMax)
        matplotlib.pylab.ylim(yMinimum, yMaximum)
        matplotlib.pylab.yticks()
        matplotlib.pylab.title(titleStr)

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

        matplotlib.pylab.xticks()

        # add second y-axis if desired
        if altYTitle != None and altYLabels != None:
            ax2 = matplotlib.pylab.twinx()
            matplotlib.pylab.ylabel(altYTitle)
            ax2.yaxis.tick_right()
            matplotlib.pylab.yticks(range(len(altYLabels)), altYLabels)
            matplotlib.pylab.yticks()

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

        sortIndex = matplotlib.numerix.argsort(array_data[:,0])

        # if already sorted, just return original array
        if sortIndex == matplotlib.numerix.sort(sortIndex):
            return array_data
        
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

