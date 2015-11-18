#!PYTHONEXE

"""createRecordPlots.py is a script to create all the individual record plots
for an ISR Madrigal file.  Plots Ti, Te, Vi, and Nel (or Popl) vs altitude,
and puts plots in proper place in Madrigal experiment.

usage: createRecordPlots.py <full path to madrigal file>

"""

#$Id: createRecordPlots.py,v 1.8 2007/10/03 12:32:59 brideout Exp $

usage = 'createRecordPlots.py <full path to madrigal file>'

import sys
import os, os.path
import datetime
import traceback
import math

import matplotlib.numerix

import madrigal.metadata
import madrigalWeb.madrigalWeb
import madrigal.ui.madrigalPlot

def getSummaryStr(values, madFile, madInstObj, madKindatObj, rangeResl):
    """getSummaryStr builds the summary string from the isprint values

        Inputs:

            values - a list of strings from a single isprint line

            madFile - madrigal file name being plotted

            madInstObj - a madrigal.metadata.MadrigalInstrument object

            madKindatObj - a madrigal.metadata.MadrigalKindat object

            rangeResl - range resolution. If None, unknown or varying; do not print
            
    """
    retStr = ''
    kinst = int(values[12])
    retStr += 'Instrument: %s\n' % (madInstObj.getInstrumentName(kinst))
    year = int(values[14])
    month = int(values[15])
    day = int(values[16])
    retStr += '%04i-%02i-%02i\n' % (year, month,day)
    bhhmmss = int(values[17])
    ehhmmss = int(values[18])
    bh = bhhmmss / 10000
    bm = (bhhmmss - (bh*10000)) / 100
    bs = bhhmmss % 100
    eh = ehhmmss / 10000
    em = (ehhmmss - (eh*10000)) / 100
    es = ehhmmss % 100
    retStr += '%02i:%02i:%02i - %02i:%02i:%02i\n' % (bh,bm,bs,eh,em,es)
    retStr += 'Record #%i\n' % (int(values[0]))
    kindatStr = madKindatObj.getKindatDescription(int(values[13]))
    if kindatStr != None:
        retStr += 'Kindat: %s\n' % (kindatStr)
    try:
        azm = float(values[19])
        retStr += 'Az = %5.2f\n' % (azm)
    except:
        pass
    try:
        elm = float(values[20])
        retStr += 'El = %5.2f\n' % (elm)
    except:
        pass
    try:
        systmp = float(values[21])
        retStr += 'System temperature = %i K\n' % (int(systmp))
    except:
        pass
    try:
        pl = float(values[22])
        retStr += 'Pulse length = %i microsec\n' % (int(pl*1e6))
    except:
        pass

    try:
        ipp = float(values[23])
        retStr += 'IPP = %i microsec\n' % (int(ipp*1e6))
    except:
        pass

    if rangeResl != None:
        retStr += 'Range resolution = %i km\n' % (int(rangeResl))

    retStr += '\n%s\n' % (os.path.basename(madFile))
    retStr += 'Plotted - %s' % (datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S'))

    # check whether any wrapping required
    replacePointList = []
    totalLen = 0
    lineList = retStr.split('\n')
    for line in lineList:
        if len(line) > 50:
            replacePointList.append(totalLen + line[:50].rfind(' '))
        totalLen += (len(line) + 1)

    for point in replacePointList:
        retStr = retStr[:point] + '\n' + retStr[point+1:]


    return retStr

def getNelError(logNe, logDNe):
    """getNelError is used to convert the Cedar standard logarithm
    of the uncertainty (logDNe), into uncertainty of the logarithm.
    Returns (lower value, upper value)
    """
    Ne = math.pow(10.0, logNe)
    dNe = math.pow(10.0, logDNe)
    lowerNe = Ne-dNe
    if lowerNe < 1.0:
        lowerNe = 1.0
    upperNe = Ne+dNe
    lowerDNe = logNe - math.log10(lowerNe)
    upperDNe = math.log10(upperNe) - logNe
    
    return ((lowerDNe, upperDNe))




############## main script begins here ##############

if len(sys.argv) != 2:
    print usage
    sys.exit(-1)

madFile = sys.argv[1]

if madFile[0] != '/':
    raise 'Full path to madrigal file must be used, not %s' % (str(madFile))

madDir = os.path.dirname(madFile)
madBasename = os.path.basename(madFile)

# create plotDir if needed
plotDir = os.path.join(madDir, 'plots', madBasename, 'records')

try:
    os.makedirs(plotDir)
except:
    pass

madDBObj = madrigal.metadata.MadrigalDB()
madInstObj = madrigal.metadata.MadrigalInstrument(madDBObj)
madKindatObj = madrigal.metadata.MadrigalKindat(madDBObj)

url = madDBObj.getTopLevelUrl()

# create madrigalWeb Object
madWebObj = madrigalWeb.madrigalWeb.MadrigalData(url)

parameters = 'recno,gdalt,ti,dti,te,dte,vo,dvo,popl,dpopl,nel,dnel,kinst,kindat,' + \
             'year,month,day,bhhmmss,ehhmmss,azm,elm,systmp,pl,ipp,resl'
user_fullname = 'administrator'
user_email = 'admin'
user_affiliation = 'local'
filters = ''

# get data
data = madWebObj.isprint(madFile,
                         parameters,
                         filters,
                         user_fullname,
                         user_email,
                         user_affiliation)

data = data.strip()

# parse data into numeric arrays
tiList = []
tiErrList = []
teList = []
teErrList = []
voList = []
voErrList = []
poplList = []
poplErrList = []
nelList = []
nelErrList = []
lines = data.split('\n')
recno = None
values = None
prevRangeResl = None # check if range resolutions vary
rangeReslOkay = True
thisEl = None
count = 0 # print records processed
for i in range(len(lines)):
    line = lines[i]
    lastValues = values
    values = line.split()
    if len(values) < 3:
        continue
    nextRecno = int(values[0])

    try:
        nextRangeResl = int(round(float(values[24])))
        if prevRangeResl == None:
            rangeReslOkay = True
            prevRangeResl = nextRangeResl
    except:
        nextRangeResl = None
        rangeReslOkay = False
    try:
        thisEl = float(values[20])
        thisAltResl = round(prevRangeResl*math.sin(math.radians(thisEl)))
    except:
        thisAltResl = None
    if not rangeReslOkay:
        thisAltResl = None
    if recno == None:
        recno = nextRecno
    if prevRangeResl != nextRangeResl and recno == nextRecno:
            # range resolutions are changing
            rangeReslOkay = False
    if nextRecno != recno or i == len(lines)-1:
        # plot existing record
        summaryStr = getSummaryStr(lastValues, madFile, madInstObj, madKindatObj, prevRangeResl)
        tiArray = matplotlib.numerix.array(tiList)
        tiErrArray = matplotlib.numerix.array(tiErrList)
        teArray = matplotlib.numerix.array(teList)
        teErrArray = matplotlib.numerix.array(teErrList)
        voArray = matplotlib.numerix.array(voList)
        voErrArray = matplotlib.numerix.array(voErrList)
        if len(poplList) > 0:
            poplArray = matplotlib.numerix.array(poplList)
            poplErrArray = matplotlib.numerix.array(poplErrList)
            isPopl = True
        else:
            poplArray = matplotlib.numerix.array(nelList)
            poplErrArray = matplotlib.numerix.array(nelErrList)
            isPopl = False
        
        try:
            obj = madrigal.ui.madrigalPlot.madIsrRecordSummary(tiArray,
                                                               tiErrArray,
                                                               teArray,
                                                               teErrArray,
                                                               voArray,
                                                               voErrArray,
                                                               poplArray,
                                                               matplotlib.numerix.transpose(poplErrArray),
                                                               isPopl,
                                                               summaryStr,
                                                               os.path.join(plotDir, 'plot%05i.png' % (recno+1)),
                                                               altResl=thisAltResl)
        except:
            traceback.print_exc()
            print 'plot %i raised error' % (count)
            
        obj = None
        prevRangeResl = None
        print count
        count += 1
                                                     
        tiList = []
        tiErrList = []
        teList = []
        teErrList = []
        voList = []
        voErrList = []
        poplList = []
        poplErrList = []
        nelList = []
        nelErrList = []

    recno = nextRecno
    try:
        gdalt = float(values[1])
    except:
        continue
    try:
        ti = float(values[2])
        tiErr = float(values[3])
        tiList.append((ti, gdalt))
        tiErrList.append(tiErr)
    except:
        pass
    try:
        te = float(values[4])
        teErr = float(values[5])
        teList.append((te, gdalt))
        teErrList.append(teErr)
    except:
        pass
    try:
        vo = float(values[6])
        voErr = float(values[7])
        voList.append((vo, gdalt))
        voErrList.append(voErr)
    except:
        pass
    try:
        popl = float(values[8])
        try:
            poplErr = float(values[9])
        except:
            poplErr = 0.0 # no error data
        poplList.append((popl, gdalt))
        poplErrList.append(getNelError(popl, poplErr))
    except:
        pass
    try:
        nel = float(values[10])
        try:
            nelErr = float(values[11])
        except:
            nelErr = 0.0 # no error data
        nelList.append((nel, gdalt))
        nelErrList.append(getNelError(nel, nelErr))
    except:
        pass
        












