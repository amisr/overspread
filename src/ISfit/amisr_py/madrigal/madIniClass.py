#!/usr/bin/env python

"""

"""

import 

class BatchExperiment:
    """BatchExperiment is a class to create and update Madrigal experiments
    """

    # defines length of line in Cedar catalog/header file
    __CEDAR_LEN__ = 80 

    def __init__(self):
        return

    def uploadExperiment(self,iniFile,plotsdir='plots'):

        # create needed Madrigal objects
        self.madDBObj = madrigal.metadata.MadrigalDB()
        madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)  

        # read ini file
        self.__iniData__ = ConfigParser.ConfigParser()
        self.__iniData__.read(iniFile)

        # get reqiured experiment info
        expTitle = self.__iniData__.get('Experiment', 'title')
        self.instrument = int(self.__iniData__.get('Experiment', 'instrument'))
        logFile = self.__iniData__.get('Experiment', 'logFile')
        expId = self.__iniData__.get('Experiment', 'expId')
        OutPath = self.__iniData__.get('Experiment','OutPath')

        # parse the expId
        try:
            items = expId.split('.')
            date = int(items[0])
            num = int(items[1])
            expId = items[0] + '.' + str(self.instrument) + '.' + items[1]
        except:
            traceback.print_exc()
            raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))      

        # find the number of files being created
        numFiles = 0
        while True:
            try:
                self.__iniData__.get('File%i' % (numFiles + 1), 'hdf5Filename')
                numFiles += 1
            except ConfigParser.NoSectionError:
                break

        # get optional character, if any
        optChar = parseExpId(expId)[3]
            
        # next find the time range in the data
        firstTime = None
        lastTime = None
        for fileNum in range(numFiles):

            self.fileSection = 'File%i' % (fileNum + 1)

            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')

            fileHandler = hdf5Handler(hdf5Type)

            startTime, endTime = fileHandler.getStartEndTimes(hdf5Filename)

            if firstTime == None:
                firstTime = startTime
            elif firstTime > startTime:
                firstTime = startTime

            if lastTime == None:
                lastTime = endTime
            elif lastTime < endTime:
                lastTime = endTime
        
        # create new madrigal file name template
        instMnemonic = self.madInstObj.getInstrumentMnemonic(self.instrument).lower()
        madFilenameTemplate = '%s%02i%02i%02i.' % (instMnemonic, firstTime.year % 100,
                                                   firstTime.month, firstTime.day)
            
        madAdminObj = madrigal.admin.MadrigalDBAdmin()
        for fileNum in range(numFiles):

            self.fileSection = 'File%i' % (fileNum + 1)
            madFilename = madFilenameTemplate + '%03i' % (fileNum + 1)
            fullMadFilename = os.path.join(OutPath,madFilename)
            
            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')
            status = self.__iniData__.get(self.fileSection, 'status')
            category = int(self.__iniData__.get(self.fileSection, 'category'))
            fileDesc=status

            shutil.copyfile(fullMadFilename, os.path.join('/tmp',madFilename))
            fullMadFilename=os.path.join('/tmp',madFilename)

            if fileNum==0:# create the experiment
                try:
                    expPath = madAdminObj.createMadrigalExperiment(fullMadFilename, expTitle, 0, fileDesc,None,category=category,optChar=optChar)
                except IOError:
                    x,y,z=sys.exc_info()
                    print y
                    expPath=str(y).split()[1]
                    info=raw_input('Okay to Remove ' + expPath + '? type Yes: ')
                    if info=='Yes':
                        distutils.dir_util.remove_tree(expPath+'/',verbose=1)
                        expPath = madAdminObj.createMadrigalExperiment(fullMadFilename, expTitle, 0, fileDesc,None,category=category,optChar=optChar)
                    else:
                        raise IOError, y
            else: 
                madAdminObj.addMadrigalFile(expPath,fullMadFilename,0, fileDesc,category=category,kindat=None)
    
            # see if links to images are desired
            numLinks = 0
            while True:
                try:
                    imageTitle = self.__iniData__.get(self.fileSection, 'imageTitle%i' % (numLinks + 1))
                    image = self.__iniData__.get(self.fileSection, 'image%i' % (numLinks + 1))
                    self.createLink(imageTitle, image, expPath)
                    logging.info('Created link with file %s' % (image))

                    numLinks += 1
                    
                except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
                    break    

        """
        names = os.listdir(os.path.join(OutPath,plotsdir))
        for name in names:
            src = os.path.join(OutPath,plotsdir, name)
            dst = os.path.join(expPath, name)
            shutil.copyfile(src, dst)
        """

        self.expPath = expPath

        os.remove(fullMadFilename)

        return(expPath)

    def createNewExperimentFromIni(self,
                                   iniFile):
        """createNewExperimentFromIni will try to create a single new Madrigal experiment using
        information parsed from an input ini file.

        This method will also allow importing summary plots created outside this script into Madrigal.
        It will also allow the option of the automatic creation of individual record plots.

        Example ini file:

        [Experiment]

        title:              World Day
        instrument:         61
        logFile:            $BASE/amisr/pokerflat/face1/20071011-120000/log.txt
        expId:              20071011.61.000
        pi:                 Craig Heiselman
        modexp:             This is a one line experiment title
        cmodexp:            In this section you can write a multi-line description
                            of your experiment.  An ini file recognizes multiline
                            descriptions as long as every continuation line begins
                            with whitespace (as in this example).

        [File1]

        hdf5Filename:       $BASE/amisr/pokerflat/face1/20071011-120000/20071011.004_lp_3min.h5
        kindat:             5950
        type:               standard
        createRecPlots:     True
        imageTitle1:        Electron density - long pulse - beam 1
        image1:             /tmp/elecDensBeam1_lp.jpg
        imageTitle2:        Electron density - long pulse - beam 2
        image2:             /tmp/elecDensBeam2_lp.jpg
        ckindat:            In this section you can write a multi-line description
                            of how this particular file was created.  An ini file
                            recognizes multiline descriptions as long as every continuation line begins
                            with whitespace (as in this example)

        [File2]

        hdf5Filename:       $BASE/amisr/pokerflat/face1/20071011-120000/20071011.004_ac_3min.h5
        kindat:             5951
        type:               standard
        createRecPlots:     False
        imageTitle1:        Electron density - alternating code - beam 1
        image1:             /tmp/elecDensBeam1_ac.jpg
        imageTitle2:        Electron density - alternating code - beam 2
        image2:             /tmp/elecDensBeam2_ac.jpg

        [File3]

        hdf5Filename:       $BASE/amisr/pokerflat/face1/20071011-120000/20071011.004_lp_3min-vvels.h5
        kindat:             5953
        type:               velocity

        The input ini file is made up of an [Experiment] section and one or more [File*] section.
        No time values are used here. since the experiment times will be determined from the data itself.
        The names title, instrument, logFile, and expId are all required in the Experiment section.
        The expId name must be in the form YYYYMMDD.<inst_code>.<number>, where
        the date represents the first day of the experiment, <inst_code> is the instrument
        code, and the trailing <number> is between 0 and 26.

        In addition to the required fields in the Experiment section, there are also some optional
        fields designed to add experiment level information to the files' catalog record:

            pi - the experiment principle investigator
            
            modexp - a short experiment title

            cmodexp - a full description of the experiment.  These fields describe the experiment
                as a whole, and so will be the same for each file.  The field cmodexp will
                typically be multiple lines.  It is legal to have multiline values in an ini
                file as long as each new line begins with some white space.  

        For each file to be added to the experiment, a section called File* is required, and the
        numbering must be in increasing order starting with 1.  The names hdf5Filename, kindat, and type
        are required.  It is highly recommended that every file in an experiment have a unique
        kindat, because the kindat description is how the user determines the differences between
        files.  Madrigal administrators can always add additional kindats by editing the
        $MADROOT/metadata.typeTab.txt file (kindat descriptions must not contain commas).
        type deterimines the type of hdfs being loaded.  Presently supported types are standard,
        velocity, and uncorrected_ne_only.

        In addition to the required fields in the File* section, there are also some optional fields:

            createRecPlots -If set to True, the createRecPlots name will allow the creation of
                individual record plots. If not given or False, these plots will not be created.
                If type != standard, createRecPlots is ignored, since only works with standard data.

            imageTitle%i and image%i - must be given as a pair with matching numbers.  Allows images
                relating to that experiment to be imported into Madrigal.  The user will see a link to
                the imported image with text set by imageTitle.

            ckindat - a description of how this particular file was processed.  Will be included in the
                header record prepended to the file.  The field ckindat will typically be multiple lines.
                It is legal to have multiline values in an ini file as long as each new line begins
                with some white space.

            lowerRange - sets a upper range cutoff in km for uncorrected_ne_only files

            lowerRange - sets a upper range cutoff in km for uncorrected_ne_only files
        """

        # create needed Madrigal objects
        self.madDBObj = madrigal.metadata.MadrigalDB()
        madExpObj = madrigal.metadata.MadrigalExperiment(self.madDBObj)
        self.madInstObj = madrigal.metadata.MadrigalInstrument(self.madDBObj)        
        
        # read ini file
        self.__iniData__ = ConfigParser.ConfigParser()
        self.__iniData__.read(iniFile)

        # get reqiured experiment info
        expTitle = self.__iniData__.get('Experiment', 'title')
        self.instrument = int(self.__iniData__.get('Experiment', 'instrument'))
        logFile = self.__iniData__.get('Experiment', 'logFile')
        expId = self.__iniData__.get('Experiment', 'expId')
        OutPath = self.__iniData__.get('Experiment','OutPath')

        # parse the expId
        try:
            items = expId.split('.')
            date = int(items[0])
            num = int(items[1])
            expId = items[0] + '.' + str(self.instrument) + '.' + items[1]
        except:
            traceback.print_exc()
            raise ValueError, 'expId not in form YYYYMMDD.<inst_code>.<number>: <%s>' % (str(expId))        

        logging.basicConfig(level=logging.DEBUG,
                            format='%(asctime)s %(levelname)s %(message)s',
                            filename=logFile,
                            filemode='w')

        logging.info('Creating exp using ini file %s with instrument %i and title <%s> and expId <%s>' % \
                     (iniFile,  self.instrument, expTitle, expId))

        # find the number of files being created
        numFiles = 0
        while True:
            try:
                self.__iniData__.get('File%i' % (numFiles + 1), 'hdf5Filename')
                numFiles += 1
            except ConfigParser.NoSectionError:
                break

        if numFiles == 0:
            raise IOError, 'No File* section specified in ini file'

        # next find the time range in the data
        firstTime = None
        lastTime = None
        for fileNum in range(numFiles):

            self.fileSection = 'File%i' % (fileNum + 1)

            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')

            fileHandler = hdf5Handler(hdf5Type)

            startTime, endTime = fileHandler.getStartEndTimes(hdf5Filename)

            if firstTime == None:
                firstTime = startTime
            elif firstTime > startTime:
                firstTime = startTime

            if lastTime == None:
                lastTime = endTime
            elif lastTime < endTime:
                lastTime = endTime
        
        # create new madrigal file name template
        instMnemonic = self.madInstObj.getInstrumentMnemonic(self.instrument).lower()
        madFilenameTemplate = '%s%02i%02i%02i.' % (instMnemonic, firstTime.year % 100,
                                                   firstTime.month, firstTime.day)

        # header
        try: principleInvestigator = self.__iniData__.get('Experiment', 'pi')
        except: principleInvestigator = None
        try: expPurpose = self.__iniData__.get('Experiment', 'modexp')
        except: expPurpose = None
        try: expMode = self.__iniData__.get('Experiment', 'cmodexp')
        except: expMode = None
        try: cycleTime = self.__iniData__.get('Experiment', 'cycletime')
        except: cycleTime = None
        try: correlativeExp = self.__iniData__.get('Experiment', 'correxp')
        except: correlativeExp = None
        try: sciRemarks = self.__iniData__.get('Experiment', 'remarks')
        except: sciRemarks = None

        # loop through all the files, and add to madrigal
        for fileNum in range(numFiles):
            self.fileSection = 'File%i' % (fileNum + 1)
            madFilename = madFilenameTemplate + '%03i' % (fileNum + 1)
            fullMadFilename = os.path.join(OutPath,madFilename)
            hdf5Filename = self.__iniData__.get(self.fileSection, 'hdf5Filename')
            kindat = int(self.__iniData__.get(self.fileSection, 'kindat'))
            hdf5Type = self.__iniData__.get(self.fileSection, 'type')
            try:
                thisLowerRange = float(self.__iniData__.get(self.fileSection, 'lowerRange'))
            except:
                thisLowerRange = None
            try:
                thisUpperRange = float(self.__iniData__.get(self.fileSection, 'upperRange'))
            except:
                thisUpperRange = None

            # add file
            fileHandler = hdf5Handler(hdf5Type)
            fileHandler.createMadrigalFile(hdf5Filename, self.instrument, kindat, None, fullMadFilename, thisLowerRange, thisUpperRange)
            
            # header
            if writeHeader:
                try: kindatDesc = self.__iniData__.get(self.fileSection, 'ckindat')
                except: kindatDesc = None
                try: analyst = self.__iniData__.get(self.fileSection, 'analyst')
                except: analyst = None
                try: comments = self.__iniData__.get(self.fileSection, 'comments')
                except: comments = None
                try: history = self.__iniData__.get(self.fileSection, 'history')
                except: history = None
                catHeadObj = madrigal.cedar.CatalogHeaderCreator(fullMadFilename)
                catHeadObj.createCatalog(principleInvestigator=principleInvestigator, expPurpose=expPurpose, expMode=expMode, cycleTime=cycleTime, correlativeExp=correlativeExp, sciRemarks=sciRemarks)
                catHeadObj.createHeader(kindatDesc=kindatDesc, analyst=analyst, comments=comments, history=history)
                catHeadObj.write()
            
            #self.__prependCatHeader__(fullMadFilename)
            logging.info('added file from %s to %s using %s, kindat %i, type %s, lowerRange=%s, upperRange=%s' % \
                             (str(firstTime), str(lastTime), hdf5Filename, kindat, hdf5Type, str(thisLowerRange), str(thisUpperRange)))
                             
            # see if links to images are desired
            numLinks = 0
            while True:
                try:
                    imageTitle = self.__iniData__.get(self.fileSection, 'imageTitle%i' % (numLinks + 1))
                    image = self.__iniData__.get(self.fileSection, 'image%i' % (numLinks + 1))
                    self.createLink(imageTitle, image, OutPath)
                    logging.info('Created link with file %s' % (image))

                    numLinks += 1
                    
                except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
                    break

                             
                             
    def createLink(self,
                   title,
                   imageFile,
                   expPath):
        """createLink is a method to create a new html file in expPath with a link to an image file.

        Inputs:
            title - title of plot (will show up in Madrigal)
            imageFile - external image to be copied and displayed in Madrigal
            expPath - path to experiment directory
        """
        
        templateHtml = """<html><head>
        <TITLE>%s</TITLE>
        </head> <body><img src="plots/%s"> </body> </html>
        """
        # get a unique filename
        plotBasenames = []
        plotFiles = glob.glob(os.path.join(expPath, 'plot*.html'))

        for plotFile in plotFiles:
            plotBasenames.append(os.path.basename(plotFile))

        plotNum = 0
        while True:
            plotName = 'plot%i.html' % (plotNum)

            if plotName not in plotBasenames:
                break

            plotNum += 1
        
        if not os.path.exists(os.path.join(expPath, 'plots')):
            os.mkdir(os.path.join(expPath, 'plots'))

        imageFileBasename=os.path.basename(imageFile)
        outName=os.path.join(expPath, 'plots',imageFileBasename)

        if os.path.exists(outName):
            imageFileTrailing=imageFileBasename.rsplit('.',1)
            imgNum=1
            while True:
                outName = '%s-%i.%s' % (imageFileTrailing[0],imgNum,imageFileTrailing[1])
                if not os.path.exists(os.path.join(expPath, 'plots',outName)):
                    break
                imgNum+=1
            outName=os.path.join(expPath, 'plots',outName)
            
        shutil.copy(imageFile, outName)
        os.chmod(outName,0664)

        f = open(os.path.join(expPath, plotName), 'w')
        f.write(templateHtml % (title, os.path.basename(outName)))
        f.close()

    def __prependCatHeader__(self, fullMadFilename):
        """__prependCatHeader__ prepends a catalog and header record to existing fullMadFilename
        based on information in fullMadFilename and from the ini file.

        Inputs:

            fullMadFilename - full path to newly created madrigal file

        Returns: None

        Affects: prepends a catalog and header record to existing fullMadFilename
        """

        # first create catalog record as string
        catStr = ''

        catStr += self.__padLine__('KRECC       2001 Catalogue Record, Version 1')
        catStr += self.__padLine__('KINSTE     %5i %s' % \
                                   (self.instrument,
                                    self.madInstObj.getInstrumentName(self.instrument)))
        
        # add experiment description from ini file (if any)
        try:
            modexp = self.__iniData__.get('Experiment', 'modexp')
            newLine = 'MODEXP         0 ' + modexp
            catStr += self.__padLine__(newLine)
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass

        try:
            cmodexp = self.__iniData__.get('Experiment', 'cmodexp')
            lines = self.__breakTextIntoLines__(cmodexp, 'CMODEXP ')
            for line in lines:
                catStr += self.__padLine__(line)
            catStr += self.__padLine__('C')
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass
        
        # add summary info from the newly created Madrigal file
        madFileObj = madrigal.data.MadrigalFile(fullMadFilename, self.madDBObj)
        madKeyList, madDict = self.__getCatalogInfoFromMadrigal__(madFileObj)

        for key in madKeyList:
            catStr += self.__padLine__(madDict[key])

        # add PI from ini file if there
        try:
            catStr += self.__padLine__('C')
            pi = self.__iniData__.get('Experiment', 'pi')
            newLine = 'CPI      ' + pi
            catStr += self.__padLine__(newLine)
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass

        catStr += self.__padLine__('CPREPDAT  %s' % time.asctime())
            
        
        catStr = self.__getCatalogProlog__(madFileObj, len(catStr)/80) + catStr

        # header
        
        headStr = ''
        headStr += self.__padLine__('KRECH               3002 Header Record, Version 3')
        headStr += self.__padLine__('KINST          3   %5i %s' % \
                                    (self.instrument,
                                     self.madInstObj.getInstrumentName(self.instrument)))

        # add kindat and kindat description if given
        kindat = int(self.__iniData__.get(self.fileSection, 'kindat'))
        madKindatObj = madrigal.metadata.MadrigalKindat(self.madDBObj)
        kindatDesc = madKindatObj.getKindatDescription(kindat)
        if kindatDesc != None:
            try:
                headStr += self.__padLine__('KINDAT         4   %5i %s' % (kindat, kindatDesc))
            except:
                # description too long - truncate
                kindatDesc = kindatDesc[0:50]
                headStr += self.__padLine__('KINDAT         4   %5i %s' % (kindat, kindatDesc))
        
        
        headStr += self.__padLine__('C')

        try:
            ckindat = self.__iniData__.get(self.fileSection, 'ckindat')
            lines = self.__breakTextIntoLines__(ckindat, 'CKINDAT ')
            for line in lines:
                headStr += self.__padLine__(line)
            headStr += self.__padLine__('C')

        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            pass

        # add source file line
        source_file = os.path.basename(self.__iniData__.get(self.fileSection, 'hdf5Filename'))
        headStr += self.__padLine__('C SOURCE_FILE %s' % (source_file))
        headStr += self.__padLine__('C')

        # add summary info from the newly created Madrigal file
        madKeyList, madDict, madLineList = self.__getHeaderInfoFromMadrigal__(madFileObj)

        for key in madKeyList:
            headStr += self.__padLine__(madDict[key])
        for line in madLineList:
            headStr += self.__padLine__(line)

        headStr += self.__padLine__('CANDATE  %s' % time.asctime())

        headStr = self.__getHeaderProlog__(madFileObj, len(headStr)/80) + headStr

        # combine catalog, header, and data records
        orgFile = open(fullMadFilename)
        orgFileData = orgFile.read()
        orgFile.close()
        newFile = open(fullMadFilename, 'w')
        newFile.write(catStr)
        newFile.write(headStr)
        newFile.write(orgFileData)
        newFile.close()

        
    def __getCatalogInfoFromMadrigal__(self, madFileObj):
        """__getCatalogInfoFromMadrigal__ gets needed catalog information from a Madrigal file.

            Input: madFileObj - a MadrigalFile object based on Madrigal file

            Returns: a tuple containing:

                1. An ordered list of line headers produced
                
                2  A dictionary with keys = line headers produced, values = ordered list of lines
                generated for each line header. 
        """

        retKeyList = []
        retDict = {}

        templateLines = ('ALT1      %6i km. Lowest altitude measured',
                         'ALT2      %6i km. Highest altitude measured',
                         'GGLAT1    %6i degrees. Lowest geographic latitude measured',
                         'GGLAT2    %6i degrees. Highest geographic latitude measured',
                         'GGLON1    %6i degrees. Westmost geographic longitude measured',
                         'GGLON2    %6i degrees. Eastmost geographic longitude measured',
                         'PL1       %6i Shortest radar pulse length (microsec)',
                         'PL2       %6i Longest radar pulse length (microsec)',
                         'IBYRE     %6i Beginning year',
                         'IBDTE     %6i Beginning month and day',
                         'IBHME     %6i Beginning UT hour and minute',
                         'IBCSE     %6i Beginning centisecond',
                         'IEYRE     %6i Ending year',
                         'IEDTE     %6i Ending month and day',
                         'IEHME     %6i Ending UT hour and minute',
                         'IECSE     %6i Ending centisecond')

        for tempLine in templateLines:
            # get key
            firstSpace = tempLine.find(' ')
            key = tempLine[:firstSpace]

            try:
            
                if key == 'ALT1':
                    # check data good
                    if madFileObj.getMinValidAltitude() > madFileObj.getMaxValidAltitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMinValidAltitude()))
                elif key == 'ALT2':
                    # check data good
                    if madFileObj.getMinValidAltitude() > madFileObj.getMaxValidAltitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMaxValidAltitude()))
                elif key == 'GGLAT1':
                    # check data good
                    if madFileObj.getMinLatitude() > madFileObj.getMaxLatitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMinLatitude()))
                elif key == 'GGLAT2':
                    # check data good
                    if madFileObj.getMinLatitude() > madFileObj.getMaxLatitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMaxLatitude()))
                elif key == 'GGLON1':
                    # check data good
                    if madFileObj.getMinLongitude() > madFileObj.getMaxLongitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMinLongitude()))
                elif key == 'GGLON2':
                    # check data good
                    if madFileObj.getMinLongitude() > madFileObj.getMaxLongitude():
                        continue
                    retDict[key] = tempLine % (int(madFileObj.getMaxLongitude()))
                elif key == 'PL1':
                    # check data good
                    if madFileObj.getMinPulseLength() > madFileObj.getMaxPulseLength():
                        continue
                    if madFileObj.getMinPulseLength() <= 0.0 or  madFileObj.getMaxPulseLength() <= 0.0:
                        continue
                    retDict[key] = tempLine % (int(round(madFileObj.getMinPulseLength() * 1000000.0)))
                elif key == 'PL2':
                    # check data good
                    if madFileObj.getMinPulseLength() > madFileObj.getMaxPulseLength():
                        continue
                    if madFileObj.getMinPulseLength() <= 0.0 or  madFileObj.getMaxPulseLength() <= 0.0:
                        continue
                    retDict[key] = tempLine % (int(round(madFileObj.getMaxPulseLength() * 1000000.0)))
                elif key == 'IBYRE':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[0])
                elif key == 'IBDTE':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2])
                elif key == 'IBHME':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4])
                elif key == 'IBCSE':
                    retDict[key] = tempLine % (madFileObj.getEarliestTime()[5] * 100)
                elif key == 'IEYRE':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[0])
                elif key == 'IEDTE':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2])
                elif key == 'IEHME':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4])
                elif key == 'IECSE':
                    retDict[key] = tempLine % (madFileObj.getLatestTime()[5] * 100)
                elif key == 'CPREPDAT':
                    retDict[key] = tempLine % (time.asctime())
                else:
                    raise 'illegal key %s' % (str(key))
                
                retKeyList.append(key)

            except:
                continue

        return (retKeyList, retDict)

    def __getCatalogProlog__(self, madFileObj, numLines):
        """__getCatalogProlog__ generates the prolog part of the catalog record.

            Input:

                madFileObj - a MadrigalFile object based on Madrigal file created by inscal

                numLines -  number of 80 byte lines in catalog record

            Returns: the catalog record prolog as a 16*2 byte string (big-endian)
        """
        LTOT = 40 * (1+numLines)
        KRECC = 2001
        # set kinste to the first instrument found in the file
        KINSTE = madFileObj.getKinstList()[0]
        # set modexp to the first kindat found in the file
        MODEXP = madFileObj.getKindatList()[0]
        IBYRE = madFileObj.getEarliestTime()[0]
        IBDTE = madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2]
        IBHME = madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4]
        IBCSE = madFileObj.getEarliestTime()[5] * 100
        IEYRE = madFileObj.getLatestTime()[0]
        IEDTE = madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2]
        IEHME = madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4]
        IECSE = madFileObj.getLatestTime()[5] * 100
        ZERO = 0
        # pack 40 big-endian 2 byte ints
        return struct.pack('>hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh',
                           LTOT,
                           KRECC,
                           KINSTE,
                           MODEXP,
                           IBYRE,
                           IBDTE,
                           IBHME,
                           IBCSE,
                           IEYRE,
                           IEDTE,
                           IEHME,
                           IECSE,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO)


    def __getHeaderProlog__(self, madFileObj, numLines):
        """__getHeaderProlog__ generates the prolog part of the header record.

            Input:

                madFileObj - a MadrigalFile object based on Madrigal file created by inscal

                numLines -  number of 80 byte lines in header record

            Returns: the header record prolog as a 16*2 byte string (big-endian)
        """
        LTOT = 40 * (1+numLines)
        KRECC = 3002
        # set kinste to the first instrument found in the file
        KINSTE = madFileObj.getKinstList()[0]
        # set modexp to the first kindat found in the file
        MODEXP = madFileObj.getKindatList()[0]
        IBYRE = madFileObj.getEarliestTime()[0]
        IBDTE = madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2]
        IBHME = madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4]
        IBCSE = madFileObj.getEarliestTime()[5] * 100
        IEYRE = madFileObj.getLatestTime()[0]
        IEDTE = madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2]
        IEHME = madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4]
        IECSE = madFileObj.getLatestTime()[5] * 100
        LPROL = 16
        JPAR = len(madFileObj.getMeasured1dParmList())
        MPAR = len(madFileObj.getMeasured2dParmList())
        ZERO = 0
        # pack 40 big-endian 2 byte ints
        return struct.pack('>hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh',
                           LTOT,
                           KRECC,
                           KINSTE,
                           MODEXP,
                           IBYRE,
                           IBDTE,
                           IBHME,
                           IBCSE,
                           IEYRE,
                           IEDTE,
                           IEHME,
                           IECSE,
                           LPROL,JPAR,MPAR,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
                           ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO)



    def __getHeaderInfoFromMadrigal__(self, madFileObj):
        """__getHeaderInfoFromMadrigal__ get needed information fro a header record from a Madrigal file.

            Input: madFileObj - a MadrigalFile object based on Madrigal file

            Returns: a tuple containing:

                1. An ordered list of line headers produced
                
                2  A dictionary with keys = line headers produced, values = ordered list of lines

                3. A list of lines containing the KOD lines and the parameter descriptions
        """
        madParmObj = madrigal.data.MadrigalParameters()

        retKeyList = []
        retDict = {}
        parmLines = []

        templateLines = ('IBYRT             %6i Beginning year',
                         'IBDTT             %6i Beginning month and day',
                         'IBHMT             %6i Beginning UT hour and minute',
                         'IBCST             %6i Beginning centisecond',
                         'IEYRT             %6i Ending year',
                         'IEDTT             %6i Ending month and day',
                         'IEHMT             %6i Ending UT hour and minute',
                         'IECST             %6i Ending centisecond',
                         'LPROL         13     16  Length of prologue in data records',
                         'JPAR          14 %6i  Number of single-valued parameters',
                         'MPAR          15 %6i  Number of multiple-values parameters',
                         'NROW          16         Number of entries for multiple valued parameter')

        for tempLine in templateLines:
            # get key
            firstSpace = tempLine.find(' ')
            key = tempLine[:firstSpace]
            retKeyList.append(key)
            if key == 'IBYRT':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[0])
            elif key == 'IBDTT':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[1] * 100 + madFileObj.getEarliestTime()[2])
            elif key == 'IBHMT':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[3] * 100 + madFileObj.getEarliestTime()[4])
            elif key == 'IBCST':
                retDict[key] = tempLine % (madFileObj.getEarliestTime()[5] * 100)
            elif key == 'IEYRT':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[0])
            elif key == 'IEDTT':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[1] * 100 + madFileObj.getLatestTime()[2])
            elif key == 'IEHMT':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[3] * 100 + madFileObj.getLatestTime()[4])
            elif key == 'IECST':
                retDict[key] = tempLine % (madFileObj.getLatestTime()[5] * 100)
            elif key == 'LPROL':
                retDict[key] = tempLine
            elif key == 'JPAR':
                retDict[key] = tempLine % (len(madFileObj.getMeasured1dParmList()))
            elif key == 'MPAR':
                retDict[key] = tempLine % (len(madFileObj.getMeasured2dParmList()))
            elif key == 'NROW':
                retDict[key] = tempLine
            else:
                raise 'illegal key %s' % (str(key))

        # generate 1d lines
        parmLines.append('C 1D Parameters:')

        count = 0

        onedParmList = madFileObj.getMeasured1dParmList()

        for parm in onedParmList:
            count += 1
            # KODS
            thisStr = 'KODS(%i)' % (count)
            thisStr += ' ' * (8 - len(thisStr))
            # position in prolog
            nextStr = '%i' % (count + 16)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm code
            nextStr = '%i' % (parm)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm desc
            nextStr = ' %s' % (madParmObj.getSimpleParmDescription(parm))
            if len(nextStr) > 40:
                nextStr = nextStr[:40]
            else:
                nextStr += ' ' * (40 - len(nextStr))
            thisStr += nextStr
            # unit scale
            nextStr = '%1.0E' % (madParmObj.getParmScaleFactor(parm))
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # units
            nextStr = ' %s' % (madParmObj.getParmUnits(parm))
            if len(nextStr) > 8:
                nextStr[:8]
            else:
                nextStr = ' ' * (8 - len(nextStr)) + nextStr
            thisStr += nextStr
            
            parmLines.append(thisStr)

        # generate 2d lines
        parmLines.append('C 2D Parameters:')

        count += len(madFileObj.getMeasured1dParmList())
        count2d = 0

        twodParmList = madFileObj.getMeasured2dParmList()

        for parm in twodParmList:
            count += 1
            count2d += 1
            # KODM
            thisStr = 'KODM(%i)' % (count2d)
            thisStr += ' ' * (8 - len(thisStr))
            # position in prolog
            nextStr = '%i' % (count + 16)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm code
            nextStr = '%i' % (parm)
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # parm desc
            nextStr = ' %s' % (madParmObj.getSimpleParmDescription(parm))
            if len(nextStr) > 40:
                nextStr = nextStr[:40]
            else:
                nextStr += ' ' * (40 - len(nextStr))
            thisStr += nextStr
            # unit scale
            nextStr = '%1.0E' % (madParmObj.getParmScaleFactor(parm))
            thisStr += ' ' * (8 - len(nextStr))
            thisStr += nextStr
            # units
            nextStr = ' %s' % (madParmObj.getParmUnits(parm))
            if len(nextStr) > 8:
                nextStr[:8]
            else:
                nextStr = ' ' * (8 - len(nextStr)) + nextStr
            thisStr += nextStr
            
            parmLines.append(thisStr)
            
        
        return (retKeyList, retDict, parmLines)



    def __padLine__(self, inputStr):
        """__padLine__ returns a string padded with spaces to CEDAR_LEN and with carriage returns removed.

        Raises error if inputStr with final carriage return removed longer that CEDAR_LEN.
        """
        
        if inputStr[-1] == '\n':
            inputStr = inputStr[:-1]

        if len(inputStr) > self.__CEDAR_LEN__:
            raise ValueError, 'Line %s has length greater than %i' % (inputStr, self.__CEDAR_LEN__)

        inputStr = inputStr.replace('\n', ' ')

        retStr = inputStr + ' ' * (self.__CEDAR_LEN__ - len(inputStr))

        return retStr


    def __breakTextIntoLines__(self, text, prefixText):
        """__breakTextIntoLines__ returns a list of lines all less than CEDAR_LEN all starting with
        prefixText, containing the words in text
        """
        retLineList = []

        words = text.split()
        thisLine = prefixText
        for word in words:
            if len(word) + 1 > self.__CEDAR_LEN__ - len(prefixText):
                raise ValueError, 'Cannot fit word <%s> into Cedar record' % (word)
            
            if len(word) + len(thisLine) < self.__CEDAR_LEN__:
                thisLine += '%s ' % (word)
            else:
                # new line
                retLineList.append(thisLine)
                thisLine = prefixText
                thisLine += '%s ' % (word)

        # append remaining line
        retLineList.append(thisLine)

        return(retLineList)

