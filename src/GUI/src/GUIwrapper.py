#cal_processed_file3.py, but better

import os
import sys
import logging
from PyQt4 import QtGui

from loggerinit.LoggerInit import *
from configreader.ConfigReader import *

from GUI import *
from file_io import *

class GUIwrapper:
    '''
    This is the GUIwrapper class and it will contain important variables but I don't know what they'll be.
    '''
    def __init__(self):
        # Must initialize logging module first so that everything else
        # can utilize it
        LoggerInit("config/log.ini")

        # Create logger instance for current class
        self.logger = logging.getLogger(__name__)

        # Set up a ConfigReader object
        self.ConfigReader = ConfigReader() #self.logger,logger_level="INFO")

        # Read all configuration files from the config directory.

        # Return as dictionary
        self.config = self.ConfigReader.read("config/*.ini")

        self.file_io = FileIO(self.config,self.logger)

        # Return as object
        #self.config_object = self.ConfigReader.read("config/*.ini",type=object)

        self.logger.info("Configuration files loaded...")


    def run(self, kwargs):
        """
        Inputs:
            args : List of input variables
                exp_dir : MM/YYYY string of experiment
                exp_str : name of the experiment
                exp_type : "LP", "AC", "both"
                cal_file : calibration file to use
        Outputs:
            None?
        """
        self.kwargs = kwargs

        if kwargs["gui"]:

            cal_input_gui = GUI(self.config)
            outputs = cal_input_gui.get_dirs()
            AC_flag = outputs[0]
            LP_flag = outputs[1]
            print AC_flag
            print LP_flag
            print outputs[2]
            print outputs[3]

            # try:
            if AC_flag & LP_flag:
                print('both true')
                kwargs["exp_type"] = 'BOTH'
            elif AC_flag:
                print('AC is true')
                kwargs["exp_type"] = 'AC'
            elif LP_flag:
                print('LP is true')
                kwargs["exp_type"] = 'LP'

            print(kwargs)

        else:
            self.file_io.construct_full_path(self.kwargs)
