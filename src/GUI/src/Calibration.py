#cal_processed_file3.py, but better

import os
import sys
import logging
from PyQt4 import QtGui

from lib.loggerinit.LoggerInit import *
from lib.configreader.ConfigReader import *

from GUI import *
from file_io import *

class Calibration:
    '''
    This is the Calibration class and it will contain important variables but I don't know what they'll be.
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

        I walked along the avenue.
        I never thought I'd meet a girl like you;
        Meet a girl like you.
        With auburn hair and tawny eyes;
        The kind of eyes that hypnotize me through;
        Hypnotize me through.

        And I ran, I ran so far away.
        I just ran, I ran all night and day.
        I couldn't get away.

        A cloud appears above your head;
        A beam of light comes shining down on you,
        Shining down on you.
        The cloud is moving nearer still.
        Aurora borealis comes in view;
        Aurora comes in view.

        And I ran, I ran so far away.
        I just ran, I ran all night and day.
        I couldn't get away.

        Reached out a hand to touch your face;
        You're slowly disappearing from my view;
        Disappearing from my view.
        Reached out a hand to try again;
        I'm floating in a beam of light with you;
        A beam of light with you.

        And I ran, I ran so far away.
        I just ran, I ran all night and day.
        I couldn't get away.

        -Emma says it is okay to keep this! (For now)

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
            # print flags[1]
            # try:
                # kwargs["exp_dir"] = cal_input_gui.get_dirs()
            # except:
                # pass

            # try:
                # kwargs["cal_file"] = cal_input_gui.cal_file()
            # except:
                # pass

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
            # else:
            #     print 'ERROR! Must choose at least LP or AC measurement!'
            # except:
                # print('pass')
                # pass

            print(kwargs)
            #self.file_io.construct_full_path(kwargs)
        else:
            self.file_io.construct_full_path(self.kwargs)
