# this is a script

import sys
import argparse
from src.GUIwrapper import *


if __name__ == "__main__":
    gui_wrapper = GUIwrapper()

    '''
    Example inputs:
    "exp_dir" = "2014/09"
    "exp_name" = "20140920.001"
    "exp_type" = "PLCal30"
    "exp_mode" = "LP" or "AC" or "BOTH"
    "cal_file" = "/PLCal30/20140928.001/long_name_here.txt"
    '''

    parser = argparse.ArgumentParser(description="Calibration script")
    parser.add_argument('--exp_dir',type=str,help="Experiment Directory")
    parser.add_argument('--exp_name',type=str,help="Experiment Name")
    parser.add_argument('--exp_type',type=str,help="Experiment Type")
    parser.add_argument('--exp_mode',type=str,help="Experiment Mode")
    parser.add_argument('--cal_file',type=str,help="Calibration File")
    parser.add_argument('--gui',nargs='?',const=True,help="Interactive GUI")
    args = parser.parse_args()

    gui_wrapper.run(vars(args))
