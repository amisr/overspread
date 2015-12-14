import os
import glob
import sys
from multiprocessing import Pool
import subprocess

class calibration:

    def __init__(self):
        pass

    def calibration_loop(self,directory_list,calfile,AC=False,LP=False,BC=False):
        print("we are now in the calibration loop!")

        # navigate to directory top
        command_list = []
        for dir in directory_list:
            calscript_path = "/Users/fitter/Documents/amisr-src/src/calibration/cal_processed_file3.py"
            if AC:
                bash_command = "python " + calscript_path +" "+ dir + " "+ calfile + " "+"AC"
                #p = subprocess.Popen(bash_command.split(),shell=False)
                command_list.append(bash_command)
            if LP:
                bash_command = "python " + calscript_path +" "+ dir + " "+ calfile + " "+"LP"
                #p = subprocess.Popen(bash_command.split(),shell=False)
                command_list.append(bash_command)
            if BC:
                bash_command = "python " + calscript_path +" "+ dir + " "+ calfile + " "+"BC"
                #p = subprocess.Popen(bash_command.split(),shell=False)
                command_list.append(bash_command)

        print(command_list)

        for cmd in command_list:
            self.worker(cmd)

    def worker(self,cmd):
        p = subprocess.Popen(cmd.split(), shell=False);
        p.wait()
