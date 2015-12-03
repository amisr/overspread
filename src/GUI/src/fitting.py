import os
import sys
from multiprocessing import Pool
import subprocess

class fitting:

    def __init__(self):
        pass

    def fitting_loop(self,directory_list,AC=False,LP=False):
        print("we are now in the fitting loop!")

        # navigate to directory top
        command_list = []
        for dir in directory_list:
            dirpath = "/Users/fitter/Documents/amisr-src/src/ISfit/AMISR_fitter_py/src/"
            if AC:
                bash_command = dirpath + "/./run_AC.sh " + os.path.basename(dir) + " " + os.path.dirname(dir)
                #p = subprocess.Popen(bash_command.split(),shell=False)
                command_list.append(bash_command)
            if LP:
                bash_command = dirpath + "/./run_LP.sh " + os.path.basename(dir) + " " + os.path.dirname(dir)
                #p = subprocess.Popen(bash_command.split(),shell=False)
                command_list.append(bash_command)


        for cmd in command_list:
            self.worker(cmd)

    def worker(self,cmd):
        p = subprocess.Popen(cmd.split(), shell=False);
        p.wait()
