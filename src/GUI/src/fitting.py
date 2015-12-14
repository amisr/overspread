import os
import glob
import sys
from multiprocessing import Pool
import subprocess

class fitting:

    def __init__(self):
        pass

    def fitting_loop(self,directory_list,AC=False,LP=False,BC=False):
        print("we are now in the fitting loop!")

        # navigate to directory top
        command_list = []
        for dir in directory_list:
            dirpath = "/Users/fitter/Documents/amisr-src/src/ISfit/AMISR_fitter_py/src/"
            parampath = "/Users/fitter/Documents/amisr-src/experiment_type_params/"
            paramdir = os.path.join(parampath,os.path.basename(os.path.dirname(dir)))
            if AC:
                fit_file = glob.glob(os.path.join(paramdir,"*fit_AC*"))[0]
                io_list = glob.glob(os.path.join(paramdir,"*io_AC*"))
                for io_element in io_list:
                    bash_command = dirpath + "/./run_AC.sh " + dir +" "+ io_element +" "+fit_file
                    #p = subprocess.Popen(bash_command.split(),shell=False)
                    command_list.append(bash_command)
            if LP:
                fit_file = glob.glob(os.path.join(paramdir,"*fit_LP*"))[0]
                io_list = glob.glob(os.path.join(paramdir,"*io_LP*"))
                for io_element in io_list:
                    bash_command = dirpath + "/./run_LP.sh " + dir +" "+ io_element +" "+fit_file
                    #p = subprocess.Popen(bash_command.split(),shell=False)
                    command_list.append(bash_command)
            if BC:
                fit_file = glob.glob(os.path.join(paramdir,"*fit_BC*"))[0]
                io_list = glob.glob(os.path.join(paramdir,"*io_BC*"))
                for io_element in io_list:
                    bash_command = dirpath + "/./run_BC.sh " + dir +" "+ io_element +" "+fit_file
                    #p = subprocess.Popen(bash_command.split(),shell=False)
                    command_list.append(bash_command)

        print(command_list)

        for cmd in command_list:
            self.worker(cmd)

    def worker(self,cmd):
        p = subprocess.Popen(cmd.split(), shell=False);
        p.wait()
