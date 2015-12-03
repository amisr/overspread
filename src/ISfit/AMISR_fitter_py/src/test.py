import os
import sys
import subprocess

if __name__ == "__main__":
    bash_command = "./run_LP.sh 20140902.034 /Volumes/ISR_DATA_02/processed_data/PFISR/2014/09/Swarm_v01_20140902.007"
    p = subprocess.call(bash_command.split(), shell=False)
    
