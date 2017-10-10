#!/opt/local/bin/python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys, getopt
import os.path
import tables
import glob
import shutil
import copy

import os
fitter_path = os.environ['AMISR_FITTER_PATH'].split('AMISR_fitter_py')[0]
sys.path.append(fitter_path)

import amisr_py.derivedParams.vvelsLat as vv

##############################

def usage():
    print("usage: ", sys.argv[0] + "YYYYMMDD.XXX seconds")
    print("\t YYYYMMDD.XXX: experiment directory to process")
    print("\t seconds: number of seconds to integrate data for")

    sys.exit(2)

def main():
    
    try:
        expName=glob.glob(os.path.abspath(sys.argv[1]))[0]
        pd = sys.argv[2]
    except:
        usage()
    
    print(expName)
    dn = os.path.dirname(expName)

    iniF = os.path.join(dn,'config_vvelsLat.ini')        

    vf = vv.vvelsLat(iniF + ','+expName+'/config_exp.ini',pd)
    vf.dovels()
    vf.replotFromOutput()
        
    print("Finito")

if __name__ == '__main__':

    main()	
