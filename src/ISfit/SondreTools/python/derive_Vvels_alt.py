#!/usr/bin/env python

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
fitter_base_path = os.environ['AMISR_FITTER_PATH'].split('AMISR_fitter_py')[0]
sondre_tools_path = os.path.join(fitter_base_path,'SondreTools/python')
sys.path.append(sondre_tools_path)

import vvelsAlt as vv

##############################

def usage():
    print("usage: ", sys.argv[0] + "YYYYMMDD.XXX config_header")
    print("\t YYYYMMDD.XXX: experiment directory to process")
    print("\t config_header: which header in config_vvelsAlt.ini to use")

    sys.exit(2)

def main():
    
    try:
        expName=glob.glob(os.path.abspath(sys.argv[1]))[0]
        pd = sys.argv[2]
    except:
        usage()

        
    print expName
    dn = os.path.dirname(expName)

    iniF = os.path.join(dn,'config_vvelsAlt.ini')        

    vf = vv.vvelsAlt(iniF + ','+expName+'/config_exp.ini',pd)
    vf.dovelsAlt()

    print("Finito")


if __name__ == '__main__':

    main()