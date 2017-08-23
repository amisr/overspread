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
#amisr_tools_path = os.path.join(fitter_base_path,'AMISRtools/python')
sys.path.append(sondre_tools_path)
#sys.path.append(amisr_tools_path)

# sys.path.append('/Users/mnicolls/Documents/Work/ISfit/SondreTools/python')
# sys.path.append('/Users/mnicolls/Documents/Work/ISfit/AMISRtools/python')

import vvelsLat as vv

##############################

def usage():
    print("usage: ", sys.argv[0] + "YYYYMMDD.XXX config_header")
    print("\t YYYYMMDD.XXX: experiment directory to process")
    print("\t config_header: which header in config_vvelsLat.ini to use")

    sys.exit(2)

def main():
    
    try:
        expName=glob.glob(os.path.abspath(sys.argv[1]))[0]
        pd = sys.argv[2]
    except:
        usage()

        
    print expName
    dn = os.path.dirname(expName)

    iniF = os.path.join(dn,'config_vvelsLat.ini')        

    vf = vv.vvelsLat(iniF + ','+expName+'/config_exp.ini',pd)
    vf.dovels()
    vf.replotFromOutput()
    
    try:
        vf = vv.vvelsLat(iniF + ','+expName+'/config_exp.ini',pd)
        vf.dovels()
        vf.replotFromOutput()
        
        print("Finito")
    except e:
        print("Failed: " + str(e))


if __name__ == '__main__':

    main()