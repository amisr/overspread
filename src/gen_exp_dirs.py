#!/usr/bin/env python

"""
xxxxx

~M. Nicolls
last revised: xx/xx/2007

"""

import sys
import os.path
import tables
import scipy
import scipy.io
import glob
import shutil


config_exp = "[DEFAULT]\n\nYR= %s\n\nDATAPATH= %s\n\nEXPDIR= %s\nEXPNAME= %s"


##############################

if __name__ == '__main__':

    makeExpDirs=1

    DISKS = ['/Volumes/AMISR_005/','/Volumes/AMISR_006/','/Volumes/AMISR_007/','/Volumes/AMISR_008/',
        '/Volumes/AMISR_009/','/Volumes/AMISR_010/','/Volumes/AMISR_011/',
        '/Volumes/AMISR_012/','/Volumes/AMISR_013/','/Volumes/AMISR_014/','/Volumes/AMISR_015/',
        '/Volumes/AMISR_016/','/Volumes/AMISR_017', '/Volumes/AMISR_018','/Volumes/AMISR_019','/Volumes/AMISR_020',
        '/Volumes/PFISR_004', '/Volumes/RISRN_001', '/Volumes/RISRN_002']
    #'/Volumes/PFISR_001', '/Volumes/PFISR_002', '/Volumes/PFISR_003','/Volumes/PFISR_004']

    PATHS = {   'PFISR': {'FPATH': 'Data AMISR Poker', 'PPATH': 'PFISR'},\
                'RISRN': {'FPATH': 'Data AMISR Resolute N', 'PPATH': 'RISR-N'}}

    tinstances = ['PFISR','RISRN']
    
    for tinst in tinstances:
        DIR_WC='2015*'
        OUTPATH=os.path.join('/Volumes/ISR_DATA_02/processed_data/',PATHS[tinst]['PPATH'])
        FPATH = PATHS[tinst]['FPATH']
        print(tinst)
        dirs=[]
        for DISK_PATH in DISKS:
            FILE_PATH=os.path.join(DISK_PATH,FPATH)
            tdirs=glob.glob(os.path.join(FILE_PATH,DIR_WC)) 
            tdirs.sort()
            dirs.extend(tdirs)

        Ndirs=len(dirs)
            
        ALLEXPS=[]
        for aaa in range(Ndirs):
            print(dirs[aaa])
            # get the setup file
            exp_file = glob.glob(os.path.join(dirs[aaa],'Setup/*.exp'))
            
            if len(exp_file)==1:		
                
                exp_file=exp_file[0]
                #tmp1,tmp2,exp_file=exp_file.partition(os.path.join(dirs[aaa],'Setup/'))
                #exp_file,tmp1,tmp2=exp_file.partition('.exp')
                if 'Tracking_MEO' in exp_file or 'Tracking_LEO'in exp_file  or 'Tracking_GEO' in exp_file or 'CalSphere' in exp_file or 'Timing' in exp_file:
                    print 'Skipped ' +exp_file
                    
                else:
                    exp_file=exp_file.split(os.path.join(dirs[aaa],'Setup/'))[1]
                    exp_file=exp_file.split('.exp')[0]
                                    
                    dhead,dtail=os.path.split(dirs[aaa])
                    yr = dtail[:4]
                    mon = dtail[4:6]
                    
                    expploc = os.path.join(OUTPATH,yr,mon,exp_file)
                    expploc1 = os.path.join(OUTPATH,yr,mon,exp_file,dtail)
                    expploc_file = os.path.join(expploc,'00_experiments.txt')
                               
                    if not os.path.exists(expploc):
                        try:
                            os.makedirs(expploc)
                        except:
                            xxx

                    if makeExpDirs:

                        if len(glob.glob(expploc1+'*'))==0:
                            try:
                                print expploc1
                                os.makedirs(expploc1)
                                inip = os.path.join(expploc1,'config_exp.ini')
                                tfile = config_exp % (yr+'/'+mon,dirs[aaa],exp_file,dtail)
                                FILE = open(inip,"w")
                                FILE.write(tfile+'\n')
                                FILE.close()
                            except:
                                xxx

                    strout = "%s, %s, %s" % (dtail, exp_file, dhead)

                    #try:
                    #    ALLEXPS[expploc_file].append(str   out)
                    #except:
                    #    ALLEXPS[expploc_file]=[strout]
                     
                    mode="a"
                    if expploc_file not in ALLEXPS:
                        mode="w"
                        ALLEXPS.append(expploc_file)
                              
                    FILE = open(expploc_file,mode)
                    FILE.write(strout+'\n')
                    FILE.close()
                    
    
