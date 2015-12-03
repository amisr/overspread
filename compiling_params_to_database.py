import glob
import os
import sys
import shutil

if __name__ == "__main__":

    topdir = "/Volumes/ISR_DATA_02/processed_data/PFISR"
    newdir = "/Users/fitter/Documents/amisr-src/experiment_type_params"
    years = glob.glob(os.path.join(topdir,"20*"))[::-1]
    for year in years:
        months = glob.glob(os.path.join(year,"*[0-9]*"))[::-1]
        for month in months:
            experiments = glob.glob(os.path.join(month,"*"))
            for experiment in experiments:

                # check to make sure the directory doesn't already exist
                new_pname = os.path.join(newdir,os.path.basename(experiment))
                if not os.path.exists(new_pname):
                    print(new_pname)

                    # copy all .ini files to new directory
                    ini_files = glob.glob(os.path.join(experiment,"*.ini"))
                    if ini_files:

                        # create directory
                        os.makedirs(new_pname)

                        # copy ini files
                        for ini_file in ini_files:
                            shutil.copyfile(ini_file,os.path.join(new_pname,os.path.basename(ini_file)))
                            print(ini_file)

                        # navigate into one of experiments to collect filelist files
                        exp_folders = glob.glob(os.path.join(experiment,"20*"))[::-1]
                        for exp in exp_folders:
                            filelist_files = glob.glob(os.path.join(exp,"filelist*"))
                            if filelist_files:
                                break
                        for filelist_file in filelist_files:
                            #shutil.copyfile(filelist_file,os.path.join(new_pname,os.path.basename(filelist_file)))
                            fid_filelist = open(filelist_file,"r")
                            line = fid_filelist.readline()
                            fid_filelist.close()
                            fid = open(os.path.join(new_pname,os.path.basename(filelist_file)),"w")
                            fid.write(line)
                            fid.close()
                            print(filelist_file)
