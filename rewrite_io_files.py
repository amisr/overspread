import glob
import os
import sys
import shutil

if __name__ == "__main__":

    topdir = "/Users/fitter/Documents/amisr-src/experiment_type_params"
    exp_dirs = glob.glob(os.path.join(topdir,"*"))
    for dir in exp_dirs:
        io_files = glob.glob(os.path.join(dir,"*io*"))
        for io_file in io_files:
            lines = []
            replacements = { "%(OUTPUT_DAT_PATH)s/%(YR)s/%(EXPDIR)s/%(EXPNAME)s/filelist" : dir + "/filelist"}
            with open(io_file) as infile:
                for line in infile:
                    for src, target in replacements.iteritems():
                        line = line.replace(src, target)
                    lines.append(line)
            with open(io_file, 'w') as outfile:
                for line in lines:
                    outfile.write(line)
