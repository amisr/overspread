clear all; close all;


fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2010/WorldDay54m/20100601.002/20100601.002_lp_1min.h5',
    };



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');