clear all; close all;


fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090824.002/20090824.002_lp_3min.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090826.001/20090826.001_lp_3min.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090826.004/20090826.004_lp_3min.h5'
    };



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');