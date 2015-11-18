clear all; close all;


fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2010/WorldDay55m/20100604.006/20100604.006_lp_3min.h5'
    };



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');