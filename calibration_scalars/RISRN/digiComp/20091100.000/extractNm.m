clear all; close all;


fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay54m/20091105.001/20091105.001_lp_3min.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay54m/20091107.001/20091107.001_lp_3min.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay54m/20091108.001/20091108.001_lp_3min.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay54m/20091112.001/20091112.001_lp_3min.h5'
    };



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');