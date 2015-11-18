clear all; close all;
% 
% fnames = {
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090914.001/20090914.001_lp_3min.h5',
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090915.001/20090915.001_lp_3min.h5',
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090923.001/20090923.001_lp_3min.h5'
%     };

fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090914.001/20090914.001_lp_3min-cal.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090915.001/20090915.001_lp_3min-cal.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay53m/20090923.001/20090923.001_lp_3min-cal.h5'
    };



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');