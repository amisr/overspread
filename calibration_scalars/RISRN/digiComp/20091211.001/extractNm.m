clear all; close all;


% fnames = {
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091211.001/20091211.001_lp_3min.h5',
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091214.001/20091214.001_lp_3min.h5',
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091216.001/20091216.001_lp_3min.h5'
%     };


fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091211.001/20091211.001_lp_3min-cal.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091214.001/20091214.001_lp_3min-cal.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091216.001/20091216.001_lp_3min-cal.h5'
    };


thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');