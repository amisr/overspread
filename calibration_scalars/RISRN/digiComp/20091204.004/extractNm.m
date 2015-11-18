clear all; close all;


% fnames = {
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091204.004/20091204.004_lp_3min.h5',
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091208.001/20091208.001_lp_3min.h5',
%     '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091210.001/20091210.001_lp_3min.h5',
%     };

fnames = {
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091204.004/20091204.004_lp_3min-cal.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091208.001/20091208.001_lp_3min-cal.h5',
    '/Volumes/ISR_DATA/processed_data/RISR/2009/WorldDay55m3dt/20091210.001/20091210.001_lp_3min-cal.h5',
    };



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');