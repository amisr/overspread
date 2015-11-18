clear all; close all;

fnames = {
    '/Volumes/ISR_DATA_02/processed_data/RISR-N/2012/10/WorldDay64m/20121012.001/20121012.001_lp_1min-cal.h5'};

%fnames = {
%     '/Volumes/ISR_DATA_02/processed_data/RISR-N/2012/04/WorldDay64m/20120430.001/20120430.001_lp_1min-cal.h5'};


thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');