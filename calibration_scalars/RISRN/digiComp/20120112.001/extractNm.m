clear all; close all;

% fnames = {
%     '/Volumes/ISR_DATA_02/processed_data/RISR-N/2012/01/WorldDay64m/20120112.001/20120112.001_lp_1min.h5'};

fnames = {
     '/Volumes/ISR_DATA_02/processed_data/RISR-N/2012/01/WorldDay64m/20120112.001/20120112.001_lp_1min-cal.h5'};


thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');