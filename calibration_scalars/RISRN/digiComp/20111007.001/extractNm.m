clear all; close all;

%fnames = {
%    '/Volumes/ISR_DATA_01/processed_data/RISR/2011/WorldDay64m/20111007.001/20111007.001_lp_1min.h5'};

fnames = {
     '/Volumes/ISR_DATA_01/processed_data/RISR/2011/WorldDay64m/20111007.001.done/20111007.001_lp_1min-cal.h5'};


thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');