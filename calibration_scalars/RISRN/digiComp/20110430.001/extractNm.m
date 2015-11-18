clear all; close all;

%fnames = {
%    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay54m/20110430.001/20110430.001_lp_1min.h5'};

fnames = {
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay54m/20110430.001/20110430.001_lp_1min-cal.h5'};


thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');