clear all; close all;

fnames = {
   '/Volumes/ISR_DATA/processed_data/RISR/2010/WorldDay55m-2/20101207.001/20101207.001_lp_3min-cal.h5'};

thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');