clear all; close all;


fnames = {
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay57rm/20110105.001/20110105.001_lp_1min.h5',
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay57rm/20110105.002/20110105.002_lp_1min.h5',
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay57rm/20110114.001/20110114.001_lp_1min.h5',
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay54m/20110118.001/20110118.001_lp_1min-cal.h5',
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay54m/20110201.001/20110201.001_lp_1min-cal.h5',
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay54m/20110209.001/20110209.001_lp_1min-cal.h5',
    '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay54m/20110209.002/20110209.002_lp_1min-cal.h5'};



thisdir=pwd;
cd('..');

extractNmrun;

cd(thisdir);

save('out.mat','fnames','AllNmax','AllHmax','AllNmax1','AllHmax1','AllStdNmax','AllStdHmax','AllTime');