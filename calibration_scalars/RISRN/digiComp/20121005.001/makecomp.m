clear all; close all;

titstr = '20121005.001';

% load cadi
cadiDir = '/Volumes/ISR_DATA_02/calibration/AMISR/calibration_RISRN/RISR_CADI/unb/';
thisdir=pwd; cd(cadiDir); cadiStruct=read_unbcadi('rescOutputFile-20121005-20121010.txt'); cd(thisdir);
cadiStruct=setfield(cadiStruct,'NmF2', (cadiStruct.fof2*1e6/8980).^2*1e6);
cadiStruct=setfield(cadiStruct,'dNmF2', cadiStruct.NmF2.*cadiStruct.dfof2./cadiStruct.fof2);

% load isr
load out.mat;

thisdir = pwd;
cd('..');
makecomprun;
cd(thisdir);

print(1,'-depsc2','-r600',[titstr '-1.eps']);

print(2,'-depsc2','-r600',[titstr '-2.eps']);
