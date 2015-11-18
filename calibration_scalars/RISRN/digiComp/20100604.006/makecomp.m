clear all; close all;

titstr = '20100604.006';

% load cadi
cadiDir = '/Users/mnicolls/Documents/Work/AMISR/RISR_CADI/unb/';
thisdir=pwd; cd(cadiDir); cadiStruct=read_unbcadi('rescOutputFile-20100531-20100604.txt'); cd(thisdir);
cadiStruct=setfield(cadiStruct,'NmF2', (cadiStruct.fof2*1e6/8980).^2*1e6);
cadiStruct=setfield(cadiStruct,'dNmF2', cadiStruct.NmF2.*cadiStruct.dfof2./cadiStruct.fof2);

% load isr
load out.mat;

thisdir = pwd;
cd('..');
makecomprun;
cd(thisdir);

print(gcf,'-depsc2','-r600',[titstr '.eps']);
