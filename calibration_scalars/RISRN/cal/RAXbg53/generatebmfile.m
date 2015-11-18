clear all; close all;

exp='RAXbg53';
fname = '/Volumes/AMISR_016/Data AMISR Resolute N/20121015.001/d0054551.dt3.h5';

bmcodes = hdf5read(fname,'/S/Data/Beamcodes');
bmcodes = bmcodes(:,1); Nbeams=length(bmcodes);

bmmap = read_data(fname,'/Setup'); bmmap=bmmap.Setup.BeamcodeMap;

bm=zeros(4,Nbeams);
for ibm=1:Nbeams
    i = find(bmmap(1,:)==bmcodes(ibm));
    bm(:,ibm) = bmmap(:,i);
end

bm(4,:) = 7.71e-22;

date=datestr(now,'mm.dd.yyyy');

fid1=fopen([exp '-default-' 'ksys' '-' date '.txt'],'w');
for i=1:Nbeams
    fprintf(fid1,'%d %2.2f %2.2f %2.2e \n',bm(:,i));
end
fclose(fid1);

