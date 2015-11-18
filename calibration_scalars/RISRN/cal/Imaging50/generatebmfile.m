clear all; close all;

exp='Imaging50';
fname = '/Volumes/AMISR_015/Data AMISR Resolute N/20111017.002/d0046065.dt2.h5';

bmcodes = hdf5read(fname,'/S/Data/Beamcodes');
bmcodes = bmcodes(:,1); Nbeams=length(bmcodes);

bmmap = read_data(fname,'/Setup'); bmmap=bmmap.Setup.BeamcodeMap;

bm=zeros(4,Nbeams);
for ibm=1:Nbeams
    i = find(bmmap(1,:)==bmcodes(ibm));
    bm(:,ibm) = bmmap(:,i);
end

bm(4,:) = 1.95e-21;

date=datestr(now,'mm.dd.yyyy');

fid1=fopen([exp '-default-' 'ksys' '-' date '.txt'],'w');
for i=1:Nbeams
    fprintf(fid1,'%d %2.2f %2.2f %2.2e \n',bm(:,i));
end
fclose(fid1);

