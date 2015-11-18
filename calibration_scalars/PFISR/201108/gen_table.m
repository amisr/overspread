clear all; close all;

exp='cal-201108';
date=datestr(now,'mm.dd.yyyy');

load('cal-201108-filelist_lp.txt_1.54_4.25-04.06.2012.mat');

bmo = read_data('bm_orig.h5'); Nbeams = size(bmo.BeamcodeMap,2);

fid=fopen([exp '-calibration-' 'scalar' '-' date '.txt'],'w');
fid1=fopen([exp '-calibration-' 'ksys' '-' date '.txt'],'w');

for ibm = 1:Nbeams
    
    tbm = bmo.BeamcodeMap(:,ibm);
        
    az=bmo.BeamcodeMap(2,ibm)*pi/180;
    el=bmo.BeamcodeMap(3,ibm)*pi/180;
    kold = bmo.BeamcodeMap(4,ibm)
    
    alphBS=get_BS_angle(az,el)
    
    ksys = x(1)*cos(alphBS*pi/180.0+x(3)).^x(2)*1e-19;
    ksysCorr = ksys / kold;
    
    BMnew(ibm,4) = ksys;
    BMnew1(ibm,4) = ksysCorr;
        
    fprintf(fid,'%d %2.2f %2.2f %2.2e %3.5f\n',tbm(1),tbm(2),tbm(3),ksys,ksysCorr);
    fprintf(fid1,'%d %2.2f %2.2f %2.2e\n',tbm(1),tbm(2),tbm(3),ksys);

end

fclose(fid);
fclose(fid1);
