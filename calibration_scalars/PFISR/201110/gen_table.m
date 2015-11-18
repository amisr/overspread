clear all; close all;

exp='cal-201110';
date=datestr(now,'mm.dd.yyyy');

BM=load('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/dat/bcotable7.txt');
x=[1.8655    6.2833   -0.1575];
Aold =1.73; Bold =2.79 ;

BMnew = BM; BMnew(:,4)=0;
BMnew1 = BM; BMnew1(:,4)=0;

fid=fopen([exp '-calibration-' 'scalar' '-' date '.txt'],'w');
fid1=fopen([exp '-calibration-' 'ksys' '-' date '.txt'],'w');

for ibm = 1:length(BM)
    
    az=BM(ibm,2)*pi/180;
    el=BM(ibm,3)*pi/180;
    
    alphBS=get_BS_angle(az,el)
    
    ksys = x(1)*cos(alphBS*pi/180.0+x(3)).^x(2);
    ksysCorr = ksys / (Aold*cos(alphBS*pi/180.0).^Bold);
    
    BMnew(ibm,4) = ksys;
    BMnew1(ibm,4) = ksysCorr;
        
    fprintf(fid,'%d %2.2f %2.2f %2.2e %3.5f\n',[BM(ibm,1:3)'; ksys*1e-19; ksysCorr]);
    fprintf(fid1,'%d %2.2f %2.2f %2.2e\n',[BM(ibm,1:3)'; ksys*1e-19]);

end

fclose(fid);
fclose(fid1);
