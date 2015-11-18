clear all; close all;

date=datestr(now,'mm.dd.yyyy');
exp='cal-201104';

GL=load('../../GratingLimits.txt');
BM=load('/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/dat/bcotable7.txt');

fname={'filelist_ac.txt'};
fname={'filelist_lp.txt'};

% get file list
FILES={};
%c=strvcat('b','r');
for aa=1:length(fname)
    fid=fopen(char(fname(aa)),'r');
    FPATH=fgetl(fid);
    while ~feof(fid)
        tline=fgetl(fid);
        if tline(1)~='#'
            FILES=[FILES; FPATH tline];
        end
    end
end

thbs=linspace(0,40,100);
A=0.98395;
B=3.8781;
oldksys=A*cos(thbs*pi/180.0).^B;

% % % bmo = read_data('bm_orig.h5');
% % % ksysbco=bmo.BeamcodeMap(1,:); %ksysbco=[64037, 64157];
% % % ksystab=bmo.BeamcodeMap(4,:)*1e19; %ksystab=[1.451, 1.084];

% cal from feb 2011
bmo = load('../201102/cal-201102-calibration-ksys-03.06.2012.txt');
ksysbco=bmo(:,1);
ksystab=bmo(:,4)*1e19;

figure; hold on;

for aa=1:length(FILES)
    char(FILES(aa))
    FID = fopen(char(FILES(aa)));
    TLINE1 = fgetl(FID);
    TLINE2 = fgetl(FID);    
    fclose(FID);
    TLINE1 = sscanf(TLINE1,'%f');
    TLINE2 = sscanf(TLINE2,'%f');
 
    
    az=TLINE1(2)*pi/180;
    el=TLINE1(3)*pi/180;

    
    alphBS(aa)=get_BS_angle(az,el); %angle off boresight
    [a,I]=min(abs(az*180/pi-GL(:,1)));
    aGL(aa)=GL(I,3)-alphBS(aa); % angle off grating lobe
    
    KSYScorr(aa)=TLINE2(1);
    
    I=find(TLINE1(1)==ksysbco);
    if isempty(I)
        xxx
        KSYS(aa)=KSYScorr(aa)*A*cos(alphBS(aa)*pi/180)^B;
    else
        KSYS(aa)=ksystab(I)*KSYScorr(aa);
    end
    
    fprintf('beam: %d, %2.2f,%2.2f,%2.2f,%2.2f\n',TLINE1(1),az*180/pi,el*180/pi,alphBS(aa),KSYS(aa));
    
    eKSYS(aa)=TLINE2(2)*KSYS(aa);

    plot(alphBS(aa),KSYS(aa),'k.');
    hold on;
    plot([alphBS(aa) alphBS(aa)],[KSYS(aa)-eKSYS(aa) KSYS(aa)+eKSYS(aa)]);
end

for i=1:length(ksysbco)
    I=find(BM(:,1)==ksysbco(i))
    az=BM(I,2)*pi/180
    el=BM(I,3)*pi/180
    if el>0
        tbs=get_BS_angle(az,el)

        tksys=ksystab(i);
        plot(tbs,tksys,'rx');
        hold on;
        plot(tbs,tksys*1.1,'k');
        plot(tbs,tksys*0.9,'k');
    end
end

plot(thbs,oldksys,'r-'); hold on;

oname = [exp '-' char(fname(1)) '-' date];
%save([oname '.mat'],'x');
print(gcf,'-dpng',[oname '.png']);