clear all; close all;

expType = 'WorldDay55m';

ddir='/Volumes/ISR_DATA/processed_data/RISR/2010/WorldDay55m/20100504.001/cal-0/';

fnames='*_lp*_.txt'; tittext='Long Pulse';

FILES=dir([ddir fnames]);

for rrr=1:length(FILES)
   
    fprintf('Doing %s\n',FILES(rrr).name);
    
    FID = fopen([ddir FILES(rrr).name]);
    TLINE1 = fgetl(FID); TLINE2 = fgetl(FID);    
    TLINE3 = fgetl(FID); TLINE4 = fgetl(FID);    
    TLINE5 = fgetl(FID); TLINE6 = fgetl(FID); TLINE7 = fgetl(FID); 
    fclose(FID);
    TLINE1 = sscanf(TLINE1,'%f'); TLINE2 = sscanf(TLINE2,'%f');
    TLINE3 = sscanf(TLINE3,'%f'); TLINE4 = sscanf(TLINE5,'%f');
    TLINE5 = sscanf(TLINE5,'%f'); TLINE6 = sscanf(TLINE6,'%f');
    TLINE7 = sscanf(TLINE7,'%f');
 
    UTIME(rrr)=mean(TLINE4); 
    KSYScorr(rrr)=TLINE2(1); eKSYScorr(rrr)=TLINE2(2); 
%     if eKSYScorr(rrr)/KSYScorr(rrr)>0.4
%         eKSYScorr(rrr)=NaN;
%         KSYScorr(rrr)=NaN;
%     end
    Power(rrr)=TLINE7;
        
end
tmp=diff(UTIME); I=find(tmp<0); I=I+1; KSYScorr(I)=NaN; 
%KSYScorr(find(KSYScorr<0.6 | KSYScorr>2.0))=NaN;

UTIME=UTIME(~isnan(KSYScorr));
Power=Power(~isnan(KSYScorr));
eKSYScorr=eKSYScorr(~isnan(KSYScorr));
KSYScorr=KSYScorr(~isnan(KSYScorr));

%P=polyfit(DNTIME,KSYScorr,4);
%modKSYScorr=polyval(P,DNTIME);

UTIMEout=UTIME(1):15*60:UTIME(end);

sc=24*3600;
%modKSYScorrout=csaps(UTIME/sc,KSYScorr,.9,UTIMEout/sc);
modKSYScorrout=median(KSYScorr)*ones(size(UTIMEout));

omat=[UTIMEout; modKSYScorrout]';
save([expType '_cal_' tittext '.txt'],'-ascii','omat');

%modKSYScorr=csaps(UTIME/sc,KSYScorr,.9,UTIME/sc) ;
modKSYScorr=median(KSYScorr)*ones(size(UTIME));

if isempty(strfind(fnames,'calman'))
    dksys=(KSYScorr-modKSYScorr)./modKSYScorr;
else 
    dksys=KSYScorr-1;
    'xx'
end

[mu,sigma,muci,sigmaci] = normfit(dksys);

% setup plot
npr=2;
npc=1;
fpos=[0 0 0.75 0.8];
f1=figure('Units','normalized','Position',fpos,'Visible','On'); set(gcf,'paperpositionmode','auto');
POS=[0.05 0.5 fpos(3)/(npc) fpos(4)/(npr)]; dx=0.025; dy=0.05;
ii=0;
for ff=1:npr
    for gg=1:npc
        ii=ii+1;
        H(ff,gg)=axes('Units','normalized','Position',[POS(1)+(POS(3)+dx)*(gg-1) POS(2)-(POS(4)+dy)*(ff-1) POS(3) POS(4)]);
        H2(ff,gg)=axes('Units','normalized','Position',[POS(1)+(POS(3)+dx)*(gg-1) POS(2)-(POS(4)+dy)*(ff-1) POS(3) POS(4)]...
            ,'Color','None','YAxisLocation','Right');
    end
end

axes(H(1,1));
plot(utc2date(UTIME),Power/1e6,'k.');
xlim([min(utc2date(UTIME)) max(utc2date(UTIME))]);
datetick('x','mm/dd','keepticks');
title(tittext);
ylabel('Power (MW) - Black dots');
xlabel('Date');

axes(H2(1,1));
%errorbar(DNTIME,KSYScorr,eKSYScorr,eKSYScorr,'b.');
plot(utc2date(UTIME),KSYScorr,'b.'); hold on;
plot(utc2date(UTIME),modKSYScorr,'b-');
xlim([min(utc2date(UTIME)) max(utc2date(UTIME))]);
datetick('x','mm/dd','keepticks');
set(gca,'Color','None','YAxisLocation','Right');
ylabel('Ksys Corr - Blue dots');

X=linspace(-0.5,0.5,30);
axes(H(2,1));
%errorbar(DNTIME,KSYScorr,eKSYScorr,eKSYScorr,'b.');
N=hist(dksys,X); 
bar(X,N/max(N)); hold on;
plot(X,normpdf(X,mu,sigma)*sigma*sqrt(2*pi),'k');
%plot(DNTIME,modKSYScorr,'b-');
%xlim([min(DNTIME) max(DNTIME)]);
%datetick('x','mm/dd','keepticks');
%set(gca,'Color','None','YAxisLocation','Right');
xlabel(['\sigma=' num2str(sigma,'%2.2e') ' \mu=' num2str(mu,'%2.2e')]);
axes(H2(2,1));
set(gca,'Color','None','YAxisLocation','Right','xtick',[],'ytick',[],'xticklabel',[],'yticklabel',[]);

print(gcf,'-dpng',[expType '_cal_' tittext '.png']);
