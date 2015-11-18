xticks = floor(AllTime(1)):1:ceil(AllTime(end));


Ibad=find(AllStdNmax./AllNmax>2 | AllNmax>1e13); 
AllStdNmax(Ibad)=NaN; AllNmax(Ibad)=NaN; AllNmax1(Ibad)=NaN;
AllStdHmax(Ibad)=NaN; AllHmax(Ibad)=NaN; AllHmax1(Ibad)=NaN;

cinterp = interp1(cadiStruct.dnum,cadiStruct.NmF2,AllTime,'linear'); %cinterp(AllNmax<1e11)=NaN;
%cinterp = interp1(AllTime,AllNmax,cadiStruct.dnum,'linear',NaN);
doff = (AllNmax)./cinterp; moff = nanmedian(doff);    
%doff = (cadiStruct.NmF2-cinterp)./cadiStruct.NmF2; moff = nanmedian(doff);    
[N,X]=hist(doff,linspace(0,2,100));

cinterp = interp1(cadiStruct.dnum,cadiStruct.hmf2,AllTime,'linear');
doff2 = (cinterp-AllHmax); moff2 = nanmedian(doff2);    
[N2,X2]=hist(doff2,linspace(-100,100,100));

fpos=[0 0 1 1];
f1=figure('Units','normalized','Position',fpos); set(gcf,'paperpositionmode','auto');
set(gcf,'visible','on');

smt = 10;

subplot(2,3,1:2);
l=plot(AllTime,AllNmax,'k'); set(l,'linewidth',1.0);
hold on;
l=plot(AllTime,AllNmax1,'b'); set(l,'linewidth',1.0);
l=plot(AllTime,AllNmax+smooth(AllStdNmax,smt)','k--');
l=plot(AllTime,AllNmax-smooth(AllStdNmax,smt)','k--');
hold on;
l=plot(cadiStruct.dnum,cadiStruct.NmF2,'r'); set(l,'linewidth',1.0);
l=plot(cadiStruct.dnum,cadiStruct.NmF2+cadiStruct.dNmF2,'r--'); 
l=plot(cadiStruct.dnum,cadiStruct.NmF2-cadiStruct.dNmF2,'r--'); 
ylabel('N_{max} (m^{-3})');
xlim([xticks(1) xticks(end)]); 
set(gca,'xtick',xticks);
datetick('x','keeplimits','keepticks');
title(titstr);

subplot(2,3,3);
bar(X,N);
xlim([0 2]);
hold on;
plot([moff,moff],[0 max(N)*1.1],'r--');
text(moff,max(N)*1.05,sprintf('med: %2.2f',moff));
title('ISR/CADI N_{max} Histogram');
xlabel('Fractional Offset');

subplot(2,3,4:5);
l=plot(AllTime,AllHmax,'k'); set(l,'linewidth',1.0);
hold on;
l=plot(AllTime,AllHmax1,'b'); set(l,'linewidth',1.0);
l=plot(AllTime,AllHmax+smooth(AllStdHmax,smt)','k--');
l=plot(AllTime,AllHmax-smooth(AllStdHmax,smt)','k--');
l=plot(cadiStruct.dnum,cadiStruct.hmf2,'r'); set(l,'linewidth',1.0);
ylabel('H_{max} (km)');
xlim([xticks(1) xticks(end)]); 
set(gca,'xtick',xticks);
datetick('x','keeplimits','keepticks');

subplot(2,3,6);
bar(X2,N2);
xlim([-100 100]);
hold on;
plot([moff2,moff2],[0 max(N2)*1.1],'r--');
text(moff2,max(N2)*1.05,sprintf('med: %2.2f',moff2));
title('CADI-ISR H_{max} Histogram');
xlabel('Offset (km)');



%%
%cross-correlation check
ind = find(~isnan(cadiStruct.NmF2));
ind1 = find(isnan(AllNmax));
AllTime_tmp = AllTime;AllTime_tmp(ind1) = [];
AllNmax_tmp = AllNmax;AllNmax_tmp(ind1) = [];
AllNmax1_tmp = AllNmax1;AllNmax1_tmp(ind1) = [];
AllStdNmax_tmp = AllStdNmax;AllStdNmax_tmp(ind1) = [];
AllStdHmax_tmp = AllStdHmax;AllStdHmax_tmp(ind1) = [];
AllHmax_tmp = AllHmax;AllHmax_tmp(ind1) = [];
AllHmax1_tmp = AllHmax;AllHmax1_tmp(ind1) = [];

NmF2_interp = interp1(cadiStruct.dnum(ind),cadiStruct.NmF2(ind),AllTime_tmp);
ind = find(~isnan(cadiStruct.dNmF2));
dNmF2_interp = interp1(cadiStruct.dnum(ind),cadiStruct.dNmF2(ind),AllTime_tmp);

max_xcorr = 0;
shift = 0;
for i =1:length(AllTime_tmp)
    a = xcorr(NmF2_interp,AllNmax_tmp,0);
    if a > max_xcorr
       max_xcorr = a;
       shift = i;
    end
    
    NmF2_interp = circshift(NmF2_interp,[0 1]);
end

xticks = floor(AllTime(1)):1:ceil(AllTime(end));


NmF2_shifted = interp1(AllTime_tmp,circshift(NmF2_interp,[0 shift]),cadiStruct.dnum);
dNmF2_shifted = interp1(AllTime_tmp,circshift(dNmF2_interp,[0 shift]),cadiStruct.dnum);

I = find(isnan(cadiStruct.NmF2));
NmF2_shifted(I) = NaN;
I = find(isnan(cadiStruct.dNmF2));
dNmF2_shifted(I) = NaN;


Ibad=find(AllStdNmax./AllNmax>2 | AllNmax>1e13); 
AllStdNmax(Ibad)=NaN; AllNmax(Ibad)=NaN; AllNmax1(Ibad)=NaN;
AllStdHmax(Ibad)=NaN; AllHmax(Ibad)=NaN; AllHmax1(Ibad)=NaN;

cinterp = interp1(cadiStruct.dnum,NmF2_shifted,AllTime,'linear'); %cinterp(AllNmax<1e11)=NaN;
%cinterp = interp1(AllTime,AllNmax,cadiStruct.dnum,'linear',NaN);
doff = (AllNmax)./cinterp; moff = nanmedian(doff);    
%doff = (cadiStruct.NmF2-cinterp)./cadiStruct.NmF2; moff = nanmedian(doff);    
[N,X]=hist(doff,linspace(0,2,100));

cinterp = interp1(cadiStruct.dnum,cadiStruct.hmf2,AllTime,'linear');
doff2 = (cinterp-AllHmax); moff2 = nanmedian(doff2);    
[N2,X2]=hist(doff2,linspace(-100,100,100));

fpos=[0 0 1 1];
f1=figure('Units','normalized','Position',fpos); set(gcf,'paperpositionmode','auto');
set(gcf,'visible','on');

smt = 10;

subplot(2,3,1:2);
l=plot(AllTime,AllNmax,'k'); set(l,'linewidth',1.0);
hold on;
l=plot(AllTime,AllNmax1,'b'); set(l,'linewidth',1.0);
l=plot(AllTime,AllNmax+smooth(AllStdNmax,smt)','k--');
l=plot(AllTime,AllNmax-smooth(AllStdNmax,smt)','k--');
hold on;
l=plot(cadiStruct.dnum,NmF2_shifted,'r'); set(l,'linewidth',1.0);
l=plot(cadiStruct.dnum,NmF2_shifted+dNmF2_shifted,'r--'); 
l=plot(cadiStruct.dnum,NmF2_shifted-dNmF2_shifted,'r--'); 
ylabel('N_{max} (m^{-3})');
xlim([xticks(1) xticks(end)]); 
set(gca,'xtick',xticks);
datetick('x','keeplimits','keepticks');
title(titstr);

subplot(2,3,3);
bar(X,N);
xlim([0 2]);
hold on;
plot([moff,moff],[0 max(N)*1.1],'r--');
text(moff,max(N)*1.05,sprintf('med: %2.2f',moff));
title('ISR/CADI N_{max} Histogram');
xlabel('Fractional Offset');

subplot(2,3,4:5);
l=plot(AllTime,AllHmax,'k'); set(l,'linewidth',1.0);
hold on;
l=plot(AllTime,AllHmax1,'b'); set(l,'linewidth',1.0);
l=plot(AllTime,AllHmax+smooth(AllStdHmax,smt)','k--');
l=plot(AllTime,AllHmax-smooth(AllStdHmax,smt)','k--');
l=plot(cadiStruct.dnum,cadiStruct.hmf2,'r'); set(l,'linewidth',1.0);
ylabel('H_{max} (km)');
xlim([xticks(1) xticks(end)]); 
set(gca,'xtick',xticks);
datetick('x','keeplimits','keepticks');

subplot(2,3,6);
bar(X2,N2);
xlim([-100 100]);
hold on;
plot([moff2,moff2],[0 max(N2)*1.1],'r--');
text(moff2,max(N2)*1.05,sprintf('med: %2.2f',moff2));
title('CADI-ISR H_{max} Histogram');
xlabel('Offset (km)');



