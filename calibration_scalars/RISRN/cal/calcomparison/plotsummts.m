clear all; close all;

expdirs = {'WorldDay54m','WorldDay54m5','WorldDay54mTest','WorldDay54mTest2'}; n1='WorldDay54m';
%expdirs = {'WorldDay55m'}; n1='WorldDay55m';

expdirs= 'WorldDay*'; n1='AllWorldDay';

matfs=[];
if iscell(expdirs)
    for idir = 1:length(expdirs)
        tmatfs = dir([char(expdirs{idir}) '/*.mat']);
        if idir==1
            matfs=tmatfs;
        else
            matfs=cat(matfs,tmatfs);
        end
    end
else
    expdirs=dir(expdirs);
    for idir = 1:length(expdirs)
        tmatfs = dir([expdirs(idir).name '/*.mat']);
        for ifile=1:length(tmatfs)
            matfs=strvcat(matfs,[expdirs(idir).name '/' tmatfs(ifile).name]);
        end
    end    
end

 
cnt=1;

Cal=zeros(length(matfs),100)*NaN;
Noise=zeros(length(matfs),100)*NaN;
Ksys=zeros(length(matfs),100)*NaN;

for ifile=1:length(matfs)
    
    d=load(deblank(matfs(ifile,:)));
    Nbeams = size(d.AllCal,2);
    
    Time(cnt) = mean(d.AllTime);
    Cal(cnt,1:Nbeams) = median(d.AllCal);
    Noise(cnt,1:Nbeams)=median(d.AllCal);
    Ksys(cnt,1:Nbeams)=median(d.Tsys*d.AllNoise./d.AllCal);
    
    cnt=cnt+1
end
       
    
    


xlims=[datenum(2009,10,1), now];

fpos=[0 0 0.5 0.8];
f1=figure('Units','normalized','Position',fpos); set(gcf,'paperpositionmode','auto');
set(gcf,'visible','off');

subplot(311);
set(gcf,'visible','off');
l=semilogy(Time,Cal,'k.'); set(l,'markersize',10);
xlim(xlims);
datetick('x','mmm-yy','keeplimits');
title(n1);
ylabel('Cal Power');

subplot(312);
set(gcf,'visible','off');
l=semilogy(Time,Noise,'k.'); set(l,'markersize',10);
xlim(xlims);
datetick('x','mmm-yy', 'keeplimits');
ylabel('Noise Power');

subplot(313);
set(gcf,'visible','off');
l=plot(Time,Ksys,'k.'); set(l,'markersize',10);
xlim(xlims);
datetick('x','mmm-yy','keeplimits');
ylabel('Tsys (K)');

set(gcf,'visible','on');

print(gcf,'-dpng','-r300',[n1 '-summ.png']);
