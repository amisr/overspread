function []=cadiread()

dir2do='/Volumes/AMISR_010/Resolute CADI/091114RB/';
odir=[dir2do 'grams/'];
fnames=dir([dir2do '*.md2']);
dofof2=0;

if ~exist(odir,'dir')
    mkdir(odir);
end

for ifname=1:length(fnames)
    fprintf('doing %s\n',fnames(ifname).name);
    fname=[dir2do fnames(ifname).name];
    fid=fopen(fname,'r','l');
    hdr=readhdr(fid);
    while ~feof(fid)
        try
            [x,y,p,tit]=readbuf(fid,hdr);
            print(gcf,'-dpng',[odir tit '.png']);
        catch
        end
        if dofof2
            fof2=findfof2(x,y);
        end
    end
end

return

function hdr=readhdr(fid)

timestamp=fread(fid,25,'schar'); timestamp=char(timestamp)'; % station and timestamp
filetype=fread(fid,1,'schar'); filetype=char(filetype); % I=individual, H=hourly
numfreq=fread(fid,1,'short'); % number of frequencies
dopp=fread(fid,1,'uchar'); % doppler series length
minht=fread(fid,1,'short'); % minimum height
maxht=fread(fid,1,'short'); % maximum height
pps=fread(fid,1,'uchar'); % pulses per sec
navg=fread(fid,1,'uchar'); % # of pulses averaged
basethreshold=fread(fid,1,'short'); % 100 x minpower (bits sq.) before save
noisethreshold=fread(fid,1,'short'); % 100 x mult for avg noise power
mindopp=fread(fid,1,'uchar'); % min number of dopplers before saving
secbwsamp=fread(fid,1,'short'); % sec between samples
gaincontrol=fread(fid,1,'schar'); gaincontrol=char(gaincontrol); % 'N'=none, or a number
sigprocess=fread(fid,1,'schar'); sigprocess=char(sigprocess); % F=FFT, T=timeseq, N=None
numrec=fread(fid,1,'uchar'); % number of receivers
spares=fread(fid,11,'uchar'); % spares
freq=fread(fid,numfreq,'float'); % frequencies

% decode timestamp
site=timestamp(1:3);
date=timestamp(5:10);
hour=str2num(timestamp(12:13));
min=str2num(timestamp(15:16));
sec=str2num(timestamp(18:19));
yr=str2num(timestamp(21:24));

hdr=struct('TimeStamp',timestamp,'filetype',filetype,'NumFreqs',numfreq,...
    'DoppSerLen',dopp,'minHt',minht,'maxHt',maxht,'PulsesPerSec',pps,'nAvg',navg,...
    'BaseThreshold',basethreshold,'NoiseThreshold',noisethreshold,'minDopp',mindopp,...
    'secBWsamp',secbwsamp,'GainControl',gaincontrol,'SigProcess',sigprocess,...
    'NumReceivers',numrec,'Spares',spares,'Frequencies',freq,...
    'Site',site,'Date',date,'Hour',hour,'Minute',min,'Second',sec,'Year',yr);

function [x,y,val,tittxt]=readbuf(fid,hdr)

dh=3.0; maxFreq=10;

time_min=fread(fid,1,'uint8');
time_sec=fread(fid,1,'uint8');

tittxt=sprintf('%s %s %.2d%.2d%.2d %d',hdr.Site,hdr.Date,hdr.Hour,time_min,time_sec,hdr.Year);

figure('visible','off'); hold on; xlabel('Frequency (MHz)','fontname','Sans-Serif','fontsize',12); ylabel('Virtual Height (km)','fontname','Sans-Serif','fontsize',12); 
title(tittxt,'fontname','Sans-Serif','fontsize',12); 
axis([0 maxFreq hdr.minHt hdr.maxHt]); set(gca,'xtick',[0:1:maxFreq]); grid on; grid minor;

flag=1; jpt=0; ipt=1; ifreq=0;
while flag & ~feof(fid)
    gainflag=fread(fid,1,'uint8');
    if gainflag==255  % end of frequencies
        flag=0;
    elseif gainflag>=224 & gainflag<=239 % have frequency marker
        if jpt>0
            for l=1:jpt
                sig(ipt-l)=m2^0.25;
            end
        end
        jpt=0; m1=0; m2=0;
        noiseflag=fread(fid,1,'uint8');
        noise=fread(fid,1,'uint16');
        ifreq=ifreq+1;
    else % have height marker
        hnum=gainflag;
        tnum=fread(fid,1,'uint8');
        
        for l=1:tnum
            dnum=fread(fid,1,'uint8'); % doppler bin number
            
            powr=0;
            for ii=1:hdr.NumReceivers
                samp=fread(fid,1,'uint8') + j*fread(fid,1,'uint8');
                powr=powr+samp*conj(samp);
            end
            powr=powr/hdr.NumReceivers;
            
            f=hdr.Frequencies(ifreq)/1.0e6;
            h=hnum*dh;
            if powr>0
                p=10*log10(powr/noise);

                if f>=0 & p>10
                    x(ipt)=f;
                    y(ipt)=h;
                    val(ipt)=p;
                    ipt=ipt+1;
                    jpt=jpt+1;
                end
            end
        end
    end
end

try
    scatter(x,y,[],val,'filled');
    caxis([0 35]);
    ch=colorbar; ylabel(ch,'SNR (dB)');
    set(gca,'fontname','Sans-Serif','fontsize',12);
catch
end

return