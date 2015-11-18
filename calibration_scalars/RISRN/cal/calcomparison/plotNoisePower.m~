clear all;
close all;

ddir='/Volumes/AMISR_013/Data AMISR Resolute N/20100504.001/';
files=dir([ddir '*.dt3.h5']);

for ifile=1:1:length(files)
    fname=[ddir files(ifile).name];
    fprintf('Doing %s, file %d of %d\n',files(ifile).name,ifile,length(files));
    dat=read_data(fname);

    Time=dat.Time.MatlabTime;
    Cal=median(dat.S.Cal.Power.Data,1); Noise=median(dat.S.Noise.Power.Data,1);
    Cal=squeeze(Noise);
    
    if ifile==1
        AllCal=Cal;
        AllTime=Time;
    else
        AllCal=cat(2,AllCal,Cal);
        AllTime=cat(2,AllTime,Time);
    end
end