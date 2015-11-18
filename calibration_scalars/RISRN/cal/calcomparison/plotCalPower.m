clear all;
close all;

dpaths = {'/Volumes/AMISR_010/Data AMISR Resolute N/', '/Volumes/AMISR_013/Data AMISR Resolute N/','/Volumes/AMISR_015/Data AMISR Resolute N/'};

for ipath = 1:length(dpaths)
   tdpath = char(dpaths{ipath});
   ddirs = dir([tdpath '*.*']);
  
   for idir = 1:length(ddirs)
       tpath = [tdpath ddirs(idir).name '/'];
       
       files=dir([tpath '*.dt3.h5']);
       Nfiles = length(files);
       
       if Nfiles>0
           dfile = ceil(Nfiles/10);
           try
               
               expfile=dir([tpath 'Setup/*.exp']);
               expname=expfile.name(1:end-4);

               oname = [expname '/' ddirs(idir).name];
               
               if ~exist([oname '.mat'],'file')
                   
                   for ifile=1:dfile:Nfiles
                       fname=[tpath files(ifile).name];
                       fprintf('Doing %s - %s, file %d of %d\n',expname,ddirs(idir).name,ifile,length(files));
                       %dat=read_data(fname,strvcat('/S','/Time'));
                       
                       Cal=double(hdf5read(fname,'/S/Cal/Power/Data'));
                       CalInt=double(hdf5read(fname,'/S/Cal/PulsesIntegrated')); CalInt=reshape(CalInt,[1 size(CalInt,1) size(CalInt,2)]);
                       
                       Noise=double(hdf5read(fname,'/S/Noise/Power/Data'));
                       NoiseInt=double(hdf5read(fname,'/S/Noise/PulsesIntegrated')); NoiseInt=reshape(NoiseInt,[1 size(NoiseInt,1) size(NoiseInt,2)]);
                       
                       Time=hdf5read(fname,'/Time/MatlabTime');
                       
                       AeuTx = hdf5read(fname,'/Tx/AeuTx');
                       AeuRx = hdf5read(fname,'/Rx/AeuRx');
                       TxPower = hdf5read(fname,'/Tx/Power');
                       SysInfo = [mean(mean(AeuTx)), mean(mean(AeuRx)), mean(mean(TxPower))];
                       
                       Time=mean(mean(Time));
                       Cal=median(Cal,1); Noise=median(Noise,1);
                       Cal=Cal./CalInt; Noise=Noise./NoiseInt;
                       Cal=squeeze(median(Cal-Noise,3));
                       Noise = squeeze(median(Noise,3));
                       
                       if ifile==1
                           AllNoise=Noise;
                           AllCal=Cal;
                           AllTime=Time;
                           AllSysInfo=SysInfo;
                       else
                           AllCal=cat(1,AllCal,Cal);
                           AllNoise=cat(1,AllNoise,Noise);
                           AllTime=cat(1,AllTime,Time);
                           AllSysInfo=cat(1,AllSysInfo,SysInfo);;
                           
                       end
                       
                   end
                   
                   BW = hdf5read(fname,'/Rx/Bandwidth');
                   try
                       Tsys = hdf5read(fname,'/Rx/CalTemp');
                   catch
                       Tsys=115;
                   end
                   
                   if ~exist(expname,'dir')
                       mkdir(expname);
                   end
                   
                   save([oname '.mat'],'AllTime','AllCal','AllNoise','BW','Tsys','AllSysInfo');
                   
                   plotcomp;
                   
               else 
                   fprintf('Skipping %s - %s\n',expname,ddirs(idir).name);
               end
               
           catch
               fprintf('Error in %s - %s\n',expname,ddirs(idir).name);
               %xxxx
           end
            
        end
   end

    
    
end



xxx




%ddir='/Volumes/AMISR_015/Data AMISR Resolute N/20110610.001/'; out = '20110610.001.mat';
%ddir='/Volumes/AMISR_015/Data AMISR Resolute N/20110430.001/'; out = '20110430.001.mat';
%ddir='/Volumes/AMISR_013/Data AMISR Resolute N/20110201.001/'; out = '20110201.001.mat';
%ddir='/Volumes/AMISR_013/Data AMISR Resolute N/20110209.002/'; out = '20110209.002.mat';
%ddir='/Volumes/AMISR_013/Data AMISR Resolute N/20110118.001/'; out = '20110118.001.mat';
ddir='/Volumes/AMISR_013/Data AMISR Resolute N/20110114.001/'; out = '20110114.001.mat';
%ddir='/Volumes/AMISR_013/Data AMISR Resolute N/20101209.004/'; out = '20101209.004.mat';


for ifile=1:1:length(files)
    fname=[ddir files(ifile).name];
    fprintf('Doing %s, file %d of %d\n',files(ifile).name,ifile,length(files));
    %dat=read_data(fname,strvcat('/S','/Time'));
    
    Cal=double(hdf5read(fname,'/S/Cal/Power/Data'));
    CalInt=double(hdf5read(fname,'/S/Cal/PulsesIntegrated')); CalInt=reshape(CalInt,[1 size(CalInt,1) size(CalInt,2)]);

    Noise=double(hdf5read(fname,'/S/Noise/Power/Data'));
    NoiseInt=double(hdf5read(fname,'/S/Noise/PulsesIntegrated')); NoiseInt=reshape(NoiseInt,[1 size(NoiseInt,1) size(NoiseInt,2)]);

    Time=hdf5read(fname,'/Time/MatlabTime');
    
    AeuTx = hdf5read(fname,'/Tx/AeuTx');
    AeuRx = hdf5read(fname,'/Rx/AeuRx');
    TxPower = hdf5read(fname,'/Tx/Power');
    SysInfo = [mean(mean(AeuTx)), mean(mean(AeuRx)), mean(mean(TxPower))];
    
    Time=mean(mean(Time));
    Cal=median(Cal,1); Noise=median(Noise,1);
    Cal=Cal./CalInt; Noise=Noise./NoiseInt;
    Cal=squeeze(median(Cal-Noise,3));
    Noise = squeeze(median(Noise,3));
    
    if ifile==1
        AllNoise=Noise;
        AllCal=Cal;
        AllTime=Time;
        AllSysInfo=SysInfo;
    else
        AllCal=cat(1,AllCal,Cal);
        AllNoise=cat(1,AllNoise,Noise);
        AllTime=cat(1,AllTime,Time);
        AllSysInfo=cat(1,AllSysInfo,SysInfo);;
        
    end    
    
end

BW = hdf5read(fname,'/Rx/Bandwidth');
Tsys = hdf5read(fname,'/Rx/CalTemp');

save(out,'AllTime','AllCal','AllNoise','BW','Tsys','AllSysInfo');
