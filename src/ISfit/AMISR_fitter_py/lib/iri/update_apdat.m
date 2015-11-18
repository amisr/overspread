% updates ap.dat file

clear all; close all;

FID=fopen('ap.dat','a+');

geophys_dir='~/DATA/GEOPHYSICAL_PARAMS';

linelen=39;
fseek(FID,-linelen+1,1);
tline=fgetl(FID);
mon=str2num(tline(4:6));
day=str2num(tline(7:9));
year=2000+str2num(tline(1:4));
if mon==12 & day==31
    mon=00; day=00;
    year=year+1;
end
yearend=year-2000;

fname=[geophys_dir '/' num2str(year)];
if ~exist(fname)
    fprintf('Already up to date');
    return
end

FID2=fopen(fname);
if ~(mon==0 & day==0) 
    done=0;
    while ~feof(FID2) & ~done
        tline=fgetl(FID2);
        tmon=str2num(tline(3:4));
        tday=str2num(tline(5:6));
        if tmon==mon & tday==day
            done=1;
        end
    end
    if ~done
        fprintf('err, monkey?');
        return
    end
end
while ~feof(FID2)
    tline=fgetl(FID2);
    fprintf(FID,'%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3.1f\n',yearend,str2num(tline(3:4)),str2num(tline(5:6)),str2num(tline(32:34)),...
        str2num(tline(35:37)),str2num(tline(38:40)),str2num(tline(41:43)),str2num(tline(44:46)),str2num(tline(47:49)),...
        str2num(tline(50:52)),str2num(tline(53:55)),str2num(tline(66:70)));
end

fclose('all');

