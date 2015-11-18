clear all; close all;

tb=load('tb.txt');
GL=load('../grating_lobes.txt');

az_bs=15*pi/180;
el_bs=74*pi/180;

az=tb(:,2)*pi/180;
el=tb(:,3)*pi/180;
for aa=1:length(az)
    k=[cos(el(aa))*cos(az(aa)); cos(el(aa))*sin(az(aa)); sin(el(aa))];
    tk=rotmat(k,3,az_bs);
    tk2=rotmat(tk,2,pi/2-el_bs);
    BS(aa)=90-asin(tk2(3))*180/pi;
    try
        I=findClosestIndex(az(aa)*180/pi,GL(:,1));
        aGL(aa)=GL(I,3)-BS(aa);
    catch
        aGL(aa)=NaN;
    end
end