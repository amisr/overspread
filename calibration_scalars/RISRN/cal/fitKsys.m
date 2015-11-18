clear all; %close all;

files={
    'WorldDay53m/WorldDay53m-calibration-ksys-10.23.2009.txt',
    %'Sporadic51/Sporadic51-calibration-ksys-10.23.2009.txt'
    };

for i=1:length(files)
    if i==1
        BMCODES=load(char(files(i)))';
    else
        BMCODES=cat(2,BMCODES,load(char(files(i)))');
    end
end

load('../GratingBS.mat');
Nbeams=size(BMCODES,2);

for ibm=1:Nbeams
    IBM=find((bst.icode+32768)==BMCODES(1,ibm));
    alphBS(ibm)=bst.Bst(IBM);
    alphGL(ibm)=bst.ThGrating(IBM)
end
[Y,I] = sort(alphBS);
%alphGL(alphGL<2)=0;

sc=1e21;

thbs=linspace(0,40,100);

options = optimset(@lsqnonlin); options=optimset('TolFun',1e-12,'TolX',1e-12,'Display','iter','MaxFunEvals',1000);

x = lsqnonlin(inline('(ks-x(1)*(cos(thetas).^x(2))-x(3)*exp(-thetagl*180/pi))','x','thetas','thetagl','ks'),[1.8 1.4 1],[],[],options,alphBS(I)*pi/180,alphGL(I)*pi/180,sc*BMCODES(end,I));
x
newksys=(x(1)*cos(alphBS*pi/180).^x(2) + x(3)*exp(-alphGL))/sc;

% x = lsqnonlin(inline('(ks-x(1)*(cos(thetas).^x(2)))','x','thetas','thetagl','ks'),[1.8 1.4],[],[],options,alphBS(I(1:end-2))*pi/180,alphGL(I(1:end-2))*pi/180,sc*BMCODES(end,I(1:end-2)));
% newksys=(x(1)*cos(alphBS*pi/180).^x(2) - 1*exp(-(alphGL)*pi/180))/sc;

figure;
plot(alphBS,BMCODES(end,:),'k.');
hold on;
plot(alphBS(I),newksys(I),'-r.');
