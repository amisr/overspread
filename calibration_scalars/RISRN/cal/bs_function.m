clear all; close all;

bfile = '/Users/mnicolls/Documents/Work/AMISR/RISR/cal/WorldDay54m/07122011/WorldDay54m-calibration-ksys-07.12.2011.txt';

bmcodes = load(bfile); ksys=bmcodes(:,4)';

alphBS=get_BS_angle(bmcodes(:,2),bmcodes(:,3));
[a,amin] = min(alphBS); kmin = ksys(amin);

thbs=linspace(0,40,100);

sc=ones(size(alphBS));
I=1:length(alphBS);
options = optimset(@lsqnonlin); options=optimset('TolFun',1e-12,'TolX',1e-12,'Display','iter','MaxFunEvals',1000);
x = lsqnonlin(inline('(ks-x(1)*cos(thetas).^x(2)).*sc','x','thetas','ks','sc'),[kmin*1e19 2],[],[],options,alphBS(I)*pi/180,ksys(I)*1e19,sc(I));
ksysmod=x(1)*cos(thbs*pi/180).^(x(2))*1e-19;

bfile2 = '/Volumes/AMISR_004/processed_data/RISR/2011/WorldDay57rm/20110105.001/WorldDay57m-calibration-ksys-07.12.2011.txt';

bmcodes2 = load(bfile2); ksys2=bmcodes2(:,4)';

alphBS2=get_BS_angle(bmcodes2(:,2),bmcodes2(:,3));

figure;
plot(alphBS,bmcodes(:,4),'k.')
hold on;
plot(alphBS2,bmcodes2(:,4),'r.')
plot(thbs,ksysmod,'r');