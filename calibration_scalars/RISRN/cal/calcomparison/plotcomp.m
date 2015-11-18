% clear all; close all;
% 
% n1='20110430.001';
% %n1='20110610.001';
% %n1='20110201.001';
% n1='20110209.002';
% n1='20101209.004';
% n1='20110118.001';
% n1='20110114.001';

n1 = oname;

outname=[n1 '.png'];

d1=load([n1 '.mat']); 
syst = d1.Tsys*d1.AllNoise./d1.AllCal;

fpos=[0 0 0.3 0.8];
f1=figure('Units','normalized','Position',fpos); set(gcf,'paperpositionmode','auto');
set(gcf,'visible','off');

subplot(311);
set(gcf,'visible','off');
plot(d1.AllTime,d1.AllCal);
axis tight; datetick('x','keeplimits');
title(n1);
ylabel('Cal Power (W)');

subplot(312);
set(gcf,'visible','off');
plot(d1.AllTime,d1.AllNoise);
axis tight; datetick('x','keeplimits');
ylabel('Noise Power (W)');

subplot(313);
set(gcf,'visible','off');
plot(d1.AllTime,syst);
axis tight; datetick('x','keeplimits');
ylabel('Tsys (K)');

print(gcf,'-dpng',outname);
close all;