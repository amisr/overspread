clear all; close all;

N=10; Ninc=1;

theta=linspace(0,2*pi,100);
PP=zeros(size(theta));

for bb=1:Ninc

    x=(rand(N,1)-0.5)*2;
    y=(rand(N,1)-0.5)*2;

    for aa=1:length(theta)
        ox=cos(theta(aa)); oy=sin(theta(aa));

        d=sqrt((x-ox).^2+(y-oy).^2);

        E=exp(j*2*pi*d);

        P(aa)=abs(sum(E)).^2;
    end
    
    PP=PP+P;
end
P=10*log10(P);
PP=10*log10(PP/Ninc);

fontname='Helvetica';
axsize=12;
labsize=12;

figure; 
subplot(221);
l=plot(x,y,'k.'); set(l,'Markersize',10);
axis([-1 1 -1 1]);
set(gca,'fontname',fontname,'fontsize',axsize);
title([num2str(N) ' Electron Positions'],'fontname',fontname,'fontsize',labsize);

subplot(222);
l=polar(theta,P,'k'); set(l,'linewidth',1.5);
l=get(gca,'Position'); 
set(gca,'Position',[l(1)-0.1 l(2)-0.1 l(3)*1.3 l(4)*1.3]);
set(gca,'fontname',fontname,'fontsize',axsize);
set(gca,'Ylim',[-21 21],'Xlim',[-21 21]);
title(['Relative cross section (dB)'],'fontname',fontname,'fontsize',labsize);

figure
l=polar(theta,PP,'k'); set(l,'linewidth',1.5);
set(gca,'fontname',fontname,'fontsize',axsize);
title([num2str(Ninc) ' Integrations'],'fontname',fontname,'fontsize',labsize);
set(gca,'Ylim',[-21 21],'Xlim',[-21 21]);