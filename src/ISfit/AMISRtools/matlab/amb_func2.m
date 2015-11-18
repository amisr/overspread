function [tau,R,W2dS,W2d]=amb_func(bl,L,penv,h)
%clear all; close all;

% bl=30e-6;
% L=0:16;
% penv=ones(1,16);
% h(1)=1;

c=3e8;
st=10; % oversampling factor

pl=length(penv)*bl; % pulse length

t=0; % sampling time
u=(t-2*pl):bl/st:(t+2*pl); % time
S=(t-2*pl:bl/st:(t+2*pl)); % range
R=c*S/2;
tau=(L(1)-1)*bl:bl/st:(L(end)+1)*bl;

env=zeros(size(u));
for aa=1:length(penv)
    env((aa-1)*st+1:aa*st)=penv(aa);
end
impulse=zeros(size(u));
for aa=1:length(h)
    impulse((aa-1)*st+1:aa*st)=h(aa);
end

W2d=zeros(length(S),length(tau),length(L));
for aa=1:length(L)
    v=L(aa)*bl;
    for bb=1:length(S)
        env1=interp1(u,env,t+u-S(bb),'linear',0); env1=sign(env1).*ceil(abs(env1));
        env2=interp1(u,env,t-v+u-S(bb),'linear',0); env2=sign(env2).*ceil(abs(env2));
        Wra1=impulse.*env1;
        Wra2=impulse.*env2;
        [tW2d(bb,:),lags]=xcorr(Wra1,Wra2);
        [tW2dw(bb,:)]=fft(Wra1).*fft(Wra2);
    end
    tW2d=tW2d(end:-1:1,:)/st;
    taut=lags*bl/st+v;
    I=find(taut>=(tau(1)-1e-10) & taut<=(tau(end)+1e-10));
    W2d(:,:,aa)=tW2d(:,I);
    
    clear tW2d;
    
    figure;
    imagesc(tau/bl,S/bl,W2d(:,:,aa));
    set(gca,'ydir','normal');
    colormap(gray);
    colorbar;
end

Wr=squeeze(sum(W2d,2));
Wl=squeeze(sum(W2d,1));

W2dS=sqrt(sum(W2d.^2,3));

figure;
imagesc(tau/bl,S/bl,W2dS);
set(gca,'ydir','normal');
colormap(gray);
colorbar;


return



% % pulse envelope and impulse function
% env=zeros(size(u));
% for aa=1:length(penv)
%     env((aa-1)*st+1:aa*st)=penv(aa);
% end
% impulse=zeros(size(u));
% for aa=1:length(h)
%     impulse((aa-1)*st+1:aa*st)=h(aa);
% end
% 
% % compute amplitude ambiguity function
% Wra=zeros(length(u),length(S));
% for bb=1:length(S)
%     for cc=1:length(u)
%         Wra(cc,bb)=0;
%         if (t-u(cc))>=0 & (u(cc)-S(bb))>=0
%             %I=interp1(u,impulse,t-u(cc)); I=sign(I)*ceil(abs(I)); % p(t-u)
%             E=interp1(u,env,u(cc)-S(bb)); E=sign(E)*ceil(abs(E)); % env(u-S)
%             Wra(cc,bb)=I(cc)*E; 
%         end
%     end
% end
% 
% % 2-D ambiguity function
% W2d=zeros(length(tau),length(S),length(L));
% for bb=1:length(L)
%     ll=L(bb)*bl;
%     Wra2=interp2(u',S,Wra',u'-tau(aa)+ll,S+ll,'linear',0)';
%     W2d(aa,:,bb)=conv(Wra
%     
% %    for aa=1:length(tau)
% %        Wra2=interp2(u',S,Wra',u'-tau(aa)+ll,S+ll,'linear',0)';
% %        W2d(aa,:,bb)=sum(Wra.*Wra2);
% %    end
% end
% W2d=W2d(:,end:-1:1,:); 
% W2dS=sqrt(sum(W2d.^2,3)); W2dS=W2dS/max(max(W2dS));
% 
% figure;
% imagesc(tau/bl,S/bl,W2dS');
% set(gca,'ydir','normal');
% colormap(gray);

