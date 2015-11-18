%function [tau,R,W2dS,W2d]=amb_func(bl,L,penv,h)

bl=30e-6;
L=0:4;
penv=zeros(1,5);
h=1;


c=3e8;
st=5; % oversampling factor

pl=length(penv)*bl; % pulse length

u=(0*bl:bl/st:(3*pl)); % time
t=pl*2; % sampling time
S=(0*bl:bl/st:(2*pl)); % range
R=c*S/2;
tau=(-bl:bl/st:bl*L(end));

% pulse envelope and impulse function
env=zeros(size(u));
for aa=1:length(penv)
    env((aa-1)*st+1:aa*st)=penv(aa);
end
impulse=zeros(size(u));
for aa=1:length(h)
    impulse((aa-1)*st+1:aa*st)=h(aa);
end

% compute amplitude ambiguity function
Wra=zeros(length(u),length(S));
for bb=1:length(S)
    for cc=1:length(u)
        Wra(cc,bb)=0;
        if (t-u(cc))>=0 & (u(cc)-S(bb))>=0
            I=interp1(u,impulse,t-u(cc)); I=sign(I)*ceil(abs(I)); % p(t-u)
            E=interp1(u,env,u(cc)-S(bb)); E=sign(E)*ceil(abs(E)); % env(u-S)
            Wra(cc,bb)=I*E; 
        end
    end
end

% 2-D ambiguity function
W2d=zeros(length(tau),length(S),length(L));
for bb=1:length(L)
    ll=L(bb)*bl;
    for aa=1:length(tau)
        Wra2=interp2(u',S,Wra',u'-tau(aa)+ll,S+ll,'linear',0)';
        W2d(aa,:,bb)=sum(Wra.*Wra2);
    end
end
W2d=W2d(:,end:-1:1,:); 
W2dS=sqrt(sum(W2d.^2,3)); W2dS=W2dS/max(max(W2dS));

figure;
imagesc(tau/bl,S/bl,W2dS');
set(gca,'ydir','normal');
colormap(gray);

