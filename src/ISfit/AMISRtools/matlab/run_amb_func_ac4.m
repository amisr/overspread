clear all; close all;

bl=30e-6; % baud length (us)
L=2:2:10; % lags to compute
penv=[1 1 1 1 ;
    1 -1 1 1;
    1 1 -1 1;
    1 -1 -1 1;
    1 1 1 -1;
    1 -1 1 -1;
    1 1 -1 -1;
    1 -1 -1 -1;]; % pulse envelope
h=[1 1 1 1 ;
    1 -1 1 1;
    1 1 -1 1;
    1 -1 -1 1;
    1 1 1 -1;
    1 -1 1 -1;
    1 1 -1 -1;
    1 -1 -1 -1;]; % impulse response

for aa=1:size(penv,1)
    [tau,R,W2dS,W2d]=amb_func2(bl,L,penv(aa,:),h(aa,:));
    W2dt(:,:,:,aa)=W2d;
end
W2dt2=sum(W2dt,4);
