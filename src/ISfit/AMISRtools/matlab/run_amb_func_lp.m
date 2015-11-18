clear all;

bl=30e-6; % baud length (us)
L=0:16; % lags to compute
penv=ones(1,16); % pulse envelope
h=[1]; % impulse response

[tau,R,W2dS,W2d]=amb_func(bl,L,penv,h);

