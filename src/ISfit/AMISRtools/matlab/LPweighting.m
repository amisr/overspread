clear all; close all;

n=16; % number of lags
v=4; % volume index

k=0:n-1; % lag number

W=-1/n/v*k.^2+(n-v)/(n*v)*k+1;

figure;
plot(k,W)