%function [tau,range,W]=amb_func

clear all; close all;

t=30; % sampling time
S=0:100; % range
u=0:200;
L=0:20;

env(1:16)=1;
impulse=[1];

for aa=1:length(L)
    lag=L(aa);
    for bb=1:length(S)
        a=conv(impulse,env);
        try
            a1=a(t-S(bb));
            a2=a(t-S(bb)-lag);
        catch
            a1=0;
            a2=0;
        end
        Wtt(aa,bb)=a1*a1; % range ambiguity function
    end
end

figure; 
surf(L,S,Wtt');