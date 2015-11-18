clear all; close all;

fname='/Volumes/ISR_Data/Data AMISR Poker Flat/20070123.004/D0016185.Dt1.h5';

output=read_data([fname]); % reads the data

Acf.Range=output.S.Data.Acf.Range; Acf.dR=Acf.Range(2)-Acf.Range(1);
Acf.Lags=output.S.Data.Acf.Lags; Acf.dL=Acf.Lags(2)-Acf.Lags(1);

LPm=repmat(Acf.Range,[1 length(Acf.Lags)])-repmat(Acf.Lags*Acf.dR/Acf.dL,[1 length(Acf.Range)])';

