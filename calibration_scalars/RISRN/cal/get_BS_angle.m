function alphBS=get_BS_angle(azi,eli)

az_bs=26*pi/180;
el_bs=55*pi/180;

for i=1:length(azi)
    az=azi(i)*pi/180; el=eli(i)*pi/180;
    
    k=[cos(el)*cos(az); cos(el)*sin(az); sin(el)];
    tk=rotmat(k,3,az_bs);
    tk2=rotmat(tk,2,pi/2-el_bs);
    alphBS(i)=90-asin(tk2(3))*180/pi; %angle off boresight
end