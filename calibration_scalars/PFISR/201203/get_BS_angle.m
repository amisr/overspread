function alphBS=get_BS_angle(az,el)


az_bs=15*pi/180; 
el_bs=74*pi/180;

k=[cos(el)*cos(az); cos(el)*sin(az); sin(el)];
tk=rotmat(k,3,az_bs);
tk2=rotmat(tk,2,pi/2-el_bs);
alphBS=90-asin(tk2(3))*180/pi; %angle off boresight
