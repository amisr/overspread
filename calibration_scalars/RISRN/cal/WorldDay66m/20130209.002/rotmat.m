function out=rotmat(in,dir,angle)

if dir==1 % x
    rotmat=[
        1 0 0;
        0 cos(angle) sin(angle);
        0 -sin(angle) cos(angle)];
elseif dir==2 % y
    rotmat=[
        cos(angle) 0 -sin(angle);
        0 1 0;
        sin(angle) 0 cos(angle)];
elseif dir==3 % z
    rotmat=[
        cos(angle) sin(angle) 0;
        -sin(angle) cos(angle) 0;
        0 0 1];
end

out=rotmat*in;