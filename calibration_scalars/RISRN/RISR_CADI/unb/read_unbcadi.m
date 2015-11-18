function ostrc = read_unbcadi(fname)

try
    [yr,mon,day,hr,min,sec,fof2,dfof2,hmf2]=textread(fname,'%d %d %d %d %d %d %f %f %f \n','headerlines',1);
    dnum = datenum(yr,mon,day,hr,min,sec);    
catch
    fid = fopen(fname,'r');
    tline = fgetl(fid);
    i=0;
    while ~feof(fid)
        tline=fgetl(fid);
        i=i+1;
        A = sscanf(tline,'%f');
        dnum(i) = datenum(A(1),A(2),A(3),A(4),A(5),A(6)); 
        fof2(i)=A(7);
        dfof2(i)=A(8);
        hmf2(i)=A(9);     
    end
    fclose(fid);
end

Ibad = find(fof2==999.999 | dfof2==999.999 | hmf2==999.999);
fof2(Ibad)=NaN; dfof2(Ibad)=NaN; hmf2(Ibad)=NaN;

ostrc = struct('dnum',dnum,'fof2',fof2,'dfof2',dfof2,'hmf2',hmf2);

return