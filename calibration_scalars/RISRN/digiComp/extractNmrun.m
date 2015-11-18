minel=60.0;

for ifile = 1:length(fnames)
    fname=char(fnames{ifile});
    fprintf('Doing file %s\n',fname);
    
    Beamcodes=double(hdf5read(fname,'/BeamCodes')); 
    Ibeams = find(Beamcodes(3,:)>minel);
    Nbeams = length(Ibeams); 
    Ne=double(hdf5read(fname,'/FittedParams/Ne'));
    Altitude=double(hdf5read(fname,'/FittedParams/Altitude'))/1e3;
    Time=double(hdf5read(fname,'/Time/UnixTime')); mtime=mean(Time); mtime=utc2date(mtime);
    Nrecs=length(mtime);
    
    Nmax=zeros(Nbeams,Nrecs)*NaN;  Hmax=zeros(Nbeams,Nrecs)*NaN; 
    for ibms=1:Nbeams
        ibm=Ibeams(ibms);
        ihts = find(Altitude(:,ibm)>170 & Altitude(:,ibm)<450);
        tNe = Ne(ihts,ibm,:); tAlt = Altitude(ihts,ibm);
        for itime=1:Nrecs
            ttNe = squeeze(tNe(:,1,itime));
            I=find(~isnan(ttNe));
            [a,b] = max(ttNe(I));
            b=I(b);
            try
                P = polyfit(tAlt(b-1:b+1),ttNe(b-1:b+1),2);
                Hmax(ibms,itime) = -P(2)/2/P(1);
                Nmax(ibms,itime) = polyval(P,Hmax(ibms,itime));
            catch  
               %fprintf('failed\n');
            end
        end
    end
   
    meanNmax = nanmean(Nmax,1); medNmax = nanmedian(Nmax,1); stdNmax = nanstd(Nmax,1);
    meanHmax = nanmean(Hmax,1); medHmax = nanmedian(Hmax,1); stdHmax = nanstd(Hmax,1);
    
    if ifile==1
        AllNmax=medNmax;
        AllNmax1=meanNmax;
        AllHmax=medHmax;
        AllHmax1=meanHmax;
        AllStdHmax=stdHmax;
        AllStdNmax=stdNmax;
        AllTime=mtime;
    else
        AllNmax=cat(2,AllNmax,medNmax);
        AllNmax1=cat(2,AllNmax1,meanNmax);
        AllHmax=cat(2,AllHmax,medHmax);
        AllHmax1=cat(2,AllHmax1,meanHmax);
        AllStdNmax=cat(2,AllStdNmax,stdNmax);
        AllStdHmax=cat(2,AllStdHmax,stdHmax);
        AllTime=cat(2,AllTime,mtime);
    end
            
end