clear all; close all;

codes8=[1 1 1 1 ;
    1 -1 1 1;
    1 1 -1 1;
    1 -1 -1 1;
    1 1 1 -1;
    1 -1 1 -1;
    1 1 -1 -1;
    1 -1 -1 -1;];
[a16code,codes16]=a16rand;
codes=codes16;

rf=3; % replication factor
lag=1;

h=[1 1 1]/rf; % receive filter

% replicate the code
codes2=zeros(size(codes,1),size(codes,2)*rf);
for aa=1:size(codes,1)
    for bb=1:size(codes,2)
        codes2(aa,1+(bb-1)*rf:bb*rf)=codes(aa,bb);
    end
end
codes=codes2;

% mess up the code
codes0=codes;
codes(3:3:end)=-1;


cc=zeros(size(codes,1),size(codes,2)/rf);
cc0=zeros(size(codes,1),size(codes,2)/rf);
for aa=1:size(codes,1)
    for bb=1:size(cc,2)
        cc(aa,bb)=sum(h.*codes(aa,(bb-1)*rf+1:(bb-1)*rf+length(h)));
        cc0(aa,bb)=sum(h.*codes0(aa,(bb-1)*rf+1:(bb-1)*rf+length(h)));
    end
     W1r(aa,:)=cc(aa,:).*[zeros(1,lag) cc(aa,1:end-lag)];
     W1r0(aa,:)=cc0(aa,:).*[zeros(1,lag) cc0(aa,1:end-lag)];
end

% %lag products
% W1r=zeros(size(codes,1),size(codes0,2)-1);
% for aa=1:size(W1r,2)
%     W1r(:,aa)=codes(:,aa).*codes(:,aa+1);
%     W1r0(:,aa)=codes0(:,aa).*codes0(:,aa+1);
% end

c1=figure; hold on;
for aa=1:size(W1r,2)
    tmp=W1r0.*repmat(W1r0(:,(aa-1)*1+1),[1 size(W1r,2)]); % decode
    tmp2=sum(tmp);
    r0(aa,:)=[0 tmp2 0];
    plot(r0(aa,:)+(aa-1)*size(codes,1),1:size(r0,2),'k');

    tmp=W1r.*repmat(W1r0(:,(aa-1)*1+1),[1 size(W1r,2)]); % decode
    tmp2=sum(tmp);
    r(aa,:)=[0 tmp2 0];
    plot(r(aa,:)+(aa-1)*size(codes,1),1:size(r,2));

    plot([(aa-1)*size(codes,1) (aa-1)*size(codes,1)],[0 size(r,2)+1],'k--');
end
title(['Lag=' num2str(lag)]);

ddd













codes16=[];
codes=codes8
codes0=codes;

rf=3; % replication factor
codes2=zeros(size(codes,1),size(codes,2)*rf);

for aa=1:size(codes,1)
    for bb=1:size(codes,2)
        codes2(aa,1+(bb-1)*rf:bb*rf)=codes(aa,bb);
    end
end
codes20=codes2; % this is what is used for decoding

% adjust transmitted signal here
%codes2(:,3:3:end)=1; % this is what is transmitted

codes=codes2;


h=ones(1,rf);

c1=figure; hold on;
c2=figure; hold on;
for aa=1:size(codes,1)
    c(aa,:)=conv(h,codes(aa,:));
    figure(c1); plot(c(aa,:)-aa*1+1);
   
    w1(aa,:)=c(aa,:).*[0 c(aa,1:end-1)];
    figure(c2); plot(w1(aa,end:-1:1)+(aa-1)*2.2,0:length(w1(aa,:))-1);
end

figure; hold on;
for aa=1:size(w1,2)
    r(aa,:)=sum(w1.*repmat(w1(:,aa),[1 size(w1,2)]))/rf;
    plot(r(aa,:)+(aa-1)*size(codes,1),1:size(r,2));
    plot([(aa-1)*size(codes,1) (aa-1)*size(codes,1)],[0 size(r,2)+1],'k--')
end

ffff


% lag products
W1r=zeros(size(codes,1),size(codes,2)-rf);
for aa=1:size(W1r,2)
    W1r(:,aa)=codes(:,aa).*codes(:,aa+rf);
    W1r0(:,aa)=codes20(:,aa).*codes20(:,aa+rf);
end

c1=figure; hold on;
for aa=1:size(W1r,2)/rf
    tmp=W1r.*repmat(W1r0(:,(aa-1)*rf+1),[1 size(W1r,2)]); % decode
    tmp2=sum(tmp);
    for bb=1:length(tmp2)/rf
        tmp3(bb)=sum(tmp2((bb-1)*rf+1:rf*bb))/rf;
    end
    r(aa,:)=[0 tmp3 0];
    plot(r(aa,:)+(aa-1)*size(codes,1),1:size(r,2));
    plot([(aa-1)*size(codes,1) (aa-1)*size(codes,1)],[0 size(r,2)+1],'k--');
end



ddd

