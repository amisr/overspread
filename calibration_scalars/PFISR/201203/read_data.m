function output=read_data(filename,todo)

% Reads hdf5 file.
% Pass in filename (required).
% todo (optional input) allows you to select only certain 'trees'.
% E.g., to read only the '/S' and '/Integration' trees, pass in strvcat('/S','/Integration') for todo.
% If todo is not passed in, it defaults to reading the entire tree (entire file).
%
% Uses a recursive call to function hierc_read().
%
% ~MJN,11.15.2006


if ~exist('todo','var')
    %todo=strvcat('/S','/IncohCode','/Integration','/Rx','/Time','/Setup');
    todo='all';
end

fileinfo=hdf5info(filename);

nGroups=length(fileinfo.GroupHierarchy.Groups);
for aaa=1:nGroups
    todo2=todo;
    tname0=fileinfo.GroupHierarchy.Groups(aaa).Name;
    k=strfind(tname0,'/');
    if ~isempty(strmatch(tname0,todo,'exact')) todo2='all'; xx=1;
    else xx=strmatch(tname0(k+1:end),todo); end
    yy=strcmp(todo2,'all'); if yy==1 xx=1; end
    if ~isempty(xx) | yy==1        
        ff=hierc_read(filename,fileinfo.GroupHierarchy.Groups(aaa).Groups,fileinfo.GroupHierarchy.Groups(aaa).Datasets,todo2(xx,:));
        output.(tname0(k+1:end))=ff;
    end
end
k=1;
b=fileinfo.GroupHierarchy.Datasets;
for bbb=1:length(b)
    tt=hdf5read(filename,b(bbb).Name);
    output.(b(bbb).Name(k(end)+1:end))=tt;
end

return

    
function strct=hierc_read(fname,a,b,todo)

flip=0;
if ~isempty(b)
    for bbb=1:length(b)
        if strcmp(todo,'all')
            k=strfind(b(bbb).Name,'/');
            tt=hdf5read(fname,b(bbb).Name);
            if strfind(b(bbb).Datatype.Class,'STRING')
                strct.(b(bbb).Name(k(end)+1:end))=tt;
            else
                if flip
                    t=1:ndims(tt);
                    t(2)=1; t(1)=2;
                    tt=permute(tt,t);
                end
                strct.(b(bbb).Name(k(end)+1:end))=double(tt);
            end
        end
    end
end

if ~isempty(a)
    for aaa=1:length(a)
        k=strfind(a(aaa).Name,'/'); 
        todo2=todo; GO=1;
        if ~strcmp(todo2,'all');
            if ~isempty(strmatch(a(aaa).Name,todo,'exact'))
                todo2='all';
            elseif isempty(strmatch(a(aaa).Name,todo))
                GO=0;
            end
        end
        if GO
            hhh=hierc_read(fname,a(aaa).Groups,a(aaa).Datasets,todo2);
            strct.(a(aaa).Name(k(end)+1:end))=hhh;
        end
    end
end
