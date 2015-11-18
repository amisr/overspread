% pldftab.m : function to create the plasma dispersion function table
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%
% res=pldftab(dx,dy,nx,ny,nx1,ny1) 
%
  function res=pldftab(dx,dy,nx,ny,nx1,ny1) 
%
  res=zeros(nx*ny,1);
  for i=1:nx  
    disp(i);
    for j=1:ny 
      res(i+(j-1)*nx,1)=pldf( (nx1+i-1)*dx +sqrt(-1)*(ny1+j-1)*(-dy) );
    end  
  end
