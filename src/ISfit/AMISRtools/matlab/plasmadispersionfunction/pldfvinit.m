% pldfvinit.m : script file to create and save the plasma dispersion function table
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%
  pldfv=pldftab(0.075,0.075,61,48,-3,0);
  save(fullfile(path_GUP,'matfiles','pldfv'),'pldfv');
  disp('pldfv saved');
