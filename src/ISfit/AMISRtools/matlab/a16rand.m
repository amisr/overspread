function [a16code,signs]=a16rand
% function [a16code,signs]=a16rand
% Calculates randomized set of A16 codes, strong condition
%
% Octal
aoct = ['000'; '001'; '002'; '004'; '010'; '020'; '017'; '037';...
    '021'; '014'; '031'; '035'; '024'; '006'; '015'; '032'];
a = base2dec(aoct,8);

randomizer = [1 1 1 1 -1 -1 -1 1 -1 -1 1 -1 -1 1 -1 1];

for jpul=0:31
    codetmp = 0;
    signstmp = [];
    for ibaud=0:15
        if randomizer(ibaud+1)*walsh(a(ibaud+1),jpul)==1
            %signstmp = [signstmp, '+'];
            signstmp = [signstmp, 1];
            codetmp = codetmp + 2^(15-ibaud);
        else
           % signstmp = [signstmp, '-'];
            signstmp = [signstmp, -1];
        end
    end
    a16code(jpul+1,1) = codetmp;
    signs(jpul+1,:) = signstmp;
end

function s = walsh(i,j)
% function s = walsh(i,j)
% calculates elements of a walsh sign matrix for alternating codes
bita = bitand(i,j);
s = 1;
for ibit = 0:16
    if bitand(bita,2^ibit)>0
        s = -s;
    end
end
