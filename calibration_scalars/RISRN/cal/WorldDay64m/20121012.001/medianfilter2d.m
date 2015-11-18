function  B = medianfilter2d(A,dim)
    modifyA = zeros(size(A)+2);
    B = zeros(size(A));
    
    for x = 1:size(A,1)
        for y = 1:size(A,2)
            modifyA(x+1,y+1)=A(x,y);
        end
    end

    for i = 1:size(modifyA,1)-dim(1)
        for j=1:size(modifyA,2)-dim(2)
            window = zeros([dim(1),dim(2)]);
            inc=1;
            for x=1:dim(1)
                for y=1:dim(2)
                    window(inc)=modifyA(i+x-1,j+y-1);
                    inc=inc+1;
                end
            end
            med=sort(window);
            
            B(i,j)=med(14);
        end
    end


