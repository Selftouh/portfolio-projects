function x =  moyenne(im)
imBis=single(im)./max(ones(size(im,1),size(im,2)),sum(im,3));
x=[mean(mean(imBis(:,:,1)));mean(mean(imBis(:,:,2)))];
end

