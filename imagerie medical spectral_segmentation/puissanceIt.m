function [vec,val]=puissanceIt(L,k)
[vec2,val]=eig(L);
[d,ind]=sort(diag(val),'descend');

vec=vec2(:,ind);
vec=vec(:,1:k);
size(vec)
val=d(1:k);
end

