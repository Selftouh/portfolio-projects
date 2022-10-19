function [ cluster_index ] = classification_spectrale( S,k,sigma)
n=size(S,1);

S1=repmat(S,size(S,1),1);
S2=kron(S,ones(size(S,1),1));

diff=sum((S1-S2).^2,2);

A=exp(-reshape(diff,n,n)/(2*sigma^2));
A(find(eye(size(A,1))))=zeros(1,size(A,1));


A;

sqrt_D=zeros(size(A,1));
sqrt_D(find(eye(size(A,1))))=1./sqrt(sum(A,2));

sqrt_D;

L=sqrt_D*A*sqrt_D;

[X,val]=puissanceIt(L,k);
Y=X./repmat(sqrt(sum(X.^2,2)),1,size(X,2));

cluster_index=kmeans(Y,k);

end
