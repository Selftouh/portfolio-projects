
function vect_classe = classification_spectral2(data, nbClasse, sigma)
    %construction de la matrice d'affinit�
    n = size(data,1);
    A = zeros(n,n);
    for i = 1:n
        for j = i+1:n
            A(i,j) = exp(-(norm(data(i,:) - data(j,:)).^2/(2*(sigma^2))));
            A(j,i) = A(i,j);
        end
    end


    %construction de la matrice normalis�e
    D = diag(1./sqrt(sum(A)));
    L = D*A*D;


    %construction de la matrice X(puissance it�r�e � faire plus tard)
    [V,D] = eig(L);
    [~,ind] = sort(diag(D), 'descend');
    V = V(:,ind);
    X = V(:,1:nbClasse)


    %normalisation des lignes de X
    N =sqrt(sum(X.^2,2));
    Y = X./repmat(N,1,nbClasse);


    %classification avec K-means des lignes d eY
    vect_classe = kmeans(Y,nbClasse);


end