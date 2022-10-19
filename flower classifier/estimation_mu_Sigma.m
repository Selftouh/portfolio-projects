function [mu, Sigma] = estimation_mu_Sigma(X)
mu=mean(X)';
Sigma=(X-mu')'*(X-mu')/length(X);
end

