function [normal pctExplained meanX sse] = getPlaneFeatures(X)

[signals,coeff,roots] = pca2(X');

normal = coeff(:,3);

if (normal(1)<0)
    normal = normal.*-1;
end

if (sum(roots)==0)
    pctExplained = [0 0 0];
else
    pctExplained = roots' ./ sum(roots);
end;
[n,p] = size(X);
meanX = mean(X,1);

error = abs((X - repmat(meanX,n,1))*normal);
sse = sum(error.^2);