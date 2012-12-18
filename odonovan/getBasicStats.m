function features=getBasicStats(x,addLog)  


if ~isempty(x)
    log_x=log(x+0.000001);
    features=[mean(x) std(x) min(x) max(x) mean(log_x) std(log_x) min(log_x) max(log_x) ];
else
    features=zeros(1,8);
end

features(isinf(features))=0;
features(isnan(features))=0;

