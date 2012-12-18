function [hueFeatures]=getHueProbFeatures(hsv,satValThresh,hueProbs)
       

hues=[];


selectColors=(min(hsv(2:3,:))>=satValThresh);

hsv2=round(hsv.*repmat([359 100 100]',1,5))+1;

visHues=hsv2(1,selectColors);


hueJointList=[];
for h1=1:length(visHues)
    for h2=h1:length(visHues)

      hueJointList=[hueJointList (hueProbs.hueJoint(visHues(h2),visHues(h1)))];
      
    end
end

hueAdjList=[];
for h1=1:(length(visHues)-1)
   hueAdjList=[hueAdjList (hueProbs.hueAdjacency(visHues(h1),visHues(h1+1)))];
 
end

hueProbFeatures= getBasicStats(hueProbs.hueProb(visHues),1);
hueJointProbFeatures= getBasicStats(hueJointList,1);
hueAdjProbFeatures= getBasicStats(hueAdjList,1);


alpha = linspace(0, 2*pi, 361)';
alpha = alpha(1:end-1);
pMix=0.001*ones(size(alpha));
for j=1:length(visHues)
    pMix = pMix+ circ_vmpdf(alpha, (visHues(j)')*2*pi, 2*pi);
end

pMix=pMix./sum(pMix);
if length(visHues)~=0
    entropy=-sum(pMix.*log(pMix));
else
    %if no visible hues, set the entropy high
    entropy=5.9;
end

hueFeatures= [ hueProbFeatures hueJointProbFeatures hueAdjProbFeatures entropy];
