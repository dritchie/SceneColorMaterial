
function [allFeatures featureNames numThemes rgbs labs] = createFeaturesFromData(data,maxFeatures)


allFeatures=[];
featureNames=[];


if size(maxFeatures,1)==0
    numThemes=size(data,1);
else
    numThemes= min([maxFeatures size(data,1)]);
end


load hueProbsRGB

load kulerX
y=(0:360)./360;
mapping  = spline(x,y);



rgbs=zeros(numThemes,15);
labs=zeros(numThemes,15);
color=zeros(numThemes,15);
sortedCol=zeros(numThemes,15);

diff=zeros(numThemes,12);
sortedDiff=zeros(numThemes,12);

means=zeros(numThemes,3);
stddevs=zeros(numThemes,3);
medians=zeros(numThemes,3);
mins=zeros(numThemes,3);
maxs=zeros(numThemes,3);
maxMinDiff=zeros(numThemes,3);

plane=zeros(numThemes,7);


satValThresh=0.2;


[hueFeatures]=getHueProbFeatures(rand(3,5),satValThresh,hueProbs);

hueProbFeatures=-99*ones(numThemes,length(hueFeatures));

for c=1:4
    if (c==1)
        name='chsv';
    elseif(c==2)
        name='lab';
    elseif(c==3)
        name='hsv';
    elseif(c==4)
        name='rgb';
    end
    
    for i=1:numThemes


        if size(data,3)==1
            rgb=data;
        else
        %Color features
            rgb = squeeze(data(i,:,:))';
        end
        
        
        numColors = sum(rgb(1,:)>=0);
        
        rgb = rgb(:,1:numColors);
   
        rgbs(i,1:(3*numColors))=rgb(:)';
        

        [hsv lab chsv] = getColorSpaces(rgb,mapping);
        
        labs(i,1:(3*numColors))=lab(:)';
        
        if strcmp(name,'chsv')
            col=chsv;
        elseif strcmp(name,'lab')
            col=lab;
        elseif strcmp(name,'hsv')
            col=hsv;
        elseif strcmp(name,'rgb')
            col=rgb;
        end
        
        color(i,1:(3*numColors))=col(:)';
  

        if strcmp(name,'hsv')
            hueProbFeatures(i,:)=getHueProbFeatures(hsv,satValThresh,hueProbs);
        end
        
        diffs=zeros(3,numColors-1);
        for j=2:numColors
            
            %if this is hsv, then do the correct wraparound diff if
            %saturated and light enough
            if strcmp(name,'hsv')
                minSatVal = min([hsv(2,j-1:j) hsv(3,j-1:j)]);
                if (minSatVal>=satValThresh)
                    pts = sort([col(1,j) col(1,j-1)]);
                    diffs(1,j-1)= min((pts(2)-pts(1)),(1-(pts(2)-pts(1))));
                end
            else
                diffs(1,j-1)=col(1,j)-col(1,j-1);
            end
           diffs(2,j-1)=col(2,j)-col(2,j-1);
           diffs(3,j-1)=col(3,j)-col(3,j-1);
        end

        diff(i,1:3*(numColors-1))=[diffs(1,:) diffs(2,:) diffs(3,:)];
        
       
        numDiffs=numColors-1;
        
        sortedDiff(i,1:numDiffs)=sort(diffs(1,:),'descend');
        sortedDiff(i,(numDiffs+1):2*numDiffs)=sort(diffs(2,:),'descend');
        sortedDiff(i,(2*numDiffs+1):3*numDiffs)=sort(diffs(3,:),'descend');


        means(i,:)=mean(col');
        stddevs(i,:)=std(col');
        medians(i,:)=median(col');
        mins(i,:)=min(col');
        maxs(i,:)=max(col');
        maxMinDiff(i,:)=maxs(i,:)-mins(i,:);
        


        
        %http://www.mathworks.com/products/statistics/demos.html?file=/products
        %/demos/shipping/stats/orthoregdemo.html
        [plane(i,1:3) plane(i,4:6) planemean plane(i,7)] = getPlaneFeatures(col');

        %sort colors
        [B sortIdx] = sort(col(3,:));
        col = col(:,sortIdx);
        sortedCol(i,1:(3*numColors))=col(:); 
        
    end 
    
    allFeatures.([name,'Col'])=color;
    allFeatures.([name,'SortedCol'])=sortedCol;
    
    allFeatures.([name,'Diff'])=diff;
    allFeatures.([name,'SortedDiff'])=sortedDiff;
   
    allFeatures.([name,'Mean'])=means;
    allFeatures.([name,'StdDev'])=stddevs;
    allFeatures.([name,'Median'])=medians;
    allFeatures.([name,'Max'])=maxs;
    allFeatures.([name,'Min'])=mins;
    allFeatures.([name,'MaxMinDiff'])=maxMinDiff;
   
    
    
    first=[{[name,'-D1-C']} {[name,'-D2-C']}  {[name,'-D3-C']} ];
  
    labelList=[];
    for j=1:5
        for i=1:3
            labelList= [labelList ; {[first{i},num2str(j)]}];
        end
    end
    
    featureNames.([name,'Col'])=labelList;
   
    
    first=[{[name,'Sorted-D1-C']} {[name,'Sorted-D2-C']}  {[name,'Sorted-D3-C']} ];
  
    labelList=[];
    for j=1:5
        for i=1:3
            labelList= [labelList; {[first{i},num2str(j)]}];
        end
    end
    
    featureNames.([name,'SortedCol'])=labelList;
    
    
    
    first=[{[name,'Diff-D1-C']} {[name,'Diff-D2-C']}  {[name,'Diff-D3-C']} ];
    labelList=[];
    for i=1:3
        for  j=1:4
            labelList= [labelList; {[first{i},num2str(j)]}];
        end
    end
    
    featureNames.([name,'Diff'])=labelList;
    
    
    first=[{[name,'SortedDiff-D1-C']} {[name,'SortedDiff-D2-C']}  {[name,'SortedDiff-D3-C']} ];
    labelList=[];
    for i=1:3
        for j=1:4
            labelList= [labelList; {[first{i},num2str(j)]}];
        end
    end
    
    featureNames.([name,'SortedDiff'])=labelList;

    featureNames.([name,'Mean'])=mat2cell([repmat([name,'Mean-D'],3,1) num2str([1:3]')],[1 1 1]);
    featureNames.([name,'StdDev'])=mat2cell([repmat([name,'StdDev-D'],3,1) num2str([1:3]')],[1 1 1]);
    featureNames.([name,'Median'])=mat2cell([repmat([name,'Median-D'],3,1) num2str([1:3]')],[1 1 1]);
    featureNames.([name,'Max'])=mat2cell([repmat([name,'Max-D'],3,1) num2str([1:3]')],[1 1 1]);
    featureNames.([name,'Min'])=mat2cell([repmat([name,'Min-D'],3,1) num2str([1:3]')],[1 1 1]);
    featureNames.([name,'MaxMinDiff'])=mat2cell([repmat([name,'MaxMinDiff-D'],3,1) num2str([1:3]')],[1 1 1]);

    
    if strcmp(name,'hsv')==0
        allFeatures.([name,'Plane'])=plane;  
        planeNames= [{[name,'PlaneNormal1']}; {[name,'PlaneNormal2']}; {[name,'PlaneNormal3']};...
             {[name,'PlaneVariance-D1']} ; {[name,'PlaneVariance-D2']}; {[name,'PlaneVariance-D3']} ;...
             {[name,'SSE']}];
    
        featureNames.([name,'Plane'])=planeNames;  
    else
        
        for i=1:size(hueProbFeatures,2)   
            hueProbFeatures((hueProbFeatures(:,i)==-99),i)=max(hueProbFeatures(:,i)) + 0.0001;
        end
        
   
        allFeatures.([name,'HueProb'])=hueProbFeatures; 
        featureNames.([name,'HueProb'])= [...
                                         getBasicStatFeatNames([name,'HueProb'],1)   ; ...
                                         getBasicStatFeatNames([name,'HueJointProb'],1) ; ...
                                         getBasicStatFeatNames([name,'HueAdjProb'],1)  ;...
                                         {[name,'Entropy']}
                                        ];  
                         
    end
    
end
