%iRGB: [R1 G1 B1 R2 G2 B2 R3 G3 B3 R4 G4 B4 R5 G5 B5]

function rating = getRating(iRGB)

global fit1;   %make variable global, avoid loading each time
global hueProbs;
global mapping;
%global x;

%my_features = [-0.759422962229815,-0.182406056086875,0.537254901960784,-0.204823569259001,0.849211400065557,0.682352941176471,-0.197001249153034,-0.945506880534308,0.917647058823529,0.934716579671682,-0.193550873106426,0.690196078431373,-0.204823569259001,0.849211400065557,0.682352941176471,-0.759422962229815,-0.182406056086875,0.537254901960784,-0.204823569259001,0.849211400065557,0.682352941176471,-0.204823569259001,0.849211400065557,0.682352941176471,0.934716579671682,-0.193550873106426,0.690196078431373,-0.197001249153034,-0.945506880534308,0.917647058823529,0.554599392970815,0.00782232010596654,1.13171782882472,-1.13954014893068,1.03161745615243,-1.79471828059987,0.751956007427882,1.04276227317198,0.145098039215686,0.235294117647059,-0.227450980392157,-0.00784313725490193,1.13171782882472,0.554599392970815,0.00782232010596654,-1.13954014893068,1.04276227317198,1.03161745615243,0.751956007427882,-1.79471828059987,0.235294117647059,0.145098039215686,-0.00784313725490193,-0.227450980392157,-0.0862709540458337,0.0753917980807009,0.701960784313725,0.619661160137335,0.771138372607107,0.136524668121798,-0.204823569259001,-0.182406056086875,0.682352941176471,0.934716579671682,0.849211400065557,0.917647058823529,-0.759422962229815,-0.945506880534308,0.537254901960784,1.69413954190150,1.79471828059987,0.380392156862745,0.0436987047717980,-0.0845142312560712,-0.995463594470668,0.609675532870111,0.376871945724230,0.0134525214056588,0.0536633631739972,0.707731040074575,-0.373405345424391,0.296758141756498,0.584064564656172,0.0198848445344679,-0.335184335863245,0.900419557582490,-0.0989195896664574,0.636027197213998,0.548217948365332,0.359484910968385,0.372032844800431,0.584064564656172,0.0198848445344679,-0.335184335863245,0.584064564656172,0.0198848445344679,-0.335184335863245,0.584064564656172,0.0198848445344679,-0.335184335863245,0.707731040074575,-0.373405345424391,0.296758141756498,0.548217948365332,0.359484910968385,0.372032844800431,0.900419557582490,-0.0989195896664574,0.636027197213998,-0.123666475418403,0.316354992926318,-0.352201609217158,0.0358466162908399,0.393290189958859,-0.118804434200925,0.458404500634843,-0.339600066433917,-0.631942477619742,0.971211533077243,-0.263994352413567,-0.707217180663676,0.316354992926318,0.0358466162908399,-0.123666475418403,-0.352201609217158,0.458404500634843,0.393290189958859,-0.118804434200925,-0.339600066433917,0.971211533077243,-0.263994352413567,-0.631942477619742,-0.707217180663676,0.664899535066949,-0.0146140670107055,0.126889902408888,0.144902895403067,0.263708644974236,0.440226154735963,0.584064564656172,0.0198848445344679,0.296758141756498,0.900419557582490,0.359484910968385,0.636027197213998,0.548217948365332,-0.373405345424391,-0.335184335863245,0.352201609217158,0.732890256392776,0.971211533077243,0.940119302199108,0.275822901686358,-0.200243413219960,0.723975284774879,0.256417532677937,0.0196071825471846,0.0223002812913361,0.338006230529595,0.781021897810219,0.537254901960784,0.614035087719298,0.873563218390805,0.682352941176471,0.143067846607670,0.965811965811966,0.917647058823529,0.0198412698412698,0.954545454545455,0.690196078431373,0.614035087719298,0.873563218390805,0.682352941176471,0.338006230529595,0.781021897810219,0.537254901960784,0.614035087719298,0.873563218390805,0.682352941176471,0.614035087719298,0.873563218390805,0.682352941176471,0.0198412698412698,0.954545454545455,0.690196078431373,0.143067846607670,0.965811965811966,0.917647058823529,0.276028857189703,0.470967241111629,0.123226576766400,0.405806182121972,0.0925413205805856,0.0922487474211613,-0.0112665112665112,-0.0809822361546501,0.145098039215686,0.235294117647059,-0.227450980392157,-0.00784313725490193,0.470967241111629,0.405806182121972,0.276028857189703,0.123226576766400,0.0925413205805856,0.0922487474211613,-0.0112665112665112,-0.0809822361546501,0.235294117647059,0.145098039215686,-0.00784313725490193,-0.227450980392157,0.345797104483426,0.889701150989850,0.701960784313725,0.269865840351266,0.0747156035332025,0.136524668121798,0.338006230529595,0.873563218390805,0.682352941176471,0.614035087719298,0.965811965811966,0.917647058823529,0.0198412698412698,0.781021897810219,0.537254901960784,0.594193817878028,0.184790068001747,0.380392156862745,0.326446526553493,0.226279238359844,0.0931006477428045,0.647742804508449,-1.33532538307732,0.762198587669392,-2.37406339622196,-0.434260024219471,2.85698025467772e-05,3.18643686629756e-05,2.21435161848640e-06,0.000120423417292862,-10.9771642941633,1.13573480739419,-12.6478848944903,-9.01622680425957,9.42710654117895e-06,1.18528491345816e-05,9.46499791558304e-07,2.63158942192059e-05,-12.0251220005574,1.24567667852132,-13.1494777769813,-10.5080418193577,4.60065260535442,0.117647058823529,0.537254901960784,0.129411764705882,0.0862745098039216,0.274509803921569,0.682352941176471,0.917647058823529,0.792156862745098,0.0313725490196078,0.690196078431373,0.109803921568627,0.0313725490196078,0.0862745098039216,0.274509803921569,0.682352941176471,0.917647058823529,0.792156862745098,0.0313725490196078,0.690196078431373,0.109803921568627,0.0313725490196078,0.117647058823529,0.537254901960784,0.129411764705882,0.0862745098039216,0.274509803921569,0.682352941176471,0.0862745098039216,0.274509803921569,0.682352941176471,-0.0313725490196078,0.831372549019608,-0.227450980392157,-0.603921568627451,-0.262745098039216,0.517647058823529,-0.682352941176471,0.164705882352941,0.552941176470588,-0.650980392156863,0,0.650980392156863,0.831372549019608,-0.0313725490196078,-0.227450980392157,-0.603921568627451,0.517647058823529,0.164705882352941,-0.262745098039216,-0.682352941176471,0.650980392156863,0.552941176470588,0,-0.650980392156863,0.379607843137255,0.397647058823529,0.311372549019608,0.395810431559909,0.268471401647362,0.341014160174393,0.117647058823529,0.274509803921569,0.129411764705882,0.917647058823529,0.792156862745098,0.682352941176471,0.0862745098039216,0.109803921568627,0.0313725490196078,0.831372549019608,0.682352941176471,0.650980392156863,0.605353273925108,0.121524182392921,0.786625251846065,0.742363311461311,0.164279794778289,0.0933568937604001,0.128845004046418;];
%my_rating = 2.57292832881101;

% data(:,:,1) = [0.1176    0.0863    0.9176    0.6902    0.0863];           %5 color R
% data(:,:,2) = [0.5373    0.2745    0.7922    0.1098    0.2745];           %5 color G
% data(:,:,3) = [0.1294    0.6824    0.0314    0.0314    0.6824];           %5 color B

data = zeros(1,5,3);
for i=1:3   
    data(:,:,i) = iRGB(i:3:15); 
end


%1) Copy from function [allFeatures featureNames numThemes rgbs labs] = createFeaturesFromData(data,maxFeatures)
%maxFeatures = 1;
allFeatures=[];
featureNames=[];
numThemes = 1;

% if size(maxFeatures,1)==0
%     numThemes=size(data,1);
% else
%     numThemes= min([maxFeatures size(data,1)]);
% end

% y=(0:360)./360;               
% mapping  = spline(x,y);   %avoid computing everytime, load from prerequisite

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


%[hueFeatures]=getHueProbFeatures(rand(3,5),satValThresh,hueProbs);
%% 1.1) Copy from getHueProbFeatures()     avoid function call
    %hues=[];
    hsv= rand(3,5);

    selectColors=(min(hsv(2:3,:))>=satValThresh);

%     hsv2=round(hsv.*repmat([359 100 100]',1,5))+1;        %for speed
    hsv2=round(hsv.* [359 359 359 359 359; 100 100 100 100 100; 100 100 100 100 100] )+1;

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
    if ~isempty(visHues)
        entropy=-sum(pMix.*log(pMix));
    else
        %if no visible hues, set the entropy high
        entropy=5.9;
    end

    hueFeatures= [ hueProbFeatures hueJointProbFeatures hueAdjProbFeatures entropy];


%%
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
            %hueProbFeatures(i,:)=getHueProbFeatures(hsv,satValThresh,hueProbs);           
            %% copy from getHueProbFeatures, avoid calling
                        selectColors=(min(hsv(2:3,:))>=satValThresh);

                %     hsv2=round(hsv.*repmat([359 100 100]',1,5))+1;        %for speed
                hsv2=round(hsv.* [359 359 359 359 359; 100 100 100 100 100; 100 100 100 100 100] )+1;

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

                hueProbFeatures2= getBasicStats(hueProbs.hueProb(visHues),1);
                hueJointProbFeatures= getBasicStats(hueJointList,1);
                hueAdjProbFeatures= getBasicStats(hueAdjList,1);


                alpha = linspace(0, 2*pi, 361)';
                alpha = alpha(1:end-1);
                pMix=0.001*ones(size(alpha));
                for j=1:length(visHues)
                pMix = pMix+ circ_vmpdf(alpha, (visHues(j)')*2*pi, 2*pi);
                end

                pMix=pMix./sum(pMix);
                if ~isempty(visHues)
                entropy=-sum(pMix.*log(pMix));
                else
                %if no visible hues, set the entropy high
                entropy=5.9;
                end

                hueProbFeatures(i,:)= [ hueProbFeatures2 hueJointProbFeatures hueAdjProbFeatures entropy];
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


%         means(i,:)=mean(col');
%         stddevs(i,:)=std(col');
%         medians(i,:)=median(col');
%         mins(i,:)=min(col');
%         maxs(i,:)=max(col');

        means(i,:)=mean(col,2)';
        stddevs(i,:)=std(col,[],2)';
        medians(i,:)=median(col,2)';
        mins(i,:)=min(col,[],2)';
        maxs(i,:)=max(col,[],2)';

        maxMinDiff(i,:)=maxs(i,:)-mins(i,:);
        
        
        %http://www.mathworks.com/products/statistics/demos.html?file=/products
        %/demos/shipping/stats/orthoregdemo.html
        [plane(i,1:3) plane(i,4:6) planemean plane(i,7)] = getPlaneFeatures(col');

        %sort colors
        [~, sortIdx] = sort(col(3,:));
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
   
%     first=[{[name,'-D1-C']} {[name,'-D2-C']}  {[name,'-D3-C']} ];
%     labelList=[];
%     for j=1:5
%         for i=1:3
% %             labelList= [labelList ; {[first{i},num2str(j)]}];
%             labelList= [labelList ; {[first{i},sprintf('%d', j)]}];
%         end
%     end
%     featureNames.([name,'Col'])=labelList;
    %for speed up
       featureNames.([name,'Col']) = {[name '-D1-C1'],[name '-D2-C1'],[name '-D3-C1'],...
                                    [name '-D1-C2'],[name '-D2-C2'],[name '-D3-C2'],...
                                    [name '-D1-C3'],[name '-D2-C3'],[name '-D3-C3'],...
                                    [name '-D1-C4'],[name '-D2-C4'],[name '-D3-C4'],...
                                    [name '-D1-C5'],[name '-D2-C5'],[name '-D3-C5']
                                    };
    
    
    
    
%     first=[{[name,'Sorted-D1-C']} {[name,'Sorted-D2-C']}  {[name,'Sorted-D3-C']} ];  
%     labelList=[];
%     for j=1:5
%         for i=1:3
%             labelList= [labelList; {[first{i},sprintf('%d', j)]}];
%         end
%     end
%     featureNames.([name,'SortedCol'])=labelList;
    %for speed up
       featureNames.([name,'SortedCol']) = {[name 'Sorted-D1-C1'],[name 'Sorted-D2-C1'],[name 'Sorted-D3-C1'],...
                                    [name 'Sorted-D1-C2'],[name 'Sorted-D2-C2'],[name 'Sorted-D3-C2'],...
                                    [name 'Sorted-D1-C3'],[name 'Sorted-D2-C3'],[name 'Sorted-D3-C3'],...
                                    [name 'Sorted-D1-C4'],[name 'Sorted-D2-C4'],[name 'Sorted-D3-C4'],...
                                    [name 'Sorted-D1-C5'],[name 'Sorted-D2-C5'],[name 'Sorted-D3-C5']
                                    };        



%     first=[{[name,'Diff-D1-C']} {[name,'Diff-D2-C']}  {[name,'Diff-D3-C']} ];
%     labelList=[];
%     for i=1:3
%         for  j=1:4
%             labelList= [labelList; {[first{i},sprintf('%d', j)]}];
%         end
%     end
%     featureNames.([name,'Diff'])=labelList;
        %for speed up
       featureNames.([name,'Diff']) = {[name 'Diff-D1-C1'],[name 'Diff-D1-C2'],[name 'Diff-D1-C3'],[name 'Diff-D1-C4'],...
                                            [name 'Diff-D2-C1'],[name 'Diff-D2-C2'],[name 'Diff-D2-C3'],[name 'Diff-D2-C4'],...
                                            [name 'Diff-D3-C1'],[name 'Diff-D3-C2'],[name 'Diff-D3-C3'],[name 'Diff-D3-C4'],...
                                            };
                                    
    
    
    
%     first=[{[name,'SortedDiff-D1-C']} {[name,'SortedDiff-D2-C']}  {[name,'SortedDiff-D3-C']} ];
%     labelList=[];
%     for i=1:3
%         for j=1:4
%             labelList= [labelList; {[first{i},sprintf('%d', j)]}];
%         end
%     end
%     featureNames.([name,'SortedDiff'])=labelList;
        %for speed up
       featureNames.([name,'SortedDiff']) = {[name 'SortedDiff-D1-C1'],[name 'SortedDiff-D1-C2'],[name 'SortedDiff-D1-C3'],[name 'SortedDiff-D1-C4'],...
                                            [name 'SortedDiff-D2-C1'],[name 'SortedDiff-D2-C2'],[name 'SortedDiff-D2-C3'],[name 'SortedDiff-D2-C4'],...
                                            [name 'SortedDiff-D3-C1'],[name 'SortedDiff-D3-C2'],[name 'SortedDiff-D3-C3'],[name 'SortedDiff-D3-C4'],...
                                            }; 
    
    
%     featureNames.([name,'Mean'])=mat2cell([repmat([name,'Mean-D'],3,1) num2str([1:3]')],[1 1 1]);
%     featureNames.([name,'StdDev'])=mat2cell([repmat([name,'StdDev-D'],3,1) num2str([1:3]')],[1 1 1]);
%     featureNames.([name,'Median'])=mat2cell([repmat([name,'Median-D'],3,1) num2str([1:3]')],[1 1 1]);
%     featureNames.([name,'Max'])=mat2cell([repmat([name,'Max-D'],3,1) num2str([1:3]')],[1 1 1]);
%     featureNames.([name,'Min'])=mat2cell([repmat([name,'Min-D'],3,1) num2str([1:3]')],[1 1 1]);
%     featureNames.([name,'MaxMinDiff'])=mat2cell([repmat([name,'MaxMinDiff-D'],3,1) num2str([1:3]')],[1 1 1]);

    featureNames.([name,'Mean'])={[name,'Mean-D1']; [name,'Mean-D2']; [name,'Mean-D3']};
    featureNames.([name,'StdDev'])={[name,'StdDev-D1']; [name,'StdDev-D2']; [name,'StdDev-D3']};
    featureNames.([name,'Median'])={[name,'Median-D1']; [name,'Median-D2']; [name,'Median-D3']};
    featureNames.([name,'Max'])={[name,'Max-D1']; [name,'Max-D2']; [name,'Max-D3']};
    featureNames.([name,'Min'])={[name,'Min-D1']; [name,'Min-D2']; [name,'Min-D3']};
    featureNames.([name,'MaxMinDiff'])={[name,'MaxMinDiff-D1']; [name,'MaxMinDiff-D2']; [name,'MaxMinDiff-D3']};

    
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

%% 2) Copy from createDatapoints
%set the output structure
datapoints=[];
% datapoints.rgb=rgbs;
% datapoints.lab=labs;
% datapoints.allFeatureNames=featureNames;
datapoints.allFeatures=allFeatures;
datapoints.ids = 1;
% datapoints.names ='none';
% datapoints.targets = 0;


%% 2.1) avoid calling createFeatureMatrix
%[datapoints.features datapoints.featureNames]=createFeatureMatrix(datapoints,{'*'},1);
%[datapoints.features datapoints.featureNames]=createFeatureMatrix(datapoints,{'*'},0);  %don't scale
% [datapoints.features]=createFeatureMatrix(datapoints,{'*'},0);  %don't scale
    names = fieldnames(datapoints.allFeatures);
    datapoints.features=[];
    for i=1:size(names,1)
       datapoints.features=[datapoints.features datapoints.allFeatures.(cell2mat(names(i)))];
    end

%% 3) get rating by fitting
testingPt = 1;
rating = glmnetPredict(fit1, 'response', datapoints.features(testingPt,:));

end