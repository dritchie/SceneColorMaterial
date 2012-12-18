
function [features featureNames offsets scales]= createFeatureMatrix(datapointObj,selectedFeature,scaleFeatures)


featureNames={};

if cell2mat(selectedFeature(1))=='*'
    names = fieldnames(datapointObj.allFeatures);
    for i=1:size(names,1)
       featureNames = [featureNames; datapointObj.allFeatureNames.(cell2mat(names(i)))];
    end
    
    
    %features=zeros(size(datapointObj.ids,1),size(featureNames,1))
    features=[];
    for i=1:size(names,1)
       features=[features datapointObj.allFeatures.(cell2mat(names(i)))];
       
      % clear datapointObj.allFeatures.(cell2mat(names(i)));
    end
else

    names= fieldnames(datapointObj.allFeatureNames);
    features=[]
    for i=1:size(selectedFeature,2)

        if isfield(datapointObj.allFeatures,selectedFeature{i})
            currFeatures=datapointObj.allFeatures.(selectedFeature{i});
            features=[features currFeatures];
            featureNames = [featureNames ;datapointObj.allFeatureNames.(selectedFeature{i})];
            
        else
            for j=1:size(names,1)
                featnames= datapointObj.allFeatureNames.(names{j});
                for k=1:size(featnames,1)
                    if strcmp(featnames{k},selectedFeature{i})

                        currFeatures=datapointObj.allFeatures.(names{j});
                        features=[features currFeatures(:,k)];
                        featureNames = [featureNames ; selectedFeature(i)];   
                    end
                end
            end
        end
        
    end

end


%features=[features features.^2];

%featureNames= [featureNames ; featureNames];

offsets=[];
scales=[];


if scaleFeatures
    %scale features to [0]1]
    for i=1:size(features,2)
        minfeat = min(features(:,i));
        maxfeat = max(features(:,i));
        
        offsets(i)=minfeat;
        scales(i)=(maxfeat-minfeat);

        features(:,i) =(features(:,i)-minfeat);
        features(:,i) =features(:,i)./(maxfeat-minfeat);
    end
end




