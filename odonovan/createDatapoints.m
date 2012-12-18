function [datapoints] = createDatapoints(datasetName,maxDatapoints)


load(datasetName)

%only test on themes with >=100 views
if strcmp(datasetName,'colorLoversData')
    data=data(views>=100,:,:);
    ids=ids(views>=100);
    targets=targets(views>=100);
    names=names(views>=100);
end


%Randomize the data
randomize=randperm(length(targets));
data=data(randomize,:,:);
ids=ids(randomize);
targets=targets(randomize);
names=names(randomize);

[allFeatures featureNames numThemes rgbs labs]= createFeaturesFromData(data,maxDatapoints);

%set the output structure
datapoints=[];
datapoints.rgb=rgbs;
datapoints.lab=labs;
datapoints.allFeatureNames=featureNames;
datapoints.allFeatures=allFeatures;
datapoints.ids=ids(1:numThemes);
datapoints.names=names(1:numThemes);
datapoints.targets=targets(1:numThemes);

%[datapoints.features datapoints.featureNames]=createFeatureMatrix(datapoints,{'*'},1);
[datapoints.features datapoints.featureNames]=createFeatureMatrix(datapoints,{'*'},0);      %don't scale, see  what happens





