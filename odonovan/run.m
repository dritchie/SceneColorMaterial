%This is supplementary code for SIGGRAPH submission #248
%Color Compatibility for Large Datasets
%This code/data is not yet public. Please do not distribute.
%January 16, 2011


%set the code directory...
codeRoot= 'C:\Git\SceneColorMaterial\odonovan\'
addpath([codeRoot])
addpath([codeRoot,'data/'])
addpath([codeRoot,'circstat/'])
addpath([codeRoot,'glmnet_matlab/'])

%%choose a dataset
%dataset='mturkData'
dataset='kulerData'
%dataset='colorLoversData'
maxNumberOfDatapoints=50000;

%create the datapoints and features
datapoints = createDatapoints(dataset,maxNumberOfDatapoints);

%create a random split
datasplit=[0.6 0.4];
numPts=size(datapoints.features,1);
randomize=randperm(numPts);
trainingPts=randomize(1:(round(datasplit(1)*numPts)));
testingPts=setdiff(randomize,trainingPts);

%train LASSO regressor
options=glmnetSet();  
%this parameter set through cross-validation to minimize regression error 
%increase the value if you want a sparser solution (ie, more zero weights)
options.lambda=1.6e-004;

%This has been tested on Matlab 2008 and 2010 on windows XP.
%If you are running this on MacOS or Linux, you may have to compile the mex
%file with a fortran compiler. Please see the readme in the glmnet folder
fit = glmnet(datapoints.features(trainingPts,:), datapoints.targets(trainingPts),'gaussian',options);

%predict test set
testingTargets=datapoints.targets(testingPts);
testingPredictions = glmnetPredict(fit, 'response', datapoints.features(testingPts,:));

%compute the mean absolute error for regressor and for the fixed estimator baseline
meanAbsErr=mean(abs(testingTargets-testingPredictions))
fixedMeanAbsErr=mean(abs(testingTargets-mean(datapoints.targets(trainingPts))))

%compute the mean squared error for regressor and for the fixed estimator baseline
meanSqdErr=mean((testingTargets-testingPredictions).^2)
fixedMeanSqrErr=mean((testingTargets-mean(datapoints.targets(trainingPts))).^2)

%output weights
for i=1:length(datapoints.featureNames)
    fprintf('%s, %.2f \n ',datapoints.featureNames{i},fit.beta(i) )
end

%save fit for testing
%fit1=fit;
%save('fit1.mat','fit1');
