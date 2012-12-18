addpath('./data/');
addpath('./circstat/');
addpath('./glmnet_matlab/');

global fit1;
global hueProbs; 
global mapping;         %make variable global, avoid loading each time
load 'fit1.mat';
load hueProbsRGB;
load kulerX;

iRGB = [0.1176 0.5373 0.1294,...    %RGB of color1
        0.0863 0.2745 0.6824,...    %RGB of color2
        0.9176 0.7922 0.0314,...    %so on...
        0.6902 0.1098 0.0314,... 
        0.0863 0.2745 0.6824];

%avoid computing everytime, put in prerequisite    
y=(0:360)./360;
mapping  = spline(x,y);


% function [c, d] = math_test(a, b)
% 
% % This code is provided for example purposes only
% 
% % Copyright 2006-2010 The MathWorks, Inc.
% 
% c(1) = a(1) + b(1);
% c(2) = a(1) - b(1);
% d(1) = a(2) * b(2);
% d(2) = a(2) / b(2);
