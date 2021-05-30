% Solve a Pattern Recognition Problem with a Neural Network
% Script generated by Neural Pattern Recognition app

% This script assumes these variables are defined:
%load('ASC_MLP_WHOLE.mat')
tr=table2array(readtable('dat_asc.csv'));
test = table2array(readtable('dat_asc_test.csv'));
tag =table2array(readtable('tag_ord.csv'));
x =[tr;test];
idx = 6;
if idx == 1
    x = x';
    t = tag(:,idx)';
    size_test = size(test,1);
    size_tr = size(tr,1);
else 
    x = x(sum(tag(:,[1:(idx-1)]),2)~=1,:)';
    t = tag(sum(tag(:,[1:(idx-1)]),2)~=1,idx)';
    size_test = size(test(sum(tag((size(tr,1)+1):size(tag,1),[1:(idx-1)]),2)~=1,:),1);
    size_tr = size(tr(sum(tag(1:size(tr,1),[1:(idx-1)]),2)~=1,:),1);
end   
 %x=mlpsubgroup20(:,[1:621])';
 
%x = x(sum(tag(:,[1:idx]),2)~=1,:)';
 %x = tr';

 %x = tr(sum(tag(:,[1:idx]),2)~=1,:)';
 %x = tr(tag(:,[1:6])~= 1,:)';%(:,[1:420])';
 %t = mlpsubgroup20(:,[622:641])';
 %t = tag(:,1)';
%t = tag(sum(tag(:,[1:idx]),2)~=1,(idx+1))';
%t = tag(tag(:,13)~= 1,12)';
%x(isnan(x))=0;

% Choose a Training Function
% For a list of all training functions type: help nntrain
% 'trainbr' takes longer but may be better for challenging problems.
% 'trainscg' uses less memory. Suitable in low memory situations.
trainFcn = 'trainscg'; 
%trainFcn = 'traingdm';  % Scaled conjugate gradient backpropagation.
%trainFcn = 'trainbr'; 
% Create a Pattern Recognition Network
hiddenLayerSize = 128;%[256,64];
net = patternnet(hiddenLayerSize, trainFcn);
net.trainParam.max_fail = 50;
net.trainParam.epochs = 1000;
net.trainParam.goal	= 0;
% Choose Input and Output Pre/Post-Processing Functions
% For a list of all processing functions type: help nnproces
% 'trainlm' is usually fastest.s
net.input.processFcns = {'removeconstantrows','mapminmax'};

% Setup Division of Data for Training, Validation, Testing
% For a list of all data division functions type: help nndivision
net.divideFcn = 'divideblock';%'dividerand';  % Divide data randomly
net.divideMode = 'sample';  % Divide up every sample

net.divideParam.trainRatio = size_tr/(size_test+size_tr)*0.8;
net.divideParam.valRatio = size_tr/(size_test+size_tr)*0.2;
%net.divideParam.testRatio = 8.28/100;
net.divideParam.testRatio = size_test/(size_test+size_tr);

% Choose a Performance Function
% For a list of all performance functions type: help nnperformance
net.performFcn = 'crossentropy';  % Cross-Entropy
net.performParam.regularization = 0.5;
%net.performParam.normalization = 'standard';
% Choose Plot Functions
% For a list of all plot functions type: help nnplot
net.plotFcns = {'plotperform','plottrainstate','ploterrhist', ...
    'plotconfusion', 'plotroc'};

% Train the Network
[net,tr] = train(net,x,t);

% Test the Network
y = net(x);
e = gsubtract(t,y);
 %weights = [0.5,2,0.5,0.5,0.5,0.5,1,0.5,1,0.5,1,1];%[1 ;repmat(1/65,65,1);repmat(1/15,15,1);repmat(1/5,55,1);...
%     repmat(1/25,50,1);repmat(1/65,65,1);repmat(1/105,105,1);...
%     repmat(1/88,88,1);repmat(1/8,8,1);repmat(1/13,13,1);...
%     repmat(1/5,5,1);repmat(1/15,15,1);]
performance = perform(net,t,y);%,weights);
tind = vec2ind(t);
yind = vec2ind(y);
percentErrors = sum(tind ~= yind)/numel(tind);

% Recalculate Training, Validation and Test Performance
trainTargets = t .* tr.trainMask{1};
valTargets = t .* tr.valMask{1};
testTargets = t .* tr.testMask{1};
trainPerformance = perform(net,trainTargets,y);
valPerformance = perform(net,valTargets,y);
testPerformance = perform(net,testTargets,y);

% View the Network
view(net)

% Plots
% Uncomment these lines to enable various plots.
%figure, plotperform(tr)
%figure, plottrainstate(tr)
%figure, ploterrhist(e)
%figure, plotconfusion(t,y)
%figure, plotroc(t,y)

% Deployment
% Change the (false) values to (true) to enable the following code blocks.
% See the help for each generation function for more information.
if (false)
    % Generate MATLAB function for neural network for application
    % deployment in MATLAB scripts or with MATLAB Compiler and Builder
    % tools, or simply to examine the calculations your trained neural
    % network performs.
    genFunction(net,'myNeuralNetworkFunction');
    y = myNeuralNetworkFunction(x);
end
if (false)
    % Generate a matrix-only MATLAB function for neural network code
    % generation with MATLAB Coder tools.
    genFunction(net,'myNeuralNetworkFunction','MatrixOnly','yes');
    y = myNeuralNetworkFunction(x);
end
if (false)
    % Generate a Simulink diagram for simulation or deployment with.
    % Simulink Coder tools.
    gensim(net);
end