clear all 
close all
%%%% Illustration of Output-Oriented DEA in Matlab

%% Generate X and Y
y1 = [1, 2, 1, 0.5, 1.5, 2, 0]'; 
y2 = [2, 1, 1, 1.5, 0.5, 0, 2]'; 
x1 = [1, 1, 1, 1  , 1  , 1, 1]';

Y = [y1, y2];
X = [x1];

%% Define parameters
rts = 'CRS';
% rts = 'VRS';

M = size(Y,2); N = size(X,2); n = size(X,1);

lb = [1; zeros(n,1)];  % Lower bounds for decision variables
ub  = [];              % Upper bounds

theta_all = [];

% Choose between VRS and CRS
if strcmp('VRS', rts) 
    Aeq = [0, ones(1,n)];  % Matrix for linear equality constraints; 
    beq  = [1];            % Vector for linear equality constraint;
 elseif strcmp('CRS', rts)
    Aeq = [];              % CRS: no extra equality constraint
    beq  = [];  
end 

%% Estimate theta_j
for j=1:n 
x_j = X(j,:);  % Select x and y for a DMU of interest, j  
y_j = Y(j,:);

f = -[1; zeros(n,1)];  % Parameters of the objective function

Aineq = [zeros(N,1), X' ; y_j', -Y'];  % Matrix for linear inequality constraints
bineq = [x_j'; zeros(M,1)];            % Vector for linear inequality constraints 

[x,fval,exitflag,output,lambda] = linprog(f,Aineq,bineq,Aeq,beq,lb,ub);
   theta_DEA = -fval;  % Minus, because "min" was used instead of "max" 
        
theta_all = [theta_all; theta_DEA];  % Collect estimates

end

%% Print the results
disp('Firm #    Estimated efficiency')
disp([(1:n)', theta_all])
