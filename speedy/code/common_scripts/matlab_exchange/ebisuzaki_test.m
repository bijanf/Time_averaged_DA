function [correlation,significance] = ebisuzaki_test(x,y,sig,nsim)

% EBISUZAKI - a method for estimating significant correlation values for
%             autocorrelated time series
%
%   [F] = ebisuzaki(x,y,sig,nsim)
%
% This function creates 'nsim' random time series that have the same power
% spectrum as the original time series but with random phases.
%
% Input
% x : vector of (real) numbers with the first time series 
% y : vector of (real) numbers with the second time series
% sig : [optional] significance level for critical value estimation [0.05]  
% nsim : [optional] the number of simulations [10000]
%
% Output
% F : Fraction of time series with higher correlation coefficents
%
% Ebisuzaki, W, 1997: A method to estimate the statistical 
% significance of a correlation when the data are serially correlated.  
% J. of Climate, 10, 2147-2153.
%
% modified by Kevin Anchukaitis from the original Matlab function by Vincent Moron
% and the original C code by W. Ebisuzaki. Includes 'standardize.m' as subfunction, 
% from Tapio Schneider.

% v1.0, 2008, Kevin Anchukaitis, Lamont Doherty Earth Observatory, kja@ldeo.columbia.edu
% Ebisuzaki's C code: ftp://ftp.cpc.ncep.noaa.gov/wd51we/random_phase
% v1.1: Now includes 'standardize.m' as subfunction, from Tapio Schneider.

if nargin < 4; nsim = 10000; end
if nargin < 3; sig = 0.05; end
if nargin < 2; error('Minimum input is the 2 time series vectors'); end

rand('seed',-22459); randn('seed',-22459); % Exact from Ebisuzaki's C code
%rand('twister',sum(100*clock)); randn('twister',sum(100*clock)); % Anchukaitis change

n=length(x);
n2=floor(n/2);
x=x(:); y=y(:);
x=standardize(x); y=standardize(y);

if size(x,1)~=size(y,1); error('Size of x and y must be the same'); end

[rr] = corrcoef(x,y,'rows','pairwise'); correlation = rr(2,1);

xf=fft(x); yf=fft(y);

modx=abs(xf); mody=abs(yf);  
X = NaN(size(x,1),nsim); Y = NaN(size(y,1),nsim);

%disp('Creating synthetic time series...')
for i=1:nsim; % this is very slow ...
   if n/2==n2;
      an1=angle(xf(2:n2));
      tt=randn(n2-1,1);
      anr1=(tt.*2.*pi)-pi;
      anr1=[0;anr1(:);0;flipud(anr1(:).*(-1))];
      recf=modx.*exp(sqrt(-1)*anr1); 
      X(:,i)=real(ifft(recf));
 
      an1=angle(yf(2:n2));
      tt=randn(n2-1,1);
      anr1=(tt.*2.*pi)-pi;
      anr1=[0;anr1(:);0;flipud(anr1(:).*(-1))];
      recf=mody.*exp(sqrt(-1)*anr1);
      Y(:,i)=real(ifft(recf));            
      
   else 
      an1=angle(xf(2:n2+1));
      tt=randn(n2,1);
      anr1=(tt.*2.*pi)-pi;
      anr1=[0;anr1(:);flipud(anr1(:).*(-1))];
      recf=modx.*exp(sqrt(-1)*anr1); 
      X(:,i)=real(ifft(recf));

      an1=angle(yf(2:n2+1));
      tt=randn(n2,1);
      anr1=(tt.*2.*pi)-pi;
      anr1=[0;anr1(:);flipud(anr1(:).*(-1))];
      recf=mody.*exp(sqrt(-1)*anr1); 
      Y(:,i)=real(ifft(recf));      
      
   end

[rs] = corrcoef(standardize(X(:,i)),standardize(Y(:,i))); rSim(i) = rs(2,1);

end

%disp('Performing Monte Carlo significance test')
F = sum(abs(rSim) > abs(correlation))/nsim;
significance = 1-F;
%disp('---')
%disp(['Observed correlation coefficent: ', num2str(correlation)])
%disp(['Fraction of |coefficients| larger than Observed: ',num2str(F,3), ' [Threshold Value: ', num2str(sig), ']'])

% Find critical value
%rSims = sort(abs(rSim(:))); cv = floor(nsim - (nsim*sig));
%disp(['Critical R Value: ', num2str(rSims(cv))])


%% NOTES: Compare Ebisuzaki's code with this one (use set random number seed above):
 
% bash$ r_phase 0.01 64 x y
% reading x
% reading y
% calculating
% testing x and y (64 points) for 0.010000 significance
% sample |corr| 0.078581,  fraction of samples with larger |corr| 0.519120
% critical |corr| 0.304759 at 0.010000 sig level, 50000 random samples used

% ebisuzaki(xE,yE,0.01,50000);
% Creating synthetic time series...
% Performing Monte Carlo significance test
% ---
% Observed correlation coefficent: 0.078581
% Fraction of |coefficients| larger than Observed: 0.521 [Threshold Value: 0.01]
% Critical R Value: 0.30726


function [x, xm, xs] = standardize(x, scale)
%STANDARDIZE   Centers and normalizes data.
%
%    [XC, XM, XS] = STANDARDIZE(X) centers and normalizes the data in X to
%    XC by subtracting the mean XM of each column and dividing each
%    column by its standard deviation XS. If X contains missing
%    values, indicated by NaNs, the mean and standard deviation of X
%    are computed from the available data.
%
%    [XC, XM, XS] = STANDARDIZE(X, SCALE) centers and normalizes the data
%    in X to zero mean and standard deviation SCALE. The column means
%    are returned as XM and the scale factors as XS = std(X) ./ SCALE.
%
%    Constant columns of X are not scaled.  
%
%    See also CENTER, NANMEAN, MEAN, NANSTD, STD.

  error(nargchk(1,2,nargin))          % check number of input arguments 
  
  if nargin < 2 
    scale = 1;
  end
  
 % if ndims(x) > 2,  error('X must be vector or 2-D array.'); end  
  if ndims(x) > 2,  bsxops(1); end  

  % if x is a vector, make sure it is a row vector
  if length(x)==prod(size(x))         
    x = x(:);                         
  end 
  [m,n]  = size(x);

  % get mean and standard deviation of x
  if any(any(isnan(x)))               % there are missing values in x
    xm   = nanmean(x);
    xs   = nanstd(x) ./ scale;
  else                                % no missing values
    xm   = mean(x);
    xs   = std(x) ./ scale;
  end

  % test for constant columns
  const  = (abs(xs) < eps);
  nconst = ~const;
  if sum(const) ~= 0
    warning('Constant or nearly constant columns not rescaled.');
    xm   = xm .* nconst + 0*const;
    xs   = xs .* nconst + 1*const;
  end
   
  % remove mean and divide by standard deviation
  x      = (x - repmat(xm, m, 1) ) ./ repmat(xs, m, 1);       
  




