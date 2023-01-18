%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% RESPONSE FUNCTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [gX] = ramp(X,X_down,X_up)
    gX = NaN(size(X));
    gX(X_down >  X           ) = 0;
    gX(X_down <= X & X <=X_up) = (X(X_down<=X & X<=X_up)-X_down)/(X_up-X_down);
    gX(              X > X_up) = 1;
end