%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% RESPONSE FUNCTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [gX] = M_cosine(X,X_down,X_up)
    gX = NaN(size(X));
    gX(X_down >  X           ) = 0;
    gX(X_down <= X & X <=X_up) = (cos(pi*((X(X_down<=X & X<=X_up)-X_down)/(X_up-X_down)+1)) + 1)/2;
    gX(              X > X_up) = 1;
end