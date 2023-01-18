function [response] = response_funct_vsl(x, x_bottom, x_top)
uni_vect= ones(size(x));
response = min(max((x-x_bottom*uni_vect)/abs(x_top-x_bottom),0*uni_vect),1*uni_vect);
