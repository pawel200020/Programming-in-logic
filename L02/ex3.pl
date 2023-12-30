delta(A, B, C, R):- R is B*B-4*A*C.

pierw(A,B,C,W):- delta(A,B,C,D),D>0, (   W is (-B-sqrt(D))/(2*A) ; W is (-B+sqrt(D))/(2*A) ).
pierw(A,B,C,W):- delta(A,B,C,D),D=0, W is (-B)/(2*A).