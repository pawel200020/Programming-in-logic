app(L1-L2,L3-L4,L5-L6):- L1=L5, L2=L3, L4=L6.
%A = [1,2,3|Z1],
%B = [4,5|Z2],
%app(A-Z1,B-Z2,Z).

f([],[]):-!.
f([H|T],L):-!, f(H,L1), f(T,L2), append(L1,L2,L).
f(X,[X]).

f3(X,L):-f2(X,L-[]).

f2([],L-L):-!.
f2([H|T],A1-B2):-!, f2(H,A1-A2), f2(T,A2-B2). %app(A1-B1,A2-B2,Z1-Z2). %wczesniej jako ostatni bylo Z1-Z2
f2(X,[X|L]-L).
%f2([a,[a,b,[c],d]],Z-[]). przerobienie na zwykła liste


mirror([],[]).
mirror([H|T],W):- mirror(T,M), append([H|M],[H],W).
%mirror([H|T],[H|M]):- mirror(T,M), append(M,[H],W). %takie samo jak linijke wyzej

mirror2([],L-L).
mirror2([H|T],[H|A]-C):- mirror2(T,A-[H|C]).  %B=[H|C], W=[H|A]-C. %app([H|A]-B,[H|C]-C,W).

%mirror2([H|T],W):- mirror2(T,A-B),  B=[H|C], W=[H|A]-C. %app([H|A]-B,[H|C]-C,W). tak bylo
%mirror2([a,b,c],Z).
rotate([],[]).
%rotate([H|T],W):- app(T,[H|C]-C, W). 
%rotate([H|T], W):- rotate(T,R),append(R,[H],W). %shift
rotate([H|T], W):- append(T,[H],W). 

rotate2(L-L,L-L).
%rotate2([H|T]-B,W):- app(T-B,[H|C]-C,W).
rotate2([H|T]-[H|C],T-C).
