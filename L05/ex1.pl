suma1(0,0).
suma1(N,S):- N>0, N1 is N-1, 
    suma1(N1,S1), S is S1 + N.

suma(N,S) :- suma(N,0,S).
suma(0,A,A).
suma(N,A,S):- N1 is N-1, A1 is A+N, suma(N1,A1,S).

silnia(N,S):-silnia(N,1,S).
silnia(N,A,S):- N>0, N1 is N-1, A1 is A*N, silnia(N1,A1,S).

f1(0,0).
f1(1,1).
f1(N,F):- N>1, N1 is N-1, N2 is N-2, f1(N1,F1), f1(N2,F2), F is F1 + F2.

f(N,F):-f(N,0,1,F).
f(0,_A,B,B).
f(N,A,B,F):- A1 is B, B1 is A+B,N1 is N-1, f(N1,A1,B1,F).

sumy(L, D, U):-sumy(L,0,0,D,U).
sumy([], D, U, D,U).
sumy([H|T], X, Y, Z ,I):- H>0, X1 is X+H, sumy(T,X1,Y,Z,I).
sumy([H|T], X, Y, Z, I):- H<0, Y1 is Y+H, sumy(T,X,Y1,Z,I).
sumy([0|T], X, Y, Z, I):- sumy(T,X,Y,Z,I).


splaszcz([a, [1, [ b, [], c ], a, 1]], P).
















rev(X, Y) :- rev(X, [], Y).
rev( [], Y, Y).
rev([X | XL], Y, Z) :- rev(XL, [X|Y], Z).

lacz_listy([], X, X).
lacz_listy([H|T], B, [H|C]) :- lacz_listy(T, B, C).

splaszcz(L, P) :- splaszcz(L, [], P1), rev(P1, P).
splaszcz([], P, P).
splaszcz([H|T], A, P) :- is_list(H), splaszcz(H, [], P1), lacz_listy(P1, A, W), splaszcz(T, W, P).
splaszcz([H|T], A, P) :- \+ is_list(H), W = [H|A], splaszcz(T, W, P).