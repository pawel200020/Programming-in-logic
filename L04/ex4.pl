last([],_):- false.
last([X],X).
last([_|T],A) :- last(T,A).

prefix([A],[B]):- A==B.
prefix([],_).
prefix([T|A],[T|B]):- prefix(A,B). 
    
podlista(A,B):-prefix(A,B).
podlista(A.[_,T]):-podlista(A,T).

wstaw(C,[],[C]). 
wstaw(A,[H|T], [A,H|T]):- A =< H.
wstaw(A,[H|T],[H|W]):- A>H, wstaw(A,T,W).

sortuj([],[]).
sortuj([H|T],S) :- sortuj(T,TS), wstaw(H,TS,S).

%wyb([A|T],A,L):- L=T.
%wyb([H|T],A,L): wyb(T,A,[H|L]).

%permutacja(A,A).
%permutacja([H|T],P):-

del([], _, []).
del([A|T] ,A, T).
del([H|T], A, L):-A\= H, del(T,A,U), L= [H|U].

delneib([],[]).
delneib([A],[A]).
delneib([A, A|T],L):-delneib([A|T],L).
delneib([A, B|T],[A|L]):-A\=B, delneib([B|T],L).


%double2([],[]).
%double2([H|T],[W1|W2]) :- W1 is 2* X, double2(T,W2).

double2([], []).
double2([H|T], [W1|W2]) :- W1 is 2 * H, double2(T, W2).

funa([],0).
funa([H|T],X) :- H >= 0, funa(T,X).
funa([H|T],X) :- H < 0, funa(T, X1), X is X1+1.

last([],_).
last([X],X).
last([_|T],X) :-last(T,X).




