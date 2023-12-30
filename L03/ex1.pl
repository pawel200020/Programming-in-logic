krawedz(a,b).
krawedz(b,c).
krawedz(b,d).
krawedz(c,e).
krawedz(c,f).
krawedz(d,f).
krawedz(f,e).

sciezka(P,Q) :- krawedz(P,Q).
sciezka(P,Q) :- krawedz(X,Q), sciezka(P,X).

sciezka_dl(X,Y,1) :- krawedz(X,Y). 
sciezka_dl(X,Y,N) :- N>1, N1 is N - 1, krawedz(X, Z), sciezka_dl(Z, Y, N1).

dl_sciezki(X, Y, N) :- krawedz(X, Y), N is 1.
dl_sciezki(X, Y, N) :- sciezka(X,Y), krawedz(X, Z), dl_sciezki(Z, Y, N1), N is N1 + 1.


collatz(X,Y) :- X mod 2 =:= 0, Y is X/2.
collatz(X,Y) :- X mod 2 =:= 1, Y is 3*X+1.

collatz1(1) :- writeln(1).
collatz1(X) :- writeln(X), collatz(X,Y), collatz1(Y).

collatz2(1, N) :- N is 1.
collatz2(X, N) :- collatz(X,Y), collatz2(Y, N1), N is N1 + 1.


binom(K, 0, X) :- K>0, X is 1.
binom(K, K, X) :- K>0, X is 1.

binom(K, L, X) :- K1 is K-1, L1 is L-1, binom(K1, L, X1), binom(K1, L1, X2), X is X1 + X2.