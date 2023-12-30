wiekszy(A,B,A):- A>B.
wiekszy(A,B,B):- A<B.

modul(A,A):- A>0.
modul(A,N2):- N2 is A *(-1).

suma(0,0).
suma(N,S1):- N>0, N1 is N-1, suma(N1,S), S1 is N+S.