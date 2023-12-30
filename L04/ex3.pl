liczba(0).
liczba(s(X)):-liczba(X).
       
dodaj(0,Y,Y).
dodaj(s(X),Y,s(Z)) :-dodaj(X,Y,Z1), Z= s(Z1).

m(0,_,0).
m(s(X),Y,Z) :- m(X,Y,Z1), dodaj(Y,Z1,Z).
app([],L,L).
app([H|T],L,W):-app(T,L,W1),W=[H|W1].

last([],_):-false.
last([X],X).
last([_|T],X):-last(T,X).

////////////////uzycia
app([a,b],[a,b,c],W).
L=[a,b,c], E=x, app(L,[E],W).

last([a,b,c],X).