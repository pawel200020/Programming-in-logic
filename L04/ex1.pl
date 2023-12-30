liczba(0).
liczba(s(X)):-liczba(X).
       
dodaj(0,Y,Y).
dodaj(s(X),Y,s(Z)) :-dodaj(X,Y,Z1), Z= s(Z1).

m(0,_,0).
m(s(X),Y,Z) :- m(X,Y,Z1), dodaj(Y,Z1,Z).