zgaduj(X,Y):-X=Y,
	write('liczba='),
    writeln(X).
zgaduj(X,Y) :- X<Y,
    Z is (X+Y)//2,
    write('czy liczba jest wieksza od '),
    write(Z), write('? '), read(A),
    sprawdz_odp(X,Y,Z,A).

sprawdz_odp(_X,Y,Z,A):-A='t', Z1 is Z+1, zgaduj(Z1,Y).
sprawdz_odp(X,_Y,Z,A):-A\='t', zgaduj(X,Z).


wygrywa(L,K,X):- member(X,L), X>=K.
wygrywa(L,K,X):- member(X,L), K1 is K-X, \+wygrywa(L,K1,_).



graj(L,K):- wygrywa(L,K,X),   
    write('Moj ruch '),writeln(X), 
    sprawdz(L,K,X).

graj(L,K):- L=[X|_],   
    write('Moj ruch '),writeln(X), 
    sprawdz(L,K,X).

sprawdz(L,K,X):- wygrywa(L,K,X), 
    X>=K,  
    write('Wygrana! ').

sprawdz(L,K,X):- wygrywa(L,K,X),
    write('Twoj ruch?: '),read(UX),
    K1 is K-X-UX, 
    graj(L,K1).

%findall([K,LX],setof( X,(numlist(1,20,_LK), member(k,_LK), wygrywa([2,5,7],K,X)),LX),Res).
