mezcyzna(jacek).
mezcyzna(michal).
mezcyzna(karol).
mezcyzna(czarek).
mezcyzna(roman).
mezcyzna(witold).
mezcyzna(franek).
mezcyzna(mariusz).
mezcyzna(onufry).
kobieta(ola).
kobieta(ala).
kobieta(magda).
kobieta(wanda).
kobieta(kunegunda).
kobieta(zuzanna).
kobieta(aneta).
kobieta(ewa).
kobieta(nina).
dziecko(ola,ala).
dziecko(magda,ala).
dziecko(michal,ala).
dziecko(ola,jacek).
dziecko(magda,jacek).
dziecko(michal,jacek).
dziecko(czarek,wanda).
dziecko(roman,wanda).
dziecko(czarek,karol).
dziecko(roman,karol).
dziecko(franek,kunegunda).
dziecko(zuzanna,kunegunda).
dziecko(franek,witold).
dziecko(zuzanna,witold).
dziecko(onfury,magda).
dziecko(aneta,magda).
dziecko(onfury,mariusz).
dziecko(aneta,mariusz).
dziecko(ewa,ola).
dziecko(nina,ola).
dziecko(ewa,onfury).
dziecko(nina,onfury).
syn(ala,X):-mezcyzna(X), dziecko(X,ala).
syn(jacek,X):-mezcyzna(X), dziecko(X,jacek).
syn(wanda,X):-mezcyzna(X),dziecko(X,wanda).
syn(karol,X):-mezcyzna(X),dziecko(X,karol).
syn(witold,X):-mezcyzna(X),dziecko(X,witold).
syn(kunegunda,X):-mezcyzna(X),dziecko(X,kunegunda).
syn(magda,X):-mezcyzna(X),dziecko(X,magda).
syn(mariusz,X):-mezcyzna(X),dziecko(X,mariusz).
syn(onfury,X):-mezcyzna(X),dziecko(X,onfury).
syn(ola,X):-mezcyzna(X),dziecko(X,ola).

corka(ala,X):- dziecko(X,ala), (\+ mezcyzna(X)).
corka(jacek,X):-kobieta(X), dziecko(X,jacek).
corka(wanda,X):-kobieta(X),dziecko(X,wanda).
corka(karol,X):-kobieta(X),dziecko(X,karol).
corka(witold,X):-kobieta(X),dziecko(X,witold).
corka(kunegunda,X):-kobieta(X),dziecko(X,kunegunda).
corka(magda,X):-kobieta(X),dziecko(X,magda).
corka(mariusz,X):-kobieta(X),dziecko(X,mariusz).
corka(onfury,X):-kobieta(X),dziecko(X,onfury).
corka(ola,X):-kobieta(X),dziecko(X,ola).

wnuk(ala,X):-dziecko(magda,X).
wnuk(jacek,X):-wnuk(ala,X).
wnuk(magda,X):-dziecko(onufry,X).
wnuk(mariusz,X):-wnuk(magda,X).


potomek(ala,X):-syn(ala,X) ; corka(ala,X).
potomek(jacek,X):-potomek(ala,X).
potomek(wanda,X):-syn(wanda,X) ; corka(wanda,X).
potomek(karol,X):-potomek(wanda,X).
potomek(kunegunda,X):-syn(kunegunda,X) ; corka(kunegunda,X).
potomek(witold ,X):-potomek(kunegunda,X).
potomek(magda,X):-syn(magda,X) ; corka(magda,X).
potomek(mariusz ,X):-potomek(magda,X).
potomek(ola,X):-syn(ola,X) ; corka(ola,X).
potomek(onufry  ,X):-potomek(ola,X).

dziadek(mariusz).
dziadek(jacek).
dziadek(jacek,X):-potomek(ola,X).
dziadek(mariusz,X):-potomek(onfury,X).